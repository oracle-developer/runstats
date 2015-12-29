create or replace package runstats_pkg authid current_user as

   /*
   || ----------------------------------------------------------------------------
   ||
   || Name:        RUNSTATS_PKG
   ||
   || Script:      runstats_pkg.sql
   ||
   || Version:     2.01
   ||
   || Author:      Adrian Billington
   ||              www.oracle-developer.net
   ||              (c) oracle-developer.net 
   ||
   || Description: PL/SQL-only variation on Tom Kyte's RUNSTATS test harness.
   ||              Output is similar in format and structure but this version has
   ||              more reporting options and additional reports (see Key
   ||              Differences below). Runtimes are the same.
   ||
   ||              Key Differences
   ||              ---------------
   ||
   ||                 a) All logic is encapsulated in a single PL/SQL package
   ||                    (no runstats global temporary table or stats view);
   ||
   ||                 b) This uses invoker rights and dynamic SQL to workaround
   ||                    the sites where developers cannot get explicit grants
   ||                    on the required V$ views, but instead have access via
   ||                    roles or other privileges;
   ||
   ||                 c) There is a new option to pause and resume runstats in
   ||                    between runs. This is useful, for example, when you 
   ||                    need to reset some data before the second run. See 
   ||                    usage notes below for details;
   ||
   ||                 d) There is a new set of advanced reporting options (see
   ||                    Usage section below);
   ||
   ||                 e) There is a new time model report;
   ||                    
   ||                 f) This requires at least version 10.1 to run because it
   ||                    makes use of collection methods such as MEMBER OF and
   ||                    also reports on V$SESS_TIME_MODEL statistics.
   ||
   ||                 g) Because this version uses associative arrays, a small
   ||                    "primer" procedure exists in the body to minimise the
   ||                    potential side-effects on PGA memory reporting. As a 
   ||                    result, PGA memory reporting when using this package
   ||                    should be valid (i.e. incurred by whatever is being
   ||                    tested and not from the intrusion of RUNSTATS_PKG).
   ||
   || Usage:       Standard Runstats
   ||              -------------------------------------------------------------
   ||              BEGIN
   ||                 runstats_pkg.rs_start;
   ||                 --<do run 1>--
   ||                 runstats_pkg.rs_middle;
   ||                 --<do run 2>--
   ||                 runstats_pkg.rs_stop(<reporting options>, <include time model>);
   ||              END;
   ||
   ||              Resumable Runstats
   ||              -------------------------------------------------------------
   ||              BEGIN
   ||                 runstats_pkg.rs_start;
   ||                 --<do run 1>--
   ||                 runstats_pkg.rs_pause;
   ||                 --<do some work e.g. to reset test data>--
   ||                 runstats_pkg.rs_resume;
   ||                 --<do run 2>--
   ||                 runstats_pkg.rs_stop(<reporting options>, <include time model>);
   ||              END;
   ||
   ||              This mode will raise one of the following exceptions if not
   ||              used as above:
   ||
   ||                -20000: Attempt to resume runstats when it was 
   ||                        never paused.
   ||
   ||                -20001: Attempt to stop a paused runstats that was never
   ||                        resumed.
   ||
   ||              Reporting Options
   ||              -------------------------------------------------------------
   ||              1. Output all statistics:
   || 
   ||                   runstats_pkg.rs_stop;
   ||
   ||              2. Output all statistics with delta value of at least 1,000:
   ||
   ||                   runstats_pkg.rs_stop(1000);
   ||
   ||              3. Output statistics for given statistic names only:
   ||
   ||                   runstats_pkg.rs_stop(
   ||                      runstats_pkg.statname_ntt('redo size','user commits')
   ||                      );
   ||
   ||              4. Output statistics for statistics containing search phrase:
   ||
   ||                    runstats_pkg.rs_stop('memory');
   ||
   ||              5. Suppress the time model report (can be applied to examples
   ||                 2, 3 and 4):
   ||
   ||                    runstats_pkg.rs_stop(1000, p_include_time_model => false)
   ||
   || Notes:       1. Serveroutput must be on (and set higher than default);
   ||
   ||              2. A free-standing, SQL*Plus-script version of RunStats is also 
   ||                 available. The script version works without creating any
   ||                 database objects.
   ||
   || Disclaimer:  http://www.oracle-developer.net/disclaimer.php
   ||
   || ----------------------------------------------------------------------------
   */

   type statname_ntt is table of varchar2(64);

   procedure rs_start;
   procedure rs_middle;
   procedure rs_pause;
   procedure rs_resume; 

   procedure rs_stop;

   procedure rs_stop( p_difference_threshold in integer,
                      p_include_time_model   in boolean default true );

   procedure rs_stop( p_statnames          in runstats_pkg.statname_ntt,
                      p_include_time_model in boolean default true );

   procedure rs_stop( p_statname_like      in varchar2,
                      p_include_time_model in boolean default true );

end;
/

create or replace package body runstats_pkg as

   /*
   || Array offsets into the main runstats array, used to
   || determine the start and end points of a run...
   */
   c_run1 constant pls_integer := 1;
   c_run2 constant pls_integer := 2;
   c_run3 constant pls_integer := 3;
   c_run4 constant pls_integer := 4;

   /*
   || Array offsets for time snapshots...
   */
   c_run1_start constant pls_integer := 1;
   c_run1_end   constant pls_integer := 2;
   c_run2_start constant pls_integer := 3;
   c_run2_end   constant pls_integer := 4;

   /*
   || Discriminators for cpu or ela time...
   */
   c_ela constant pls_integer := 1;
   c_cpu constant pls_integer := 2;

   /*
   || Timer record and array type for capturing accurate timings...
   */
   type rt_timer is record
   ( ela_time integer
   , cpu_time integer );

   type aat_timer is table of rt_timer
      index by pls_integer;

   /*
   || A range of (sub)types for capturing statistics information...
   */
   subtype st_stattype  is varchar2(6);
   subtype st_statname  is varchar2(64);
   subtype st_statvalue is integer;
   subtype st_output    is varchar2(255);

   type rt_statistic is record
   ( type  st_stattype
   , name  st_statname
   , value st_statvalue );

   type aat_statistic is table of rt_statistic
      index by st_statname;

   type aat_runstats is table of aat_statistic
      index by pls_integer;

   /*
   || This is the "runstats array". It will hold 3 sets of statistics on 
   || a standard "start-middle-stop" run, or 4 sets of statistics on a 
   || "start-pause-resume-stop" run...
   */
   ga_runstats aat_runstats;

   /*
   || Global timer...
   */
   ga_timer    aat_timer;

   /*
   || Booleans to enable pausing and resuming between runs...
   */
   g_paused    boolean := false;
   g_resumed   boolean := false;

   -----------------------------------------------------------------------------
   procedure rs_info is
   begin
      dbms_output.put_line('- RunStats v2.01 by Adrian Billington ' ||
                           '(http://www.oracle-developer.net)');
      dbms_output.put_line('- Based on the original RUNSTATS utility by Tom Kyte');
   end rs_info;

   -----------------------------------------------------------------------------
   procedure rs_snap( p_run      in pls_integer,
                      p_runstats in out nocopy aat_runstats ) is

      rc_stat sys_refcursor;
      r_stat  rt_statistic;

   begin
      /*
      || Dynamic SQL (combined with invoker rights in the spec) works around
      || the need to have explicit select granted on the referenced v$ views.
      || Of course, we still need access granted via a role or other privilege
      || but I've always been able to get the latter and rarely the former.
      ||
      || Using a ref cursor and loop rather than bulk collect to minimise
      || memory stats interference...
      */
      open rc_stat for q'[select 'STAT' as type
                          ,      a.name
                          ,      b.value
                          from   v$statname a
                          ,      v$mystat   b
                          where  a.statistic# = b.statistic#
                          union all
                          select 'LATCH'
                          ,      name
                          ,      gets 
                          from   v$latch
                          union all
                          select 'TIME'
                          ,      stat_name
                          ,      value
                          from   v$sess_time_model
                          where  sid = sys_context('userenv','sid')]';
      loop
         fetch rc_stat into r_stat;
         exit when rc_stat%notfound;
         p_runstats(p_run)(r_stat.name).type := r_stat.type;
         p_runstats(p_run)(r_stat.name).value := r_stat.value;
      end loop;
      close rc_stat;
   end rs_snap;

   -----------------------------------------------------------------------------
   procedure rs_timer( p_run   in pls_integer,
                       p_timer in out nocopy aat_timer ) is
   begin
      p_timer(p_run).ela_time := dbms_utility.get_time; 
      p_timer(p_run).cpu_time := dbms_utility.get_cpu_time;
   end rs_timer;

   -----------------------------------------------------------------------------
   procedure rs_reset is
   begin
      ga_runstats.delete;
      ga_timer.delete;
      g_paused := false;
      g_resumed := false;
   end rs_reset;

   -----------------------------------------------------------------------------
   /*
   || This procedure exists purely to add some PGA memory to our session
   || before we start filling associative arrays with runstats data. This
   || will prevent the PGA memory associated stats from showing up in the
   || report purely as a result of this package. We'll load up four sets of
   || stats as this will roughly cover the stat-snaps during the use of
   || runstats...
   */
   procedure rs_prime is
      aa_dummy aat_runstats;
   begin
      rs_snap(1,aa_dummy);
      for i in 2 .. 4 loop
         aa_dummy(i) := aa_dummy(1);
      end loop;
   end rs_prime;

   -----------------------------------------------------------------------------
   procedure rs_report ( p_difference_threshold in integer default null,
                         p_statnames            in runstats_pkg.statname_ntt default null,
                         p_statname_like        in varchar2 default null,
                         p_include_time_model   in boolean default true ) is

      ix st_statname;      --<-- statname; offset for runstats array
      s1 pls_integer;      --<-- array offset for start of run1
      e1 pls_integer;      --<-- array offset for end of run1
      s2 pls_integer;      --<-- array offset for start of run2
      e2 pls_integer;      --<-- array offset for end of run2

      r1 integer;          --<-- holds value of run1 statistic
      r2 integer;          --<-- holds value of run2 statistic

      t1 rt_timer;         --<-- holds value of run1 timer
      t2 rt_timer;         --<-- holds value of run2 timer

      l1 integer := 0;     --<-- keeps running total of latching in run1
      l2 integer := 0;     --<-- keeps running total of latching in run2

      st st_stattype;      --<-- current statistic type (i.e. STAT, LATCH, TIME)

      n  pls_integer := 0; --<-- report section number

      /*
      || Downside of using associative arrays is that we have to sort
      || the output. So here's a couple of types and a variable to enable us 
      || to do that...
      */
      type aat_runstats_output is table of st_output
         index by st_statname;
      type aat_runstats_sorted is table of aat_runstats_output
         index by pls_integer;
      aa_runstats_sorted aat_runstats_sorted; --<-- stat/latch statistics
      aa_stmstats_sorted aat_runstats_sorted; --<-- time model statistics
      aa_tmrstats_sorted aat_runstats_sorted; --<-- timer statistics
      
      /*
      || Procedure to add a statistic to the sorted runstats or time model stats array...
      */
      procedure s ( p_stattype in st_stattype,
                    p_statname in st_statname,
                    p_r1_value in integer,
                    p_r2_value in integer ) is

         v_offset pls_integer;
         v_output st_output;

      begin

         /*
         || Workaround the offset limits of a PLS_INTEGER...
         */
         v_offset := least(abs(p_r2_value-p_r1_value),2147483647);

         v_output := rpad(p_stattype, 6) || rpad(p_statname, 50)  || ' ' ||
                     to_char(p_r1_value,'999,999,999' )           || ' ' ||
                     to_char(p_r2_value,'999,999,999' )           || ' ' ||
                     to_char(p_r2_value-p_r1_value,'999,999,999');

         case p_stattype
            when 'TIME' then
               aa_stmstats_sorted(v_offset)(p_statname) := v_output;
            when 'TIMER' then
               aa_tmrstats_sorted(v_offset)(p_statname) := v_output;
            else
               aa_runstats_sorted(v_offset)(p_statname) := v_output;
         end case;

      end s;

      /*
      || Small function to calculate the statistic for a run. Calculates 
      || the difference between the start and end offset for a run in the
      || runstats array...
      */
      function f ( p_start_offset in pls_integer,
                   p_end_offset   in pls_integer,
                   p_statname     in st_statname ) return number is
      begin
         return ga_runstats(p_end_offset)(p_statname).value - 
                ga_runstats(p_start_offset)(p_statname).value;
      end f;

      /*
      || Small function to return the difference in two timer values...
      */
      function t ( p_start_offset in pls_integer,
                   p_end_offset   in pls_integer ) return rt_timer is
         r_timer rt_timer;
      begin
         r_timer.ela_time := ga_timer(p_end_offset).ela_time - ga_timer(p_start_offset).ela_time;
         r_timer.cpu_time := ga_timer(p_end_offset).cpu_time - ga_timer(p_start_offset).cpu_time;
         return r_timer;
      end t;

      /*
      || Report formatting procedures...
      */
      procedure div( p_divider in varchar2 default '-',
                     p_width   in pls_integer default 95 ) is
      begin
         dbms_output.put_line( rpad(p_divider, p_width, p_divider) );
      end div;

      procedure nl( p_newlines in pls_integer default 1 ) is
      begin
         for i in 1 .. p_newlines loop
            dbms_output.put_line(null);
         end loop;
      end nl;

      procedure sh ( p_title  in varchar2,
                     p_header in boolean default true ) is
      begin
         n := n + 1;
         nl(2);
         div;
         dbms_output.put_line(to_char(n) || '. ' || p_title);
         div;
         if p_header then
            nl;
            dbms_output.put_line(rpad('Type',6) || rpad('Name',50) || lpad('Run1',13) ||
                                 lpad('Run2',13) || lpad('Diff',13));
            dbms_output.put_line(rpad('-',5,'-') || ' ' || rpad('-',50,'-') || ' ' ||
                                 rpad('-',12,'-') || ' ' || rpad('-',12,'-') || ' ' || 
                                 rpad('-',12,'-'));
         end if;
      end sh;

      /*
      || Procedure to output the sorted runstats or time model stats array...
      */
      procedure o ( p_runstats in out nocopy aat_runstats_sorted ) is
         i1 pls_integer;
         i2 st_statname;
      begin
         i1 := p_runstats.first;
         while i1 is not null loop
            i2 := p_runstats(i1).first;
            while i2 is not null loop
               dbms_output.put_line( p_runstats(i1)(i2) );
               i2 := p_runstats(i1).next(i2);
            end loop;
            i1 := p_runstats.next(i1);
         end loop;
         p_runstats.delete;
      end o;

   begin

      /*
      || Set up the run1 and run2 offsets into the runstats array according
      || to whether the middle section was a straight run through or paused...
      */
      s1 := c_run1;
      e1 := c_run2;

      if not (g_paused and g_resumed) then
         s2 := c_run2;
         e2 := c_run3;
      else
         s2 := c_run3;
         e2 := c_run4;
      end if;

      /*
      || Report header...
      */
      nl;
      div('=');
      dbms_output.put_line('RunStats report : ' || 
                              to_char(sysdate,'dd-MON-YYYY hh24:mi:ss'));
      div('=');

      /*
      || Summary timings report...
      */
      sh('Summary timings');

      /*
      || Output the elapsed timings...
      */
      t1 := t(c_run1_start,c_run1_end);
      t2 := t(c_run2_start,c_run2_end);
      s('TIMER','elapsed time (hsecs)',t1.ela_time,t2.ela_time);
      s('TIMER','cpu time (hsecs)',t1.cpu_time,t2.cpu_time);
      o(aa_tmrstats_sorted);
      nl;
      dbms_output.put_line( 'Comments:' );
      dbms_output.put_line( '1) ' || case 
                                        when t1.ela_time = t2.ela_time
                                        then 'Run1 took the same time as Run2'
                                        when t2.ela_time > t1.ela_time
                                        then 'Run1 was ' || round((1-(t1.ela_time/t2.ela_time))*100,1) ||
                                             '% quicker than Run2'
                                        else 'Run2 was ' || round((1-(t2.ela_time/t1.ela_time))*100,1) ||
                                             '% quicker than Run1'
                                     end );
      dbms_output.put_line( '2) ' || case
                                        when t1.cpu_time = t2.cpu_time
                                        then 'Run1 used the same amount of CPU time as Run2'
                                        when t2.ela_time > t1.ela_time
                                        then 'Run1 used ' || round((1-(t1.ela_time/t2.ela_time))*100,1) ||
                                             '% less CPU time than Run2'
                                       else 'Run2 used ' || round((1-(t2.ela_time/t1.ela_time))*100,1) ||
                                             '% less CPU time than Run1'
                                     end );

      /*
      || Now prepare the main stats/latch output...
      */
      ix := ga_runstats(s1).first;
      while ix is not null loop

         /*
         || Store the stattype...
         */
         st := ga_runstats(s1)(ix).type;

         /*
         || Calculate the value of the current statistic for run1 and run2...
         */
         r1 := f(s1,e1,ix);
         r2 := f(s2,e2,ix);

         /*
         || If it's a statistic we want to output, then output it. Now here's the 
         || downside of using purely associative arrays - we don't have any easy way of sorting.
         || So we have to do it ourselves. A bit of a pain, but simple enough...
         */
         if (    st = 'TIME'
             and abs(r2-r1) > 0)
         or (    st != 'TIME'
             and (   (p_difference_threshold is not null and abs(r2-r1) >= p_difference_threshold)
                  or (p_statnames is not empty and ix member of p_statnames)
                  or (p_statname_like is not null and ix like '%'||p_statname_like||'%')))
         then 
            s(st,ix,r1,r2);
         end if;
         
         /*
         || Keep a running tally of latching...
         */
         if st = 'LATCH' then
            l1 := l1 + r1;
            l2 := l2 + r2;
         end if;

         /*
         || Next statname please...
         */ 
         ix := ga_runstats(s1).next(ix);

      end loop;

      /*
      || Now we can output the sorted runstats information and save off the time model statistics...
      */
      sh('Statistics report');
      o(aa_runstats_sorted);
      
      /*
      || Total latches report...
      */
      sh('Latching report');
      s('LATCH','total latches used',l1,l2);
      o(aa_runstats_sorted);
      nl;
      dbms_output.put_line('Comments:');
      dbms_output.put_line( '1) ' || case 
                                        when l1 = l2
                                        then 'Run1 used the same number of latches as Run2'
                                        when l2 > l1
                                        then 'Run1 used ' || round((1-(l1/l2))*100,1) ||
                                             '% fewer latches than Run2'
                                        else 'Run2 used ' || round((1-(l2/l1))*100,1) ||
                                             '% fewer latches than Run1'
                                     end );

      /*
      || Time model report...
      */
      if p_include_time_model then
         sh('Time model report');
         o(aa_stmstats_sorted);
      end if;

      sh('About', false);
      rs_info;

      nl;
      div('=');
      dbms_output.put_line('End of report');
      div('=');

   end rs_report;

   -----------------------------------------------------------------------------
   procedure rs_start is
   begin
      rs_reset;
      rs_prime;
      rs_snap(c_run1, ga_runstats);
      rs_timer(c_run1_start, ga_timer);
   end rs_start;

   -----------------------------------------------------------------------------
   procedure rs_middle is
   begin
      rs_timer(c_run1_end, ga_timer);
      rs_snap(c_run2, ga_runstats);
      g_paused := false;
      rs_timer(c_run2_start, ga_timer);
   end rs_middle;

   -----------------------------------------------------------------------------
   procedure rs_pause is
   begin
      rs_timer(c_run1_end, ga_timer);
      rs_snap(c_run2, ga_runstats);
      g_paused := true;
   end rs_pause;

   -----------------------------------------------------------------------------
   procedure rs_resume is
   begin
      if g_paused then
         rs_snap(c_run3, ga_runstats);
         g_resumed := true;
         rs_timer(c_run2_start, ga_timer);
      else
         raise_application_error(-20000, 'Runstats was not paused. Nothing to resume!' );
      end if;
   end rs_resume;

   -----------------------------------------------------------------------------
   procedure rs_stop_internal( p_difference_threshold in integer default null,
                               p_statnames            in runstats_pkg.statname_ntt default null,
                               p_statname_like        in varchar2 default null,
                               p_include_time_model   in boolean default true ) is
   begin
      if (g_paused=g_resumed) then
         rs_timer(c_run2_end, ga_timer);
         rs_snap(case
                    when not (g_paused and g_resumed)
                    then c_run3
                    else c_run4
                 end, ga_runstats);
         case
            when p_difference_threshold is not null
            then rs_report(p_difference_threshold => p_difference_threshold,
                           p_include_time_model   => p_include_time_model);
            when p_statnames is not null
            then rs_report(p_statnames          => p_statnames,
                           p_include_time_model => p_include_time_model);
            when p_statname_like is not null
            then rs_report(p_statname_like      => p_statname_like,
                           p_include_time_model => p_include_time_model);
            else rs_report(p_include_time_model => p_include_time_model);
         end case;
         rs_reset;
      elsif (g_paused and not g_resumed) then
         raise_application_error(-20001, 'Runstats was paused but not resumed. Cannot continue.' );
      end if;
   end rs_stop_internal;

   -----------------------------------------------------------------------------
   procedure rs_stop is
   begin
      rs_stop_internal(p_difference_threshold => 0,
                       p_include_time_model   => true);
   end rs_stop;

   procedure rs_stop( p_difference_threshold in integer,
                      p_include_time_model   in boolean default true ) is
   begin
      rs_stop_internal(p_difference_threshold => p_difference_threshold,
                       p_include_time_model   => p_include_time_model );
   end rs_stop;

   ------------------------------------------------------------------------------
   procedure rs_stop( p_statnames          in runstats_pkg.statname_ntt,
                      p_include_time_model in boolean default true ) is
   begin
      rs_stop_internal(p_statnames          => p_statnames,
                       p_include_time_model => p_include_time_model);
   end rs_stop;

   ------------------------------------------------------------------------------
   procedure rs_stop( p_statname_like      in varchar2,
                      p_include_time_model in boolean default true ) is
   begin
      rs_stop_internal(p_statname_like      => p_statname_like,
                       p_include_time_model => p_include_time_model );
   end rs_stop;

end runstats_pkg;
/

create or replace public synonym runstats_pkg for runstats_pkg;
grant execute on runstats_pkg to public;

