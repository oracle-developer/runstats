

-- ----------------------------------------------------------------------------------------------
--
-- Utility:         RunStats
--
-- Script:          runstats.sql
--
-- Version:         2.02
--
-- Author:          Adrian Billington
--                  www.oracle-developer.net
--                  (c) oracle-developer.net
--
-- Description:     SQL*Plus-only variation on the RunStats utility, based on Tom Kyte's
--                  original tool of the same name. This runs as a standalone script (i.e.
--                  no database objects need to be created).
--
--                  Output is similar in format and structure but this version has more
--                  reporting options and additional reports (see Key Differences below).
--
--                  Key Differences
--                  ---------------
--
--                     a) This is a SQL*Plus script that requires no objects to be created;
--
--                     b) There is a new option to pause and resume runstats in
--                        between runs. This is useful, for example, when you
--                        need to reset some data before the second run. See
--                        usage notes below for details;
--
--                     c) There is a new set of advanced reporting options (see
--                        Usage section below);
--
--                     d) There is a new time model report;
--
--                     e) This requires at least version 10.1 to run because it
--                        makes use of collection methods such as MEMBER OF and
--                        also reports on V$SESS_TIME_MODEL statistics.
--
-- Configuration:   Edit the c_ms_rmcmd variable in the Constants section at the start of this
--                  script to use the correct file deletion command for your SQL*Plus client
--                  platform. It is defaulted to a Windows "del" command, so you will need to
--                  change it if you are using a Linux/Unix SQL*Plus client.
--
--                  Reason: To make this run in standalone mode, a couple of temporary files
--                  are written to your current directory. These files are automatically
--                  removed on completion of this script.
--
-- Usage:           Standard Runstats
--                  -------------------------------------------------------------
--                  @runstats start
--                  --<do run 1>--
--                  @runstats middle
--                  --<do run 2>--
--                  @runstats stop [reporting options] [include time model]
--                  END;
--
--                  Resumable Runstats
--                  -------------------------------------------------------------
--                  @runstats start
--                  --<do run 1>--
--                  @runstats pause
--                  --<do some work e.g. to reset test data>--
--                  @runstats resume
--                  --<do run 2>--
--                  @runstats stop [reporting options] [include time model]
--
--                  Optional Reporting Parameter Formats
--                  ------------------------------------
--
--                  Position Short Format      Long Format Equivalent
--                  -------- ---------------   ----------------------
--                  2        t=<number>        threshold=<number>
--                  2        l=<string>        like=<string>
--                  2        n=<string list>   names=<string list>
--                  3        t or f            true or false
--
--                  Use double-quotes when the strings contain spaces.
--
-- Examples:        Reporting Options (using short formats)
--                  -------------------------------------------------------------
--                  1. Output all statistics:
--
--                       @runstats stop
--
--                  2. Output all statistics with delta value of at least 1,000:
--
--                       @runstats stop t=1000
--
--                  3. Output statistics for given statistic names only:
--
--                       @runstats stop "n=redo size,user commits"
--
--                  4. Output statistics for statistics containing search phrase:
--
--                        @runstats stop l=memory
--
--                  5. Suppress the time model report (all formats):
--
--                        @runstats stop "" false
--                        @runstats stop t=1000 false
--                        @runstats stop "n=redo size" false
--                        @runstats stop l=memory false
--
-- Notes:           1. As described in Configuration above, this script writes and removes
--                     a couple of temporary files during execution.
--
--                  2. A PL/SQL package version of RunStats is also available.
--
-- Disclaimer:      http://www.oracle-developer.net/disclaimer.php
--
-- ----------------------------------------------------------------------------------------------

set define on autoprint off
set serveroutput on format wrapped

-- Constants...
-- -----------------------------------------------------------------------
define c_rs_version = 2.02
define c_rs_rmcmd   = "del"  --Windows
--define c_rs_rmcmd   = "rm"   --*nix
define c_rs_init    = "_rs_init.sql"
define c_rs_clear   = "_rs_teardown.sql"


-- Initialise default substitution variables 1 and 2...
-- -----------------------------------------------------------------------
set termout off

col 1 new_value 1
col 2 new_value 2
col 3 new_value 3

select null as "1"
,      null as "2"
,      null as "3"
from   dual
where  1=2;


-- Input parameters...
-- -----------------------------------------------------------------------
define p_rs_snap    = &1
define p_rs_option  = "&2"
define p_rs_include = "&3"


-- Initialisation section...
-- -----------------------------------------------------------------------
column snap      noprint new_value v_rs_snap
column if_start  noprint new_value v_rs_if_start
column if_middle noprint new_value v_rs_if_middle
column if_pause  noprint new_value v_rs_if_pause
column if_resume noprint new_value v_rs_if_resume
column if_stop   noprint new_value v_rs_if_stop

select snap
,      decode(snap, 'start', '', '--')  as if_start
,      decode(snap, 'middle', '', '--') as if_middle
,      decode(snap, 'pause', '', '--')  as if_pause
,      decode(snap, 'resume', '', '--') as if_resume
,      decode(snap, 'stop', '', '--')   as if_stop
from  (
         select rtrim(lower('&p_rs_snap'),';') as snap
         from   dual
      );

spool "&c_rs_init" replace
prompt &v_rs_if_start var bv_rs_start      clob;
prompt &v_rs_if_start var bv_rs_ela_start  number;
prompt &v_rs_if_start var bv_rs_cpu_start  number;
prompt &v_rs_if_start var bv_rs_middle     clob;
prompt &v_rs_if_start var bv_rs_ela_middle number;
prompt &v_rs_if_start var bv_rs_cpu_middle number;
prompt &v_rs_if_start var bv_rs_pause      clob;
prompt &v_rs_if_start var bv_rs_ela_pause  number;
prompt &v_rs_if_start var bv_rs_cpu_pause  number;
prompt &v_rs_if_start var bv_rs_resume     clob;
prompt &v_rs_if_start var bv_rs_ela_resume number;
prompt &v_rs_if_start var bv_rs_cpu_resume number;
prompt &v_rs_if_start var bv_rs_stop       clob;
prompt &v_rs_if_start var bv_rs_ela_stop   number;
prompt &v_rs_if_start var bv_rs_cpu_stop   number;
spool off
@"&c_rs_init"
host &c_rs_rmcmd "&c_rs_init"


-- Parse the options...
-- -----------------------------------------------------------------------
column threshold  noprint new_value v_rs_threshold
column namelist   noprint new_value v_rs_name_list
column namelike   noprint new_value v_rs_name_like
column rs_option  noprint new_value v_rs_option
column rs_include noprint new_value v_rs_include

select case
         when o in ('threshold','t')
         then 1
         when o in ('names','n')
         then 2
         when o in ('like','l')
         then 3
         else 4
       end as rs_option
,      case
         when o in ('threshold','t')
         then to_number(v)
         else 0
       end as threshold
,      case
         when o in ('names','n')
         then '''' || regexp_replace(v, ' *, *', ''',''') || ''''
         else 'null'
       end as namelist
,      case
         when o in ('like','l')
         then '''%' || v || '%'''
         else 'null'
       end as namelike
,      case
          when i in ('false','f')
          then 'false'
          else 'true'
       end as rs_include
from  (
        select trim(regexp_substr(lower('&p_rs_option'), '[^=]+')) as o
        ,      trim(regexp_substr('&p_rs_option', '[^=]+', 1, 2))  as v
        ,      trim(lower('&p_rs_include'))                        as i
        from   dual
      );

set termout on


-- The utility...
-- -----------------------------------------------------------------------
declare

   -- Run constants...
   -- -------------------------------------------------------------------------
   c_snap1 constant pls_integer := 1;
   c_snap2 constant pls_integer := 2;
   c_snap3 constant pls_integer := 3;
   c_snap4 constant pls_integer := 4;

   -- Snapshots...
   -- -------------------------------------------------------------------------
   type aat_snaps is table of clob
      index by pls_integer;

   g_snaps aat_snaps;

   -- Elapsed time calculation...
   -- -------------------------------------------------------------------------
   type rt_time is record
   ( ela integer
   , cpu integer );

   type aat_time is table of rt_time
      index by pls_integer;

   g_times aat_time;

   -- About...
   -----------------------------------------------------------------------------
   procedure rs_info is
   begin
      dbms_output.put_line('- RunStats v&c_rs_version by Adrian Billington ' ||
                           '(http://www.oracle-developer.net)');
      dbms_output.put_line('- Based on the original RUNSTATS utility by Tom Kyte');
   end rs_info;

   -- Snapshot procedure...
   -- -------------------------------------------------------------------------
   procedure rs_snap( p_snap in pls_integer ) is
   begin
      select dbms_xmlgen.getxml(
                q'[select 'STAT' as type
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
                          where  sid = sys_context('userenv','sid')]'
                 ) into g_snaps(p_snap)
      from   dual;
   end rs_snap;

   -- Time snapshot...
   -- -------------------------------------------------------------------------
   procedure rs_time( p_snap in pls_integer ) is
   begin
      g_times(p_snap).ela := dbms_utility.get_time;
      g_times(p_snap).cpu := dbms_utility.get_cpu_time;
   end rs_time;

   -- Assertion...
   -- -------------------------------------------------------------------------
   procedure rs_assert ( p1 in pls_integer,
                         p2 in varchar2,
                         p3 in varchar2 ) is
   begin
      if p1 is null then
         raise_application_error( -20001,
                                  'Error: the ' || p2 || ' option must be preceded' ||
                                  ' by the ' || p3 || ' option', false );
      end if;
   end rs_assert;

   -- Reporting procedure...
   -- -------------------------------------------------------------------------
   procedure rs_report is

      subtype st_stattype  is varchar2(6);
      subtype st_statname  is varchar2(64);
      subtype st_statvalue is integer;
      subtype st_output    is varchar2(255);

      type aat_output is table of st_output
         index by pls_integer;

      aa_runstats aat_output;    --<-- main run stats (stats/latches)
      aa_tmrstats aat_output;    --<-- timer stats (ela/cpu)
      aa_stmstats aat_output;    --<-- time model stats

      t1 rt_time;                --<-- run1 timer
      t2 rt_time;                --<-- run2 timer
      l1 integer     := 0;       --<-- run1 latches
      l2 integer     := 0;       --<-- run2 latches
      n  pls_integer := 0;       --<-- report section

      -- Small function to return the difference in two timer values...
      -- --------------------------------------------------------------
      function t ( p_start in integer,
                   p_end   in integer ) return rt_time is
         r_time rt_time;
      begin
         r_time.ela := g_times(p_end).ela - g_times(p_start).ela;
         r_time.cpu := g_times(p_end).cpu - g_times(p_start).cpu;
         return r_time;
      end t;

      -- Report formatting procedures...
      -- -------------------------------
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

      -- Procedure to sort the statistics into separate arrays for reporting...
      -- ----------------------------------------------------------------------
      procedure s ( p_stattype in st_stattype,
                    p_statname in st_statname,
                    p_r1_value in integer,
                    p_r2_value in integer,
                    p_diff     in integer default null) is

         v_output st_output;
         v_diff   integer := coalesce(p_diff, p_r2_value-p_r1_value);

      begin

         v_output := rpad(p_stattype, 6) || rpad(p_statname, 50)  || ' ' ||
                     to_char(p_r1_value,'999,999,999' )           || ' ' ||
                     to_char(p_r2_value,'999,999,999' )           || ' ' ||
                     to_char(v_diff,'999,999,999');

         case p_stattype
            when 'TIME' then
               if abs(v_diff) > 0 then
                  aa_stmstats(aa_stmstats.count+1) := v_output;
               end if;
            when 'TIMER' then
               aa_tmrstats(aa_tmrstats.count+1) := v_output;
            else
               if p_statname != 'total latches used' then
                  if p_stattype = 'LATCH' then
                     l1 := l1 + p_r1_value;
                     l2 := l2 + p_r2_value;
                  end if;
                  if (&v_rs_option = 1 and abs(v_diff) >= &v_rs_threshold)
                  or (&v_rs_option = 2 and p_statname in (&v_rs_name_list))
                  or (&v_rs_option = 3 and p_statname like &v_rs_name_like)
                  or  &v_rs_option = 4
                  then
                     aa_runstats(aa_runstats.count+1) := v_output;
                  end if;
               else
                  aa_runstats(aa_runstats.count+1) := v_output;
               end if;
         end case;

      end s;

      -- Procedure to output the sorted output...
      -- ----------------------------------------
      procedure o ( p_stats in out nocopy aat_output ) is
      begin
         for i in 1 .. p_stats.count loop
            dbms_output.put_line( p_stats(i) );
         end loop;
         p_stats.delete;
      end o;

   begin

      -- Report header...
      -- ----------------
      nl;
      div('=');
      dbms_output.put_line('RunStats report : ' || to_char(sysdate,'dd-MON-YYYY hh24:mi:ss'));
      div('=');

      -- Summary timings report...
      -- -------------------------
      sh('Summary timings', true);

      -- Output the elapsed timings...
      -- -----------------------------
      t1 := t(c_snap1, c_snap2);
      t2 := t(c_snap3, c_snap4);
      s('TIMER','elapsed time (hsecs)',t1.ela,t2.ela);
      s('TIMER','cpu time (hsecs)',t1.cpu,t2.cpu);
      o(aa_tmrstats);
      nl;
      dbms_output.put_line( 'Comments:' );
      dbms_output.put_line( '1) ' || case
                                        when t1.ela = t2.ela
                                        then 'Run1 took the same time as Run2'
                                        when t2.ela > t1.ela
                                        then 'Run1 was ' || round((1-(t1.ela/t2.ela))*100,1) ||
                                             '% quicker than Run2'
                                        else 'Run2 was ' || round((1-(t2.ela/t1.ela))*100,1) ||
                                             '% quicker than Run1'
                                     end );
      dbms_output.put_line( '2) ' || case
                                        when t1.cpu = t2.cpu
                                        then 'Run1 used the same amount of CPU time as Run2'
                                        when t2.cpu > t1.cpu
                                        then 'Run1 used ' || round((1-(t1.cpu/t2.cpu))*100,1) ||
                                             '% less CPU time than Run2'
                                        else 'Run2 used ' || round((1-(t2.cpu/t1.cpu))*100,1) ||
                                             '% less CPU time than Run1'
                                     end );

      -- Prepare the runstats...
      -- -----------------------
      for r in (  with rs_snap1 as (
                        select extractValue(xs.object_value, '/ROW/TYPE')             as type
                        ,      extractValue(xs.object_value, '/ROW/NAME')             as name
                        ,      to_number(extractValue(xs.object_value, '/ROW/VALUE')) as value
                        from   table(xmlsequence(extract(xmltype(g_snaps(c_snap1)), '/ROWSET/ROW'))) xs
                        )
                  ,    rs_snap2 as (
                        select extractValue(xs.object_value, '/ROW/TYPE')             as type
                        ,      extractValue(xs.object_value, '/ROW/NAME')             as name
                        ,      to_number(extractValue(xs.object_value, '/ROW/VALUE')) as value
                        from   table(xmlsequence(extract(xmltype(g_snaps(c_snap2)), '/ROWSET/ROW'))) xs
                        )
                  ,    rs_snap3 as (
                        select extractValue(xs.object_value, '/ROW/TYPE')             as type
                        ,      extractValue(xs.object_value, '/ROW/NAME')             as name
                        ,      to_number(extractValue(xs.object_value, '/ROW/VALUE')) as value
                        from   table(xmlsequence(extract(xmltype(g_snaps(c_snap3)), '/ROWSET/ROW'))) xs
                        )
                  ,    rs_snap4 as (
                        select extractValue(xs.object_value, '/ROW/TYPE')             as type
                        ,      extractValue(xs.object_value, '/ROW/NAME')             as name
                        ,      to_number(extractValue(xs.object_value, '/ROW/VALUE')) as value
                        from   table(xmlsequence(extract(xmltype(g_snaps(c_snap4)), '/ROWSET/ROW'))) xs
                        )
                  ,    rs_diffs as (
                        select type
                        ,      name
                        ,      rs2.value - rs1.value as r1_value
                        ,      rs4.value - rs3.value as r2_value
                        from   rs_snap1      rs1
                               inner join
                               rs_snap2      rs2
                               using (type, name)
                               inner join
                               rs_snap3      rs3
                               using (type, name)
                               inner join
                               rs_snap4      rs4
                               using (type, name)
                        )
                  select type
                  ,      name
                  ,      r1_value
                  ,      r2_value
                  ,      r2_value - r1_value as diff
                  from   rs_diffs
                  order  by
                         abs(diff)
                  ,      type
                  ,      name)
      loop
         s(r.type, r.name, r.r1_value, r.r2_value, r.diff);
      end loop;

      -- Output the sorted runstats information...
      -- -----------------------------------------
      sh('Statistics report');
      o(aa_runstats);

      -- Total latches report...
      -- -----------------------
      sh('Latching report');
      s('LATCH','total latches used',l1,l2);
      o(aa_runstats);
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

      -- Time model report...
      -- --------------------
      if '&v_rs_include' = 'true' then
         sh('Time model report');
         o(aa_stmstats);
      end if;

      -- About...
      -- --------
      sh('About', false);
      rs_info;

      nl;
      div('=');
      dbms_output.put_line('End of report');
      div('=');

   end rs_report;

   procedure rs_reset is
   begin
      :bv_rs_start      := null;
      :bv_rs_ela_start  := null;
      :bv_rs_cpu_start  := null;
      :bv_rs_middle     := null;
      :bv_rs_ela_middle := null;
      :bv_rs_cpu_middle := null;
      :bv_rs_pause      := null;
      :bv_rs_ela_pause  := null;
      :bv_rs_cpu_pause  := null;
      :bv_rs_resume     := null;
      :bv_rs_ela_resume := null;
      :bv_rs_cpu_resume := null;
      :bv_rs_stop       := null;
      :bv_rs_ela_stop   := null;
      :bv_rs_cpu_stop   := null;
   end rs_reset;

begin

   -- Runtime program...
   -- -------------------------------------------------------------------------
   case '&v_rs_snap'

      when 'start' then
         rs_snap(c_snap1);
         :bv_rs_start := g_snaps(c_snap1);
         rs_time(c_snap1);
         :bv_rs_ela_start := g_times(c_snap1).ela;
         :bv_rs_cpu_start := g_times(c_snap1).cpu;

      when 'middle' then
         rs_assert(:bv_rs_ela_start, 'middle', 'start');
         rs_snap(c_snap2);
         :bv_rs_middle := g_snaps(c_snap2);
         rs_time(c_snap2);
         :bv_rs_ela_middle := g_times(c_snap2).ela;
         :bv_rs_cpu_middle := g_times(c_snap2).cpu;

      when 'pause' then
         rs_assert(:bv_rs_ela_start, 'pause', 'start');
         rs_snap(c_snap2);
         :bv_rs_pause := g_snaps(c_snap2);
         rs_time(c_snap2);
         :bv_rs_ela_pause := g_times(c_snap2).ela;
         :bv_rs_cpu_pause := g_times(c_snap2).cpu;

      when 'resume' then
         rs_assert(:bv_rs_ela_pause, 'resume', 'pause');
         rs_snap(c_snap3);
         :bv_rs_resume := g_snaps(c_snap3);
         rs_time(c_snap3);
         :bv_rs_ela_resume := g_times(c_snap3).ela;
         :bv_rs_cpu_resume := g_times(c_snap3).cpu;

      when 'stop' then
         rs_assert(coalesce(:bv_rs_ela_middle, :bv_rs_ela_resume), 'stop', 'middle or resume');
         rs_time(c_snap4);
         rs_snap(c_snap4);
         g_times(c_snap1).ela := :bv_rs_ela_start;
         g_times(c_snap1).cpu := :bv_rs_cpu_start;
         g_times(c_snap2).ela := coalesce(:bv_rs_ela_middle, :bv_rs_ela_pause);
         g_times(c_snap2).cpu := coalesce(:bv_rs_cpu_middle, :bv_rs_cpu_pause);
         g_times(c_snap3).ela := coalesce(:bv_rs_ela_resume, :bv_rs_ela_middle);
         g_times(c_snap3).cpu := coalesce(:bv_rs_cpu_resume, :bv_rs_cpu_middle);
         g_snaps(c_snap1)     := :bv_rs_start;
         g_snaps(c_snap2)     := case when :bv_rs_ela_middle is not null then :bv_rs_middle else :bv_rs_pause end;
         g_snaps(c_snap3)     := case when :bv_rs_ela_middle is not null then :bv_rs_middle else :bv_rs_resume end;
         rs_report;
         rs_reset;
   else
      raise_application_error(
         -20000,
         'Incorrect option used at position 1 '||
         '[used="&v_rs_snap"] [valid=start,middle,pause,resume,stop]',
         false );
   end case;

end;
/


-- Teardown section...
-- -----------------------------------------------------------------------
set termout off
spool "&c_rs_clear" replace
prompt &v_rs_if_stop undefine bv_rs_start
prompt &v_rs_if_stop undefine bv_rs_ela_start
prompt &v_rs_if_stop undefine bv_rs_cpu_start
prompt &v_rs_if_stop undefine bv_rs_middle
prompt &v_rs_if_stop undefine bv_rs_ela_middle
prompt &v_rs_if_stop undefine bv_rs_cpu_middle
prompt &v_rs_if_stop undefine bv_rs_pause
prompt &v_rs_if_stop undefine bv_rs_ela_pause
prompt &v_rs_if_stop undefine bv_rs_cpu_pause
prompt &v_rs_if_stop undefine bv_rs_resume
prompt &v_rs_if_stop undefine bv_rs_ela_resume
prompt &v_rs_if_stop undefine bv_rs_cpu_resume
spool off
@"&c_rs_clear"
host &c_rs_rmcmd "&c_rs_clear"
undefine 1
undefine 2
undefine 3
undefine p_rs_snap
undefine p_rs_option
undefine p_rs_include
undefine v_rs_snap
undefine c_rs_rmcmd
undefine c_rs_init
undefine c_rs_clear
undefine c_rs_version
undefine v_rs_if_start
undefine v_rs_if_middle
undefine v_rs_if_pause
undefine v_rs_if_resume
undefine v_rs_if_stop
undefine v_rs_threshold
undefine v_rs_name_list
undefine v_rs_name_like
undefine v_rs_option
undefine v_rs_include
set termout on
