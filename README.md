
# RUNSTATS

## 1.0 Introduction
This archive contains two oracle-developer.net versions of Tom Kyte's RunStats utility. This compares the resource consumption of two alternative methods to perform the same database task. I've added some functionality and flexibility around the statistics reporting section and also a pause/resume option. The key differences are described in the headers of the source files.

## 2.0 Versions
There are two versions provided:

### 2.1 runstats_pkg.sql
This creates via a single PL/SQL package named RUNSTATS_PKG. This uses invoker rights and dynamic SQL to workaround the common issue whereby developers are not given explicit grants on the required V$ views but have V$ access via a role. See the comments in the package header for more details and usage instructions.

### 2.2 runstats.sql
This version is a standalone SQL*Plus script that runs RunStats from your SQLPATH without the need to create any database objects. This can be used if you are not able to create the PL/SQL package version of RunStats. See the comments in the script header for more details and usage instructions.

### 3.0 Version History
```
Version  Date            Description
-------  --------------  ----------------------------------------
1.0      January 2007    Original version
1.1      January 2009    Added extended reporting options
2.0      October 2011    Re-design for standalone script version
2.01     November 2011   Bug-fix for numeric overflow
```

### 4.0 Credits
Credit of course must go to Tom Kyte. His RUNSTATS tool has proved invaluable to Oracle developers worldwide. All I've done here is re-factor it to work under different environments/circumstances, extend the reporting capability and include pause/resume functionality.

### 5.0 License
This project uses the MIT License.
See https://github.com/oracle-developer/runstats/blob/master/LICENSE

Adrian Billington
www.oracle-developer.net
