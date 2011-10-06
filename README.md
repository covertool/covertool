Cover-to-Cobertura Conversion Tool 
==================================

A simple tool to convert exported Erlang `cover` data sets into Cobertura XML
reports. The report could be then feed to the Jenkins Cobertura plug-in.

Usage
-----

Standalone:

1. Install Jenkins Cobertura Plug-in.
2. Configure `cover` to export data. Sample cover.spec for `Common Test`:

        {incl_app, app0, details}.
        {export, "all.coverdata"}.
3. Configure Jenkins to convert `cover` reports into `Cobertura` format:
  
        $ covertool -cover all.coverdata -output coverage.xml -src src/

   or:
   
   Configure rebar to generate reports in `Cobertura` format:

        {plugins, [rebar_covertool]}.
        {covertool_eunit, "eunit.coverage.xml"}. % Output report file name
        {covertool_ct, {"ct.coverdata", "ct.coverage.xml"}}. % Source file name, output report file name

4. Configure "Publish Cobertura Coverage Report" post-build action, set path
to the generated `coverage.xml`
5. Run the build. At the end, "Coverage Report" link should appear on project page.

Screenshots
-----------

![Screenshot1](covertool/raw/master/screenshots/shot1.png)

![Screenshot2](covertool/raw/master/screenshots/shot2.png)

