Cover-to-Cobertura Conversion Tool 
==================================

A simple tool to convert exported Erlang `cover` data sets into Cobertura XML
reports. The report could be then feed to the Jenkins Cobertura plug-in.

Usage
-----

Standalone:

1. Build command line script (WARNING: EUnit test for plugin may fail for OTP =< 17.3, due to a bug in `cover` app)

        $ make

2. Install Jenkins Cobertura Plug-in.
3. Configure `cover` to export data. Sample cover.spec for `Common Test`:

        {incl_app, app0, details}.
        {export, "all.coverdata"}.
4. Configure Jenkins to convert `cover` reports into `Cobertura` format:
  
        $ covertool -cover all.coverdata -output coverage.xml -src src/

   or:
   
   Configure rebar to generate reports in `Cobertura` format:

        {plugins, [rebar_covertool]}.
        {cover_export_enabled, true}.
        {covertool_eunit, {".eunit/eunit.coverdata", "eunit.coverage.xml"}}. % Source file name, output report file name
        {covertool_ct, {"ct.coverdata", "ct.coverage.xml"}}. % Source file name, output report file name
        {covertool_prefix_len, 2}. % Optional: Use module prefix as (imaginary) package name

4. Configure "Publish Cobertura Coverage Report" post-build action, set path
to the generated `coverage.xml`
5. Run the build. At the end, "Coverage Report" link should appear on project page.

Screenshots
-----------

![Screenshot1](covertool/raw/master/screenshots/shot1.png)

![Screenshot2](covertool/raw/master/screenshots/shot2.png)

