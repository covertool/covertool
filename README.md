Cover-to-Cobertura Conversion Tool 
==================================

A simple tool to convert exported Erlang `cover` data sets into Cobertura XML
reports. The report could be then feed to the Jenkins Cobertura plug-in.

Usage:

0. Install Jenkins Cobertura Plug-in
1. Configure `cover` to export data. Sample cover.spec for `Common Test`:
    {incl_app, app0, details}.
    {export, "all.coverdata"}.

2. Configure Jenkins to convert `cover` reports into `Cobertura` format:
    $ covertool -cover all.coverdata -output coverage.xml -src src/
3. Configure "Publish Cobertura Coverage Report" post-build action, set path
to the generated `coverage.xml`
4. Run the build. At the end, "Coverage Report" link should appear on project page.

Screenshots
-----------

![Screenshot1](covertool/raw/master/screenshots/shot1.png)

![Screenshot2](covertool/raw/master/screenshots/shot2.png)

