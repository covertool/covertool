Cover-to-Cobertura Conversion Tool 
==================================

[![Hex.pm version](https://img.shields.io/hexpm/v/covertool.svg?style=flat)](https://hex.pm/packages/covertool)

A simple tool to convert exported Erlang `cover` data sets into Cobertura XML
reports. The report could be then feed to the Jenkins Cobertura plug-in.

Plugin renamed
--------------

This plugin was known on hex as `rebar_covertool` and that is also how you would use it in your rebar2/rebar3 configuration. This was mainly because the `rebar_covertool` module was responsible for exposing the rebar API. The API has now moved to the main `covertool` module. This makes it slightly easier to integrate it because the name of the dependency is the same as the name of the main module. The instructions below should help if you got stuck by the rename.

Usage
-----

1. Install Jenkins Cobertura Plug-in.
2. Pick one of the options below ([standalone](#standalone)/[rebar](#rebar)/[rebar3](#rebar3)/[mix](#mix))
3. Configure "Publish Cobertura Coverage Report" post-build action, set path
to the generated `coverage.xml`
4. Run the build. At the end, "Coverage Report" link should appear on project page.

## Standalone

1. Build command line script (WARNING: EUnit test for plugin may fail for OTP =< 17.3, due to a bug in `cover` app)

        $ make

2. Configure `cover` to export data. Sample cover.spec for `Common Test`:

        {incl_app, app0, details}.
        {export, "all.coverdata"}.

3. Configure Jenkins to convert `cover` reports into `Cobertura` format:
  
        $ covertool -cover all.coverdata -output coverage.xml -src src/

## Rebar

Configure rebar to generate reports in `Cobertura` format:

```
{plugins, [covertool]}.
{cover_export_enabled, true}.
{covertool_eunit, {".eunit/eunit.coverdata", "eunit.coverage.xml"}}. % Source file name, output report file name
{covertool_ct, {"ct.coverdata", "ct.coverage.xml"}}. % Source file name, output report file name
{covertool_prefix_len, 2}. % Optional: include the module prefix in the package name
```

The `covertool_prefix_len` option allows including the first *n* sections of the '_'-delimited module name in the package name. For example, with a `covertool_prefix_len` of 2 and a module named `app0_worker_srv_sup`, the term `app0.worker` would be added to the end of the package name. Defaults to 0 (no module prefix included).

## Rebar3

Configure rebar3 to generate reports in `Cobertura` format:

```
{project_plugins, [covertool]}.
{cover_export_enabled, true}.
{covertool, [{coverdata_files, ["ct.coverdata", "eunit.coverdata"]},
             {include_apps, [dep0, dep1]},
             {prefix_len, 2}]}. % Optional: Use module prefix as (imaginary) package name
```

The `include_apps` option allows specifying a list of dependent OTP applications to include in the coverage export (default: `[]`). Note that the coverage data must be included in the input `.coverdata` file in order for any values to be populated in the output XML file. This can be done using the [ct cover spec file](http://erlang.org/doc/apps/common_test/cover_chapter.html#id85714).

The `include_apps` option can also be specified via the command line as a CSV of application names, e.g.:

```
rebar3 covertool generate -a"dep0,dep1"
```

The `prefix_len` option allows including the first *n* sections of the '_'-delimited module name in the package name. For example, with a `prefix_len` of 2 and a module named `app0_worker_srv_sup`, the term `app0.worker` would be added to the end of the package name. Defaults to 0 (no module prefix included). This option can also be specified via the command line, e.g.:

```
rebar3 covertool generate -p2
```

## Mix

Configure mix to generate reports in `Cobertura` format:

```elixir
def project do
  [
    test_coverage: [tool: :covertool]
  ]
end
```

Screenshots
-----------

![Screenshot1](screenshots/shot1.png)

![Screenshot2](screenshots/shot2.png)
