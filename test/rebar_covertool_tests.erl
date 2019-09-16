-module(rebar_covertool_tests).

-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% Rebar covertool plugin tests
%% ====================================================================

eunit_test_() ->
    {"Ensure Cover runs with tests in a test dir and no defined suite",
     setup,
     fun() ->
        ProjectSetupResult = setup_project(),
         {ProjectSetupResult, rebar("-v compile eunit ct")}
     end,
     fun teardown/1,
     fun({_ProjectSetupResult, RebarOut}) ->
             [{"Plugin is found",
               ?_assert(string:str(RebarOut, "WARN:  Missing plugins:") =:= 0)},

              {"Plugin does not contain compilation errors",
               ?_assert(string:str(RebarOut, "contains compilation errors:") =:= 0)},

              {"All cover reports are generated",
               assert_files_in("the temporary eunit directory",
                               expected_cover_generated_files())},

              {"Only production modules get coverage reports",
               assert_files_not_in("the temporary eunit directory",
                                   [".eunit/myapp_mymod_tests.COVER.html"])},

              {"myapp_mymod should be fully covered and package name must match!",
               assert_report(".eunit/eunit.coverage.xml", "otherapp")},

              {"myapp_mymod should be fully covered and package name must match!",
               assert_report("test/ct.coverage.xml", "otherapp")}
              ]
     end}.

expected_cover_generated_files() ->
    [".eunit/cover.coverdata",
     ".eunit/eunit.coverage.xml",
     "test/ct.coverdata",
     "test/ct.coverage.xml"].

%% ====================================================================
%% Setup and Teardown
%% ====================================================================

-define(myapp_mymod,
        ["-module(myapp_mymod).\n",
         "-export([myfunc/0]).\n",
         "-include_lib(\"eunit/include/eunit.hrl\").\n",
         "myfunc() -> ok.\n",
         "myprivate_test() -> ?assert(true).\n"]).

-define(myapp_mymod_tests,
        ["-module(myapp_mymod_tests).\n",
         "-compile([export_all]).\n",
         "-include_lib(\"eunit/include/eunit.hrl\").\n",
         "myfunc_test() -> ?assertMatch(ok, myapp_mymod:myfunc()).\n"]).



-define(myapp_mymod_SUITE,
        ["-module(myapp_mymod_SUITE).\n",
         "-export([all/0]).\n",
         "-export([myfunc_test/1]).\n",
         "-include_lib(\"common_test/include/ct.hrl\").\n",
         "all() -> [myfunc_test].\n",
         "myfunc_test(_Config) -> myapp_mymod:myfunc().\n"]).

-define(rebar_config,
        ["{covertool_app_name, 'otherapp'}.\n",
         "{lib_dirs, [\"../lib\"]}.\n",
         "{plugins, [rebar_covertool]}.\n",
         "{cover_enabled, true}.\n",
         "{cover_export_enabled, true}.\n",
         "{covertool_eunit, {\".eunit/cover.coverdata\", \".eunit/eunit.coverage.xml\"}}.\n",
         "{covertool_ct, {\"test/ct.coverdata\", \"test/ct.coverage.xml\"}}.\n"]).

-define(cover_spec,
        ["{export, \"ct.coverdata\"}.\n",
         "{incl_dirs, [\"../test\", \"../src\"]}.\n"]).

make_tmp_dir(StartCWD) ->
    TmpDir = filename:join([StartCWD, "_build", "test", "tmp_eunit"]),
    ok = rebar_file_utils:rm_rf(TmpDir),
    ok = file:make_dir(TmpDir),
    TmpDir.

setup_environment() ->
    {ok, StartCWD} = file:get_cwd(),
    TmpDir = make_tmp_dir(StartCWD),
    ok = file:set_cwd(TmpDir),
    {StartCWD, TmpDir}.

setup_project() ->
    {StartCWD, TmpDir} = setup_environment(),
    rebar("create-app appid=myapp"),
    ok = file:make_dir("ebin"),
    ok = file:make_dir("test"),
    ok = file:write_file("test/myapp_mymod_tests.erl", ?myapp_mymod_tests),
    ok = file:write_file("src/myapp_mymod.erl", ?myapp_mymod),
    ok = file:write_file("test/myapp_mymod_SUITE.erl", ?myapp_mymod_SUITE),
    ok = file:write_file("test/cover.spec", ?cover_spec),
    ok = file:write_file("rebar.config", ?rebar_config),
    {StartCWD, TmpDir}.

teardown({{StartCWD, TmpDir}, _RebarOut}) ->
    ok = file:set_cwd(StartCWD),
    ok = rebar_file_utils:rm_rf(TmpDir).

%% ====================================================================
%% Helper Functions
%% ====================================================================

rebar_executable() ->
    RebarName = case os:getenv("REBAR") of
        Missing when Missing =:= false; Missing =:= [] -> "rebar";
        CustomRebar -> CustomRebar
    end,
    find_rebar_executable(RebarName).

find_rebar_executable(RebarName) ->
    case filelib:is_file(RebarName) of
        true -> RebarName;
        false ->
            case os:find_executable(RebarName) of
                false -> erlang:error(missing_rebar, [RebarName]);
                RebarExecutable -> RebarExecutable
            end
    end.

rebar(Args) when is_list(Args) ->
    Out = os:cmd(rebar_executable() ++ " " ++ Args),
    Out.

assert_files_in(Name, [File|T]) ->
    [{Name ++ " has file: " ++ File, ?_assert(filelib:is_regular(File))} |
     assert_files_in(Name, T)];
assert_files_in(_, []) -> [].

assert_files_not_in(Name, [File|T]) ->
    [{Name ++ " does not have file: " ++ File,
      ?_assertNot(filelib:is_regular(File))} | assert_files_not_in(Name, T)];
assert_files_not_in(_, []) -> [].

assert_report(File, AppName) ->
    fun() ->
        {ok, F} = file:read_file(File),
        Str = binary_to_list(F),
        Pattern = "<class name=\"myapp_mymod\" filename=\"[^\"]*myapp_mymod.erl\"[^<>]* line-rate=\"1.0",
        Opts = [{capture, none}],
        [?assertEqual(match, re:run(Str, Pattern, Opts)),
         ?assert(string:str(Str, "package name=\"" ++ AppName ++ "\"") =/= 0)]
    end.

