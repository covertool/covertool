-module(rebar_covertool_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%% Assuming this test is run inside the rebar 'eunit'
%% command, the current working directory will be '.eunit'
-define(REBAR_SCRIPT, "../deps/rebar/rebar").

-define(TMP_DIR, "tmp_eunit/").

%% ====================================================================
%% Rebar covertool plugin tests
%% ====================================================================

eunit_test_() ->
    {"Ensure Cover runs with tests in a test dir and no defined suite",
     setup, fun() -> setup_project(), rebar("-v compile eunit ct") end,
     fun teardown/1,

     fun(RebarOut) ->
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
		 "{lib_dirs, [\"../../../\"]}.\n",
		 "{plugins, [rebar_covertool]}.\n",
		 "{cover_enabled, true}.\n",
		 "{cover_export_enabled, true}.\n",
		 "{covertool_eunit, {\".eunit/cover.coverdata\", \".eunit/eunit.coverage.xml\"}}.\n",
		 "{covertool_ct, {\"test/ct.coverdata\", \"test/ct.coverage.xml\"}}.\n"]).

-define(cover_spec,
		["{export, \"ct.coverdata\"}.\n",
		"{incl_dirs, [\"../../test\", \"../../src\"]}.\n"]).

make_tmp_dir() ->
    ok = file:make_dir(?TMP_DIR).

setup_environment() ->
    ok = make_tmp_dir(),
    prepare_rebar_script(),
    ok = file:set_cwd(?TMP_DIR).

setup_project() ->
    setup_environment(),
    rebar("create-app appid=myapp"),
    ok = file:make_dir("ebin"),
    ok = file:make_dir("test"),
    ok = file:write_file("test/myapp_mymod_tests.erl", ?myapp_mymod_tests),
    ok = file:write_file("src/myapp_mymod.erl", ?myapp_mymod),
	ok = file:write_file("test/myapp_mymod_SUITE.erl", ?myapp_mymod_SUITE),
	ok = file:write_file("test/cover.spec", ?cover_spec),
	ok = file:write_file("rebar.config", ?rebar_config).


teardown(_) ->
    ok = file:set_cwd(".."),
    ok = remove_tmp_dir().

remove_tmp_dir() ->
    remove_tmp_dir(arg_for_eunit).

remove_tmp_dir(_) ->
    ok = rebar_file_utils:rm_rf(?TMP_DIR).

%% ====================================================================
%% Helper Functions
%% ====================================================================

prepare_rebar_script() ->
    Rebar = ?TMP_DIR ++ "rebar",
    {ok, _} = file:copy(?REBAR_SCRIPT, Rebar),
    case os:type() of
        {unix, _} ->
            [] = os:cmd("chmod u+x " ++ Rebar);
        {win32, _} ->
            {ok, _} = file:copy(?REBAR_SCRIPT ++ ".bat",
                                ?TMP_DIR ++ "rebar.bat")
    end.

rebar() ->
    rebar([]).

rebar(Args) when is_list(Args) ->
    Out = os:cmd(filename:nativename("./rebar") ++ " " ++ Args),
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
			[?assert(string:str(Str, "filename=\"myapp_mymod.erl\" line-rate=\"1.000000\"") =/= 0),
			 ?assert(string:str(Str, "package name=\"" ++ AppName ++ "\"") =/= 0)]
    end.

