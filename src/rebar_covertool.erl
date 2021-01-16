-module(rebar_covertool).

%% rebar3 plugin callback
-export([init/1]).

%% rebar2 callbacks
-export([eunit/2, ct/2]).

%% private interface for other plugin modules
-export([cover_init/1]).


-include("covertool.hrl").

-define(EUNIT_DIR, ".eunit").

%% ===================================================================
%% Rebar3 plugin registration callback
%% ===================================================================
init(State) ->
    rebar_api:warn("The 'covertool' plugin is being referenced by deprecated
                    name 'rebar_covertool'. Use 'covertool' instead in your
                    rebar.config file. See the covertool README for more
                    information.", []),
    rebar3_covertool_gen:init( State ).

%% ===================================================================
%% Rebar2 plugin implementation
%% ===================================================================

%% Run after eunit tests. Convert coverage data exported after tests are
%% executed into Cobertura format.
eunit(Config, AppFile) ->
    case is_empty_dir(AppFile) of
        true ->
            ok;
        false ->
            rebar2_generate(Config, AppFile, covertool_eunit)
    end.

%% Run after common tests. Convert coverage data exported after tests are
%% executed into Cobertura format.
ct(Config, AppFile) ->
    case is_empty_dir(AppFile) of
        true ->
            ok;
        false ->
            rebar2_generate(Config, AppFile, covertool_ct)
    end.

rebar2_generate(Config, AppFile, ConfigKey) ->
    AppName = get_app_name(Config, AppFile),
    case rebar_config:get_local(Config, ConfigKey, undefined) of
        undefined -> ok;
        {From, To} ->
            CoverLogFile = filename:join([?EUNIT_DIR, "cover.log"]),
            case cover_init(CoverLogFile) of
                {ok, CoverLog} ->
                    cover:import(From),
                    PrefixLen = rebar_config:get_local(Config, covertool_prefix_len, 0),
                    Summary = rebar_config:get_local(Config, covertool_summary, false),
                    CoverConfig = #config{appname = AppName,
                                          prefix_len = PrefixLen,
                                          summary = Summary,
                                          output = To},
                    covertool:generate_report(CoverConfig,
                                              cover:imported_modules()),
                    file:close(CoverLog),
                    ok;
                {error, _} ->
                    ok
            end
    end.

%% ===================================================================
%% Internal Functions
%% ===================================================================

%% Determine application name from the .app.src. If name cannot be
%% determined, use "Application"
get_app_name(Config, AppFile) ->
    case rebar_config:get_local(Config, covertool_app_name, undefined) of
        undefined ->
            case rebar_app_utils:is_app_src(AppFile) of
                true ->
                    {_Config, AppName} = rebar_app_utils:app_name(Config, AppFile),
                    AppName;
                false -> 'Application'
            end;
        AppName ->
            AppName
    end.

cover_init(CoverLog) ->
    %% Attempt to start the cover server, then set it's group leader to
    %% .eunit/cover.log, so all cover log messages will go there instead of
    %% to stdout. If the cover server is already started we'll reuse that
    %% pid.
    {ok, CoverPid} = case cover:start() of
                         {ok, _P} = OkStart ->
                             OkStart;
                         {error,{already_started, P}} ->
                             {ok, P};
                         {error, _Reason} = ErrorStart ->
                             ErrorStart
                     end,
    ok = filelib:ensure_dir(CoverLog),
    {ok, F} = file:open(CoverLog, [write, append]),
    group_leader(F, CoverPid),
    {ok, F}.

is_empty_dir(AppFile) ->
    case file:read_file_info(AppFile) of
        {ok, _} ->
            false;
        {error, _} ->
            true
    end.
