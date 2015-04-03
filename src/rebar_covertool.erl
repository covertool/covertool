-module(rebar_covertool).

%% Application callbacks
-export([eunit/2, ct/2]).

-include("covertool.hrl").

-define(EUNIT_DIR, ".eunit").

%% Run after eunit tests. Convert coverage data exported after tests are
%% executed into Cobertura format.
eunit(Config, AppFile) ->
    case is_empty_dir(AppFile) of
        true ->
            ok;
        false ->
            generate_report(Config, AppFile, covertool_eunit)
    end.

%% Run after common tests. Convert coverage data exported after tests are
%% executed into Cobertura format.
ct(Config, AppFile) ->
    case is_empty_dir(AppFile) of
        true ->
            ok;
        false ->
            generate_report(Config, AppFile, covertool_ct)
    end.

generate_report(Config, AppFile, ConfigKey) ->
    AppName = get_app_name(Config, AppFile),
    case rebar_config:get_local(Config, ConfigKey, undefined) of
        undefined -> ok;
        {From, To} ->
            case cover_init() of
                {ok, CoverLog} ->
                    cover:import(From),
                    PrefixLen = rebar_config:get_local(Config, covertool_prefix_len, 0),
                    CoverConfig = #config{appname = AppName,
                                          prefix_len = PrefixLen,
                                          output = To,
                                          sources = ["src/"]},
                    covertool:generate_report(CoverConfig,
                                              cover:imported_modules()),
                    file:close(CoverLog),
                    ok;
                {error, _} ->
                    ok
            end
    end.

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

cover_init() ->
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
    CoverLog = filename:join([?EUNIT_DIR, "cover.log"]),
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
