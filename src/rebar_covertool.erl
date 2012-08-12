-module(rebar_covertool).

%% Application callbacks
-export([eunit/2, ct/2]).

-include("covertool.hrl").

-define(EUNIT_DIR, ".eunit").

%% Run after eunit tests, export current coverage data in Cobertura format
eunit(Config, AppFile) ->
    AppName = get_app_name(Config, AppFile),
    case rebar_config:get_local(Config, covertool_eunit, undefined) of
        undefined -> ok;
        Output ->
            {ok, CoverLog} = cover_init(),
            PrefixLen = rebar_config:get_local(Config, covertool_prefix_len, 0),
            CoverConfig = #config{appname = AppName,
                                  prefix_len = PrefixLen,
                                  output = Output,
                                  sources = "src/"},
            covertool:generate_report(CoverConfig,
                                      cover:modules() ++ cover:imported_modules()),
            file:close(CoverLog),
            ok
    end.

%% Run after common tests. Convert coverage data exported after tests are
%% executed into Cobertura format.
ct(Config, AppFile) ->
    AppName = get_app_name(Config, AppFile),
    case rebar_config:get_local(Config, covertool_ct, undefined) of
        undefined -> ok;
        {From, To} ->
            {ok, CoverLog} = cover_init(),
            cover:import(From),
            PrefixLen = rebar_config:get_local(Config, covertool_prefix_len, 0),
            CoverConfig = #config{appname = AppName,
                                  prefix_len = PrefixLen,
                                  output = To,
                                  sources = "src/"},
            covertool:generate_report(CoverConfig,
                                      cover:imported_modules()),
            file:close(CoverLog),
            ok
    end.

%% Determine application name from the .app.src. If name cannot be
%% determined, use "Application"
get_app_name(Config, AppFile) ->
    case rebar_app_utils:is_app_src(AppFile) of
        true ->
            case rebar_app_utils:load_app_file(Config, AppFile) of
                {ok, _Config1, AppName, _AppData} -> AppName;
                {error, _Reason} -> 'Application'
            end;
        false -> 'Application'
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
    {ok, F} = file:open(filename:join([?EUNIT_DIR, "cover.log"]), [write, append]),
    group_leader(F, CoverPid),
    {ok, F}.
