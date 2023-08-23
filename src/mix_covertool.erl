-module(mix_covertool).

%% mix plugin callbacks
-export([start/2]).

-include("covertool.hrl").

%% ===================================================================
%% Mix plugin callbacks
%% ===================================================================
start(_CompilePath, Opts) ->
    _ = cover:start(),

    Summary = proplists:get_bool(summary, Opts),

    AppName = proplists:get_value(app, mix_project(config)),
    ExtraApps = proplists:get_value(include_apps, Opts, []),

    AppsPaths = mix_project(deps_paths),
    BuildPath = mix_project(build_path),
    
    {ok, Cwd} = file:get_cwd(),

    Configs = lists:map(fun(App) ->
        AppPath = maps:get(App, AppsPaths, Cwd),
        BeamDir = filename:join([unicode:characters_to_list(BuildPath), "lib", App, "ebin"]),
        Sources = lists:map(fun(Dir) -> filename:join([unicode:characters_to_list(AppPath), Dir]) end, ["lib", "src"]),

        compile_beam_directory(BeamDir),

        OutputFile = filename:join([Cwd, atom_to_list(App) ++ ".covertool.xml"]),

        #config{appname = App, sources = Sources, beams = [BeamDir], summary = Summary, output = OutputFile}
    end, [AppName | ExtraApps]),

    fun() ->
        lists:foreach(fun(Config) -> covertool:generate_report(Config, cover:modules()) end, Configs)
    end.

compile_beam_directory(CompilePath) ->
    case cover:compile_beam_directory(CompilePath) of
        Results when is_list(Results) ->
            ok;
        {error, _} ->
            mix(raise, <<"Failed to cover compile directory">>)
    end.

%% ===================================================================
%% Mix helpers
%% ===================================================================
mix(Fun, Arg) ->
    'Elixir.Mix':Fun(Arg).

mix_project(Fun) ->
    'Elixir.Mix.Project':Fun().
