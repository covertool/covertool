-module(mix_covertool).

%% mix plugin callbacks
-export([start/2]).

-include("covertool.hrl").

%% ===================================================================
%% Mix plugin callbacks
%% ===================================================================
start( CompilePath, Opts ) ->
    _ = cover:start(),

    case cover:compile_beam_directory(binary:bin_to_list(CompilePath)) of
        Results when is_list(Results) ->
            ok;
        {error, _} ->
            mix(raise, <<"Failed to cover compile directory">>)
    end,

    AppName = proplists:get_value(app, mix_project(config)),
    {ok, SrcDir} = file:get_cwd(),
    BeamDir = binary:bin_to_list(mix_project(compile_path)),
    Summary = proplists:get_bool(summary, Opts),
    Config = #config{appname = AppName, sources = [SrcDir], beams = [BeamDir],
                     summary = Summary},

    fun() ->
        covertool:generate_report(Config, cover:modules())
    end.

%% ===================================================================
%% Mix helpers
%% ===================================================================
mix(Fun, Arg) ->
    'Elixir.Mix':Fun(Arg).

mix_project(Fun) ->
    'Elixir.Mix.Project':Fun().
