-module(rebar_covertool).
-include_lib("rebar/include/rebar.hrl").

%% Application callbacks
-export([eunit/2]).

eunit(Config, _X) ->
    Export = rebar_config:get_local(Config, cover_export, undefined),
    case Export of
        undefined -> ok;
        File ->
            ?CONSOLE("Exporting cover data to '~p'~n", [File]),
            cover:export(File)
    end.
