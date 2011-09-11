-module(rebar_covertool).

%% Application callbacks
-export([eunit/2, ct/2]).

-include_lib("xmerl/include/xmerl.hrl").

-define(EUNIT_DIR, ".eunit").

-record(result, {line = {0, 0},
                 branches = {0, 0},
                 data = []}).

eunit(Config, _X) ->
    case rebar_config:get_local(Config, covertool_eunit, undefined) of
        undefined -> ok;
        File ->
                {ok, CoverLog} = cover_init(),
                generate_report(cover:modules() ++ cover:imported_modules(), File),
                file:close(CoverLog),
                ok
    end.

ct(Config, _X) ->
    case rebar_config:get_local(Config, covertool_ct, undefined) of
        undefined -> ok;
        {From, To} ->
            cover:import(From),
            generate_report(cover:imported_modules(), To),
            ok
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

generate_report(Modules, Output) ->
    io:format("Generating report '~s'...~n", [Output]),
    Prolog = ["<?xml version=\"1.0\" encoding=\"utf-8\"?>\n",
              "<!DOCTYPE coverage SYSTEM \"http://cobertura.sourceforge.net/xml/coverage-04.dtd\">\n"],

    {MegaSecs, Secs, MicroSecs} = now(),
    Timestamp = MegaSecs * 1000000000 + Secs * 1000 + (MicroSecs div 1000), % in milliseconds

    Version = "1.9.4.1", % emulate Cobertura 1.9.4.1
    Complexity = 0,

    Result = generate_packages(Modules),
    {LinesCovered, LinesValid} = Result#result.line,
    LineRate = rate(Result#result.line),

    {BranchesCovered, BranchesValid} = Result#result.branches,
    BranchRate = rate(Result#result.branches),

    Sources = filename:absname("src"),
    Root = {coverage, [{timestamp, Timestamp},
                       {'line-rate', LineRate},
                       {'lines-covered', LinesCovered},
                       {'lines-valid', LinesValid},
                       {'branch-rate', BranchRate},
                       {'branches-covered', BranchesCovered},
                       {'branches-valid', BranchesValid},
                       {complexity, Complexity},
                       {version, Version}],
            [{sources, [{source, [Sources]}]},
             {packages, [Result#result.data]}]},
    Report = xmerl:export_simple([Root], xmerl_xml, [{prolog, Prolog}]),
    write_output(Report, Output),
    ok.

generate_packages(Modules) ->
    Classes = generate_classes(Modules),
    AppName = "Application", % FIXME: get from .app file
    % for now, the whole data file is a single "package"
    Data = {package, [{name, AppName},
                      {'line-rate', rate(Classes#result.line)},
                      {'branch-rate', rate(Classes#result.branches)},
                      {complexity, 0}],
            [{classes, Classes#result.data}]},
    Classes#result{data = Data}. % leave the metrics as is

% generate <classes> element, each Erlang module is "class"
generate_classes(Modules) ->
    % generate XML for every class, collect summary metric
    Fun = fun(Module, Result) ->
                  Class = generate_class(Module),
                  {Class#result.data, sum(Result, Class)}
          end,
    {Classes, Result} = lists:mapfoldl(Fun, #result{}, Modules),
    Result#result{data = Classes}.

generate_class(Module) ->
    Fun = fun({{_Module, Line}, Value}, Result) ->
                  % XXX: ignore zero-indexed line, for some reason it is always present and always not hit
                  % so, if hits > 0 or line number is zero, assume that line is covered
                  Covered = case Value of 0 when Line =/= 0 -> 0; _Other -> 1 end,
                  LineCoverage = sum(Result#result.line, {Covered, 1}), % add one line to the summary
                  Data = {line, [{number, Line},
                                 {hits, Value}],
                          []},
                  {Data, Result#result{line = LineCoverage}}
          end,
    {ok, Lines} = cover:analyse(Module, calls, line),
    {LinesData, Result} = lists:mapfoldl(Fun, #result{}, Lines),

    Data = {class, [{name, Module},
                    {filename, lookup_source(Module)},
                    {'line-rate', rate(Result#result.line)},
                    {'branch-rate', rate(Result#result.branches)},
                    {complexity, 0}],
            [{methods, []},
             {lines, LinesData}]},
    #result{data = Data}.

write_output(Report, Output) ->
    io:format("Writing output report '~s'...~n", [Output]),
    case file:open(Output, [write, {encoding, utf8}]) of
        {ok, Fd} ->
            ok = file:write(Fd, [Report, "\n"]),
            file:close(Fd);
        {error, Reason} ->
            io:format("Could not open '~s' due to ~p.~n", [Output, Reason]),
            halt(1)
    end,
    ok.

% sum metrics
sum(#result{line = {LineCovered1, LineValid1}, branches = {BranchesCovered1, BranchesValid1}},
    #result{line = {LineCovered2, LineValid2}, branches = {BranchesCovered2, BranchesValid2}}) ->
    #result{line = {LineCovered1 + LineCovered2, LineValid1 + LineValid2},
            branches = {BranchesCovered1 + BranchesCovered2, BranchesValid1 + BranchesValid2}};
sum({Covered1, Valid1}, {Covered2, Valid2}) ->
    {Covered1 + Covered2, Valid1 + Valid2}.

rate({_Covered, 0}) -> "0.0";
rate({Covered, Valid}) -> [Res] = io_lib:format("~f", [Covered / Valid]), Res.

% lookup source in source directory
lookup_source(Module) ->
    Sources = "src",
    Glob = "^" ++ atom_to_list(Module) ++ "\.erl\$",
    Fun = fun (Name, _In) ->
                   % substract directory
                   case lists:prefix(Sources, Name) of
                       true -> lists:nthtail(length(Sources), Name);
                       false -> Name
                   end
          end,
    Name = filelib:fold_files(Sources, Glob, true, Fun, ""),
    case Name of
        [$/ | Relative] -> Relative;
        _Other -> Name
    end.
