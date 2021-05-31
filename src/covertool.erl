-module(covertool).

%% command line entry point for escriptize
-export([main/1]).

%% application entry point
-export([generate_report/2]).

%% rebar3 callbacks
-export([init/1]).

%% rebar2 callbacks
-export([eunit/2, ct/2]).

%% mix callbacks
-export([start/2]).

-include("covertool.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-record(result, {line = {0, 0},
                 branches = {0, 0},
                 data = []}).

main([]) ->
    usage();
main(Args) ->
    Config = process_args(Args, #config{}),
    CoverData = Config#config.cover_data,
    io:format("Importing '~s' data file...~n", [CoverData]),
    cover:import(CoverData),
    Modules = cover:imported_modules(),
    io:format("Found ~w modules.~n", [length(Modules)]),
    generate_report(Config#config{cover_data = no_import}, Modules),
    io:format("Done.~n"),
    ok.

%% Plugin callbacks

%% Rebar3 plugin registration callback
init(State) ->
    rebar3_covertool_gen:init(State).

%% Rebar plugin callbacks
eunit(Config, AppFile) ->
    rebar_covertool:eunit(Config, AppFile).

ct(Config, AppFile) ->
    rebar_covertool:ct(Config, AppFile).

%% Mix callbacks
start(CompilePath, Opts) ->
    mix_covertool:start(CompilePath, Opts).

%% End of plugin callbacks

usage() ->
    ScriptName = escript:script_name(),
    io:format("Usage: ~s [Options]~n", [ScriptName]),
    io:format("Options:~n"),
    io:format("    -cover   CoverDataFile  Path to the cover exported data set (default: \"all.coverdata\")~n"),
    io:format("    -output  OutputFile     File to put generated report to (default: \"coverage.xml\")~n"),
    io:format("    -ebin    EbinDir        Directory to look for beams (default: \"ebin\")~n"),
    io:format("    -src     SourceDir      Directory to look for sources (default: \"src\")~n"),
    io:format("    -prefix  PrefixLen      Length used for package name (default: 0)~n"),
    io:format("    -appname AppName        Application name to put in the report (default: \"Application\")~n"),
    ok.

% Parse arguments into record
process_args([], Config) -> Config;
process_args([[$- | Name] , Value | Args], Config) ->
    NameAtom = list_to_atom(Name),
    process_args(Args, update_config(NameAtom, Value, Config));
process_args(_Args, _Config) ->
    usage(),
    halt(1).

update_config(cover, Value, Config) ->
    Config#config{cover_data = Value};
update_config(output, Value, Config) ->
    Config#config{output = Value};
update_config(src, Value, Config) ->
    Config#config{sources = string:tokens(Value, ",")};
update_config(ebin, Value, Config) ->
    Config#config{beams = string:tokens(Value, ",")};
update_config(prefix, Value, Config) ->
    Config#config{prefix_len = list_to_integer(Value)};
update_config(appname, Value, Config) ->
    Config#config{appname = list_to_atom(Value)};
update_config(_Other, _Value, _Config) ->
    usage(),
    halt(1).

generate_report(Config, Modules) ->
    AppName = Config#config.appname,
    PrefixLen = Config#config.prefix_len,
    Output = Config#config.output,
    case Config#config.cover_data of
        no_import ->
            ok;
        CoverData ->
            cover:import(CoverData)
    end,
    put(src, Config#config.sources),
    put(ebin, Config#config.beams),
    io:format("Generating report '~s'...~n", [Output]),
    Prolog = ["<?xml version=\"1.0\" encoding=\"utf-8\"?>\n",
              "<!DOCTYPE coverage SYSTEM \"http://cobertura.sourceforge.net/xml/coverage-04.dtd\">\n"],

    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    Timestamp = MegaSecs * 1000000000 + Secs * 1000 + (MicroSecs div 1000), % in milliseconds

    Version = "1.9.4.1", % emulate Cobertura 1.9.4.1
    Complexity = 0, % not supported at the moment

    Result = generate_packages(AppName, PrefixLen, Modules),
    {LinesCovered, LinesValid} = Result#result.line,
    LineRate = rate(Result#result.line),

    {BranchesCovered, BranchesValid} = Result#result.branches,
    BranchRate = rate(Result#result.branches),

    Sources = [{source, [filename:absname(SrcDir)]} || SrcDir <- get(src)],

    Root = {coverage, [{timestamp, Timestamp},
                       {'line-rate', LineRate},
                       {'lines-covered', LinesCovered},
                       {'lines-valid', LinesValid},
                       {'branch-rate', BranchRate},
                       {'branches-covered', BranchesCovered},
                       {'branches-valid', BranchesValid},
                       {complexity, Complexity},
                       {version, Version}],
            [{sources, Sources},
             {packages, Result#result.data}]},
    Report = xmerl:export_simple([Root], xmerl_xml, [{prolog, Prolog}]),
    write_output(Report, Output),
    if
        Config#config.summary ->
            io:format("Line total: ~s~nBranch total: ~s~n", [
                                                             percentage(Result#result.line),
                                                             percentage(Result#result.branches)
                                                            ]);
        true ->
            ok
    end,
    ok.

generate_packages(AppName, PrefixLen, Modules) ->
    PackageAndModules =
        lists:foldl(fun(Module, Acc) ->
                        PackageName = package_name(AppName, PrefixLen, Module),
                        case lists:keyfind(PackageName, 1, Acc) of
                            false ->
                                [{PackageName, [Module]} | Acc];
                            {_Key, ModulesInPackage} ->
                                lists:keyreplace(
                                  PackageName, 1, Acc,
                                  {PackageName, [Module|ModulesInPackage]})
                        end
                    end, [], Modules),
    Fun = fun({PackageName, ModulesInPackage}, Result) ->
                  Package = generate_package(PackageName, ModulesInPackage),
                  {Package#result.data, sum(Result, Package)}
          end,
    {Packages, Result} = lists:mapfoldl(Fun, #result{}, PackageAndModules),
    Result#result{data = Packages}.

%% Package name is generated by
%% - AppName itself
%% - source directory
%% - module prefix (name divided by "_")
package_name(AppName, PrefixLen, Module)
    when is_atom(AppName), is_atom(Module) ->
    AppNameStr = atom_to_list(AppName),
    SourceDirs = case lookup_source(Module) of
                    false ->
                        [];
                    SourceFile ->
                        case filename:dirname(SourceFile) of
                            "." ->
                                [];
                            DirName ->
                                string:tokens(DirName, "/")
                        end
                end,
    Prefix = case PrefixLen of
                 0 ->
                     "";
                 _Other ->
                     lists:sublist(string:tokens(atom_to_list(Module), "_"),
                                   PrefixLen)
             end,
    string:join([AppNameStr] ++ SourceDirs ++ Prefix, ".").

generate_package(PackageName, Modules) ->
    Classes = generate_classes(Modules),
    Data = {package, [{name, PackageName},
                      {'line-rate', rate(Classes#result.line)},
                      {'branch-rate', rate(Classes#result.branches)},
                      {complexity, 0}],
            [{classes, Classes#result.data}]},
    Classes#result{data = Data}.

% generate <classes> element, each Erlang module is "class"
generate_classes(Modules) ->
    % generate XML for every class, collect summary metric
    Fun = fun(Module, Result) ->
                  Class = generate_class(Module),
                  {Class#result.data, sum(Result, Class)}
          end,

    % Skip modules without sources
    Filter = fun(Module) ->
                     case lookup_source(Module) of
                         false -> false;
                         _Other -> true
                     end
             end,
    Modules2 = lists:filter(Filter, Modules),
    {Classes, Result} = lists:mapfoldl(Fun, #result{}, Modules2),
    Result#result{data = Classes}.

generate_class(Module) ->
    Fun = fun({{_Module, Line}, Value}, Result) ->
                  Covered = case Value of 0 -> 0; _Other -> 1 end,
                  LineCoverage = sum(Result#result.line, {Covered, 1}), % add one line to the summary
                  Data = {line, [{number, Line},
                                 {hits, Value},
                                 {branch, "False"}],
                          []},
                  {Data, Result#result{line = LineCoverage}}
          end,
    {ok, Lines0} = cover:analyse(Module, calls, line),
    Lines = dedup(Lines0),

    % ignore zero-indexed lines, these are lines for generated code
    Filter = fun({{_Module, 0}, _}) -> false;
                (_Other) -> true
             end,
    Lines2 = lists:filter(Filter, Lines),
    {LinesData, Result} = lists:mapfoldl(Fun, #result{}, Lines2),

    Data = {class, [{name, Module},
                    {filename, lookup_source(Module)},
                    {'line-rate', rate(Result#result.line)},
                    {'branch-rate', rate(Result#result.branches)},
                    {complexity, 0}],
            [{methods, generate_methods(Module, LinesData)},
             {lines, LinesData}]},
    Result#result{data = Data}.

generate_methods(Module, LinesData) ->
    ResultFun = fun ({line, LineData, _}, Result) ->
                    Hits = proplists:get_value(hits, LineData, 0),
                    Covered = case Hits of 0 -> 0; _Other -> 1 end,
                    LineCoverage = sum(Result#result.line, {Covered, 1}),
                    Result#result{line = LineCoverage}
                end,

    {ok, Functions} = cover:analyse(Module, calls, function),
    MFAs = lists:map(fun (F) -> element(1, F) end, Functions),
    lists:flatten(lists:map(
        fun ({_M, F, A} = MFA) ->
                case function_lines(MFA, LinesData) of
                    [] -> [];
                    FunLinesData ->
                        Result = lists:foldl(ResultFun, #result{}, FunLinesData),
                        {method, [{name, F},
                                  {signature, io_lib:format("~s/~B", [F, A])},
                                  {'line-rate', rate(Result#result.line)},
                                  {'branch-rate', rate(Result#result.branches)}],
                                 [{lines, FunLinesData}]}
                end
        end, MFAs)).

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
rate({Covered, Valid}) -> float_to_list(Covered / Valid, [{decimals, 3}, compact]).

percentage({_Covered, 0}) -> "100.0%";
percentage({Covered, Valid}) ->
    float_to_list(100 * Covered / Valid, [{decimals, 1}, compact]) ++ "%".

% lookup source in source directory
lookup_source(Module) ->
    lookup_source(get(ebin), Module).

lookup_source([EbinDir | RDirs], M) ->
    Beam = io_lib:format("~s/~s.beam", [EbinDir, M]),
    case beam_lib:chunks(Beam, [compile_info]) of
        {ok, {M, [{compile_info, CompileInfo}]}} ->
            case proplists:get_value(source, CompileInfo) of
                undefined -> false;
                AbsPath -> relative_to_src_path(AbsPath)
            end;
        _ ->
            lookup_source(RDirs, M)
    end;
lookup_source(_, _) ->
    false.

relative_to_src_path(AbsPath) ->
    Src = get(src),
    relative_to_src_path(Src, AbsPath).

relative_to_src_path([SrcDir|RDirs], AbsPath) ->
    case lists:prefix(SrcDir, AbsPath) of
        true -> ensure_relative_path(lists:nthtail(length(SrcDir), AbsPath));
        false -> relative_to_src_path(RDirs, AbsPath)
    end;
relative_to_src_path([], AbsPath) ->
    AbsPath.

ensure_relative_path([$/ | RelPath]) -> RelPath;
ensure_relative_path(RelPath) -> RelPath.

% lookup start and end lines for function
function_range({M, F, A}) ->
    function_range(get(ebin), M, F, A).

function_range([EbinDir | RDirs], M, F, A) ->
    Beam = io_lib:format("~s/~s.beam", [EbinDir, M]),
    case beam_lib:chunks(Beam, [abstract_code]) of
        {error, beam_lib, _} -> function_range(RDirs, M, F, A);
        {ok, {M, [{abstract_code, no_abstract_code}]}} -> {0, 0};
        {ok, {M, [{abstract_code, {_Version, AC}}]}} ->
            Filter = fun ({function, _, Fun, Ary, _}) ->
                           not (Fun =:= F andalso Ary =:= A);
                         (_) -> true
                     end,
            case lists:dropwhile(Filter, AC) of
                [] -> {0, 0}; %% Should never happen unless beam is out of sync
                [{_, Line, F, A, _}] -> {Line, Line};
                [{_, Line, F, A, _}, {eof, Next}] -> {Line, Next};
                [{_, Line, F, A, _}, {_, N, _, _, _} | _] when is_integer(N) ->
                    {Line, N - 1};
                [{_, Line, F, A, _} | _] ->
                    {Line, Line}
            end
    end;
function_range(_, _M, _F, _A) ->
    {0, 0}.

% filter lines by function
function_lines(MFA, LinesData) ->
    {Start, End} = function_range(MFA),
    lists:filter(fun (LineData) ->
                     Line = proplists:get_value(number, element(2, LineData)),
                     Line > Start andalso Line =< End
                 end, LinesData).

dedup(List) -> dedup(lists:sort(List), []).

dedup([], Agg) -> lists:reverse(Agg);
dedup([{Pos, C1}, {Pos, C2} | Rest], Agg) ->
    dedup([{Pos, C1 + C2} | Rest], Agg);
dedup([Entry | Rest], Agg) -> dedup(Rest, [Entry | Agg]).
