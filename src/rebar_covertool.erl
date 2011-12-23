-module(rebar_covertool).

%% Application callbacks
-export([eunit/2, ct/2]).

-include_lib("xmerl/include/xmerl.hrl").

-define(EUNIT_DIR, ".eunit").

-record(result, {line = {0, 0},
                 branches = {0, 0},
                 data = []}).

%% Run after eunit tests, export current coverage data in Cobertura format
eunit(Config, AppFile) ->
    AppName = get_app_name(AppFile),
    case rebar_config:get_local(Config, covertool_eunit, undefined) of
        undefined -> ok;
        Output ->
                {ok, CoverLog} = cover_init(),
                generate_report(AppName,
                                rebar_config:get_local(Config, covertool_prefix_len, 0),
                                cover:modules() ++ cover:imported_modules(), Output),
                file:close(CoverLog),
                ok
    end.

%% Run after common tests. Convert coverage data exported after tests are
%% executed into Cobertura format.
ct(Config, AppFile) ->
    AppName = get_app_name(AppFile),
    case rebar_config:get_local(Config, covertool_ct, undefined) of
        undefined -> ok;
        {From, To} ->
            cover:import(From),
            {ok, CoverLog} = cover_init(),
            generate_report(AppName,
                            rebar_config:get_local(Config, covertool_prefix_len, 0),
                            cover:imported_modules(), To),
            file:close(CoverLog),
            ok
    end.

%% Determine application name from the .app.src. If name cannot be
%% determined, use "Application"
get_app_name(AppFile) ->
    case rebar_app_utils:is_app_src(AppFile) of
        true ->
            case rebar_app_utils:load_app_file(AppFile) of
                {ok, AppName, _AppData} -> AppName;
                {error, _Reason} -> "Application"
            end;
        false -> "Application"
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

generate_report(AppName, PrefixLen, Modules, Output) ->
    io:format("Generating report '~s'...~n", [Output]),
    Prolog = ["<?xml version=\"1.0\" encoding=\"utf-8\"?>\n",
              "<!DOCTYPE coverage SYSTEM \"http://cobertura.sourceforge.net/xml/coverage-04.dtd\">\n"],

    {MegaSecs, Secs, MicroSecs} = now(),
    Timestamp = MegaSecs * 1000000000 + Secs * 1000 + (MicroSecs div 1000), % in milliseconds

    Version = "1.9.4.1", % emulate Cobertura 1.9.4.1
    Complexity = 0, % not supported at the moment

    Result = generate_packages(AppName, PrefixLen, Modules),
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
             {packages, Result#result.data}]},
    Report = xmerl:export_simple([Root], xmerl_xml, [{prolog, Prolog}]),
    write_output(Report, Output),
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
%% - source direcotry
%% - module prefix (name devided by "_")
package_name(AppName, PrefixLen, Module) ->
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
    string:join([atom_to_list(AppName)] ++ SourceDirs ++ Prefix, ".").

generate_package(PackageName, Modules) ->
    Classes = generate_classes(Modules),
    Data = {package, [{name, PackageName},
                      {'line-rate', rate(Classes#result.line)},
                      {'branch-rate', rate(Classes#result.branches)},
                      {complexity, 0}],
            [{classes, Classes#result.data}]},
    #result{data = Data}.

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
                                 {hits, Value}],
                          []},
                  {Data, Result#result{line = LineCoverage}}
          end,
    {ok, Lines} = cover:analyse(Module, calls, line),

    % XXX: ignore zero-indexed line, for some reason it is always present and always not hit
    Filter = fun({{_Module, 0}, 0}) -> false;
                (_Other) -> true
             end,
    Lines2 = lists:filter(Filter, Lines),
    {LinesData, Result} = lists:mapfoldl(Fun, #result{}, Lines2),

    Data = {class, [{name, Module},
                    {filename, lookup_source(Module)},
                    {'line-rate', rate(Result#result.line)},
                    {'branch-rate', rate(Result#result.branches)},
                    {complexity, 0}],
            [{methods, []},
             {lines, LinesData}]},
    Result#result{data = Data}.

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
    Name = filelib:fold_files(Sources, Glob, true, Fun, false),
    case Name of
        false -> false;
        [$/ | Relative] -> Relative;
        _Other -> Name
    end.
