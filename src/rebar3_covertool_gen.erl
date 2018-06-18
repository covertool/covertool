-module(rebar3_covertool_gen).

%% rebar3 plugin callbacks
-export([init/1, do/1, format_error/1]).

-include("covertool.hrl").
-include_lib("kernel/include/file.hrl").


%% ===================================================================
%% Rebar3 plugin callbacks
%% ===================================================================
init( State ) ->
    Options = [{module, ?MODULE},
               {namespace, covertool},
               {name, generate},
               {deps, [{default, app_discovery}]},
               {example, "rebar3 covertool generate"},
               {short_desc, "Generate Cobertura output"},
               {desc, "Process the rebar3 cover generated .coverdata files, "
                      "and produce an Cobertura XML into the _build/test/covertool/ "
                      "directory for each application in the rebar3 project."},
               {opts, covertool_opts(State)},
               {profiles, [test]}],
    NewState = rebar_state:add_provider(State, providers:create(Options)),
    {ok, NewState}.

    
do(State) ->
    OutputFiles = output_files(State),
    InputFiles = input_files(State),
    Apps = get_apps(State),
    case generate( State, OutputFiles, InputFiles, Apps ) of
        ok ->
            {ok, State};
        Error ->
            {error, {?MODULE, Error}}
    end.


format_error(Reason) ->
    io_lib:format("~p", [Reason]).


%% ===================================================================
%% Internal Functions
%% ===================================================================
output_files(State) ->
    filelib:wildcard(filename:join(outdir(State), "*.xml")).


input_files(State) ->
    ProfileDir = rebar_dir:base_dir(State),
    CoverDataFiles = coverdata_files(State),
    FullPaths = [filename:join([ProfileDir, "cover", File]) || File <- CoverDataFiles],
    filter_existing_inputs(FullPaths).

filter_existing_inputs([]) ->
    [];
filter_existing_inputs([H|T]) ->
    case file_exists(H) of
        true ->
            [H|filter_existing_inputs(T)];
        false ->
            rebar_api:info( "Skipping non-existing file ~s", [H] ),
            filter_existing_inputs(T)
    end.

generate( State, OutputFiles, InputFiles, Apps ) ->
    %% blow away any output files (if present), and make directory exist
    [file:delete( File ) || File <- OutputFiles],
    ProfileDir = rebar_dir:base_dir(State),
    filelib:ensure_dir( filename:join(outdir(State), "dummy") ),
    case generate_init( InputFiles, filename:join(ProfileDir, "covertool.log") ) of
        {ok, LogFile} -> generate_apps( State, Apps, LogFile );
        Otherwise -> Otherwise
    end.

generate_init( InputFiles, LogFilePath ) ->
    case rebar_covertool:cover_init( LogFilePath ) of
        {ok, LogFile} ->
            generate_import( InputFiles, LogFile );
        Otherwise -> Otherwise
    end.

generate_import( [File | T], LogFile ) ->
    rebar_api:info( "Importing file ~s", [File] ),
    case cover:import( File ) of
        ok -> generate_import( T, LogFile );
        Otherwise ->
            file:close( LogFile ),
            Otherwise
    end;
generate_import( [], LogFile ) ->
    {ok, LogFile}.

get_apps(State) ->
    ProjectApps = [app_to_atom(rebar_app_info:name(PA))
                   || PA <- rebar_state:project_apps(State)],
    IncludeApps = include_apps(State),
    lists:usort(ProjectApps ++ IncludeApps).      

generate_apps( State, Apps, LogFile ) ->
    Result = lists:foldl( fun(App, Result) -> generate_app(State, App, Result) end, ok, Apps ),
    file:close( LogFile ),
    Result.

generate_app(State, App, Result) ->
    OutputFile = filename:join(outdir(State), atom_to_list(App) ++ ".covertool.xml"),
    EbinPath = [filename:join([rebar_dir:base_dir(State), "lib", atom_to_list(App), "ebin"])],
    Config = #config{ appname = App, output = OutputFile, beams = [EbinPath]},
    case covertool:generate_report( Config, cover:imported_modules() ) of
        ok -> Result;
        Otherwise -> Otherwise
    end.

outdir(State) ->
    ProfileDir = rebar_dir:base_dir(State),
    filename:join(ProfileDir, "covertool").

file_exists(Filename) ->
    case file:read_file_info(Filename) of
        {ok, _} ->
            true;
        {error, enoent} ->
            false;
        Reason ->
            exit(Reason)
    end.

command_line_opts(State) ->
    {Opts, _} = rebar_state:command_parsed_args(State),
    Opts.

config_opts(State) ->
    rebar_state:get(State, covertool, []).

include_apps(State) ->
    Command = proplists:get_value(include_apps, command_line_opts(State), undefined),
    Config = proplists:get_value(include_apps, config_opts(State), undefined),
    Apps = case {Command, Config} of
        {undefined, undefined} -> [];
        {undefined, CfgApps}   -> CfgApps;
        {CmdApps, _}           -> string:tokens(CmdApps, ",")
    end,
    [app_to_atom(A) || A <- Apps].

coverdata_files(State) ->
    Config = config_opts(State),
    proplists:get_value(coverdata_files, Config, ["eunit.coverdata", "ct.coverdata"]).

covertool_opts(_State) ->
    [{include_apps, $a, "include_apps", string, help(include_apps)}].

help(include_apps) ->
    "A CSV of OTP app dependencies to include in covertool output. "
    "Note that this data must be present in the coverdata files to begin with; "
    "this can be accomplished using a ct cover.spec file, etc.".

app_to_atom(A) when is_atom(A) -> A;
app_to_atom(S) when is_list(S) -> list_to_atom(S);
app_to_atom(B) when is_binary(B) -> binary_to_atom(B, latin1).
