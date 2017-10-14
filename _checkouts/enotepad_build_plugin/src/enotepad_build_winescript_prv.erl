-module(enotepad_build_winescript_prv).

-export([init/1, do/1, format_error/1]).

-include_lib("wx/include/wx.hrl").

-define(PROVIDER, winescript).
-define(DEPS, [app_discovery, escriptize]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 winescript exe"}, % How to use the plugin
            {opts, [
                {target, undefined, undefined, {string, "shortcmd"}, "Feature to apply"}
            ]},
            {short_desc, "escriptize with extra features for windows"},
            {desc, "escriptize with extra features for windows, targets are:

    shortcmd - (default) replaces the rebar generated .cmd with a shorter one.
    single   - replaces the rebar generated .cmd with a single file .cmd escript (might not work after Erlang 20).
    exe      - replaces the rebar generated .cmd with a .exe file~n"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {ParsedArguments,_} = rebar_state:command_parsed_args(State),
    do_target(proplists:get_value(target, ParsedArguments), State).

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

do_target("shortcmd", State) ->
    EscriptPath = rebar_state:escript_path(State),
    CmdPath = <<EscriptPath/binary,".cmd">>,
    rebar_api:info("Replacing ~p with sorter version~n", [CmdPath]),
    file:write_file(CmdPath, shebang(windows_mini)),
    {ok, State};

do_target("single", State) ->
    rebar_api:warn("*** WARNING *** Windows `shebang` for escript stopped working on Erlang 20", []),
    EscriptPath = rebar_state:escript_path(State),

    {ok, [_Shebang, Comment, EmuArgs, Body]} = escript:extract(binary_to_list(EscriptPath), []),
    {ok, NewEscriptBin} = escript:create(binary, [Comment, EmuArgs, Body]),

    CmdPath = <<EscriptPath/binary,".cmd">>,
    ok = file:write_file(CmdPath, [shebang(windows), NewEscriptBin]),

    {ok, State};

do_target("exe", State) ->
    EscriptPath = rebar_state:escript_path(State),
    CmdPath = <<EscriptPath/binary,".cmd">>,
    ExePath = <<EscriptPath/binary,".exe">>,
    rebar_api:info("Replacing ~p with .exe version~n", [CmdPath]),
    ok = file:delete(CmdPath),
    {ok, _} = file:copy(filename:join("launcher", "escriptrun.exe"),ExePath),
    {ok, State};

do_target(Target, _State) ->
    rebar_api:abort("Invalid target: ~p~n", [Target]).

shebang(windows) -> <<"@escript %~f0 %* & exit /b\r\n">>;
shebang(windows_mini) -> <<"@escript %~dpn0 %*\r\n">>.
