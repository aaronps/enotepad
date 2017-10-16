%%%-------------------------------------------------------------------
%%% @author aaron
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Oct 2017 14:32
%%%-------------------------------------------------------------------
-module(enotepad_build_reltool_prv).
-author("aaronps").

%% API
-export([init/1, do/1, format_error/1]).

-include_lib("kernel/include/file.hrl").

-define(PROVIDER, reltool).
-define(DEPS, [app_discovery, compile]).

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
        {example, "rebar3 " ++ atom_to_list(?PROVIDER)},  % How to use the plugin
        {opts, []},
        {short_desc, "Creates a release of enotepad using reltool"},
        {desc, "Creates a release of enotepad using reltool"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    do_release(State, find_enotepad_app(State)),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

find_enotepad_app(State) ->
    Apps = rebar_state:project_apps(State),
    lists:keyfind(<<"enotepad">>, 2, Apps).

do_release(State, false) ->
    rebar_api:abort("Couldn't find enotepad app", []);

do_release(State, App) ->
    rebar_api:info("Got: ~p",[App]),
    BaseDir = rebar_dir:base_dir(State),
    ReleaseDir = release_dest_path(State),
    prepare_release_dir(ReleaseDir),

    AppVersion = rebar_app_info:original_vsn(App),
    rebar_api:info("--> making release ~p~n", [AppVersion]),

    R = reltool:create_target([{config, {sys,[
        {relocatable, true},
        {profile, standalone},
        {rel, "enotepad", AppVersion,
                rebar_app_info:applications(App) ++ [enotepad]
        },
        {app, enotepad, [
            {lib_dir, rebar_app_info:out_dir(App) }
        ]},
        {boot_rel, "enotepad"}
    ]}}], ReleaseDir),

    case os:type() of
        { win32, _} -> file:copy(filename:join("launcher", "erlrun.exe"),
            filename:join(ReleaseDir, "enotepad.exe"));

        { unix, _} ->  file:copy(filename:join("launcher", "erlrun.sh"),
            filename:join(ReleaseDir, "enotepad")),
            % althrough the filename:join is the same in this two line,
            % keep it like this for reading purpose.
            set_executable_bit(filename:join(ReleaseDir, "enotepad"))
    end,

    io:format("reltool result: ~p~n", [R]),

    {ok, State}.

release_dest_path(State) ->
    filename:join([rebar_dir:base_dir(State), "reltool"]).

prepare_release_dir(Dir) ->
    rebar_file_utils:rm_rf(Dir),
    filelib:ensure_dir(filename:join(Dir, "dummy")).

set_executable_bit(Filename) ->
    {ok, #file_info{mode = OldMode}} = file:read_file_info(Filename),
    case OldMode bor 8#111 of
        OldMode -> ok; % no change, do nothing
        NewMode -> file:change_mode(Filename, NewMode)
    end.
