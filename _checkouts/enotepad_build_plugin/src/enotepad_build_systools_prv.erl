%%%-------------------------------------------------------------------
%%% @author aaron
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Oct 2017 22:49
%%%-------------------------------------------------------------------
-module(enotepad_build_systools_prv).
-author("aaronps").

%% API
-export([init/1, do/1, format_error/1]).

-define(PROVIDER, systools).
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
        {short_desc, "(enotepad) Creates a release of enotepad using systools"},
        {desc, "Creates a release of enotepad using systools"}
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

do_release(_State, false) ->
    rebar_api:abort("Couldn't find enotepad app", []);

do_release(State, App) ->
    ReleaseDir = release_dest_path(State),
    prepare_release_dir(ReleaseDir),

    Rel = generate_systools_rel(App),
    ReleaseVersion = rebar_app_info:original_vsn(App),
    ReleaseId = "enotepad-" ++ ReleaseVersion,
    ReleaseBase = filename:join(ReleaseDir, ReleaseId),


    file:write_file(ReleaseBase ++ ".rel", io_lib:format("~p.", [Rel])),

    rebar_api:info("Making release ~s", [ReleaseId]),

    OutEbin = filename:join(rebar_app_info:out_dir(App), "ebin"),

    systools:make_script(ReleaseBase, [
        local,
        {path, [OutEbin]}
    ]),

    systools:make_tar(ReleaseBase, [
        {erts, code:root_dir()},
        {path, [OutEbin]}
    ]),
    io:format("Release ok in '~s'~n", [ReleaseBase ++ ".tar.gz"]),

    {ok, State}.

release_dest_path(State) ->
    filename:join([rebar_dir:base_dir(State), "systools"]).

prepare_release_dir(Dir) ->
    rebar_file_utils:rm_rf(Dir),
    filelib:ensure_dir(filename:join(Dir, "dummy")).

generate_systools_rel(App) ->
    Applications = rebar_app_info:applications(App),
    ReleaseVersion = rebar_app_info:original_vsn(App),
    [ application:load(X) || X <- Applications],
    { release,
        {"enotepad", ReleaseVersion},
        {erts, erlang:system_info(version) },
            [
                { X, get_app_version(X)} || X <- Applications
            ] ++ [{ enotepad, ReleaseVersion}]
    }.

get_app_version(App) ->
    case application:get_key(App, vsn) of
        {ok, Version} -> Version;
        _ -> rebar_api:abort("Cannot get version for application ~p~n", [App])
    end.