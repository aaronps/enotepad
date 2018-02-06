%%%-------------------------------------------------------------------
%%% @author aaron
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Oct 2017 12:14
%%%-------------------------------------------------------------------
-module(enotepad_build_clean_prv).
-author("aaronps").

%% API
-export([init/1, do/1, format_error/1]).

-include_lib("kernel/include/file.hrl").

-define(PROVIDER, enotepad_build_clean).
-define(DEPS, []).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},            % The 'user friendly' name of the task
        {module, ?MODULE},            % The module implementation of the task
        {bare, false},                % The task cannot be run by the user
        {deps, ?DEPS},                % The list of dependencies
        {example, "rebar3 clean"},    % How to use the plugin
        {opts, []},
        {short_desc, "(enotepad) Removes extra files which `rebar3 clean -a` doesn't"},
        {desc, "Removes extra files which `rebar3 clean -a` doesn't"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    case rebar_state:current_app(State) of
        undefined ->
            rebar_file_utils:rm_rf("ebin");

        AppInfo when element(2,AppInfo) =:= <<"enotepad">> ->
            EscriptPath = enotepad_build_plugin:escript_path(State),
            CmdPath = <<EscriptPath/binary,".cmd">>,
            ExePath = <<EscriptPath/binary,".exe">>,

            file:delete(EscriptPath),
            file:delete(CmdPath),
            file:delete(ExePath),

            rebar_file_utils:rm_rf(rebar_app_info:out_dir(AppInfo));

        _ -> % ignore other apps
            ok
    end,

    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("ERROR: ~p", [Reason]).
