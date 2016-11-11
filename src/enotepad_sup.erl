%%%-------------------------------------------------------------------
%%% @author aaronps
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Oct 2016 10:31
%%%-------------------------------------------------------------------
-module(enotepad_sup).
-author("aaronps").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
-spec start_link() -> {'ok', pid()} | 'ignore' | {'error', _}.

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
-spec init([]) -> {'ok', {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    % one_for_one, one_for_all, rest_for_one, simple_one_for_one; in seconds
    SupFlags = #{strategy => one_for_one, intensity => 0, period => 5},
    AChild = #{
        id      => enotepad, % anything but a pid()
        start   => {enotepad, start_link, []}
%%        restart => permanent, %% permanent | temporary | transient
%%        shutdown=> 5000,      %% brutal_kill | number (ms) | infinity
%%        type    => worker,    %% worker | supervisor
%%        modules => ['Module'] %% Module or dynamic (if gen_event)
    },
    {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
