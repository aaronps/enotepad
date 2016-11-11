%%%-------------------------------------------------------------------
%%% @author aaronps
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Oct 2016 10:51
%%%-------------------------------------------------------------------
-module(enotepad_app).
-author("aaronps").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-spec start(application:start_type(), _)
        -> {'ok',pid()} | {'ok',pid(),_} | {'error',_}.

start(_StartType, _StartArgs) ->
    enotepad_sup:start_link().

-spec stop(_) -> 'ok'.
stop(_State) ->
    ok.
