-module(enotepad_build_plugin).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, init_providers(State)}.

init_providers(State) ->
    lists:foldl(fun init_provider/2, State, [
        enotepad_build_winescript_prv,
        enotepad_build_escript_icon_prv,
        enotepad_build_clean_prv
    ]).

init_provider(Module, State) ->
    {ok, NewState} = Module:init(State),
    NewState.
