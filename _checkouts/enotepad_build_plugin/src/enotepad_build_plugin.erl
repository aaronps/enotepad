-module(enotepad_build_plugin).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State2 = init_providers(State),
    State3 = merge_hook(State2, pre, {escriptize,enotepad_build_escript_icon}),
    {ok, State3}.

init_providers(State) ->
    lists:foldl(fun init_provider/2, State, [
        enotepad_build_winescript_prv,
        enotepad_build_escript_icon_prv
    ]).

init_provider(Module, State) ->
    {ok, NewState} = Module:init(State),
    NewState.

merge_hook(State, Type, Hook) ->
    OldOpts = rebar_state:opts(State),
    OldHooks = rebar_opts:get(OldOpts, provider_hooks, []),

    NewHooks = add_hook(OldHooks, Type, Hook),
    NewOpts = rebar_opts:set(OldOpts, provider_hooks, NewHooks),

    rebar_state:opts(State, NewOpts).

add_hook(OldHooks, Type, Hook) when Type =:= pre; Type =:= post ->
    %% This is needed because there is a bug on rebar3 were the plugins on _checkous will be initialized multiple times
    %% and due to this, the hook will be added multiple times.
    case lists:any(fun(X) -> X =:= Hook end, proplists:get_value(Type, OldHooks, [])) of
        true ->
            OldHooks;

        false ->
            NewTypeHooks = proplists:append_values(Type, [{Type, Hook} | OldHooks]),
            lists:keystore(Type, 1, OldHooks, {Type, NewTypeHooks})
    end.
