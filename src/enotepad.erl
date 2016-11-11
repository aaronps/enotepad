%%%-------------------------------------------------------------------
%%% @author aaronps
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Oct 2016 10:02
%%%-------------------------------------------------------------------
-module(enotepad).
-author("aaronps").

-include_lib("wx/include/wx.hrl").

%% API
-export([main/0, main/1, start_link/0, start_link/1]).

-spec main() -> any().

main() -> main([]).

-spec main(list()) -> any().

main([]) ->
    application:load(enotepad),
    case enotepad_wx:start_link() of
        {error, _} = Error -> showError(Error);
        ENotepad -> enotepad_wx:wait_forever(ENotepad)
    end;

main([FileName]) ->
    application:load(enotepad),
    case enotepad_wx:start_link(FileName) of
        {error, _} = Error -> showError(Error);
        ENotepad ->
%%            enotepad_wx:open_file(ENotepad, FileName),
            enotepad_wx:wait_forever(ENotepad)
    end;

main([Filename | _]) ->
    main([Filename]).

-spec start_link() -> {'ok', pid(), wx:wx_object()}.
%% This should be the function called if running under a supervisor and if
%% running as 'application' it will check the 'file' key. It doesn't check the
%% command line parameters.
start_link() ->
    case application:get_env(file) of
        {ok, FileName} when is_list(FileName), FileName =/= "" ->
            io:format("I got a filename from app: ~p~n", [FileName]),
            start_link(FileName);

        {ok, Other} ->
            io:format("It wasn't a list: ~p~n", [Other]),
            case enotepad_wx:start_link() of
                {error, _} = E -> E;
                WxObject -> {ok, wx_object:get_pid(WxObject), WxObject}
            end;

        _ ->
            case enotepad_wx:start_link() of
                {error, _} = E -> E;
                WxObject -> {ok, wx_object:get_pid(WxObject), WxObject}
            end
    end.

-spec start_link(string()) -> {'ok', pid(), wx:wx_object()}.
start_link(FileName) ->
    case enotepad_wx:start_link(FileName) of
        {error, _} = E -> E;
        WxObject -> {ok, wx_object:get_pid(WxObject), WxObject}
    end.

%%
%% private
%%
-spec showError(any()) -> integer().
showError(Error) ->
    wx:new(),
    Message = io_lib:format("Something bad happened: ~p", [Error]),
    Dialog = wxMessageDialog:new(wx:null(), Message, [
        {caption, "Error"},
        {style, ?wxICON_ERROR}
    ]),
    try
        wxMessageDialog:showModal(Dialog)
    after
        wxMessageDialog:destroy(Dialog),
        wx:destroy()
    end.