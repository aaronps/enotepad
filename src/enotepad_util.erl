%%%-------------------------------------------------------------------
%%% @author aaronps
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Oct 2016 12:58
%%%-------------------------------------------------------------------
-module(enotepad_util).
-author("aaronps").

-include_lib("wx/include/wx.hrl").
-include("enotepad.hrl").

%% API
-export([simple_dialog/2, refresh_scroll_width/1, get_version/0]).

-spec simple_dialog(string(), integer()) -> integer().
%% Shows a simple dialog.
simple_dialog(Message, Style) ->
    Dialog = wxMessageDialog:new(wx:null(), Message, [
        {caption, ?APPNAME},
        {style, Style}
    ]),

    try
        wxMessageDialog:showModal(Dialog)
    after
        wxMessageDialog:destroy(Dialog)
    end.

-spec refresh_scroll_width(text_ctrl()) -> 'ok'.
% needed because scintilla doesn't recalculate width
% also erlang's wxwidgets doesn't have the function of auto calculate
refresh_scroll_width(TextCtrl) ->
    wxStyledTextCtrl:setScrollWidth(TextCtrl, max_line_length(TextCtrl)).

-spec max_line_length(text_ctrl()) -> integer().
max_line_length(TextCtrl) ->
    max_line_length(TextCtrl, 0, wxStyledTextCtrl:getLineCount(TextCtrl), 0).

-spec max_line_length(text_ctrl(), integer(), integer(), integer())-> integer().
max_line_length(TextCtrl, LineNum, TotalLines, Max) when LineNum < TotalLines ->
    Line = wxStyledTextCtrl:getLine(TextCtrl, LineNum),
    Length = wxStyledTextCtrl:textWidth(TextCtrl,?wxSTC_STYLE_DEFAULT,Line),
    max_line_length(TextCtrl, LineNum + 1, TotalLines, max(Max, Length));

max_line_length(_, _, _, Max) -> Max.

-spec get_version() -> string().
get_version() ->
    case application:get_key(enotepad, vsn) of
        {ok, Version} -> Version;
        undefined -> "undefined"
    end.