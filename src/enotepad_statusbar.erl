%%%-------------------------------------------------------------------
%%% @author aaronps
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Oct 2016 13:56
%%%-------------------------------------------------------------------
-module(enotepad_statusbar).
-author("aaronps").

-export_type([statusbar/0]).

%% API
-export([create/1, update_position/3, set_visible/2]).

-record(statusbar, {
    control :: wxStatusBar:wxStatusBar(),
    frame   :: wxFrame:wxFrame()
}).

%% @type statusbar(). Custom StatusBar state.
-opaque statusbar() :: #statusbar{}.


-spec create(Frame :: wxFrame:wxFrame()) -> statusbar().
create(Frame) ->
    StatusBar = wxFrame:createStatusBar(Frame, [{number, 2}]),
    wxStatusBar:setFieldsCount(StatusBar, 2, [{widths, [-77, -23]}]),
    #statusbar{control = StatusBar, frame = Frame}.

-spec update_position(StatusBar, TextCtrl, Position) -> 'ok'
      when
        StatusBar :: statusbar(),
        TextCtrl  :: wxStyledTextCtrl:wxStyledTextCtrl(),
        Position  :: integer().

update_position(#statusbar{control = StatusBar}, TextCtrl, Position) ->
    Line = wxStyledTextCtrl:lineFromPosition(TextCtrl, Position),

    % column shows the correct column position but in notepad we want the
    % offset within the line, there is a difference when there are tabs
%%    Column = wxStyledTextCtrl:getColumn(TextCtrl, Position),

    % this is what nodepad does
    Column = Position - wxStyledTextCtrl:positionFromLine(TextCtrl, Line),

    Text = "    Ln " ++ integer_to_list(Line+1)
        ++ ", Col "  ++ integer_to_list(Column+1),

    wxStatusBar:setStatusText(StatusBar, Text, [{number, 1}]).

-spec set_visible(StatuBar :: statusbar(), boolean()) -> 'ok'.
set_visible(#statusbar{control = StatusBar, frame = Frame}, true) ->
    wxStatusBar:show(StatusBar),
    wxFrame:sendSizeEvent(Frame);

set_visible(#statusbar{control = StatusBar, frame = Frame}, false) ->
    wxStatusBar:hide(StatusBar),
    wxFrame:sendSizeEvent(Frame).

