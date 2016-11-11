%%%-------------------------------------------------------------------
%%% @author aaronps
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Oct 2016 12:28
%%%-------------------------------------------------------------------
-module(enotepad_text).
-author("aaronps").

-include_lib("wx/include/wx.hrl").
-include("enotepad.hrl").

%% API
-export([
    create_control/2, has_selection/1,
    search/2, replace/3, replace_all/4
]).

-type fr_data() :: wxFindReplaceData:wxFindReplaceData().

-spec create_control(wxFrame:wxFrame(), wxFont:wxFont()) -> text_ctrl().
create_control(Frame, Font) ->

    TextCtrl = wxStyledTextCtrl:new(Frame),

    % to hide the horizontal scrollbar when start empty
    wxStyledTextCtrl:setScrollWidth(TextCtrl, 1),

    % by default, margin 1 has some width, remove it, info from:
    % http://www.garybeene.com/code/gbsnippets_gbs_00635.htm
    wxStyledTextCtrl:setMarginWidth(TextCtrl, 1, 0),

    case Font of
        undefined ->
            F = enotepad_font:default_font(),
            wxStyledTextCtrl:styleSetFont(TextCtrl, ?wxSTC_STYLE_DEFAULT, F),
            enotepad_font:destroy(F);

        _ ->
            wxStyledTextCtrl:styleSetFont(TextCtrl, ?wxSTC_STYLE_DEFAULT, Font)
    end,

    % set selected text color
    wxStyledTextCtrl:setSelBackground(TextCtrl, true, {00, 16#78, 16#d7}),
    wxStyledTextCtrl:setSelForeground(TextCtrl, true, {255, 255, 255}),

    % savepoints not needed right now
%%    wxStyledTextCtrl:connect(TextCtrl, stc_savepointreached),

    % stc_savepointleft used for enabling undo.
    wxStyledTextCtrl:connect(TextCtrl, stc_savepointleft),

    % stc_modified used for undo/redo text selection
    wxStyledTextCtrl:connect(TextCtrl, stc_modified),
    ModEventMask = ?wxSTC_PERFORMED_REDO bor ?wxSTC_PERFORMED_UNDO,
    wxStyledTextCtrl:setModEventMask(TextCtrl, ModEventMask),

    % stc_updateui needed to check if there is some text selected or not.
    wxStyledTextCtrl:connect(TextCtrl, stc_updateui),

    % to catch F1 later
    wxStyledTextCtrl:connect(TextCtrl, key_up, [{skip, true}]),


    configure_keys(TextCtrl),

    TextCtrl.

-spec has_selection(text_ctrl()) -> boolean().
has_selection(TextCtrl) ->
    case wxStyledTextCtrl:getSelection(TextCtrl) of
        {X,X} -> false;
        _ -> true
    end.

-spec search(text_ctrl(), fr_data()) -> 'ok'.
search(TextCtrl, FRData) ->
    FindString = wxFindReplaceData:getFindString(FRData),
    Flags = wxFindReplaceData:getFlags(FRData),
    SearchFlags = case Flags band ?wxFR_MATCHCASE of
                      ?wxFR_MATCHCASE -> ?wxSTC_FIND_MATCHCASE;
                      0 -> 0
                  end,

    {OldSelStart, OldSelEnd} = wxStyledTextCtrl:getSelection(TextCtrl),

    Pos =
        case Flags band ?wxFR_DOWN of
            ?wxFR_DOWN ->
                CurrentPos = wxStyledTextCtrl:getCurrentPos(TextCtrl),
                wxStyledTextCtrl:gotoPos(TextCtrl, CurrentPos),
                wxStyledTextCtrl:searchAnchor(TextCtrl),
                wxStyledTextCtrl:searchNext(TextCtrl, SearchFlags, FindString);

            _ ->
                wxStyledTextCtrl:searchAnchor(TextCtrl),
                wxStyledTextCtrl:searchPrev(TextCtrl, SearchFlags, FindString)
        end,

    case Pos of
        -1 ->
            wxStyledTextCtrl:setSelection(TextCtrl, OldSelStart, OldSelEnd),
            Message = "Cannot find \"" ++ FindString ++ "\"",
            enotepad_util:simple_dialog(Message, ?wxICON_INFORMATION);

        _ ->
            {S1,S2} = wxStyledTextCtrl:getSelection(TextCtrl),
            wxStyledTextCtrl:gotoPos(TextCtrl, S2),
            wxStyledTextCtrl:setSelection(TextCtrl, S1, S2)
    end.

-spec replace(text_ctrl(), fr_data(), string()) -> 'ok'.
replace(TextCtrl, FRData, Replacement) ->
    verify_search(TextCtrl, FRData)
        andalso wxStyledTextCtrl:replaceSelection(TextCtrl, Replacement).

-spec replace_all(text_ctrl(), string(), string(), integer()) -> 'ok'.
replace_all(TextCtrl, SearchString, ReplaceTo, Flags) ->
    wxStyledTextCtrl:gotoPos(TextCtrl, 0),
    SearchFlags = case Flags band ?wxFR_MATCHCASE of
                      ?wxFR_MATCHCASE -> ?wxSTC_FIND_MATCHCASE;
                      0 -> 0
                  end,

    TextLength = wxStyledTextCtrl:getLength(TextCtrl),

    wxStyledTextCtrl:setTargetStart(TextCtrl, 0),
    wxStyledTextCtrl:setTargetEnd(  TextCtrl, TextLength),
    wxStyledTextCtrl:setSearchFlags(TextCtrl, SearchFlags),

    case wxStyledTextCtrl:searchInTarget(TextCtrl, SearchString) of
        -1 -> ok;
        _ ->
            wxStyledTextCtrl:beginUndoAction(TextCtrl),
            undoall_hack(TextCtrl),

            internal_replace_all(TextCtrl, SearchString, ReplaceTo)
    end.

%%
%% private
%%

-spec verify_search(text_ctrl(), fr_data()) -> boolean().
verify_search(TextCtrl, FRData) ->
    case wxStyledTextCtrl:getSelection(TextCtrl) of
        {X, X} -> false;

        {SelBegin, SelEnd} ->
            SearchString = wxFindReplaceData:getFindString(FRData),
            Flags = wxFindReplaceData:getFlags(FRData),
            SearchFlags = case Flags band ?wxFR_MATCHCASE of
                              ?wxFR_MATCHCASE -> ?wxSTC_FIND_MATCHCASE;
                              0 -> 0
                          end,

            wxStyledTextCtrl:targetFromSelection(TextCtrl),
            wxStyledTextCtrl:setSearchFlags(TextCtrl, SearchFlags),

            wxStyledTextCtrl:searchInTarget(TextCtrl, SearchString) >= 0
                andalso wxStyledTextCtrl:getTargetStart(TextCtrl) =:= SelBegin
                andalso wxStyledTextCtrl:getTargetEnd(TextCtrl) =:= SelEnd
    end.

-spec internal_replace_all(text_ctrl(), string(), string()) -> 'ok'.
internal_replace_all(TextCtrl, SearchString, ReplaceTo) ->
    case wxStyledTextCtrl:searchInTarget(TextCtrl, SearchString) of
        -1 ->
            undoall_hack(TextCtrl),
            wxStyledTextCtrl:gotoPos(TextCtrl, 0),
            wxStyledTextCtrl:endUndoAction(TextCtrl);

        _ ->
            wxStyledTextCtrl:replaceTarget(TextCtrl, ReplaceTo),
            wxStyledTextCtrl:setTargetStart(TextCtrl, wxStyledTextCtrl:getTargetEnd(TextCtrl)),
            wxStyledTextCtrl:setTargetEnd(TextCtrl, wxStyledTextCtrl:getLength(TextCtrl)),
            internal_replace_all(TextCtrl, SearchString, ReplaceTo)
    end.

-spec undoall_hack(text_ctrl()) -> 'ok'.
%% this needs to be called before starting to replace all and after is finished
%% to simulate the "notepad undo", which, after a "replace all" undo selects
%% the whole document, we force scintilla to mark a whole document change.
%% needs to be before and after (within begin/end undo action) because the text
%% length may be different and in that case the undo/redo might not mark the
%% whole document.
undoall_hack(TextCtrl) ->
    % @todo test changing back to touch first and last byte.
    T = wxStyledTextCtrl:getText(TextCtrl),
    wxStyledTextCtrl:clearAll(TextCtrl),
    wxStyledTextCtrl:appendText(TextCtrl, T).

-define(none,       ?wxSTC_SCMOD_NORM).
-define(shift,      ?wxSTC_SCMOD_SHIFT).
-define(ctrl,       ?wxSTC_SCMOD_CTRL).
-define(ctrlshift,  ?wxSTC_SCMOD_CTRL bor ?wxSTC_SCMOD_SHIFT).
%% intellij-erlang doesn't like ?M, but the compilers doesn't complain.
-define(assign(K,M,Cmd), wxStyledTextCtrl:cmdKeyAssign(TextCtrl, K, ?M, Cmd)).

-spec configure_keys(text_ctrl()) -> 'ok'.
%% Also note: F1 opens help, but it is handled on_key_up on the main window.
configure_keys(TextCtrl) ->
    wxStyledTextCtrl:cmdKeyClearAll(TextCtrl),

    %% cursor keys

    ?assign(?wxSTC_KEY_UP,      none,       ?wxSTC_CMD_LINEUP),
    ?assign(?wxSTC_KEY_DOWN,    none,       ?wxSTC_CMD_LINEDOWN),
    ?assign(?wxSTC_KEY_LEFT,    none,       ?wxSTC_CMD_CHARLEFT),
    ?assign(?wxSTC_KEY_RIGHT,   none,       ?wxSTC_CMD_CHARRIGHT),
    ?assign(?wxSTC_KEY_LEFT,    ctrl,       ?wxSTC_CMD_WORDLEFT),
    ?assign(?wxSTC_KEY_RIGHT,   ctrl,       ?wxSTC_CMD_WORDRIGHT),
    ?assign(?wxSTC_KEY_UP,      shift,      ?wxSTC_CMD_LINEUPEXTEND),
    ?assign(?wxSTC_KEY_DOWN,    shift,      ?wxSTC_CMD_LINEDOWNEXTEND),
    ?assign(?wxSTC_KEY_LEFT,    shift,      ?wxSTC_CMD_CHARLEFTEXTEND),
    ?assign(?wxSTC_KEY_RIGHT,   shift,      ?wxSTC_CMD_CHARRIGHTEXTEND),

    ?assign(?wxSTC_KEY_LEFT,    ctrlshift,  ?wxSTC_CMD_WORDLEFTEXTEND),
    ?assign(?wxSTC_KEY_RIGHT,   ctrlshift,  ?wxSTC_CMD_WORDRIGHTEXTEND),

    %% PageUP and PageDown keys

    ?assign(?wxSTC_KEY_PRIOR,   none,       ?wxSTC_CMD_PAGEUP),
    ?assign(?wxSTC_KEY_NEXT,    none,       ?wxSTC_CMD_PAGEDOWN),

    ?assign(?wxSTC_KEY_PRIOR,   shift,      ?wxSTC_CMD_PAGEUPEXTEND),
    ?assign(?wxSTC_KEY_NEXT,    shift,      ?wxSTC_CMD_PAGEDOWNEXTEND),

    %% in my computer, notepad ctrl+page up/down does something weird, ignored.

    %% home and end

    ?assign(?wxSTC_KEY_HOME,    none,       ?wxSTC_CMD_HOME),
    ?assign(?wxSTC_KEY_END,     none,       ?wxSTC_CMD_LINEEND),

    ?assign(?wxSTC_KEY_HOME,    shift,      ?wxSTC_CMD_HOMEEXTEND),
    ?assign(?wxSTC_KEY_END,     shift,      ?wxSTC_CMD_LINEENDEXTEND),

    ?assign(?wxSTC_KEY_HOME,    ctrl,       ?wxSTC_CMD_DOCUMENTSTART),
    ?assign(?wxSTC_KEY_END,     ctrl,       ?wxSTC_CMD_DOCUMENTEND),

    ?assign(?wxSTC_KEY_HOME,    ctrlshift,  ?wxSTC_CMD_DOCUMENTSTARTEXTEND),
    ?assign(?wxSTC_KEY_END,     ctrlshift,  ?wxSTC_CMD_DOCUMENTENDEXTEND),

    %% delete key

    ?assign(?wxSTC_KEY_DELETE,  none,       ?wxSTC_CMD_CLEAR),
    ?assign(?wxSTC_KEY_BACK,    none,       ?wxSTC_CMD_DELETEBACK),
    ?assign(?wxSTC_KEY_DELETE,  shift,      ?wxSTC_CMD_DELETEBACK),
    ?assign(?wxSTC_KEY_DELETE,  ctrl,       ?wxSTC_CMD_DELLINERIGHT),

    %% insert key

    ?assign(?wxSTC_KEY_INSERT,  ctrl,       ?wxSTC_CMD_COPY),
    ?assign(?wxSTC_KEY_INSERT,  shift,      ?wxSTC_CMD_PASTE),

    %% add ctrl+j and ctrl+M as enter, ctrl+i as tab
    ?assign($J,                 ctrl,       ?wxSTC_CMD_NEWLINE),
    ?assign($M,                 ctrl,       ?wxSTC_CMD_NEWLINE),
    ?assign($I,                 ctrl,       ?wxSTC_CMD_TAB),

    %% alt-backspace is working as ctrl+z on my system, don't know why, ignored

    %% escape, ignore it or it will add strange characters
    ?assign(?wxSTC_KEY_ESCAPE,  none,       ?wxSTC_CMD_CANCEL),
    ?assign(?wxSTC_KEY_ESCAPE,  shift,      ?wxSTC_CMD_CANCEL).