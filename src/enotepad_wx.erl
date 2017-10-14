%%%-------------------------------------------------------------------
%%% @author aaronps
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Sep 2016 10:33 AM
%%%-------------------------------------------------------------------
-module(enotepad_wx).
-author("aaronps").

-behaviour(wx_object).

-include_lib("wx/include/wx.hrl").
-include("wxFindDialogEvent_ex.hrl").
-include("enotepad.hrl").

%% API
-export_type([enotepad/0]).
-export([start/0, start/1, start_link/0, start_link/1, stop/1, wait_forever/1]).


%% for config check
-export_type([window/0]).
-export([config_check/1]).

%% wx_object callbacks
-export([init/1, handle_event/2, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(window, {
    x       :: integer(),
    y       :: integer(),
    width   :: non_neg_integer(),
    height  :: non_neg_integer()
}).

-record(state, {
    frame      :: wxFrame:wxFrame(),
    textctrl   :: wxStyledTextCtrl:wxStyledTextCtrl(),
    menu       :: wxMenuBar:wxMenuBar(),
    statusbar  :: enotepad_statusbar:statusbar(),
    font       :: wxFont:wxFont(),
    print      :: enotepad_print:enotepad_print(),
    fr_data    :: wxFindReplaceData:wxFindReplaceData() | 'undefined',
    fr_dialog  :: wxFindReplaceDialog_ex:wxFindReplaceDialog_ex() | 'undefined',

    filename                :: string() | 'undefined',
    select_start = -1       :: integer(),
    select_end = -1         :: integer(),
    is_selecting = false    :: boolean(),
    position = -1           :: integer(),
    empty_state = true      :: boolean()
}).

-type    state() :: #state{}.
-opaque window() :: #window{}.
-type enotepad() :: wxWindow:wxWindow().

-define(MENU_EVENT(Id),
    #wx{id = Id,
        event = #wxCommand{type = command_menu_selected}
    }).

-define(MENU_EVENT(Id,CmdValue),
    #wx{id = Id,
        event = #wxCommand{ type = command_menu_selected,
            commandInt = CmdValue
        }
    }).

-define(UNDOREDOEND, (?wxSTC_LASTSTEPINUNDOREDO bor ?wxSTC_MOD_INSERTTEXT)).


-spec start() -> enotepad() | {'error', any()}.
-spec start(FileName::string()) -> enotepad() | {'error', any()}.
-spec start_link() -> enotepad() | {'error', any()}.
-spec start_link(FileName::string()) -> enotepad() | {'error', any()}.
-spec stop(enotepad()) -> 'ok'.
-spec wait_forever(enotepad()) -> 'ok'.

start()         -> wx_object:start(?MODULE, [], []).
start(FileName) -> wx_object:start(?MODULE, [FileName], []).

start_link()         -> wx_object:start_link(?MODULE, [], []).
start_link(FileName) -> wx_object:start_link(?MODULE, [FileName], []).

stop(ENotepad) -> wx_object:stop(ENotepad).

wait_forever(ENotepad) -> catch wx_object:call(ENotepad, noreply), ok.

%%open_file(ENotepad, FileName) ->
%%    wx_object:cast(ENotepad, {open_on_init, FileName}).


-spec config_check(window()) -> window() | 'undefined'.
config_check(#window{x=X, y=Y, width=W, height=H} = C)
    when
        is_integer(X), is_integer(Y),
        is_integer(W), W > 0,
        is_integer(H), H > 0 ->
    C;

config_check(_) -> undefined.

%% object_wx callbacks
-spec init(list()) -> {wxFrame:wxFrame(), state()}.
init(Args) ->
    wx:new(),
    Config = enotepad_config:load(),

    Font        = enotepad_font:load(Config#config.font),
    Frame       = create_main_frame(Config#config.window),
    MenuBar     = enotepad_menu:create(Frame),
    TextCtrl    = enotepad_text:create_control(Frame, Font),
    StatusBar   = enotepad_statusbar:create(Frame),

    WantStatusBar = Config#config.status_bar,

    wxMenuBar:check(MenuBar, ?MENU_View_StatusBar, WantStatusBar),
    wxMenuBar:check(MenuBar, ?MENU_Format_WordWrap, Config#config.word_wrap),

    case {Config#config.word_wrap, WantStatusBar} of
        {true, _} ->
            wxStyledTextCtrl:setWrapMode(TextCtrl, 1),
            wxMenuBar:enable(MenuBar, ?MENU_View_StatusBar, false),
            wxMenuBar:enable(MenuBar, ?MENU_Edit_GoTo, false),
            enotepad_statusbar:set_visible(StatusBar, false);

        {_, false} ->
            enotepad_statusbar:set_visible(StatusBar, false);

        {_, true} -> ok
    end,

    case Args of
        [FileName]  -> self() ! {open_on_init, FileName};
        _           -> undefined
    end,

    wxFrame:show(Frame),
    wxFrame:raise(Frame),
    wxFrame:setFocus(TextCtrl),

    {Frame,
        #state{
            frame = Frame,
            textctrl = TextCtrl,
            menu = MenuBar,
            statusbar = StatusBar,
            font = Font,
            filename = undefined,
            print = enotepad_print:init()
        }
    }.

-spec handle_event(Event :: wx(), State :: state())
        -> {'noreply', state()}
         | {'stop', 'normal', state()}.

%% this implements the undo/redo selection when text is reinserted, like notepad
handle_event(#wx{event = #wxStyledText{type = stc_modified} = StcEvent}, S) ->
    MType = StcEvent#wxStyledText.modificationType,
    if
        MType band ?wxSTC_MOD_BEFOREINSERT > 0 ->
            EventPos = StcEvent#wxStyledText.position,
            ModEnd = EventPos + StcEvent#wxStyledText.length,
            {noreply, S#state{select_start= min(S#state.select_start, EventPos),
                              select_end  = max(S#state.select_end, ModEnd)}};

        MType band ?UNDOREDOEND =:= ?UNDOREDOEND ->
            End = StcEvent#wxStyledText.position + StcEvent#wxStyledText.length,
            wxStyledTextCtrl:setSelection(S#state.textctrl,
                                          S#state.select_start,
                                          max(End, S#state.select_end)),
            {noreply, S#state{select_start = -1, select_end = -1}};

        true ->
            {noreply, S}
    end;

%% many of these events happens, but we need to check somewhere when the text
%% selection changes or when the cursor has moved.
handle_event(#wx{event = #wxStyledText{type = stc_updateui}}, S) ->
    S1 = check_empty_document(S),
    S2 = check_updated_position(S1),
    S3 = check_has_selection(S2),
    {noreply, S3};

handle_event(#wx{event = #wxStyledText{type = stc_savepointleft}}, S) ->
    % @todo maybe only enable first time???? maybe not
    CanUndo = wxStyledTextCtrl:canUndo(S#state.textctrl),
    wxMenuBar:enable(S#state.menu, ?wxID_UNDO, CanUndo),
    {noreply, S};

handle_event(#wx{event = #wxClose{}}, S) ->
    case enotepad_file:can_destroy_buffer(S#state.textctrl, S#state.filename) of
        true -> stop_me(S);
        false -> {noreply, S}
    end;
 % exit and close does the same, so, keep them together here
handle_event(?MENU_EVENT(?wxID_EXIT), S) ->
    case enotepad_file:can_destroy_buffer(S#state.textctrl, S#state.filename) of
        true  -> stop_me(S);
        false -> {noreply, S}
    end;

handle_event(?MENU_EVENT(?wxID_NEW), S) ->
    case enotepad_file:can_destroy_buffer(S#state.textctrl, S#state.filename) of
        true ->
            wxStyledTextCtrl:clearAll(S#state.textctrl),
            wxStyledTextCtrl:emptyUndoBuffer(S#state.textctrl),
            wxStyledTextCtrl:setScrollWidth(S#state.textctrl, 1),

            wxMenuBar:enable(S#state.menu, ?wxID_UNDO, false),

            set_window_title(S#state.frame, undefined),

            case S#state.fr_dialog of
                undefined ->
                    case S#state.fr_data of
                        undefined -> ok;
                        FRData -> wxFindReplaceData:destroy(FRData)
                    end,
                    {noreply, S#state{filename = undefined, fr_data=undefined}};

                _ ->
                    {noreply, S#state{filename = undefined}}
            end;

        false ->
            {noreply, S}

    end;

handle_event(?MENU_EVENT(?wxID_OPEN), S) ->
    TextCtrl = S#state.textctrl,
    case enotepad_file:can_destroy_buffer(TextCtrl, S#state.filename) of
        true ->
            case enotepad_file:show_open_dialog(S#state.frame, TextCtrl) of
                {ok, FullFilename} ->
                    wxMenuBar:enable(S#state.menu, ?wxID_UNDO, false),
                    set_window_title(S#state.frame, FullFilename),
                    {noreply, S#state{ filename = FullFilename }};

                undefined ->
                    {noreply, S}
            end;

        false ->
            {noreply, S}
    end;

handle_event(?MENU_EVENT(Id), #state{filename = Filename} = S)
    when
        Id =:= ?wxID_SAVE;
        Id =:= ?wxID_SAVEAS ->
    ForceName = Id =:= ?wxID_SAVEAS,
    case enotepad_file:save_buffer(S#state.textctrl, Filename, ForceName) of
        {ok, Filename} ->
            {noreply, S};

        {ok, NewFilename} ->
            set_window_title(S#state.frame, NewFilename),
            {noreply, S#state{filename = NewFilename}};

        {error, _Reason} ->
            {noreply, S}
    end;

handle_event(?MENU_EVENT(?wxID_PAGE_SETUP), S) ->
    {noreply, S#state{print = enotepad_print:page_setup(S#state.print)}};

handle_event(?MENU_EVENT(?wxID_PRINT), S) ->
    Text = wxStyledTextCtrl:getText(S#state.textctrl),
    EasyPrint = enotepad_print:print(S#state.print, Text, S#state.font),
    {noreply, S#state{print = EasyPrint}};

%% @note don't split the next line so it appears nicely on intellij to-do list
%% @todo littlebug if we start typing after some text is selected, the undo operation will revert only until the first char typed, notepad would revert to the selected text
handle_event(?MENU_EVENT(?wxID_UNDO), S) ->
    TextCtrl = S#state.textctrl,
    case wxStyledTextCtrl:canRedo(TextCtrl) of
        true  -> wxStyledTextCtrl:redo(TextCtrl);
        false -> wxStyledTextCtrl:undo(TextCtrl)
    end,
    {noreply, S#state{select_start = wxStyledTextCtrl:getLength(TextCtrl)}};

handle_event(?MENU_EVENT(?wxID_CUT), S) ->
    wxStyledTextCtrl:cut(S#state.textctrl),
    {noreply, S};

handle_event(?MENU_EVENT(?wxID_COPY), S) ->
    wxStyledTextCtrl:copy(S#state.textctrl),
    {noreply, S};

handle_event(?MENU_EVENT(?wxID_PASTE), S) ->
    wxStyledTextCtrl:paste(S#state.textctrl),
    enotepad_util:refresh_scroll_width(S#state.textctrl),
    {noreply, S};

handle_event(?MENU_EVENT(?wxID_DELETE), S) ->
    wxStyledTextCtrl:clear(S#state.textctrl),
    {noreply, S};

handle_event(?MENU_EVENT(Id) = Event, #state{fr_data = undefined} = S)
    when
        Id =:= ?wxID_FIND;
        Id =:= ?wxID_REPLACE ->
    handle_event(Event, S#state{fr_data = wxFindReplaceData:new(?wxFR_DOWN)});

handle_event(?MENU_EVENT(?wxID_FIND), #state{fr_dialog = undefined} = S) ->
    DialogStyle = ?wxFR_NOWHOLEWORD,
    Dialog = wxFindReplaceDialog_ex:new(S#state.frame,
                                        S#state.fr_data,
                                        "Find",
                                        [{style, DialogStyle}]),

    wxFindReplaceDialog_ex:connect(Dialog, find_dialog_event),
    wxDialog:show(Dialog),
    {noreply, S#state{fr_dialog = Dialog}};

handle_event(?MENU_EVENT(?wxID_REPLACE), #state{fr_dialog = undefined} = S) ->
    DialogStyle = ?wxFR_REPLACEDIALOG bor ?wxFR_NOWHOLEWORD,
    Dialog = wxFindReplaceDialog_ex:new(S#state.frame,
                                        S#state.fr_data,
                                        "Replace",
                                        [{style, DialogStyle}]),

    wxFindReplaceDialog_ex:connect(Dialog, find_dialog_event),
    wxDialog:show(Dialog),
    {noreply, S#state{fr_dialog = Dialog}};

% because there was already a dialog opened, we just raise it to the front
% no matter if it was a find or a replace, this is how notepad works.
handle_event(?MENU_EVENT(Id), #state{fr_dialog = Dialog} = S)
    when
        Id =:= ?wxID_FIND;
        Id =:= ?wxID_REPLACE ->
    wxDialog:raise(Dialog),
    {noreply, S};

% there is nothing to find, the window wasn't created, so emulate a menu Find
handle_event(?MENU_EVENT(?MENU_Edit_FindNext) = E, S) ->
    case get_find_string(S#state.fr_data) of
        "" ->
            handle_event(E#wx{id = ?wxID_FIND}, S);

        _Something ->
            enotepad_text:search(S#state.textctrl, S#state.fr_data),
            {noreply, S}
    end;

handle_event(?MENU_EVENT(?MENU_Edit_GoTo), S) ->
    CurrentLine = wxStyledTextCtrl:getCurrentLine(S#state.textctrl),

    Dialog = wxTextEntryDialog:new(wx:null(), "Line Number:", [
        {caption, "Go To Line"},
        {value, integer_to_list(CurrentLine+1)}
    ]),

    try
        case wxTextEntryDialog:showModal(Dialog) of
            ?wxID_OK ->
                StrValue = wxTextEntryDialog:getValue(Dialog),
                case catch list_to_integer(StrValue, 10) of
                    N when is_integer(N), N > 0 ->
                        wxStyledTextCtrl:gotoLine(S#state.textctrl, N - 1);

                    _ ->
                        ok
                end;

            _ ->
                ok
        end
    after
        wxTextEntryDialog:destroy(Dialog)
    end,

    {noreply, S};

handle_event(?MENU_EVENT(?wxID_SELECTALL), S) ->
    wxStyledTextCtrl:selectAll(S#state.textctrl),
    {noreply, S};

handle_event(?MENU_EVENT(?MENU_Edit_InsertDateTime), S) ->
    TS = os:timestamp(),
    {{Year, Month, Day}, {Hour, Minute, _}} = calendar:now_to_local_time(TS),
    % Notepad's use the localized date format, I don't know how to read it in
    % erlang, so I will use the international format: yyyy-mm-dd hh:mm
    DateTime = io_lib:format("~B-~2..0B-~2..0B ~2..0B:~2..0B",
                           [Year, Month, Day,    Hour, Minute]),

    wxStyledTextCtrl:addText(S#state.textctrl, DateTime),
    {noreply, S};

handle_event(?MENU_EVENT(?MENU_Format_WordWrap, Value), S) ->
    case Value of
        0 -> % disable
            wxMenuBar:enable(S#state.menu, ?MENU_View_StatusBar, true),
            wxMenuBar:enable(S#state.menu, ?MENU_Edit_GoTo, true),

            enotepad_statusbar:set_visible(S#state.statusbar,
                wxMenuBar:isChecked(S#state.menu, ?MENU_View_StatusBar)),

            enotepad_util:refresh_scroll_width(S#state.textctrl),
            wxStyledTextCtrl:setWrapMode(S#state.textctrl, 0);

        1 -> % enable
            wxMenuBar:enable(S#state.menu, ?MENU_View_StatusBar, false),
            wxMenuBar:enable(S#state.menu, ?MENU_Edit_GoTo, false),

            enotepad_statusbar:set_visible(S#state.statusbar, false),

            wxStyledTextCtrl:setWrapMode(S#state.textctrl, 1)
    end,
    {noreply, S};

handle_event(?MENU_EVENT(?MENU_Format_FontSelect), S) ->
    OldFont = S#state.font,
    case enotepad_font:show_select_dialog(S#state.frame, OldFont) of
        OldFont -> {noreply, S};
        NewFont ->
            wxStyledTextCtrl:styleSetFont(  S#state.textctrl,
                                            ?wxSTC_STYLE_DEFAULT,
                                            NewFont),

            wxStyledTextCtrl:styleClearAll(S#state.textctrl),
            enotepad_font:destroy(OldFont),
            enotepad_util:refresh_scroll_width(S#state.textctrl),
            {noreply, S#state{font = NewFont}}
    end;

handle_event(?MENU_EVENT(?MENU_View_StatusBar, Enabled), S) ->
    enotepad_statusbar:set_visible(S#state.statusbar, Enabled =/= 0),
    {noreply, S};

handle_event(?MENU_EVENT(?MENU_Help_ViewHelp), S) ->
    open_help(),
    {noreply, S};

handle_event(?MENU_EVENT(?wxID_ABOUT), S) ->
    Opts = [
        {caption, "About " ?APPNAME},
        {style, ?wxICON_INFORMATION}
    ],
    Message = ?APPNAME ++ " Version " ++ enotepad_util:get_version() ++ "\n" ++
            "A Notepad clone in Erlang",
    Dialog = wxMessageDialog:new(S#state.frame, Message, Opts),
    wxMessageDialog:showModal(Dialog),
    wxMessageDialog:destroy(Dialog),
    {noreply, S};

handle_event(#wx{event = #wxFindDialogEvent_ex{type = close}}, S) ->
    {noreply, S#state{fr_dialog = undefined}};

handle_event(#wx{event = #wxFindDialogEvent_ex{type = T}}, S)
    when
        T =:= find;
        T =:= find_next ->
    enotepad_text:search(S#state.textctrl, S#state.fr_data),
    {noreply, S#state{}};

handle_event(#wx{event = #wxFindDialogEvent_ex{type = replace} = E}, S) ->
    TextCtrl = S#state.textctrl,
    ReplaceTo = E#wxFindDialogEvent_ex.replace_string,
    enotepad_text:replace(TextCtrl, S#state.fr_data, ReplaceTo),
    enotepad_text:search(TextCtrl, S#state.fr_data),
    {noreply, S#state{}};

handle_event(#wx{event = #wxFindDialogEvent_ex{type = replace_all} = E}, S) ->
    TextCtrl = S#state.textctrl,
    #wxFindDialogEvent_ex{
        find_string = SearchString,
        replace_string = ReplaceTo,
        flags = Flags
    } = E,

    enotepad_text:replace_all(TextCtrl, SearchString, ReplaceTo, Flags),

    {noreply, S};

handle_event(#wx{event = #wxKey{type = key_up, keyCode = Key}}, S) ->
    Key =:= ?WXK_F1 andalso open_help(),
    {noreply, S};

handle_event(Event, S) ->
    io:format("Unhandled Event:~n~p~n", [Event]),
    {noreply, S}.

-spec handle_call(Request::any(), From::any(), State::state())
        -> {'noreply', state()} | {reply, ok, state()}.
handle_call(noreply, _From, State) ->
    {noreply, State}; % this way will wait until window closed

handle_call(Request, _From, State) ->
    io:format("Unhandled Call:~n~p~n", [Request]),
    {reply, ok, State}.

-spec handle_cast(Request::any(), State::state()) -> {'noreply', state()}.

handle_cast(Request, State) ->
    io:format("Unhandled Cast:~n~p~n", [Request]),
    {noreply, State}.

-spec handle_info(Info::any(), State::state()) -> {'noreply', state()}.
handle_info({open_on_init, FileName}, S) ->
    case enotepad_file:ensure_file(FileName) of
        {yes, Name} ->
            case enotepad_file:load_buffer(S#state.textctrl, Name) of
                ok ->
                    set_window_title(S#state.frame, Name),
                    {noreply, S#state{filename = Name}};

                error ->
                    {noreply, S}
            end;

        no -> {noreply, S};

        cancel -> stop_me(S)
    end;

handle_info(Info, State) ->
    io:format("Unhandled Info:~n~p~n", [Info]),
    {noreply, State}.

-spec terminate(Reason::any(), State::state()) -> 'ok'.
terminate(_Reason, S) ->
    C = #config{
        window = config_save(S),
        word_wrap  = wxMenuBar:isChecked(S#state.menu, ?MENU_Format_WordWrap),
        status_bar = wxMenuBar:isChecked(S#state.menu, ?MENU_View_StatusBar),
        font = enotepad_font:config_save(S#state.font)
    },

    enotepad_config:save(C),

    wxFrame:destroy(S#state.frame),

    enotepad_print:destroy(S#state.print),
    enotepad_font:destroy(S#state.font),

    wx:destroy(),
    ok.

-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% ~ ~ ~ ~ %%%
%%% private %%%
%%% ~ ~ ~ ~ %%%

-spec stop_me(state()) -> {'stop', 'normal', state()} | {'noreply', state()}.
%% Stops the wx_object, should be called within a handle_something and its
%% return value returned.
stop_me(S) ->
    case how_to_stop() of
        normal              -> {stop, normal, S};
        init                -> init:stop(), {noreply, S};
        {application, Name} -> application:stop(Name), {noreply, S}
    end.

-spec how_to_stop() -> 'normal' | 'init' | {'application', atom()}.
%% Decides how shall stop the thing:
%%   normal -> we are not an application, just stop how you normally would.
%%   init   -> we are a "permanent" application, maybe from a release, init:stop
%%   {application, _} -> we are an application and can use application:stop().
how_to_stop() ->
    case application:get_application() of
        undefined -> normal;

        {ok, App} ->
            Started = proplists:get_value(started, application:info(), []),
            case proplists:get_value(App, Started) of
                undefined -> normal;
                permanent -> init;
                _         -> {application, App}
            end
    end.

-spec config_save(state()) -> window().
config_save(#state{frame = Frame}) ->
    {X,Y,W,H} = wxWindow:getScreenRect(Frame),
    #window{
        x = X,
        y = Y,
        width = W,
        height = H
    }.

-spec create_main_frame('undefined' | window()) -> wxFrame:wxFrame().
create_main_frame(undefined) ->
    create_main_frame(#window{x = 100, y = 50, width = 600, height = 480});

create_main_frame(#window{} = Cfg) ->
    Options = [
        {pos, {Cfg#window.x, Cfg#window.y}},
        {size, {Cfg#window.width, Cfg#window.height}}
    ],
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, window_title(undefined), Options),
    configure_frame(Frame).

-spec configure_frame(wxFrame:wxFrame()) -> wxFrame:wxFrame().
configure_frame(Frame) ->
    set_icon(Frame),

    wxFrame:connect(Frame, close_window),
    wxFrame:connect(Frame, command_menu_selected),

    Frame.

-spec set_icon(wxFrame:wxFrame()) -> 'ok'.
set_icon(Frame) ->
    PrivDir = code:priv_dir(enotepad),
    case filelib:is_dir(PrivDir) of
%%        true -> set_icon_from_file(Frame, PrivDir, "enotepad-icon32d.png");
        true -> set_icon_from_file(Frame, PrivDir, "icon.ico");
        false-> set_icon_from_script(Frame, filename:join(["enotepad", "ebin", "icon32.raw"]))
    end.

-spec set_icon_from_file(wxFrame:wxFrame(), string(), string()) -> 'ok'.
set_icon_from_file(Frame, Dir, Name) ->
    IconFile = filename:join(Dir, Name),
%%    io:format("Loading icon from dir: ~p~n", [Dir]),
    Icon = wxIcon:new(IconFile, [{type,?wxBITMAP_TYPE_ICO}]),
    wxFrame:setIcon(Frame, Icon),
    wxIcon:destroy(Icon).

-spec set_icon_from_script(wxFrame:wxFrame(), string()) -> 'ok'.
set_icon_from_script(Frame, Name) ->
    ScriptFile = escript:script_name(),
%%    io:format("Loading icon ~p from script file: ~p~n", [Name, ScriptFile]),
    case escript:extract(ScriptFile, []) of
        {ok, [_,_,_,{archive,Escript}]} ->
            case zip:unzip(zip_part(Escript), [memory, {file_list,[Name]}]) of
                {ok, [{_, RawData}]} ->
                    Image = image_from_raw_data(RawData),
                    Bitmap = wxBitmap:new(Image),
                    Icon = wxIcon:new(),
                    wxIcon:copyFromBitmap(Icon, Bitmap),
                    wxFrame:setIcon(Frame, Icon),
                    wxIcon:destroy(Icon),
                    wxBitmap:destroy(Bitmap),
                    wxImage:destroy(Image);

                R ->
                    io:format("Cannot load icon ~p from script: ~n~p~n",
                              [Name, R])
            end;

        _ ->
            io:format("Cannot load icon, maybe is not running as script")
    end.

zip_header() -> <<"PK",3,4>>.

zip_part(Escript) -> 
    { Start, _ } = binary:match(Escript,zip_header(),[]),
    binary:part(Escript, Start, byte_size(Escript) - Start).

image_from_raw_data(RawData) ->
    <<ImageWidth:32, ImageHeight:32,
      RGBLen:32,   RGBData:RGBLen/binary,
      AlphaLen:32, AlphaData:AlphaLen/binary>> = RawData,

    Image = wxImage:new(ImageWidth, ImageHeight, RGBData),
    wxImage:setAlpha(Image, AlphaData),
    Image.

-spec window_title(Filename :: string() | 'undefined') -> string().
window_title(Filename) ->
    SimpleName = enotepad_file:simple_name(Filename),
    SimpleName ++ " - " ?APPNAME " " ++ enotepad_util:get_version().

-spec set_window_title(wxFrame:wxFrame(), string() | 'undefined') -> any().
set_window_title(Frame, Filename) ->
    wxFrame:setTitle(Frame, window_title(Filename)).

-spec get_find_string(FRData | 'undefined') -> string() when
    FRData :: wxFindReplaceData:wxFindReplaceData().
get_find_string(undefined) -> "";
get_find_string(FRData) -> wxFindReplaceData:getFindString(FRData).

-spec check_empty_document(state()) -> state().
check_empty_document(#state{empty_state = OldEmpty} = S) ->
    case wxStyledTextCtrl:getLength(S#state.textctrl) =:= 0 of
        OldEmpty -> S;

        NewEmpty ->
            wxMenuBar:enable(S#state.menu, ?wxID_FIND, not NewEmpty),
            wxMenuBar:enable(S#state.menu, ?MENU_Edit_FindNext, not NewEmpty),
            S#state{empty_state = NewEmpty}
    end.

-spec check_updated_position(state()) -> state().
check_updated_position(#state{position = OldPosition} = S) ->
    case wxStyledTextCtrl:getCurrentPos(S#state.textctrl) of
        OldPosition -> S;

        NewPosition ->
            enotepad_statusbar:update_position( S#state.statusbar,
                                                S#state.textctrl,
                                                NewPosition),
            S#state{position = NewPosition}
    end.

-spec check_has_selection(state()) -> state().
check_has_selection(#state{is_selecting = WasSelecting} = S) ->
    case enotepad_text:has_selection(S#state.textctrl) of
        WasSelecting -> S;

        NewSelecting ->
            wxMenuBar:enable(S#state.menu, ?wxID_CUT,   NewSelecting),
            wxMenuBar:enable(S#state.menu, ?wxID_COPY,  NewSelecting),
            wxMenuBar:enable(S#state.menu, ?wxID_DELETE,NewSelecting),
            S#state{is_selecting = NewSelecting}
    end.

-spec open_help() -> boolean().
open_help() ->
    wx_misc:launchDefaultBrowser("https://github.com/aaronps/enotepad").