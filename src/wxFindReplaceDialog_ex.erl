%%%-------------------------------------------------------------------
%%% @author aaronps
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Oct 2016 10:18
%%%-------------------------------------------------------------------
-module(wxFindReplaceDialog_ex).
-author("aaronps").

-behaviour(wx_object).

-include_lib("wx/include/wx.hrl").
-include("wxFindDialogEvent_ex.hrl").

%% API
-export([demo/0]).
-export([new/3, new/4, connect/2, disconnect/1]).
-export_type([wxFindReplaceDialog_ex/0]).

%% wx_object callbacks
-export([init/1,
    handle_event/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

%%% @note wxEVT_FIND for first time there is a search change, the rest find_next
%%% @note the FindReplaceData passed is saved and reused by the dialog

-type wxFindReplaceDialog_ex() :: wxDialog:wxDialog().
-type fr_data() :: wxFindReplaceData:wxFindReplaceData().
-type listener() :: pid() | 'undefined'.
-type fr_event() :: 'find' | 'find_next' | 'replace' | 'replace_all' | 'close'.



-record(ctrl, {
    dialog              :: wxDialog:wxDialog(),
    main_panel          :: wxPanel:wxPanel(),
    find_box            :: wxTextCtrl:wxTextCtrl(),
    replace_box         :: wxTextCtrl:wxTextCtrl() | 'undefined',
    find_next_button    :: wxButton:wxButton(),
    replace_button      :: wxButton:wxButton() | 'undefined',
    replace_all_button  :: wxButton:wxButton() | 'undefined',
    cancel_button       :: wxButton:wxButton(),
    whole_word_checkbox :: wxCheckBox:wxCheckBox() | 'undefined',
    match_case_checkbox :: wxCheckBox:wxCheckBox() | 'undefined',
    up_radio            :: wxRadioButton:wxRadioButton() | 'undefined',
    down_radio          :: wxRadioButton:wxRadioButton() | 'undefined'
}).

-type ctrl() :: #ctrl{}.

-record(ctrlstate, {
    find    :: string(),
    replace :: string(),
    flags   :: integer()
}).

-type ctrlstate() :: #ctrlstate{}.

-record(state, {
    parent      :: wxWindow:wxWindow(),
    listener    :: pid() | 'undefined',
    controls    :: ctrl(),
    frdata      :: fr_data()
}).

-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec demo() -> 'ok'.
demo() ->
    wx:new(),
    FRData = wxFindReplaceData:new(),
    wxFindReplaceData:setFlags(FRData,?wxFR_DOWN),
    case wx_object:start(?MODULE, [wx:null(), FRData, "Find", 0], []) of
        {error, _} = Error -> Error;
        Object ->
            connect(Object, find_dialog_event),
            demo_wait_close(),
            wx:destroy()
    end.

-spec demo_wait_close() -> 'ok'.
demo_wait_close() ->
    receive
        #wx{event = #wxFindDialogEvent_ex{type = close}} ->
            io:format("Dialog closed~n");

        #wx{} = Event ->
            io:format("Received Event: ~p~n", [Event]),
            demo_wait_close();

        Other ->
            io:format("Received Something else: ~p~n", [Other]),
            demo_wait_close()
    end.

-spec new(wxWindow:wxWindow(), fr_data(), string())
        -> wxFindReplaceDialog_ex().
new(Parent, FindReplaceData, Title) ->
    new(Parent, FindReplaceData, Title, [{style, 0}]).

-spec new(wxWindow:wxWindow(), fr_data(), string(), [{'style',integer()}])
        -> wxFindReplaceDialog_ex().
new(Parent, FindReplaceData, Title, [{style, Style}]) ->
%%    io:format("Parent is ~p~n", [Parent]),
    case wx_object:start(?MODULE, {Parent, FindReplaceData, Title, Style},[]) of
        {error, _} = Error -> Error;
        Object             -> Object
    end.

-spec connect(wxFindReplaceDialog_ex(), 'find_dialog_event') -> 'ok'.
connect(This, find_dialog_event) ->
    wx_object:call(This, {connect, find_dialog_event, self()}).

-spec disconnect(wxFindReplaceDialog_ex()) -> 'ok'.
disconnect(This) ->
    wx_object:call(This, {disconnect, self()}).

%%%===================================================================
%%% wx_object callbacks
%%%===================================================================
-spec init({wxWindow:wxWindow(), fr_data(), string(), integer()})
    -> {wxDialog:wxDialog(), state()}.
init({Parent, FindReplaceData, Title, Style}) ->

    DialogSize = case Style band ?wxFR_REPLACEDIALOG of
                     ?wxFR_REPLACEDIALOG -> {361, 140 + 52};
                     _                   -> {370, 140}
                 end,

    Dialog = wxDialog:new(Parent, -1, Title, [
        {pos,{1350,100}},
        {size, DialogSize}
    ]),

    Controls = create_controls(Dialog, Style),

    set_initial_values(Controls, FindReplaceData),

    connect_events(Controls),

    wxDialog:show(Dialog),

    {Dialog, #state{
        parent = Parent,
        controls = Controls,
        frdata = FindReplaceData
    }}.



-define(BUTTON_CLICKED(Id),
    #wx{id=Id,
        event=#wxCommand{type = command_button_clicked}
    }).

-spec handle_event(Event::wx(), state())
        -> {'noreply', state()}
         | {'stop', 'normal', state()}.
% this event is only called on the find box
handle_event(#wx{event = #wxCommand{type = command_text_updated} = Cmd}, S) ->
    enable_buttons(S#state.controls, Cmd#wxCommand.cmdString =/= ""),
    {noreply, S};

handle_event(#wx{event = #wxCommand{type = command_text_enter}}, S) ->
    notify_event(find, S#state.listener, S#state.controls, S#state.frdata),
    {noreply, S};

handle_event(?BUTTON_CLICKED(?wxID_FIND), S) ->
    notify_event(find, S#state.listener, S#state.controls, S#state.frdata),
    {noreply, S};

handle_event(?BUTTON_CLICKED(?wxID_REPLACE), S) ->
    notify_event(replace, S#state.listener, S#state.controls, S#state.frdata),
    {noreply, S};

handle_event(?BUTTON_CLICKED(?wxID_REPLACE_ALL), S) ->
    notify_event(replace_all,S#state.listener,S#state.controls,S#state.frdata),
    {noreply, S};

handle_event(?BUTTON_CLICKED(?wxID_CANCEL), S) ->
    {stop, normal, S};

handle_event(#wx{event = #wxClose{}}, S) ->
    {stop, normal, S};

handle_event(#wx{event = #wxShow{show = Show}}, S) ->
    case Show of
        false -> {stop, normal, S};
        true  -> {noreply, S}
    end;

handle_event(Event, State) ->
    io:format(?MODULE_STRING ++ " Unhandled Event: ~p~n", [Event]),
    {noreply, State}.

-spec handle_call(Request::any(), From::any(), State::state())
        -> {reply, ok, state()}.
handle_call({connect, _EventType, Pid}, _From, S) ->
%%    io:format("Will notify ~p events to ~p~n",[EventType, Pid]),
    {reply, ok, S#state{listener = Pid}};

handle_call(Request, _From, State) ->
    io:format("Unhandled Call: ~p~n", [Request]),
    {reply, ok, State}.


-spec handle_cast(Request::any(), State::state()) -> {noreply, state()}.
handle_cast(Request, State) ->
    io:format("Unhandled Cast: ~p~n", [Request]),
    {noreply, State}.

-spec handle_info(Info::any(), State::state()) -> {noreply, state()}.
handle_info(Info, State) ->
    io:format("Unhandled Info: ~p~n", [Info]),
    {noreply, State}.

-spec terminate(Reason::any(), state()) -> 'ok'.
terminate(_Reason, S) ->
%%    io:format("Terminate: ~p~n", [Reason]),
    notify_event(close, S#state.listener, S#state.controls, S#state.frdata),
    wxDialog:destroy(S#state.controls#ctrl.dialog),
    ok.

-spec code_change(OldVsn::any(), state(), Extra::any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec create_controls(Dialog, Style) -> ctrl()
      when
        Dialog :: wxDialog:wxDialog(),
        Style :: integer().

create_controls(Dialog, Style) when Style band ?wxFR_REPLACEDIALOG =/= 0 ->
    MainPanel = wxPanel:new(Dialog),

    _FindWhat = wxStaticText:new(MainPanel, ?wxID_ANY, "Fi&nd what:", [
        {pos, {6, 12}}
    ]),

    FindBox = wxTextCtrl:new(MainPanel, ?wxID_ANY, [
        {pos, {81, 11}},
        {size, {171, 20}},
        {style, ?wxTE_PROCESS_ENTER} % needed to detect enter press
    ]),

    _ReplaceWhat = wxStaticText:new(MainPanel, ?wxID_ANY, "Re&place with:",[
        {pos, {6, 40}}
    ]),

    ReplaceBox = wxTextCtrl:new(MainPanel, ?wxID_ANY, [
        {pos, {81, 39}},
        {size, {171, 20}},
        {style, ?wxTE_PROCESS_ENTER} % needed to detect enter press
    ]),

    FindNextButton = wxButton:new(MainPanel, ?wxID_FIND, [
        {label, "&Find Next"},
        {pos, {261, 7}},
        {size, {75, 23}}
    ]),

    ReplaceButton = wxButton:new(MainPanel, ?wxID_REPLACE, [
        {label, "&Replace"},
        {pos, {261, 34}},
        {size, {75, 23}}
    ]),

    ReplaceAllButton = wxButton:new(MainPanel, ?wxID_REPLACE_ALL, [
        {label, "&ReplaceAll"},
        {pos, {261, 62}},
        {size, {75, 23}}
    ]),

    CancelButton = wxButton:new(MainPanel, ?wxID_CANCEL, [
        {label, "Cancel"},
        {pos, {261, 89}}, % should be 38
        {size, {75, 23}}
    ]),

    WholeWord = case Style band ?wxFR_NOWHOLEWORD of
        0 ->
            wxCheckBox:new(MainPanel, ?wxID_ANY, "Match &whole word only", [
                {pos, {8, 77}}
            ]);
        _ -> undefined
    end,

    MatchCase = case Style band ?wxFR_NOMATCHCASE of
        0 ->
            wxCheckBox:new(MainPanel, ?wxID_ANY, "Match &case", [
                {pos, {8, 103}}
            ]);
        _ -> undefined
    end,

    #ctrl{
        dialog = Dialog,
        main_panel = MainPanel,
        find_box = FindBox,
        replace_box = ReplaceBox,
        find_next_button = FindNextButton,
        replace_button = ReplaceButton,
        replace_all_button = ReplaceAllButton,
        cancel_button = CancelButton,
        whole_word_checkbox = WholeWord,
        match_case_checkbox = MatchCase
    };

create_controls(Dialog, Style) ->
    MainPanel = wxPanel:new(Dialog),

    _FindWhat = wxStaticText:new(MainPanel, ?wxID_ANY, "Fi&nd what:", [
        {pos, {6, 12}}
    ]),

    FindBox = wxTextCtrl:new(MainPanel, ?wxID_ANY, [
        {pos, {71, 11}},
        {size, {192, 20}},
        {style, ?wxTE_PROCESS_ENTER} % needed to detect enter press
    ]),

    FindNextButton = wxButton:new(MainPanel, ?wxID_FIND, [
        {label, "&Find Next"},
        {pos, {273, 8}},
        {size, {75, 23}}
    ]),

    CancelButton = wxButton:new(MainPanel, ?wxID_CANCEL, [
        {label, "Cancel"},
        {pos, {273, 37}},
        {size, {75, 23}}
    ]),

    WholeWord = case Style band ?wxFR_NOWHOLEWORD of
        0 ->
            wxCheckBox:new(MainPanel, ?wxID_ANY, "Match &whole word only", [
                {pos, {6, 44}}
            ]);
        _ -> undefined
    end,

    MatchCase = case Style band ?wxFR_NOMATCHCASE of
        0 ->
            wxCheckBox:new(MainPanel, ?wxID_ANY, "Match &case", [
                {pos, {6, 70}}
            ]);
        _ -> undefined
    end,


    {UpRadioButton, DownRadioButton} = case Style band ?wxFR_NOUPDOWN of
        0 ->
            _RadioBox = wxStaticBox:new(MainPanel, ?wxID_ANY, "Direction",[
                {pos, {161, 41}},
                {size, {102, 47}}
            ]),

            % in windows and linux, the "pos" works differently:
            % windows: offset from top/left of the RadioBox element, no padding
            % linux: offset from "content start" wtf.
            % so use all offsets from the top panel...
            {
                wxRadioButton:new(MainPanel, ?wxID_ANY, "&Up", [
                    {pos, {167, 64}}
%%                    {pos, {6, 23}}
                ]),
                wxRadioButton:new(MainPanel, ?wxID_ANY, "&Down", [
                    {pos, {207, 64}}
%%                    {pos, {47, 23}}
                ])
            };

        _ ->
            {undefined, undefined}
    end,

    #ctrl{
        dialog = Dialog,
        main_panel = MainPanel,
        find_box = FindBox,
        find_next_button = FindNextButton,
        cancel_button = CancelButton,
        whole_word_checkbox = WholeWord,
        match_case_checkbox = MatchCase,
        up_radio = UpRadioButton,
        down_radio = DownRadioButton
    }.

-spec enable_buttons(ctrl(), boolean()) -> 'ok'.
%% Enables or disables the find/replace/replace_all buttons
enable_buttons(Controls, Enable) ->
    wxButton:enable(Controls#ctrl.find_next_button, [{enable, Enable}]),

    case {Controls#ctrl.replace_button, Controls#ctrl.replace_all_button} of
        {undefined, undefined} -> ok;
        {B1,B2} ->
            wxButton:enable(B1, [{enable, Enable}]),
            wxButton:enable(B2, [{enable, Enable}])
    end.

-spec set_initial_values(ctrl(), fr_data()) -> 'ok'.
set_initial_values(Controls, FRData) ->
    wxButton:setDefault(Controls#ctrl.find_next_button),

    case wxFindReplaceData:getFindString(FRData) of
        []      -> enable_buttons(Controls, false);
        Value   -> wxTextCtrl:changeValue(Controls#ctrl.find_box, Value)
    end,

    Controls#ctrl.replace_box =/= undefined andalso begin
        wxTextCtrl:changeValue( Controls#ctrl.replace_box,
                                wxFindReplaceData:getReplaceString(FRData))
    end,

    Flags = wxFindReplaceData:getFlags(FRData),

    Controls#ctrl.up_radio =/= undefined andalso case Flags band ?wxFR_DOWN of
        ?wxFR_DOWN -> wxRadioButton:setValue(Controls#ctrl.down_radio, true);
        _          -> wxRadioButton:setValue(Controls#ctrl.up_radio, true)
    end,

    Controls#ctrl.whole_word_checkbox =/= undefined andalso begin
        wxCheckBox:setValue(Controls#ctrl.whole_word_checkbox,
                            Flags band ?wxFR_WHOLEWORD =:= ?wxFR_WHOLEWORD)
    end,

    Controls#ctrl.match_case_checkbox =/= undefined andalso begin
        wxCheckBox:setValue(Controls#ctrl.match_case_checkbox,
                            Flags band ?wxFR_MATCHCASE =:= ?wxFR_MATCHCASE)
    end.
-spec connect_events(ctrl()) -> 'ok'.
connect_events(#ctrl{} = C) ->
    % when text changes
    wxTextCtrl:connect(C#ctrl.find_box, command_text_updated),

    % when user press enter, it is needed
    wxTextCtrl:connect(C#ctrl.find_box, command_text_enter),

    % I tried to :connect(Dialog) to avoid some typing, the problem
    % is if someone connects to the dialog events and don't "skip" them, we
    % won't receive them, hence we connect to the controls one by one.
    wxButton:connect(C#ctrl.find_next_button, command_button_clicked),
    wxButton:connect(C#ctrl.cancel_button, command_button_clicked),

    C#ctrl.replace_box =/= undefined andalso begin
        wxTextCtrl:connect(C#ctrl.replace_box, command_text_enter),
        wxButton:connect(C#ctrl.replace_button, command_button_clicked),
        wxButton:connect(C#ctrl.replace_all_button, command_button_clicked)
    end,

    % when dialog is hidden we destroy it
    wxDialog:connect(C#ctrl.dialog, show),

    % when the dialog is closed
    wxDialog:connect(C#ctrl.dialog, close_window).

-spec notify_event(fr_event(), listener(), ctrl(), fr_data()) -> 'ok'.
notify_event(Event, undefined, _, _) ->
    io:format("wxFindReplaceDialog_ex: event ~p lost in oblivion~n", [Event]);

notify_event(find, Pid, Controls, FRData) ->
    ControlState = get_controls_state(Controls),
    % @note need to check for the empty string because it might come from the
    % "enter key" event which may happen even if buttons are disabled.
    ControlState#ctrlstate.find =/= "" andalso begin
        case is_same_find_condition(ControlState, FRData) of
            false ->
                update_FindReplaceData(FRData, ControlState),
                send_event(find, Pid, Controls#ctrl.dialog, ControlState);

            true ->
                send_event(find_next, Pid, Controls#ctrl.dialog, ControlState)

        end
    end;

notify_event(replace, Pid, Controls, FRData) ->
    % @note doesn't need to check for empty find box because the buttons shall
    % be disabled.
    ControlState = get_controls_state(Controls),
    update_FindReplaceData(FRData, ControlState),
    send_event(replace, Pid, Controls#ctrl.dialog, ControlState);

notify_event(replace_all, Pid, Controls, FRData) ->
    ControlState = get_controls_state(Controls),
    update_FindReplaceData(FRData, ControlState),
    send_event(replace_all, Pid, Controls#ctrl.dialog, ControlState);

notify_event(close, Pid, Controls, _) ->
    send_event(close, Pid, Controls#ctrl.dialog, undefined).

-spec send_event(fr_event(), pid(), Dialog, ctrlstate() | 'undefined') -> any()
    when
        Dialog :: wxFindReplaceDialog_ex().
send_event(close, Pid, Dialog, _) ->
    EventObject = #wxFindDialogEvent_ex{type = close},
    Pid ! #wx{id = -1, obj = Dialog, event = EventObject, userData = []};

send_event(EventName, Pid, Dialog, ControlState) ->
    EventObject = #wxFindDialogEvent_ex{
        type            = EventName,
        find_string     = ControlState#ctrlstate.find,
        replace_string  = ControlState#ctrlstate.replace,
        flags           = ControlState#ctrlstate.flags
    },
    Pid ! #wx{id = -1, obj = Dialog, event = EventObject, userData = []}.

-spec update_FindReplaceData(fr_data(), ctrlstate()) -> 'ok'.
update_FindReplaceData(FRData, ControlState) ->
    wxFindReplaceData:setFindString(    FRData, ControlState#ctrlstate.find),
    wxFindReplaceData:setReplaceString( FRData, ControlState#ctrlstate.replace),
    wxFindReplaceData:setFlags(         FRData, ControlState#ctrlstate.flags).

-spec get_replace_string('undefined' | wxTextCtrl:wxTextCtrl()) -> string().
get_replace_string(undefined) -> "";
get_replace_string(Ctrl) -> wxTextCtrl:getValue(Ctrl).

-spec get_flags(ctrl()) -> integer().
get_flags(Controls) ->
    get_flag(?wxFR_DOWN,   wxRadioButton, Controls#ctrl.down_radio) bor
    get_flag(?wxFR_WHOLEWORD, wxCheckBox, Controls#ctrl.whole_word_checkbox) bor
    get_flag(?wxFR_MATCHCASE, wxCheckBox, Controls#ctrl.match_case_checkbox).

-spec get_flag(integer(), Type, wxControl:wxControl()) -> integer()
    when
        Type :: 'wxRadioButton' | 'wxCheckBox'.
% down radio gets special treatment
get_flag(?wxFR_DOWN, wxRadioButton, undefined) -> 1;
get_flag(_, _, undefined) -> 0;
get_flag(Value, Mod, Ctrl) ->
    case Mod:getValue(Ctrl) of
        true -> Value;
        false -> 0
    end.

-spec get_controls_state(ctrl()) -> ctrlstate().
get_controls_state(Controls) ->
    #ctrlstate{
        find = wxTextCtrl:getValue(Controls#ctrl.find_box),
        replace = get_replace_string(Controls#ctrl.replace_box),
        flags = get_flags(Controls)
    }.

-spec is_same_find_condition(ctrlstate(), fr_data()) -> boolean().
is_same_find_condition(#ctrlstate{find = FindString, flags = Flags}, FRData) ->
    FindString =:= wxFindReplaceData:getFindString(FRData) andalso
        Flags =:= wxFindReplaceData:getFlags(FRData).