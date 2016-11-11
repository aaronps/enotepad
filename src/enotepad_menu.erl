%%%-------------------------------------------------------------------
%%% @author aaronps
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Oct 2016 20:25
%%%-------------------------------------------------------------------
-module(enotepad_menu).
-author("aaronps").

-include_lib("wx/include/wx.hrl").
-include("enotepad.hrl").

%% API
-export([create/1]).

-type menuItemDesc() :: {integer(), string()}
                      | {integer(), string(), 'check' | 'disabled'}
                      | 'separator'.

-spec create(wxFrame:wxFrame()) -> wxMenuBar:wxMenuBar().
%% TRICK 1: the space before "Del"
%%      This is so you can see the key on the menu BUT IT DOESN'T CREATE AN
%% ACCELERATOR. This trick is needed because if the menu entry is disabled, the
%% key won't work in the editor.
%%      This trick on Linux doesn't work, it might show some warnings on the
%% console, and they key might not appear on the menu.
%%
create(Frame) ->
    MenuBar = wxMenuBar:new(),

    FileMenu = create_menu([
        {?wxID_NEW,     "&New\tCtrl+N"},
        {?wxID_OPEN,    "&Open...\tCtrl+O"},
        {?wxID_SAVE,    "&Save\tCtrl+S"},
        {?wxID_SAVEAS,  "Save &As..."},
        separator,
        {?wxID_PAGE_SETUP,  "Page Set&up..."},
        {?wxID_PRINT,       "&Print...\tCtrl+P"},
        separator,
        {?wxID_EXIT,    "E&xit"}
    ]),

    EditMenu = create_menu([
        {?wxID_UNDO,    "&Undo\tCtrl+Z", disabled},
        separator,      %% these are for "selected text"
        {?wxID_CUT,     "Cu&t\tCtrl+X",  disabled},
        {?wxID_COPY,    "&Copy\tCtrl+C", disabled},
        {?wxID_PASTE,   "&Paste\tCtrl+V"},
        {?wxID_DELETE,  "De&lete\t Del", disabled}, % see TRICK 1 about space
        separator,
        {?wxID_FIND,            "&Find...\tCtrl+F"},
        {?MENU_Edit_FindNext,   "Find &Next\tF3"}, % add find next
        {?wxID_REPLACE,         "&Replace...\tCtrl+H"},
        {?MENU_Edit_GoTo,       "&Go To...\tCtrl+G"},
        separator,
        {?wxID_SELECTALL,      "Select &All\tCtrl+A"},
        {?MENU_Edit_InsertDateTime, "Time/&Date\tF5"}
    ]),

    FormatMenu = create_menu([
        {?MENU_Format_WordWrap,     "&Word Wrap", check},
        {?MENU_Format_FontSelect,   "&Font..."}
    ]),

    ViewMenu = create_menu([
        {?MENU_View_StatusBar, "&Status Bar", check}
    ]),

    HelpMenu = create_menu([
        {?MENU_Help_ViewHelp,    "View &Help"},
        separator,
        {?wxID_ABOUT,   "&About " ?APPNAME }
    ]),

    wxMenuBar:append(MenuBar, FileMenu, "&File"),
    wxMenuBar:append(MenuBar, EditMenu, "&Edit"),
    wxMenuBar:append(MenuBar, FormatMenu, "F&ormat"),
    wxMenuBar:append(MenuBar, ViewMenu, "&View"),
    wxMenuBar:append(MenuBar, HelpMenu, "&Help"),

    wxFrame:setMenuBar(Frame, MenuBar),

    MenuBar.

%% Originally was like this, I think it is more clear, because one function
%% only creates items, and then are appended to the menu.
%% But due to wxGTK which would complain about changing the "enable" flag on
%% a menu item which has not been appended to the menu yet, we have to use the
%% other method.
%%
%%menu_item({Id}) -> wxMenuItem:new([{id, Id}]);
%%
%%menu_item({Id, Text}) -> wxMenuItem:new([{id, Id}, {text, Text}]);
%%
%%menu_item({Id, Text, check}) ->
%%    wxMenuItem:new([{id, Id}, {text, Text}, {kind, ?wxITEM_CHECK}]);
%%
%%menu_item({Id, Text, disabled}) ->
%%    MenuItem = wxMenuItem:new([{id, Id}, {text, Text}]),
%%    wxMenuItem:enable(MenuItem, [{enable, false}]),
%%    MenuItem;
%%
%%menu_item(separator) ->
%%    wxMenuItem:new([{kind, ?wxITEM_SEPARATOR}]).
%%
%%create_menu(Items) ->
%%    Menu = wxMenu:new(),
%%
%%    [ wxMenu:append(Menu, menu_item(Item)) || Item <- Items ],
%%
%%    Menu.



-spec create_menu(list(menuItemDesc())) -> wxMenu:wxMenu().
create_menu(Items) ->
    Menu = wxMenu:new(),

    [ append_item(Menu, Item) || Item <- Items ],

    Menu.

-spec append_item(wxMenu:wxMenu(), menuItemDesc()) -> 'ok' | wx:wx_object().

append_item(Menu, {Id, Text}) ->
    Item = wxMenuItem:new([{id, Id}, {text, Text}]),
    wxMenu:append(Menu, Item);

append_item(Menu, {Id, Text, check}) ->
    Item = wxMenuItem:new([{id, Id}, {text, Text}, {kind, ?wxITEM_CHECK}]),
    wxMenu:append(Menu, Item);

append_item(Menu, {Id, Text, disabled}) ->
    Item = wxMenuItem:new([{id, Id}, {text, Text}]),
    wxMenu:append(Menu, Item),
    wxMenuItem:enable(Item, [{enable, false}]);

append_item(Menu, separator) ->
    wxMenu:appendSeparator(Menu).


