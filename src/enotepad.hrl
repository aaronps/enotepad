%%%-------------------------------------------------------------------
%%% @author aaronps
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Oct 2016 20:28
%%%-------------------------------------------------------------------
-author("aaronps").

-define(APPNAME, "ENotepad").
-define(CONFIG_FILE, "econfig.cfg").

-define(MENU_Edit_FindNext,         2).
-define(MENU_Edit_GoTo,             3).
-define(MENU_Edit_InsertDateTime,   4).
-define(MENU_Format_WordWrap,       5).
-define(MENU_Format_FontSelect,     6).
-define(MENU_View_StatusBar,        7).
-define(MENU_Help_ViewHelp,         8).

-record(config, {
    window             :: enotepad_wx:window() | 'undefined',
    word_wrap  = false :: boolean(),
    status_bar = false :: boolean(),
    font               :: enotepad_font:font() | 'undefined'
}).

%% this export_type is needed to avoid dialyzer warning on booleans
-export_type([config/0]).

-type config() :: #config{}.
-type text_ctrl() :: wxStyledTextCtrl:wxStyledTextCtrl().