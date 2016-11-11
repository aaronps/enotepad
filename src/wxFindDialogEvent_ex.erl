%%%-------------------------------------------------------------------
%%% @author aaronps
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Oct 2016 19:13
%%%-------------------------------------------------------------------
-module(wxFindDialogEvent_ex).
-author("aaronps").

%% API

-export([getFlags/1, getFindString/1, getReplaceString/1]).

-include("wxFindDialogEvent_ex.hrl").

-spec getFlags(wxFindDialogEvent_ex()) -> integer().
getFlags(#wxFindDialogEvent_ex{flags = Value}) -> Value.

-spec getFindString(wxFindDialogEvent_ex()) -> string().
getFindString(#wxFindDialogEvent_ex{find_string = Value}) -> Value.

-spec getReplaceString(wxFindDialogEvent_ex()) -> string().
getReplaceString(#wxFindDialogEvent_ex{replace_string = Value}) -> Value.

% getDialog -> not implemented, use the reference in the #wx{} event
