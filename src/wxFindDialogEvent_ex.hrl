%%%-------------------------------------------------------------------
%%% @author aaronps
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Oct 2016 19:11
%%%-------------------------------------------------------------------
-author("aaronps").

% event types: find_next, find_replace, find_replace_all, close

-record(wxFindDialogEvent_ex, {type, find_string, replace_string, flags}).

-type wxFindDialogEvent_ex() :: #wxFindDialogEvent_ex{}.

