%%%-------------------------------------------------------------------
%%% @author aaronps
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Oct 2016 06:22
%%%-------------------------------------------------------------------
-module(enotepad_config).
-author("aaronps").

-include("enotepad.hrl").

%% API
-export([load/0, save/1]).

-spec load() -> config().
load() ->
    FileName = filename:join(config_dir(), ?CONFIG_FILE),
    case file:consult(FileName) of
        {ok, [#config{} = Config]} -> config_check(Config);
        _                          -> #config{}
    end.

-spec save(config()) -> 'ok'.
save(#config{} = C) ->
    FileName = filename:join(config_dir(), ?CONFIG_FILE),
    case filelib:ensure_dir(FileName) of
        ok          -> file:write_file(FileName, io_lib:format("~p.\n", [C]));
        {error, _}  -> ok
    end.

%% private

-spec config_dir() -> file:filename_all().
config_dir() ->
    filename:basedir(user_config, ?APPNAME).

-spec config_check(config()) -> config().
config_check(#config{} = C)
    when
        is_boolean(C#config.word_wrap),
        is_boolean(C#config.status_bar) ->
    % @anecdote here I had a bug, the 'C' was missing, so the initial booleans
    % were always false, but I didn't realize this UNTIL I began checking with
    % dialyzer, there was this message bugging me:
    % "The pattern 'a pattern involving both booleans' can never match
    % {'false','false'}"
    %
    % And I was so confused, it was not until I followed the code visually...
    % that I arrived here and noticed the C missing... looks like dialyzer works
    C#config{
        window = enotepad_wx:config_check(C#config.window),
        font = enotepad_font:config_check(C#config.font)
    };

config_check(_) -> #config{}.