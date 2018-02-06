%%%-------------------------------------------------------------------
%%% @author aaron
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Sep 2017 11:23
%%%-------------------------------------------------------------------

-module(enotepad_build_escript_icon_prv).
-author("aaronps").

-export([init/1, do/1, format_error/1]).

-include_lib("wx/include/wx.hrl").

-define(PROVIDER, enotepad_build_escript_icon).
-define(DEPS, []).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},            % The 'user friendly' name of the task
        {module, ?MODULE},            % The module implementation of the task
        {bare, false},                % The task cannot be run by the user
        {deps, ?DEPS},                % The list of dependencies
        {example, "rebar3 " ++ atom_to_list(?PROVIDER)}, % How to use the plugin
        {opts, []},
        {short_desc, "(enotepad) Generates the raw icon which is loaded by enotepad escript version"},
        {desc, "Generates the raw icon which is loaded by enotepad escript version"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    SourceIcon = filename:join("icon","icon32.png"),
    TargetIcon = filename:join("ebin", "icon32.raw"),
    case filelib:last_modified(TargetIcon) < filelib:last_modified(SourceIcon) of
        true  -> convert_icon(SourceIcon, TargetIcon);
        false -> rebar_api:info("escript icon is up to date",[])
    end,
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

-spec load_icon(string())
        -> {ok, RawFile :: binary()} | no_return().
%% Loads an icon file and generates the Raw and Mask files.
%%
%% Unfortunately, wxErlang can only load icons from files, not from zips,
%% binaries or other streams, otherwise, this code would be more simple:
%%    {ok, IconBin} = file:read_file("priv/icon.png"),
%%    IconFile = {"icon.png", IconBin},
load_icon(Filename) ->
    wx:new(),
    Image = wxImage:new(Filename, [{type, ?wxBITMAP_TYPE_PNG}]),
    case wxImage:ok(Image) of
        true ->
            ImageWidth = wxImage:getWidth(Image),
            ImageHeight = wxImage:getHeight(Image),
            RGBData = wxImage:getData(Image),
            RGBLen = byte_size(RGBData),
            AlphaData = wxImage:getAlpha(Image),
            AlphaLen = byte_size(AlphaData),

            {ok, <<ImageWidth:32, ImageHeight:32,
                RGBLen:32,   RGBData/binary,
                AlphaLen:32, AlphaData/binary>>};

        false ->
            io:format("Error: icon ~p load error~n", [Filename]),
            halt(2)
    end.

-spec convert_icon(file:filename_all(), file:filename_all()) -> ok | no_return().
convert_icon(Source, Target) ->
    {ok, IconBin} = load_icon(Source),
    rebar_api:info("Writting raw icon file on ~p ...",[Target]),
    ok = filelib:ensure_dir(Target),
    ok = file:write_file(Target, IconBin).