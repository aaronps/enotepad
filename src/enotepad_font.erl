%%%-------------------------------------------------------------------
%%% @author aaronps
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Oct 2016 06:54
%%%-------------------------------------------------------------------
-module(enotepad_font).
-author("aaronps").

-include_lib("wx/include/wx.hrl").

%% API
-export([load/1, destroy/1, default_font/0, show_select_dialog/2]).

-export([config_save/1, config_check/1]).
-export_type([font/0]).

-record(font,{name,style,weight,size}).
-opaque font() :: #font{}.

-spec load(font() | 'undefined') -> wxFont:wxFont() | 'undefined'.
load(undefined)               -> undefined;
load(#font{name = undefined}) -> undefined;
load(#font{name = Name, style = Style, weight = Weight, size = Size}) ->
    Font = wxFont:new(),
    wxFont:setFaceName(Font, Name),
    wxFont:setStyle(Font, Style),
    wxFont:setWeight(Font, Weight),
    wxFont:setPointSize(Font, Size),
    case wxFont:ok(Font) of
        true -> Font;
        false -> wxFont:destroy(Font), undefined
    end.

-spec default_font() -> wxFont:wxFont().
default_font() ->
    case load(default_config()) of
        undefined ->
            wxFont:new( 11,
                        ?wxFONTFAMILY_TELETYPE,
                        ?wxFONTSTYLE_NORMAL,
                        ?wxFONTWEIGHT_NORMAL,[]);

        Font -> Font
    end.

-spec destroy('undefined' | wxFont:wxFont()) -> 'ok'.

destroy(undefined)  -> ok;
destroy(Font)       -> wxFont:destroy(Font).

-spec show_select_dialog(Parent, Font) -> wxFont:wxFont()
    when
        Parent :: wxWindow:wxWindow(),
        Font :: 'undefined' | wxFont:wxFont().

show_select_dialog(Parent, Font) ->
    FontData = wxFontData:new(),
    wxFontData:enableEffects(FontData, false),

    (Font =/= undefined) andalso wxFontData:setInitialFont(FontData, Font),

    % if use wx:null() instead of frame, dialog is too far
    Dialog = wxFontDialog:new(Parent, FontData),
    wxFontData:destroy(FontData),
    try
        case wxDialog:showModal(Dialog) of
            ?wxID_OK ->
                NewFontData = wxFontDialog:getFontData(Dialog),
                % not clear whether getChosenFont returns a copy in wxErlang
                wxFontData:getChosenFont(NewFontData);

            ?wxID_CANCEL ->
                Font
        end
    after
        wxFontDialog:destroy(Dialog)
    end.

-spec config_save('undefined' | wxFont:wxFont()) -> font().
config_save(undefined) -> undefined;
config_save(Font) ->
    #font{
        name    = wxFont:getFaceName(Font),
        style   = wxFont:getStyle(Font),
        weight  = wxFont:getWeight(Font),
        size    = wxFont:getPointSize(Font)
    }.

-spec config_check(font() | 'undefined') -> font() | 'undefined'.
config_check(#font{} = F)
    when
        is_list(F#font.name),
        is_integer(F#font.style),
        is_integer(F#font.weight),
        is_integer(F#font.size) ->
    F;

config_check(_) -> undefined.

%%
%% private
%%

-spec default_config() -> font().
default_config() ->
    % maybe "NSimsun"
    #font{
        name    = "Consolas",
        style   = ?wxFONTSTYLE_NORMAL,
        weight  = ?wxFONTWEIGHT_NORMAL,
        size    = 11
    }.