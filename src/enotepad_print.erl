%%%-------------------------------------------------------------------
%%% @author aaronps
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Oct 2016 21:28
%%%-------------------------------------------------------------------
-module(enotepad_print).
-author("aaronps").

%% API
-export([init/0, page_setup/1, print/3, destroy/1]).
-export_type([enotepad_print/0]).

-opaque enotepad_print() :: wxHtmlEasyPrinting:wxHtmlEasyPrinting()|'undefined'.

-spec init() -> enotepad_print().
init() -> undefined.

-spec page_setup(enotepad_print()) -> enotepad_print().
page_setup(undefined) ->
    page_setup(wxHtmlEasyPrinting:new());

page_setup(EasyPrint) ->
    wxHtmlEasyPrinting:pageSetup(EasyPrint),
    EasyPrint.

-spec print(enotepad_print(), string(), Font) -> enotepad_print()
    when
        Font :: wxFont:wxFont() | 'undefined'.
print(undefined, Text, Font) ->
    print(wxHtmlEasyPrinting:new(), Text, Font);

print(EasyPrint, Text, Font) when Font =/= undefined ->
    FaceName = wxFont:getFaceName(Font),
    FontSize = wxFont:getPointSize(Font),
    Sizes = lists:duplicate(7, FontSize),

    wxHtmlEasyPrinting:setFonts(EasyPrint, FaceName, FaceName, [{sizes,Sizes}]),

    print(EasyPrint, Text, undefined);

print(EasyPrint, Text, _) ->
    HtmlText = text2Html(Text),
    wxHtmlEasyPrinting:printText(EasyPrint, HtmlText),
    EasyPrint.

-spec destroy(enotepad_print()) -> 'ok'.
destroy(undefined) -> ok;
destroy(EasyPrint) -> wxHtmlEasyPrinting:destroy(EasyPrint).


%%
%% private
%%
-spec text2Html(string()) -> list(string()).
text2Html(T) ->
    [
        "<html><head></head><body><pre>",
        lists:map(fun html_escape/1, T),
        "</pre></body></html>"
    ].

-spec html_escape(char()) -> char() | string().
html_escape($<) -> "&lt;";
html_escape($>) -> "&gt;";
html_escape($&) -> "&amp;";
html_escape($") -> "&quot;";
html_escape(C) -> C.
