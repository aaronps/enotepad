%%! -smp
%%%-------------------------------------------------------------------
%%% @author aaronps
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Oct 2016 20:59
%%%-------------------------------------------------------------------
-module(emaker).
-author("aaronps").

-define(APPNAME, enotepad).

-include_lib("wx/include/wx.hrl").
-include_lib("kernel/include/file.hrl").

%% Entry used to generate the zip file for the 'escript' generation.
-type file_entry() :: {Name :: string(), Data :: binary()}.


%% API
-export([main/1]).

main([]) ->
    main(["all"]);

main(["clean"]) ->
    clean();

main(["rebuild"]) ->
    clean(), R = make(),
    io:format("~p~n", [R]);

main(["all"]) ->
    R = make(),
    io:format("~p~n", [R]);

main(["escript" | Type]) ->
    make(),
    Scr = make_script(),
    Dir = filename:join("releases", "escript"),
    prepare_release_dir(Dir),

    R = case Type of
        [] ->
            case os:type() of
                { win32, _} -> write_file(Dir,
                                          script_name(windows),
                                          [shebang(windows), Scr]);

                { unix, _} ->  write_file(Dir,
                                          script_name(unix),
                                          [shebang(unix)   , Scr])
            end;

        ["all"] ->
            R1 = write_file(Dir, script_name(windows), [shebang(windows), Scr]),
            R2 = write_file(Dir, script_name(unix),    [shebang(unix)   , Scr]),
            case R1 of
                ok -> R2;
                _  -> R1
            end;

        ["unix"] ->
            write_file(Dir, script_name(unix), [shebang(unix), Scr]);

        ["windows"] ->
            write_file(Dir, script_name(windows), [shebang(windows), Scr]);

        ["rebar"] ->
            R1 = write_file(Dir, script_name(unix), [shebang(unix), Scr]),
            R2 = write_file(Dir, script_name(windows), shebang(windows_mini)),
            case R1 of
                ok -> R2;
                _  -> R1
            end;

        ["exe"] ->
            R1 = write_file(Dir, script_name(unix), [shebang(unix), Scr]),
            R2 = file:copy(filename:join("launcher", "escriptrun.exe"),
                           filename:join(Dir, "enotepad.exe")),
            case R1 of
                ok -> element(1,R2);
                _ -> R1
            end
    end,

    io:format("Result: ~p~n", [R]);

main(["dialyzer" | Type]) ->
    io:format("Make~n"),
    make(),
    io:format("Dialyze~n"),
    Warnings = case Type of
                   %% the extra [] is needed because Type is the TAIL of the
                   %% parameter list
                   ["src"] ->
                       dialyzer:run([
                                        {check_plt, false},
                                        {from, src_code},
                                        {files, ["src"]}
                                    ]);

                   Default when Default =:= ""; Default =:= ["ebin"] ->
                       dialyzer:run([
                                        {check_plt, false},
                                        {files, ["ebin"]}
                                    ]);

                   Other ->
                       io:format("Dialyze Other: ~p~n", [Other]),
                       help(),
                       halt(1)
               end,

    io:format([dialyzer:format_warning(X) || X <- Warnings]),
    io:format("End~n"),
    halt(0);

main(["release","systools"]) ->
    release_systools();

main(["release","reltool"]) ->
    release_reltool();

main(A) ->
    io:format("Args where: ~p~n", [A]),
    help(),
    halt(1).

help() ->
    io:format("available targets:
    all             - (default) runs make:all
    clean           - removes ebin
    rebuild         - cleans and build
    escript         - builds escript file for this system
    escript all     - builds escript file for all systems (not rebar style)
    escript windows - builds escript file for windows
    escript unix    - builds escript file for unix-like systems
    escript rebar   - builds escript file rebar style (windows file is small)
    escript exe     - same as 'escript rebar' but using .exe instead of .cmd
    dialyzer Type   - runs dializer, Type might be ebin(default) or src
    release systools- makes a release using systools
    release reltool - makes a release using reltool
").

die(Message) -> io:format(Message), halt(1).
die(Message, Params) -> io:format(Message, Params), halt(1).

write_file(Dir, File, Data) ->
    file:write_file(filename:join(Dir, File), Data).

script_name(unix) -> "enotepad";
script_name(windows) -> script_name(unix) ++ ".cmd".

shebang(unix) -> "#!/usr/bin/env escript\n";
shebang(windows) -> "@escript %~f0 %* & exit /b\r\n";
shebang(windows_mini) -> "@escript %~d0%~p0%~n0 %*\r\n".

make_script() ->
    wx:new(),
    true = code:add_path("ebin"),
    ok = application:load(enotepad),
    {ok, Modules} = application:get_key(enotepad, modules),

    % Cannot mix "filename", {"filename", Bin} on the zip files creation,
    % so, load all files as binaries.

    Beams = load_modules(Modules),
    IconFile = load_icon("icon/icon32.png"),
    AppFile = {"enotepad.app", load_file("ebin/enotepad.app")},

    % 'shebang' always prepends "#!", so to create the ".cmd" we have to
    % do it ourselves

    case escript:create(binary, [
        %% this won't work for a windows cmd:
        %% {shebang, "@echo off & escript %~f0 %* & exit /b"},
        comment,
        {emu_args, "-smp"},
        {archive, [IconFile, AppFile | Beams], []}
    ]) of
        {ok, Binary} -> Binary;
        error ->
            io:format("ERROR creating script~n"),
            halt(4)
    end.


-spec load_modules(list(module())) -> list(file_entry()) | no_return().
%% Loads the list of modules returning a list of file entries ready to 'zip'.
load_modules(Modules) ->
    [module_to_file_entry(X, code:get_object_code(X)) || X <- Modules].

-spec module_to_file_entry(Module, {Module, Binary, Filename})
        -> file_entry() | no_return()
      when
        Module   :: module(),
        Binary   :: binary(),
        Filename :: string().
%% handles result of code:get_object_code/1, will return a file entry or die
%% trying.
module_to_file_entry(_, {_, Binary, File}) -> {filename:basename(File), Binary};
module_to_file_entry(Module, error) ->
    io:format("Error: cannot load module: ~p~n", [Module]),
    halt(1).

-spec load_icon(string())
        -> {RawFile :: file_entry(), MaskFile :: file_entry()} | no_return().
%% Loads an icon file and generates the Raw and Mask files.
%%
%% Unfortunately, wxErlang can only load icons from files, not from zips,
%% binaries or other streams, otherwise, this code would be more simple:
%%    {ok, IconBin} = file:read_file("priv/icon.png"),
%%    IconFile = {"icon.png", IconBin},
load_icon(Filename) ->
    Image = wxImage:new(Filename, [{type, ?wxBITMAP_TYPE_PNG}]),
    case wxImage:ok(Image) of
        true ->
            ImageWidth = wxImage:getWidth(Image),
            ImageHeight = wxImage:getHeight(Image),
            RGBData = wxImage:getData(Image),
            RGBLen = byte_size(RGBData),
            AlphaData = wxImage:getAlpha(Image),
            AlphaLen = byte_size(AlphaData),
            {"icon32.raw", <<ImageWidth:32, ImageHeight:32,
                             RGBLen:32,   RGBData/binary,
                             AlphaLen:32, AlphaData/binary>>};

        false ->
            io:format("Error: icon ~p load error~n", [Filename]),
            halt(2)
    end.



-spec load_file(string()) -> binary() | no_return().
%% Returns a file binary or dies trying.
load_file(Name) ->
    case file:read_file(Name) of
        {ok, Bin}      -> Bin;
        {error, Error} ->
            io:format("Error: Cannot load file ~p because ~p~n", [Name, Error]),
            halt(3)
    end.

clean() ->
    delete_file("ebin").

make() ->
    % note: ensure_dir ensures that all parent directories for the specified
    % file or directory exist, trying to create them if necessary,
    % hence -> /something
    filelib:ensure_dir("ebin/something"),

    AppNameStr = atom_to_list(?APPNAME),
    case file:consult("src/" ++ AppNameStr ++ ".app.src") of
        {ok, [{application, ?APPNAME, AppProp} = AppDesc]} ->
            io:format("Making '~s' Version ~s~n", [
                proplists:get_value(description, AppProp),
                proplists:get_value(vsn, AppProp)
                ]),
            file:write_file("ebin/" ++ AppNameStr ++ ".app",
                            io_lib:fwrite("~p.\n",[AppDesc])),

            case make:all() of
                up_to_date -> ok;
                _ ->
                    io:format("*** ERROR making ***~n"),
                    halt(1)
            end;

        {ok, _} ->
            io:format("ERROR application descriptor doesn't seem right~n"),
            halt(1);

        {error, Reason} ->
            io:format("ERROR cannot make because ~p~n", [Reason]),
            halt(1)
    end.

delete_file(Name) ->
    case filelib:is_dir(Name) of
        false ->
            io:format("Delete file: ~s~n", [Name]),
            file:delete(Name);

        true ->
            case file:list_dir_all(Name) of
                {ok, FileNames} ->
                    [ delete_file(filename:join(Name, X)) || X <- FileNames],
                    io:format("Delete Dir : ~s~n", [Name]),
                    file:del_dir(Name);

                {error, Reason} ->
                    io:format("Error deleting ~p: ~p", [Name, Reason]),
                    halt(1)
            end

    end.

release_systools() ->
    io:format("--> Rebuilding~n"),
    clean(),
    make(),

    io:format("--> Configuring~n"),
    AppNameStr = atom_to_list(?APPNAME),
    case file:consult("ebin/" ++ AppNameStr ++ ".app") of
        {ok, [{application, ?APPNAME, AppProp}]} ->
            Rel = generate_systools_rel(AppProp),
            ReleaseDir = filename:join("releases","systool"),
            prepare_release_dir(ReleaseDir),
            ReleaseId = AppNameStr ++ "-" ++ proplists:get_value(vsn, AppProp),
            ReleaseBase = filename:join(ReleaseDir, ReleaseId),

            file:write_file(ReleaseBase ++ ".rel", io_lib:format("~p.", [Rel])),

            io:format("--> Making release ~s ~n", [ReleaseId]),
            systools:make_script(ReleaseBase, [
                local,
                {path, ["ebin"]}
            ]),

            systools:make_tar(ReleaseBase, [
                {erts, code:root_dir()},
                {path, ["ebin"]}
            ]),
            io:format("Release ok in '~s'~n", [ReleaseBase ++ ".tar.gz"]);

        {ok, _} -> die("*** ERROR *** App file incorrect~n");

        _ -> die("*** ERROR *** cannot load app file~n")
    end.


generate_systools_rel(AppProp) ->
    Applications = proplists:get_value(applications, AppProp),
    [ application:load(X) || X <- Applications],
    { release,
        {atom_to_list(?APPNAME), proplists:get_value(vsn, AppProp)},
        {erts, erlang:system_info(version) },
        [
            { X, get_app_version(X)} || X <- Applications
        ] ++ [{ ?APPNAME, proplists:get_value(vsn, AppProp)}]
    }.

get_app_version(App) ->
    case application:get_key(App, vsn) of
        {ok, Version} -> Version;
        _ -> die("Cannot get version for application ~p~n", [App])
    end.

prepare_release_dir(Dir) ->
    case delete_file(Dir) of
        {error, Reason} when Reason =/= enoent ->
            die("*** ERROR *** Cannot delete release dir '~s' because: ~p~n",
                [Dir, Reason]);

        _ ->
            filelib:ensure_dir(filename:join(Dir, "something"))
    end.

release_reltool() ->
    io:format("--> Rebuilding~n"),
    clean(),
    make(),

    io:format("--> Configuring~n"),
    AppNameStr = atom_to_list(?APPNAME),
    case file:consult("ebin/" ++ AppNameStr ++ ".app") of
        {ok, [{application, ?APPNAME, AppProp}]} ->
            ReleaseDir = filename:join("releases","reltool"),
            prepare_release_dir(ReleaseDir),
            AppVersion = proplists:get_value(vsn, AppProp),
            io:format("--> making release ~p~n", [AppVersion]),
            R = reltool:create_target([{config, {sys,[
                {relocatable, true},
                {profile, standalone},
                {rel, AppNameStr, AppVersion,
                    proplists:get_value(applications, AppProp) ++ [?APPNAME]
                },
                {app, ?APPNAME, [
                    {lib_dir, "."}
                ]},
                {boot_rel, AppNameStr}
            ]}}], ReleaseDir),

            case os:type() of
                { win32, _} -> file:copy(filename:join("launcher", "erlrun.exe"),
                                         filename:join(ReleaseDir, "enotepad.exe"));

                { unix, _} ->  file:copy(filename:join("launcher", "erlrun.sh"),
                                         filename:join(ReleaseDir, "enotepad")),
                    % althrough the filename:join is the same in this two line,
                    % keep it like this for reading purpose.
                    set_executable_bit(filename:join(ReleaseDir, "enotepad"))
            end,

            io:format("reltool result: ~p~n", [R]);

        {ok, _} -> die("*** ERROR *** App file incorrect~n");
        _       -> die("*** ERROR *** cannot load app file~n")
    end.

set_executable_bit(Filename) ->
    {ok, #file_info{mode = OldMode}} = file:read_file_info(Filename),
    case OldMode bor 8#111 of
        OldMode -> ok; % no change, do nothing
        NewMode -> file:change_mode(Filename, NewMode)
    end.