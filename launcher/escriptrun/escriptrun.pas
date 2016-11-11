program escriptrun;

uses windows, sysutils;

const
    ESCRIPT_FILE = 'escript ';

var
    ProcessInformation: TPROCESSINFORMATION;
    StartupInfo: TSTARTUPINFO;
    Command  : String;
    FileName : String;
    ExtensionLength : integer;

{$R *.res}

begin
    FileName := ExpandFileName(ParamStr(0));
    ExtensionLength := Length(ExtractFileExt(FileName));
    { 4 is the extension length, so lazy }
    Command := ESCRIPT_FILE + LeftStr(  FileName,
                                        Length(FileName) - ExtensionLength);

    if ParamCount > 0 then
        Command := Command + ' "' + ParamStr(1) + '"';

    StartupInfo := Default(TStartupInfo);
    StartupInfo.cb := sizeof(StartupInfo);
    windows.CreateProcess(  nil,
                            PChar(Command),
                            nil,
                            nil,
                            false,
                            CREATE_NO_WINDOW,
                            nil,
                            PChar(nil),
                            StartupInfo,
                            ProcessInformation);
end.

