program erlrun;

uses windows, sysutils;

const
    ERL_FILE = 'bin\erl.exe -detached';

function PrepareFilename(Name: String) : String;
begin
    Result := '"\"' + StringReplace(Name, '\', '\\', [rfReplaceAll]) + '\""';
end;

function FileParameter(Name: String) : String;
begin
    Result := ' -enotepad file ' + PrepareFilename(ExpandFileName(Name));
end;

var
    ProcessInformation: TPROCESSINFORMATION;
    StartupInfo: TSTARTUPINFO;
    Command : String;

{$R *.res}

begin
    Command := ExtractFilePath(ExpandFileName(ParamStr(0))) + ERL_FILE;

    if ParamCount > 0 then
        Command := Command + FileParameter(ParamStr(1));

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

