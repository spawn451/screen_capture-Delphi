unit Logging;

//TLogger.Log(TLogLevel.Error, 'An empty IDXGIAdapter instance has been received');

interface

uses
  System.SysUtils, System.Classes;

type
  TLogLevel = (
    Debug,
    Info,
    Warning,
    Error
  );

  TLogger = class
  private
    class var
      FLogFile: string;
      FLogStream: TStreamWriter;

    class function LevelToString(Level: TLogLevel): string; static;
    class procedure EnsureLogStreamExists; static;

  public
    class constructor Create;
    class destructor Destroy;

    class procedure Log(Level: TLogLevel; const Msg: string); overload; static;
    class procedure Log(Level: TLogLevel; const Msg: string; const Args: array of const); overload; static;

    class property LogFile: string read FLogFile write FLogFile;
  end;

implementation

uses
  System.IOUtils;

{ TLogger }

class constructor TLogger.Create;
begin
  //FLogFile := TPath.Combine(ExtractFilePath(ParamStr(0)), 'Application.log');
  //FLogFile := TPath.Combine(TPath.GetDocumentsPath, 'Application.log');
  FLogFile := 'D:\Application.log';
end;

class destructor TLogger.Destroy;
begin
  if Assigned(FLogStream) then
  begin
    FLogStream.Free;
    FLogStream := nil;
  end;
end;

class function TLogger.LevelToString(Level: TLogLevel): string;
begin
  case Level of
    TLogLevel.Debug:   Result := 'DEBUG';
    TLogLevel.Info:    Result := 'INFO';
    TLogLevel.Warning: Result := 'WARNING';
    TLogLevel.Error:   Result := 'ERROR';
  else
    Result := 'UNKNOWN';
  end;
end;

class procedure TLogger.EnsureLogStreamExists;
begin
  if not Assigned(FLogStream) then
  begin
    ForceDirectories(ExtractFilePath(FLogFile));
    FLogStream := TStreamWriter.Create(FLogFile, True, TEncoding.UTF8);
    FLogStream.AutoFlush := True;
  end;
end;

class procedure TLogger.Log(Level: TLogLevel; const Msg: string);
begin
  EnsureLogStreamExists;
  if Assigned(FLogStream) then
  begin
    FLogStream.WriteLine(Format('%s [%s] %s',
      [FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now),
       LevelToString(Level),
       Msg]));
  end;
end;

class procedure TLogger.Log(Level: TLogLevel; const Msg: string; const Args: array of const);
begin
  Log(Level, Format(Msg, Args));
end;

end.
