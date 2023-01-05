program codegen;

{$mode Delphi}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  { you can add units after this }
  OTUnitGenerator,
  OTClassRegenerator;


{$R codegen.rc}

type

  { TOTCodeGen }

  TOTCodeGen = class(TCustomApplication)
  protected
    procedure DoRun; override;

    procedure NewClass(const AClassName: string);
    procedure Generate(const AFileName: string);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TOTCodeGen }


procedure TOTCodeGen.NewClass(const AClassName: string);
var
  template: TOTUnitGenerator;
begin
  template := TOTUnitGenerator.Create('TEMPLATE_NEW_CLASS', AClassName);
  try
    template.Generate;
  finally
    template.Free;
  end;
end;

procedure TOTCodeGen.Generate(const AFileName: string);
var
  classGen: TOTClassRegenerator;
begin
  classGen := TOTClassRegenerator.Create(AFileName);
  try
     classGen.Generate;
  finally
    classGen.Free;
  end;
end;

procedure TOTCodeGen.DoRun;
var
  ErrorMsg: String;
  cmdLine: TStringList;
  i: Integer;
begin
  cmdLine := TStringList.Create;
  try
    // quick check parameters
    ErrorMsg:=CheckOptions('h', ['help'], nil, cmdLine);
    if ErrorMsg<>'' then begin
      ShowException(Exception.Create(ErrorMsg));
      Terminate;
      Exit;
    end;

    // parse parameters
    if HasOption('h', 'help') then begin
      WriteHelp;
      Terminate;
      Exit;
    end;

    { add your program here }

    if cmdLine.Count < 1 then begin
      WriteHelp;
      Terminate;
      Exit;
    end;

    if cmdLine[0] = 'gen' then begin
      if cmdLine.Count >= 2 then begin
        // process files on command line
        for i := 1 to cmdLine.Count - 1 do begin
          Generate(cmdLine[i]);
        end;
      end
      else begin
        WriteHelp;
      end;

      Terminate;
      Exit;
    end;

    if cmdLine[0] = 'new' then begin
      if cmdLine.Count = 3 then begin
        if cmdLine[1] = 'class' then begin
          NewClass(cmdLine[2]);
        end
        else begin
          WriteHelp;
        end;
      end
      else begin
        WriteHelp;
      end;

      Terminate;
      Exit;
    end;


    // stop program loop
    Terminate;

  finally
    cmdLine.Free;
  end;
end;

constructor TOTCodeGen.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TOTCodeGen.Destroy;
begin
  inherited Destroy;
end;

procedure TOTCodeGen.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' <command>');
  writeln('  -h, --help : display this help text');
  writeln('  new class <file> : generate a unit with skeleton generation info');
  writeln('  gen <files...> : update generated code in <files...>');
end;

var
  Application: TOTCodeGen;
begin
  Application:=TOTCodeGen.Create(nil);
  Application.Title:='OpenTemplot CodeGen';
  Application.Run;
  Application.Free;
end.

