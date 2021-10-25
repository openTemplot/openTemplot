unit DCConfiguration;

{$ifdef fpc}
  {$mode objfpc}
  {$h+}
{$endif}

interface

uses
  Classes;

type
{*----------------------------------------------------------------------------

  ----------------------------------------------------------------------------}
  TAbstractConfiguration = class(TObject)
  private
  protected
  public
    procedure AddProperty(AKey: String; AValue: String); virtual; abstract;
    procedure RemoveProperty(AKey: String); virtual; abstract;
    procedure Clear(); virtual; abstract;

    function ContainsKey(AKey: String): Boolean; virtual; abstract;
    function GetKeys(): TStrings; virtual; abstract;
    function IsEmpty(): Boolean; virtual; abstract;
    function GetProperty(AKey: String): String; virtual; abstract;


  end;

{*----------------------------------------------------------------------------

  ----------------------------------------------------------------------------}
  TBaseConfiguration = class(TAbstractConfiguration)
  private
  protected
    FMap: TStrings;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure AddProperty(AKey: String; AValue: String); override;
    procedure RemoveProperty(AKey: String); override;
    procedure Clear(); override;

    function ContainsKey(AKey: String): Boolean; override;
    function GetKeys(): TStrings; override;
    function IsEmpty(): Boolean; override;
    function GetProperty(AKey: String): String; override;
  end;

{*----------------------------------------------------------------------------

  ----------------------------------------------------------------------------}
  TFileConfiguration = class(TBaseConfiguration)
  private
  protected
  public
    procedure Load(AFilename: String); overload; virtual;
    procedure Load(AStream: TFileStream); overload; virtual;
    procedure Save(AFilename: String); overload; virtual;
    procedure Save(AStream: TFileStream); overload; virtual;
  end;


implementation

{*----------------------------------------------------------------------------
     TBaseConfiguration
  ----------------------------------------------------------------------------}
constructor TBaseConfiguration.Create;
begin
  inherited Create;
  FMap := TStringList.Create;
end;

destructor TBaseConfiguration.Destroy;
begin
  FMap.Free;
  inherited Destroy;
end;

procedure TBaseConfiguration.AddProperty(AKey: String; AValue: String);
begin
  FMap.Add(AKey + '=' + AValue);
end;

procedure TBaseConfiguration.RemoveProperty(AKey: String);
var
  index: Integer;
begin
  index := FMap.IndexOfName(AKey);
  if (index >= 0) then
    FMap.Delete(index);
end;

procedure TBaseConfiguration.Clear();
begin
  FMap.Clear;
end;

function TBaseConfiguration.ContainsKey(AKey: String): Boolean;
var
  index: Integer;
begin
  index := FMap.IndexOfName(AKey);
  Result := (index >= 0);
end;

function TBaseConfiguration.GetKeys(): TStrings;
var
  res: TStrings;
  Count: Integer;
begin
  res := TStringList.Create;
  for Count := 0 to FMap.Count - 1 do
    res.Add(FMap.Names[Count]);
  Result := res;
end;

function TBaseConfiguration.IsEmpty(): Boolean;
begin
  Result := (FMap.Count <= 0);
end;

function TBaseConfiguration.GetProperty(AKey: String): String;
begin
  Result := '';
  if (Self.ContainsKey(AKey)) then
    Result := FMap.Values[AKey];
end;


{*----------------------------------------------------------------------------
     TFileConfiguration
  ----------------------------------------------------------------------------}
procedure TFileConfiguration.Load(AFilename: String);
begin
  Self.FMap.LoadFromFile(AFilename);
end;

procedure TFileConfiguration.Load(AStream: TFileStream);
begin
  Self.FMap.LoadFromStream(AStream);
end;

procedure TFileConfiguration.Save(AFilename: String);
begin
  Self.FMap.SaveToFile(AFilename);
end;

procedure TFileConfiguration.Save(AStream: TFileStream);
begin
  Self.FMap.SaveToStream(AStream);
end;

end.
