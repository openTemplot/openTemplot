unit OTOIDManager;

{$mode Delphi}

interface

uses
  Classes,
  SysUtils,
  OTPersistent;

type

  TOTOIDManager = class
  private
    FTable: array of TOTPersistent;
    FNextOID: TOID;

  public
    constructor Create;
    destructor Destroy; override;

    function AllocateOID(AObject: TOTPersistent): TOID;
    procedure FreeOID(AOID: TOID);
    function FromOID(AOID: TOID): TOTPersistent;
  end;

var
  OIDManager: TOTOIDManager;

implementation

const
  INITIAL_TABLE_SIZE = 1024;
  TABLE_INCREMENT = 1024;

{ TOTOIDManager }

constructor TOTOIDManager.Create;
begin
  inherited;

  SetLength(FTable, INITIAL_TABLE_SIZE);
  FTable[0] := nil;
  FNextOID := 1;
end;

destructor TOTOIDManager.Destroy;
begin
  inherited;
end;

function TOTOIDManager.AllocateOID(AObject: TOTPersistent): TOID;
begin
  if (FNextOID > High(FTable)) then
    SetLength(FTable, Length(FTable) + TABLE_INCREMENT);

  FTable[FNextOID] := AObject;
  Result := FNextOID;
  Inc(FNextOID);
end;

procedure TOTOIDManager.FreeOID(AOID: TOID);
begin
  if AOID < FNextOID then begin
    FTable[AOID] := nil;
  end;
end;

function TOTOIDManager.FromOID(AOID: TOID): TOTPersistent;
begin
  if AOID >= FNextOID then
    Result := nil
  else
    Result := FTable[AOID];
end;



initialization
  OIDManager := TOTOIDManager.Create;

end.
