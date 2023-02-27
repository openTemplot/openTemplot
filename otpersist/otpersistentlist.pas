unit OTPersistentList;

{$mode Delphi}

interface

uses
  Classes,
  SysUtils,
  Generics.Collections,
  OTPersistent,
  OTYaml,
  OTYamlEmitter;

type

  { TOTPersistentList }

  TOTPersistentList<T: TOTPersistent> = class(TOTPersistent)
  private
    FList: TList<TOID>;
    FOwnsObjects: Boolean;

  protected
    function GetItem(AIndex: Integer): T;
    procedure SetItem(AIndex: Integer; AValue: T);
    procedure RestoreYamlAttribute(AName, AValue: String; AIndex: Integer; ALoader: TOTPersistentLoader); override;
    procedure SaveYamlAttributes(AEmitter: TYamlEmitter); override;

  public
    constructor Create(AParent: TOTPersistent); override;
    destructor Destroy; override;

    function Add(AValue: T): Integer;
    function Count: Integer;



    property Items[AIndex: Integer]: T Read GetItem Write SetItem; default;
  end;

  { TOTOwningList }

  TOTOwningList<T: TOTPersistent> = class(TOTPersistentList<T>)
  public
    constructor Create(AParent: TOTPersistent); override;
  end;

  { TOTReferenceList }

  TOTReferenceList<T: TOTPersistent> = class(TOTPersistentList<T>)
  public
    constructor Create(AParent: TOTPersistent); override;
  end;

implementation

{ TOTPersistentList }

constructor TOTPersistentList<T>.Create(AParent: TOTPersistent);
begin
  inherited Create(AParent);
  FOwnsObjects := True;
  FList := TList<TOID>.Create;
end;

destructor TOTPersistentList<T>.Destroy;
var
  oid: TOID;
begin
  for oid in FList do begin
    if FOwnsObjects then begin
      FromOID(oid).Free;
    end
    else begin
      // todo...
    end;
  end;
  FList.Free;
  inherited Destroy;
end;

function TOTPersistentList<T>.Add(AValue: T): Integer;
begin
  Result := FList.Add(AValue.oid);
end;

function TOTPersistentList<T>.Count: Integer;
begin
  Result := FList.Count;
end;

function TOTPersistentList<T>.GetItem(AIndex: Integer): T;
begin
  Result := T(FromOID(FList[AIndex]));
end;

procedure TOTPersistentList<T>.SetItem(AIndex: Integer; AValue: T);
begin

end;

procedure TOTPersistentList<T>.RestoreYamlAttribute(AName, AValue: String;
  AIndex: Integer; ALoader: TOTPersistentLoader);
begin
  if AName = 'list-items' then
    Add(T(FromOID(StrToInteger(AValue))))
  else
    inherited RestoreYamlAttribute(AName, AValue, AIndex, ALoader);
end;

procedure TOTPersistentList<T>.SaveYamlAttributes(AEmitter: TYamlEmitter);
var
  i: Integer;
begin
  inherited SaveYamlAttributes(AEmitter);

  SaveYamlSequence(AEmitter, 'list-items');
  for i := 0 to FList.Count - 1 do begin
    if FOwnsObjects then begin
      SaveYamlSequenceObject(AEmitter, FList[i]);
    end
    else begin
      SaveYamlSequenceObjectReference(AEmitter, FList[i]);
    end;
  end;
  SaveYamlEndSequence(AEmitter);
end;

{ TOTOwningList }

constructor TOTOwningList<T>.Create(AParent: TOTPersistent);
begin
  inherited Create(AParent);
  FOwnsObjects := True;
end;

{ TOTReferenceList }

constructor TOTReferenceList<T>.Create(AParent: TOTPersistent);
begin
  inherited Create(AParent);
  FOwnsObjects := False;
end;

end.
