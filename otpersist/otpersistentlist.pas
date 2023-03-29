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

  { TOIDList }

  TOIDList = class(TList<TOID>)
  public
    procedure SaveList(AStream: TStream);
    procedure RestoreList(AStream: TStream);
  end;

  { TOTPersistentList }

  TOTPersistentList<T: TOTPersistent> = class(TOTPersistent)
  private
    FList: TOIDList;
    FOwnsObjects: Boolean;

  protected
    function GetItem(AIndex: Integer): T;
    procedure SetItem(AIndex: Integer; AValue: T);
    procedure RestoreAttributes(AStream: TStream); override;
    procedure SaveAttributes(AStream: TStream); override;

  public
    constructor Create(AParent: TOTPersistent; AOID: TOID); override;
    destructor Destroy; override;

    procedure RestoreYamlAttribute(AName, AValue: String; AIndex: Integer; ALoader: TOTPersistentLoader); override;
    procedure SaveYamlAttributes(AEmitter: TYamlEmitter); override;

    function Add(AValue: T): Integer;
    function Count: Integer;
    procedure Remove(AValue: T);
    procedure Extract(AValue: T);

    property Items[AIndex: Integer]: T Read GetItem Write SetItem; default;
  end;

  { TOTOwningList }

  TOTOwningList<T: TOTPersistent> = class(TOTPersistentList<T>)
  public
    constructor Create(AParent: TOTPersistent; AOID: TOID = 0); override;
  end;

  { TOTReferenceList }

  TOTReferenceList<T: TOTPersistent> = class(TOTPersistentList<T>)
  public
    constructor Create(AParent: TOTPersistent; AOID: TOID = 0); override;
  end;

implementation

uses
  OTUndoRedoManager;

{ TOIDList }

procedure TOIDList.RestoreList(AStream: TStream);
var
  newLength: SizeInt;
begin
  AStream.Read(newLength, sizeof(newLength));
  SetCount(newLength);
  AStream.Read(FItems[0], newLength * sizeof(TOID));
end;

procedure TOIDList.SaveList(AStream: TStream);
begin
  AStream.Write(FLength, sizeof(FLength));
  AStream.Write(FItems[0], FLength * sizeof(TOID));
end;

{ TOTPersistentList }

constructor TOTPersistentList<T>.Create(AParent: TOTPersistent; AOID: TOID);
begin
  inherited Create(AParent, AOID);
  FOwnsObjects := True;
  FList := TOIDList.Create;
end;

destructor TOTPersistentList<T>.Destroy;
var
  valueOid: TOID;
  obj: TOTPersistent;
begin
  for valueOid in FList do begin
    if FOwnsObjects then begin
      FromOID(valueOid).Free;
    end
    else begin
      obj := FromOID(valueOid);
      if Assigned(obj) then begin
        obj.DeleteReference(oid);
      end;
    end;
  end;
  FList.Free;
  inherited Destroy;
end;

function TOTPersistentList<T>.Add(AValue: T): Integer;
var
  valueOid: TOID;
  hasActiveMark: Boolean;
begin
  hasActiveMark := UndoRedoManager.hasActiveMark;
  if not hasActiveMark then
    UndoRedoManager.SetMark('');

  SetModified;

  valueOid := AValue.oid;
  Result := FList.Add(valueOid);
  if FOwnsObjects then begin
    AValue.SetParent(self);
  end
  else begin
    AValue.AddReference(oid);
  end;

  if not hasActiveMark then
    UndoRedoManager.Commit;
end;

function TOTPersistentList<T>.Count: Integer;
begin
  Result := FList.Count;
end;

procedure TOTPersistentList<T>.Remove(AValue: T);
var
  valueOid: TOID;
  hasActiveMark: Boolean;
begin
  hasActiveMark := UndoRedoManager.hasActiveMark;
  if not hasActiveMark then
    UndoRedoManager.SetMark('');

  SetModified;

  valueOid := AValue.oid;
  FList.Remove(valueOid);

  if FOwnsObjects then begin
    AValue.Free;
  end
  else begin
    AValue.DeleteReference(oid);
  end;

  if not hasActiveMark then
    UndoRedoManager.Commit;
end;

procedure TOTPersistentList<T>.Extract(AValue: T);
var
  valueOid: TOID;
  hasActiveMark: Boolean;
begin
  hasActiveMark := UndoRedoManager.hasActiveMark;
  if not hasActiveMark then begin
    UndoRedoManager.SetMark('');
  end;

  SetModified;

  valueOid := AValue.oid;
  FList.Extract(valueOid);

  if FOwnsObjects then begin
    AValue.SetParent(nil);
  end
  else begin
    AValue.DeleteReference(oid);
  end;

  if not hasActiveMark then begin
    UndoRedoManager.Commit;
  end;
end;

function TOTPersistentList<T>.GetItem(AIndex: Integer): T;
begin
  Result := T(FromOID(FList[AIndex]));
end;

procedure TOTPersistentList<T>.SetItem(AIndex: Integer; AValue: T);
var
  oldObj: TOTPersistent;
  hasActiveMark: Boolean;
begin
  if (FList[AIndex] = 0) and (not Assigned(AValue)) then
    Exit; // no change

  if Assigned(AValue) and (AValue.OID = FList[AIndex]) then
    Exit; // no change

  hasActiveMark := UndoRedoManager.hasActiveMark;
  if not hasActiveMark then begin
    UndoRedoManager.SetMark('');
  end;

  SetModified;

  oldObj := FromOid(FList[AIndex]);
  if Assigned(oldObj) then begin
    if FOwnsObjects then begin
      oldObj.Free;
    end
    else begin
      oldObj.DeleteReference(self.oid);
    end;
  end;

  if Assigned(AValue) then begin
    FList[AIndex] := AValue.oid;

    if FOwnsObjects then begin
      AValue.SetParent(self);
    end
    else begin
      AValue.AddReference(self.oid);
    end;
  end
  else begin
    FList[AIndex] := 0;
  end;

  if not hasActiveMark then begin
    UndoRedoManager.Commit;
  end;
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

procedure TOTPersistentList<T>.RestoreAttributes(AStream: TStream);
begin
 inherited RestoreAttributes(AStream);

 FList.RestoreList(AStream);
end;

procedure TOTPersistentList<T>.SaveAttributes(AStream: TStream);
begin
  inherited SaveAttributes(AStream);

  FList.SaveList(AStream);
end;

{ TOTOwningList }

constructor TOTOwningList<T>.Create(AParent: TOTPersistent; AOID: TOID);
begin
  inherited Create(AParent, AOID);
  FOwnsObjects := True;
end;

{ TOTReferenceList }

constructor TOTReferenceList<T>.Create(AParent: TOTPersistent; AOID: TOID);
begin
  inherited Create(AParent, AOID);
  FOwnsObjects := False;
end;

end.
