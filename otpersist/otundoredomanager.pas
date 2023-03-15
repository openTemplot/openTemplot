unit OTUndoRedoManager;

{$mode Delphi}

interface

uses
  Classes,
  SysUtils,
  Generics.Collections,
  OTPersistent;

type

  EUndoOperation = (
    opCreate,
    opEdit,
    opDestroy
    );

  { TUndoEntryItem }

  TUndoEntryItem = class
  private
    FOperation: EUndoOperation;
    FItemOID: TOID;
    FClassRef: TOTPersistentClassRef;
    FStreamPos: SizeInt;

  public
    constructor Create(AOperation: EUndoOperation; AItem: TOTPersistent; AStreamPos: SizeInt);
    destructor Destroy; override;

    property operation: EUndoOperation Read FOperation;
    property itemOid: TOID Read FItemOid;
    property classRef: TOTPersistentClassRef Read FClassRef;
    property streamPos: SizeInt Read FstreamPos;
  end;

  { TUndoEntry }

  TUndoEntry = class
  private
    FText: String;
    FStream: TMemoryStream;
    FItems: TObjectList<TUndoEntryItem>;

  public
    constructor Create(AText: String);
    destructor Destroy; override;

    procedure SaveCreate(AObject: TOTPersistent);
    procedure SaveModified(AObject: TOTPersistent);
    procedure SaveDestroy(AObject: TOTPersistent);
    procedure Restore;
  end;

  { TOTUndoRedoManager }

  TOTUndoRedoManager = class
  private
    FUndo: TObjectStack<TUndoEntry>;
    FRedo: TObjectStack<TUndoEntry>;

    FCurrent: TUndoEntry;

    function GetHasActiveMark: Boolean;
    function GetUndoCount: Integer;
    function GetRedoCount: Integer;

  public
    constructor Create;
    destructor Destroy; override;

    procedure SetMark(AText: String);
    procedure SetModified(AObject: TOTPersistent);
    procedure SetCreated(AObject: TOTPersistent);
    procedure SetDestroyed(AObject: TOTPersistent);
    procedure Commit;

    procedure Undo;
    procedure Redo;

    property hasActiveMark: Boolean Read GetHasActiveMark;
    property undoCount: Integer Read GetUndoCount;
    property redoCount: Integer Read GetRedoCount;
  end;

var
  UndoRedoManager: TOTUndoRedoManager;

implementation

uses
  OTOIDManager;

{ TUndoEntryItem }

constructor TUndoEntryItem.Create(AOperation: EUndoOperation; AItem: TOTPersistent;
  AStreamPos: SizeInt);
begin
  inherited Create;

  FOperation := AOperation;
  FItemOID := AItem.oid;
  FClassRef := TOTPersistentClassRef(AItem.ClassType);
  FStreamPos := AStreamPos;
end;

destructor TUndoEntryItem.Destroy;
begin
  inherited Destroy;
end;

{ TUndoEntry }

constructor TUndoEntry.Create(AText: String);
begin
  FText := AText;
  FStream := TMemoryStream.Create;
  FItems := TObjectList<TUndoEntryItem>.Create;
end;

destructor TUndoEntry.Destroy;
begin
  FStream.Free;
  FItems.Free;

  inherited Destroy;
end;

procedure TUndoEntry.SaveCreate(AObject: TOTPersistent);
begin
  FItems.Add(TUndoEntryItem.Create(opCreate, AObject, FStream.Position));
end;

procedure TUndoEntry.SaveModified(AObject: TOTPersistent);
var
  newItem: TUndoEntryItem;
  item: TUndoEntryItem;
begin
  for item in FItems do begin
    if item.itemOid = AObject.oid then begin
      // item is already saved, so don't save it again
      Exit;
    end;
  end;
  newItem := TUndoEntryItem.Create(opEdit, AObject, FStream.Position);
  AObject.SaveToStream(FStream);
  FItems.Add(newItem);
end;

procedure TUndoEntry.SaveDestroy(AObject: TOTPersistent);
var
  newItem: TUndoEntryItem;
  item: TUndoEntryItem;
begin
  for item in FItems do begin
    if item.itemOid = AObject.oid then begin
      // item is already saved, so don't save it again,
      // but do mark it as destroyed
      if item.operation = opEdit then begin
        item.FOperation := opDestroy;
      end
      else if item.operation = opCreate then begin
        // item has been created AND destroyed in the same Undo transaction
        // so let's just delete this entry
        FItems.Remove(item);
      end;
      Exit;
    end;
  end;
  newItem := TUndoEntryItem.Create(opDestroy, AObject, FStream.Position);
  AObject.SaveToStream(FStream);
  FItems.Add(newItem);
end;

procedure TUndoEntry.Restore;
var
  item: TUndoEntryItem;
  obj: TOTPersistent;
begin
  for item in FItems do begin
    case item.operation of
      opCreate: begin
        obj := OIDManager.FromOID(item.itemOid);
        obj.Free;
      end;
      opEdit: begin
        obj := OIDManager.FromOID(item.itemOid);
        UndoRedoManager.SetModified(obj);
        FStream.Seek(item.streamPos, soFromBeginning);
        obj.RestoreFromStream(FStream);
      end;
      opDestroy: begin
        // construct a new object using "private" constructor that
        // doesn't rebuild everything or assign a new oid,
        // and then restore from stream and save to OIDManager
        obj := item.classRef.Create(nil, item.itemOid);
        FStream.Seek(item.streamPos, soFromBeginning);
        obj.RestoreFromStream(FStream);
      end;
    end;
  end;
end;

{ TOTUndoRedoManager }

constructor TOTUndoRedoManager.Create;
begin
  inherited;

  FUndo := TObjectStack<TUndoEntry>.Create;
  FRedo := TObjectStack<TUndoEntry>.Create;
end;

destructor TOTUndoRedoManager.Destroy;
begin
  FRedo.Free;
  FUndo.Free;
  inherited Destroy;
end;

procedure TOTUndoRedoManager.SetMark(AText: String);
begin
  if Assigned(FCurrent) then
    raise Exception.Create('UndoRedoManger.SetMark with active mark');

  FCurrent := TUndoEntry.Create(AText);
end;

procedure TOTUndoRedoManager.SetCreated(AObject: TOTPersistent);
var
  hasActiveMark: Boolean;
begin
  hasActiveMark := Assigned(FCurrent);
  if not hasActiveMark then
    SetMark('');

  FCurrent.SaveCreate(AObject);

  if not hasActiveMark then
    Commit;
end;

procedure TOTUndoRedoManager.SetModified(AObject: TOTPersistent);
var
  hasActiveMark: Boolean;
begin
  hasActiveMark := Assigned(FCurrent);
  if not hasActiveMark then
    SetMark('');

  FCurrent.SaveModified(AObject);

  if not hasActiveMark then
    Commit;
end;

procedure TOTUndoRedoManager.SetDestroyed(AObject: TOTPersistent);
var
  hasActiveMark: Boolean;
begin
  hasActiveMark := Assigned(FCurrent);
  if not hasActiveMark then
    SetMark('');

  FCurrent.SaveDestroy(AObject);

  if not hasActiveMark then
    Commit;
end;

procedure TOTUndoRedoManager.Commit;
begin
  if not Assigned(FCurrent) then
    raise Exception.Create('UndoRedoManager.Commit with no active mark');

  FUndo.Push(FCurrent);
  FCurrent := nil;
end;

function TOTUndoRedoManager.GetHasActiveMark: Boolean;
begin
  Result := Assigned(FCurrent);
end;

function TOTUndoRedoManager.GetUndoCount: Integer;
begin
  Result := FUndo.Count;
end;

function TOTUndoRedoManager.GetRedoCount: Integer;
begin
  Result := FRedo.Count;
end;

procedure TOTUndoRedoManager.Undo;
var
  undo: TUndoEntry;
begin
  if Assigned(FCurrent) then
    raise Exception.Create('UndoRedoManager.Undo with active mark');

  if (FUndo.Count < 1) then
    raise Exception.Create('UndoRedoManager.Undo nothing to Undo');

  undo := FUndo.Peek;

  FCurrent := TUndoEntry.Create('');

  undo.Restore;

  FRedo.Push(FCurrent);
  FCurrent := nil;

  FUndo.Pop; // and free it
end;

procedure TOTUndoRedoManager.Redo;
var
  redo: TUndoEntry;
begin
  if Assigned(FCurrent) then
    raise Exception.Create('UndoRedoManager.Redo with active mark');

  if (FRedo.Count < 1) then
    raise Exception.Create('UndoRedoManager.Redo nothing to Redo');

  redo := FRedo.Peek;

  FCurrent := TUndoEntry.Create('');

  redo.Restore;

  FUndo.Push(FCurrent);
  FCurrent := nil;

  FRedo.Pop; // and free it
end;

initialization
  UndoRedoManager := TOTUndoRedoManager.Create;

finalization
  UndoRedoManager.Free;
end.
