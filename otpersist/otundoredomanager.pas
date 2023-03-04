unit OTUndoRedoManager;

{$mode Delphi}

interface

uses
  Classes,
  SysUtils,
  Generics.Collections,
  OTPersistent;

type

  { TUndoEntryItem }

  TUndoEntryItem = class
  private
    FItemOID: TOID;
    FClassRef: TOTPersistentClassRef;
    FStreamPos: SizeInt;

  public
    constructor Create(AItem: TOTPersistent; AStreamPos: SizeInt);
    destructor Destroy; override;

    property itemOid: TOID Read FItemOid;
    property classRef: TOTPersistentClassRef Read FClassRef;
    property streamPos: SizeInt read FstreamPos;
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

    procedure SaveModified(AObject: TOTPersistent);
    procedure Restore;
  end;

  { TOTUndoRedoManager }

  TOTUndoRedoManager = class
  private
    FUndo: TObjectStack<TUndoEntry>;
    FCurrent: TUndoEntry;

    function GetHasActiveMark: Boolean;
    function GetUndoCount: Integer;

  public
    constructor Create;
    destructor Destroy; override;

    procedure SetMark(AText: String);
    procedure SetModified(AObject: TOTPersistent);
    procedure Commit;

    procedure Undo;

    property hasActiveMark: Boolean Read GetHasActiveMark;
    property undoCount: Integer Read GetUndoCount;
  end;

var
  UndoRedoManager: TOTUndoRedoManager;

implementation

uses
  OTOIDManager;

{ TUndoEntryItem }

constructor TUndoEntryItem.Create(AItem: TOTPersistent; AStreamPos: SizeInt);
begin
  inherited Create;

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

procedure TUndoEntry.SaveModified(AObject: TOTPersistent);
var
  newItem: TUndoEntryItem;
begin
  newItem := TUndoEntryItem.Create(AObject, FStream.Position);
  AObject.SaveToStream(FStream);
  FItems.Add(newItem);
end;

procedure TUndoEntry.Restore;
var
  item: TUndoEntryItem;
  obj: TOTPersistent;
begin
  for item in FItems do begin
    FStream.Seek(item.streamPos, soFromBeginning);
    obj := OIDManager.FromOID(item.itemOid);
    obj.RestoreFromStream(FStream);
  end;
end;

{ TOTUndoRedoManager }

constructor TOTUndoRedoManager.Create;
begin
  inherited;

  FUndo := TObjectStack<TUndoEntry>.Create;
end;

destructor TOTUndoRedoManager.Destroy;
begin
  FUndo.Free;
  inherited Destroy;
end;

procedure TOTUndoRedoManager.SetMark(AText: String);
begin
  if Assigned(FCurrent) then
    raise Exception.Create('UndoRedoManger.SetMark with active mark');

  FCurrent := TUndoEntry.Create(AText);
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

procedure TOTUndoRedoManager.Undo;
var
  undo: TUndoEntry;
begin
  undo := FUndo.Peek;

  undo.Restore;

  FUndo.Pop; // and free it
end;

initialization
  UndoRedoManager := TOTUndoRedoManager.Create;

end.
