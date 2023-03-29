unit OTUndoRedoManagerTest;

{$mode Delphi}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testutils,
  testregistry;

type

  { TTestOTUndoRedoManager }

  TTestOTUndoRedoManager = class(TTestCase)
  public
    procedure Setup; override;
    procedure Teardown; override;

  published
    procedure TestSingleObjectEditUndoRedo;
    procedure TestSingleObjectEditUndoCalculatedUpdateRedo;

    procedure TestMultipleEditsAndUndoRedo;
    procedure TestEditReferenceAndUndoRedo;

    procedure TestObjectCreateAndUndoRedo;
    procedure TestObjectFreeAndUndoRedo;

    procedure TestObjectEditFreeAndUndoRedo;
    procedure TestOwningObjectFreeAndUndoRedo;

    procedure TestOwningListAddAndUndoRedo;
    procedure TestOwningListRemoveAndUndoRedo;
    procedure TestOwningListEditAndUndoRedo;

    procedure TestReferringListAddAndUndoRedo;
    procedure TestReferringListRemoveAndUndoRedo;
    procedure TestReferringListEditAndUndoRedo;
  end;

implementation

uses
  OTPersistent,
  OTUndoRedoManager,
  OTOIDManager,
  ContainerClass,
  OwningClass,
  ReferringClass,
  LeafClass;

{ TTestOTUndoRedoManager }

procedure TTestOTUndoRedoManager.Setup;
begin
  inherited Setup;
  // clear it out and start anew...
  OIDManager.Free;
  OIDManager := TOTOIDManager.Create;
  UndoRedoManager.Free;
  UndoRedoManager := TOTUndoRedoManager.Create;
end;

procedure TTestOTUndoRedoManager.Teardown;
begin
  inherited Teardown;
  // clear it out and start anew...
  OIDManager.Free;
  OIDManager := TOTOIDManager.Create;
  UndoRedoManager.Free;
  UndoRedoManager := TOTUndoRedoManager.Create;
end;

procedure TTestOTUndoRedoManager.TestSingleObjectEditUndoRedo;
var
  leaf: TLeafClass;
  undoCount: Integer;
begin
  //
  // Given a leaf object with initial values
  //
  // When the value of an attribute is edited
  // Then a new undo entry is created
  //  and the redoCount is 0
  //
  // When Undo is called
  // Then the undoCount decreases by 1
  //  and the leaf attribute has its original value restored
  //  and the redoCount is 1
  //
  // When Redo is called
  // Then the redoCount is 0
  //  and the undoCount increases by 1
  //  and the leaf attribute has its edited value.
  //

  leaf := TLeafClass.Create(nil);
  try
    leaf.int1 := 0;

    undoCount := UndoRedoManager.undoCount;

    leaf.int1 := 32;
    AssertEquals('undoCount after 1st edit', undoCount + 1, UndoRedoManager.undoCount);
    AssertEquals('redoCount after 1st edit', 0, UndoRedoManager.redoCount);

    UndoRedoManager.Undo;
    AssertEquals('undoCount after Undo', undoCount, UndoRedoManager.undoCount);
    AssertEQuals('redoCount after Undo', 1, UndoRedoManager.redoCount);
    AssertEquals('int1 after Undo', 0, leaf.int1);

    UndoRedoManager.Redo;
    AssertEquals('redoCount after Redo', 0, UndoRedoManager.redoCount);
    AssertEquals('undoCount after Redo', undoCount + 1, UndoRedoManager.undoCount);
    AssertEquals('int1 after Redo', 32, leaf.int1);
  finally
    leaf.Free;
  end;
end;

procedure TTestOTUndoRedoManager.TestSingleObjectEditUndoCalculatedUpdateRedo;
var
  owning: TOwningClass;
  referring: TReferringClass;
  leaf: TLeafClass;
  Value: Integer;
begin
  //
  // Given a leaf object with initial values
  //  and a referring object that references the leaf
  //  and an owning object that owns the leaf
  //
  // When the value of a leaf attribute is edited
  //  and the referring object is calculated
  //  and the owning object is calculated
  //  and Undo is called
  //
  // Then the leaf attribute has its original value restored
  //  and the leaf object, the owning object and the referring object are marked as not calculated
  //
  // When the referring object is calculated
  //  and the owning object is calculated
  //  and Redo is called
  //
  // Then the leaf attribute has its edited value restore
  //  and the leaf object, the owning object and the referring object are marked as not calculated
  //

  referring := nil;
  leaf := nil;
  owning := TOwningClass.Create(nil);
  try
    leaf := TLeafClass.Create(nil);

    owning.leaf := leaf;

    referring := TReferringClass.Create(nil);
    referring.leaf := leaf;


    leaf.int1 := 32;
    // force calculation
    Value := owning.leafSum + referring.leafSum;
    AssertTrue('initial', referring.isCalculated);

    UndoRedoManager.Undo;

    AssertFalse('after undo:owning', owning.isCalculated);
    AssertFalse('after undo:referring', referring.isCalculated);
    AssertFalse('after undo:leaf', leaf.isCalculated);

    // force calculation
    Value := owning.leafSum + referring.leafSum;

    UndoRedoManager.Undo;

    AssertFalse('after undo:owning', owning.isCalculated);
    AssertFalse('after undo:referring', referring.isCalculated);
    AssertFalse('after undo:leaf', leaf.isCalculated);

  finally
    referring.Free;
    owning.Free;
  end;
end;

procedure TTestOTUndoRedoManager.TestMultipleEditsAndUndoRedo;
var
  leaf: TLeafClass;
  undoCount: Integer;
begin
  //
  // Give a leaf object
  //
  // When SetMark is called
  // And multiple edits are made
  // And Commit is called
  //
  // Then a single undo entry has been created
  //
  // When Undo is called
  // Then all edits are undone
  //
  // When Redo is called
  // Then all edits are restored
  //
  leaf := TLeafClass.Create(nil);
  try
    undoCount := UndoRedoManager.undoCount;

    UndoRedoManager.SetMark('');
    leaf.int1 := 26;
    leaf.int2 := 77;
    leaf.AddArray1(1234.5);
    leaf.array2[0] := 'Tom';
    UndoRedoManager.Commit;

    AssertEquals(undoCount + 1, UndoRedoManager.undoCount);

    UndoRedoManager.Undo;

    AssertEquals('int1', 0, leaf.int1);
    AssertEquals('int2', 0, leaf.int2);
    AssertEquals('array1Count', 0, leaf.array1Count);
    AssertEquals('', leaf.array2[0]);

    UndoRedoManager.Redo;

    AssertEquals('int1', 26, leaf.int1);
    AssertEquals('int2', 77, leaf.int2);
    AssertEquals('array1Count', 1, leaf.array1Count);
    AssertEquals('array1', 1234.5, leaf.array1[0]);
    AssertEquals('Tom', leaf.array2[0]);

  finally
    leaf.Free;
  end;
end;

procedure TTestOTUndoRedoManager.TestEditReferenceAndUndoRedo;
var
  owning: TOwningClass;
  referring: TReferringClass;
  leaf: TLeafClass;
  undoCount: Integer;
begin
  //
  // Given a referring object and a leaf object
  //
  // When the referring object refers to the leaf
  // Then the leaf referencesCount is 1
  // And 1 new undo entry is added
  //
  // When Undo is called
  // Then the leaf referencesCount is 0
  //
  // When Redo is called
  // Then the leaf referencesCount is 1
  //
  referring := nil;
  leaf := nil;
  owning := TOwningClass.Create(nil);
  try
    leaf := TLeafClass.Create(nil);
    leaf.int1 := 32;

    owning.leaf := leaf;

    referring := TReferringClass.Create(nil);

    undoCount := UndoRedoManager.undoCount;
    referring.leaf := leaf;

    AssertEquals('undoCount', undoCount + 1, UndoRedoManager.undoCount);
    AssertEquals('leaf referencesCount', 1, leaf.referencesCount);

    UndoRedoManager.Undo;

    AssertEquals('undo leaf referencesCount', 0, leaf.referencesCount);

    UndoRedoManager.Redo;

    AssertEquals('redo leaf referencesCount', 1, leaf.referencesCount);
  finally
    referring.Free;
    owning.Free;
  end;
end;

procedure TTestOTUndoRedoManager.TestObjectCreateAndUndoRedo;
var
  leaf: TLeafClass;
  leafOid: TOID;
  undoCount: Integer;
  obj: TOTPersistent;
begin
  //
  // Given an initial undoCount
  //
  // When a leaf object is created
  // Then the undoCount increases by 1
  //
  // When Undo is called
  // Then the leaf object is Free'd
  //  and the undoCount decreases by 1
  //
  // When Redo is called
  // Then the leaf object is recreated
  //

  undoCount := UndoRedoManager.undoCount;

  leaf := TLeafClass.Create(nil);
  leafOid := leaf.oid;

  AssertEquals(undoCount + 1, UndoRedoManager.undoCount);

  UndoRedoManager.Undo;

  AssertEquals('undoCount after Undo', undoCount, UndoRedoManager.undoCount);

  obj := OIDManager.FromOID(leafOid);
  AssertNull('null', obj);

  UndoRedoManager.Redo;

  obj := OIDManager.FromOID(leafOid);
  AssertNotNull('after redo', obj);
  AssertTrue(obj is TLeafClass);

  obj.Free;
end;

procedure TTestOTUndoRedoManager.TestObjectFreeAndUndoRedo;
var
  leaf: TLeafClass;
  leafOid: TOID;
  undoCount: Integer;
  obj: TOTPersistent;
  newLeaf: TLeafClass;
begin
  // Given an existing leaf object
  // and an initial undo count
  //
  // When the object is Free'd
  // Then the undo count increases by 1
  //
  // When Undo is called
  // Then the undoCount decreases by 1
  //  and a new leaf object is created with the same oid
  //  and it has the same values as the Free'd object had
  //
  // When Redo is called
  // Then the leaf object is Free'd again
  //
  obj := nil;
  leaf := TLeafClass.Create(nil);
  try
    leafOid := leaf.oid;
    leaf.int1 := 666;
    leaf.int2 := 333;

    undoCount := UndoRedoManager.undoCount;

    FreeAndNil(leaf);
    AssertEquals(undoCount + 1, UndoRedoManager.undoCount);

    UndoRedoManager.Undo;

    AssertEquals(undoCount, UndoRedoManager.undoCount);

    obj := OIDManager.FromOID(leafOid);
    AssertNotNull('not null', obj);
    AssertTrue(obj is TLeafClass);
    newLeaf := obj as TLeafClass;
    AssertEquals('leafOid', leafOid, newLeaf.oid);
    AssertEquals('int1', 666, newLeaf.int1);
    AssertEquals('int2', 333, newLeaf.int2);

    UndoRedoManager.Redo;

    obj := OIDManager.FromOID(leafOid);
    AssertNull('null', obj);

  finally
    leaf.Free;
    obj.Free;
  end;
end;

procedure TTestOTUndoRedoManager.TestObjectEditFreeAndUndoRedo;
var
  leaf: TLeafClass;
  leafOid: TOID;
  undoCount: Integer;
  obj: TOTPersistent;
  newLeaf: TLeafClass;
begin
  // Given an existing leaf object
  // and an initial undo count
  //
  // When the object is Edited and then Free'd within one commit
  // Then the undo count increases by 1
  //
  // When Undo is called
  // Then a new leaf object is created with the same oid
  // and it has the same values as the Free'd object had
  //
  // When Redo is called
  // Then the leaf object is Free'd again
  //
  obj := nil;
  leaf := TLeafClass.Create(nil);
  try
    leafOid := leaf.oid;
    leaf.int1 := 666;
    leaf.int2 := 333;

    undoCount := UndoRedoManager.undoCount;

    UndoRedoManager.SetMark('');

    leaf.int1 := 53;

    FreeAndNil(leaf);
    UndoRedoManager.Commit;

    AssertEquals(undoCount + 1, UndoRedoManager.undoCount);

    UndoRedoManager.Undo;

    obj := OIDManager.FromOID(leafOid);
    AssertNotNull('not null', obj);
    AssertTrue(obj is TLeafClass);

    newLeaf := obj as TLeafClass;
    AssertEquals('int1', 666, newLeaf.int1);
    AssertEquals('int2', 333, newLeaf.int2);

    UndoRedoManager.Redo;

    obj := OIDManager.FromOID(leafOid);
    AssertNull(obj);

  finally
    leaf.Free;
    obj.Free;
  end;
end;

procedure TTestOTUndoRedoManager.TestOwningObjectFreeAndUndoRedo;
var
  owning: TOwningClass;
  leaf: TLeafClass;
  undoCount: Integer;
  owningOid: TOID;
  leafOid: TOID;
begin
  //
  // Given a leaf object with initial values
  // And an owning object that owns the leaf
  //
  // When the the owning object is Free'd
  // Then the leaf object is also Free'd
  //  and the undocount increases by 1
  //
  // When Undo is called
  // Then the owning and leaf objects are both restored
  //  and relationships reset correctly
  //  and the undocount decreases by 1
  //
  // When Redo is called
  // Then the owning and leaf objects are Free'd again
  //

  leaf := nil;
  owning := TOwningClass.Create(nil);
  try
    owningOid := owning.oid;

    leaf := TLeafClass.Create(nil);
    leafOid := leaf.oid;

    owning.leaf := leaf;
    AssertSame('leaf parent is owning', owning, leaf.parent);

    leaf.int1 := 32;

    undoCount := UndoRedoManager.undoCount;

    FreeAndNil(owning);

    AssertEquals('undoCount after Free', undoCount + 1, UndoRedoManager.undoCount);
    AssertNull('leaf after Free', OIDManager.FromOID(leafOid));
    AssertNull('owning after Free', OIDManager.FromOID(owningOid));

    UndoRedoManager.Undo;

    AssertEquals('undoCount after Undo', undoCount, UndoRedoManager.undoCount);
    AssertEQuals('redoCount after Undo', 1, UndoRedoManager.redoCount);

    owning := TOwningClass(OIDManager.FromOID(owningOid));
    AssertEquals('owningOid', owningOid, owning.oid);
    AssertNotNull('owning', owning);
    leaf := owning.leaf;
    AssertNotNull('leaf', leaf);
    AssertEquals('leafOid', leafOid, leaf.oid);
    AssertSame('leaf parent is still owning', owning, leaf.parent);

    UndoRedoManager.Redo;

    owning := TOwningClass(OIDManager.FromOID(owningOid));
    AssertNull('owning after Redo', owning);
    AssertNull('leaf after Redo', OIDManager.FromOID(leafOid));

  finally
    owning.Free;
  end;
end;

procedure TTestOTUndoRedoManager.TestOwningListAddAndUndoRedo;
var
  container: TContainerClass;
  leaf: TLeafClass;
  leafOid: TOID;
begin
  //
  // Given a container object
  //
  // When a new leaf is added to the list
  //  and Undo is called
  // Then leaf is removed from the list and Free'd
  //
  // When Redo is called
  // Then the leaf is restored
  //  and is in the list
  //
  leaf := nil;
  container := TContainerClass.Create(nil);
  try
    UndoRedoManager.SetMark('');
    leaf := TLeafClass.Create(nil);
    leafOid := leaf.oid;
    container.leafs.Add(leaf);
    UndoRedoManager.Commit;

    UndoRedoManager.Undo;

    leaf := TLeafClass(OIDManager.FromOID(leafOid));
    AssertNull(leaf);
    AssertEquals('leafs.Count after Undo', 0, container.leafs.Count);

    UndoRedoManager.Redo;

    AssertEquals('leaf.Counts after Redo', 1, container.leafs.Count);
    leaf := container.leafs[0];
    AssertNotNull('leaf', leaf);
    AssertEquals('leafOid', leafOid, leaf.oid);

  finally
    container.Free;
  end;
end;

procedure TTestOTUndoRedoManager.TestOwningListRemoveAndUndoRedo;
var
  container: TContainerClass;
  undoCount: Integer;
  leaf: TLeafClass;
  leafOid: TOID;
begin
  //
  // Given a container object
  //
  // When a new leaf is added to the list
  //  and Undo is called
  // Then leaf is removed from the list and Free'd
  //
  // When Redo is called
  // Then the leaf is restored
  //  and is in the list
  //
  leaf := nil;
  container := TContainerClass.Create(nil);
  try
    leaf := TLeafClass.Create(nil);
    leafOid := leaf.oid;
    container.leafs.Add(leaf);

    undoCount := UndoRedoManager.undoCount;

    container.leafs.Remove(leaf);

    AssertEquals('undoCount+1', undoCount+1, UndoRedoManager.undoCount);

    UndoRedoManager.Undo;

    AssertEquals('undoCount', undoCount, UndoRedoManager.undoCount);
    AssertEquals('redoCount', 1, UndoRedoManager.redoCount);

    AssertEquals('leaf.Counts after Undo', 1, container.leafs.Count);
    leaf := container.leafs[0];
    AssertNotNull('leaf', leaf);
    AssertEquals('leafOid', leafOid, leaf.oid);

    UndoRedoManager.Redo;

    AssertEquals('undoCount after redo', undoCount+1, UndoRedoManager.undoCount);
    AssertEquals('redoCount after redo', 0, UndoRedoManager.redoCount);

    leaf := TLeafClass(OIDManager.FromOID(leafOid));
    AssertNull(leaf);
    AssertEquals('leafs.Count after Redo', 0, container.leafs.Count);

  finally
    container.Free;
  end;
end;

procedure TTestOTUndoRedoManager.TestOwningListEditAndUndoRedo;
var
  container: TContainerClass;
  undoCount: Integer;
  leaf1: TLeafClass;
  leafOid1: TOID;
  leaf2: TLeafClass;
begin
  //
  // Given a container object that contains a leaf object
  //
  // When the existing leaf is replaced with a new leaf
  //  and Undo is called
  // Then the original leaf is restored
  //  and the new leaf parent is nil
  //
  // When Redo is called
  // Then the new leaf parent is the list
  //  and the original leaf is Free'd
  //
  leaf1 := nil;
  leaf2 := nil;
  container := TContainerClass.Create(nil);
  try
    leaf1 := TLeafClass.Create(nil);
    leafOid1 := leaf1.oid;

    container.leafs.Add(leaf1);

    leaf2 := TLeafClass.Create(nil);

    // When
    container.leafs[0] := leaf2;
    UndoRedoManager.Undo;

    // Then
    AssertEquals('leafOid1 after Undo', leafOid1, container.leafs[0].oid);
    AssertNull('leaf2 parent after Undo', leaf2.parent);

    // When
    UndoRedoManager.Redo;

    // Then
    AssertNull('leaf1 after Redo', OIDManager.FromOID(leafOid1));
    AssertSame('leaf2 parent after Redo', container.leafs, leaf2.parent);


  finally
    container.Free;
  end;
end;

procedure TTestOTUndoRedoManager.TestReferringListAddAndUndoRedo;
var
  list: TLeafClassReferenceList;
  leaf: TLeafClass;
  leafOid: TOID;
begin
  //
  // Given a referring list
  //  and a leaf
  //  and the leaf is added to the list
  //
  // When Undo is called
  // Then leaf is removed from the list
  //  and the leaf reference count becomes zero
  //
  // When Redo is called
  // Then the leaf is put back in the list
  //  and the reference count is one
  //
  leaf := nil;
  list := TLeafClassReferenceList.Create(nil);
  try
    leaf := TLeafClass.Create(nil);
    leafOid := leaf.oid;
    list.Add(leaf);

    // When
    UndoRedoManager.Undo;

    // Then
    AssertEquals('list.Count after Undo', 0, list.Count);
    AssertEquals('leaf ref count after Undo', 0, leaf.referencesCount);

    // When
    UndoRedoManager.Redo;

    // Then
    AssertEquals('list.Counts after Redo', 1, list.Count);
    AssertSame('list[0]', leaf, list[0]);
    AssertEquals('leaf ref count after Redo', 1, leaf.referencesCount);

  finally
    list.Free;
    leaf.Free;
  end;
end;

procedure TTestOTUndoRedoManager.TestReferringListRemoveAndUndoRedo;
var
  list: TLeafClassReferenceList;
  undoCount: Integer;
  leaf: TLeafClass;
  leaf1: TLeafClass;
  leafOid: TOID;
begin
  //
  // Given a referring list
  //   and a leaf object
  //   and the leaf has been added to the list
  //
  // When the leaf is removed from the list
  //  and Undo is called
  // Then the leaf is restored to the list
  //  and the leaf reference count is 1
  //
  // When Redo is called
  // Then leaf is removed from the list
  //  and the leaf reference count is 0
  //
  leaf := nil;
  leaf1 := nil;
  list := TLeafClassReferenceList.Create(nil);
  try
    leaf := TLeafClass.Create(nil);
    leafOid := leaf.oid;
    list.Add(leaf);

    undoCount := UndoRedoManager.undoCount;

    list.Remove(leaf);

    AssertEquals('undoCount+1', undoCount+1, UndoRedoManager.undoCount);

    UndoRedoManager.Undo;

    AssertEquals('undoCount', undoCount, UndoRedoManager.undoCount);
    AssertEquals('redoCount', 1, UndoRedoManager.redoCount);

    AssertEquals('list.Count after Undo', 1, list.Count);
    leaf1 := list[0];
    AssertNotNull('leaf', leaf);
    AssertEquals('leafOid', leafOid, leaf1.oid);
    AssertEquals('leaf ref count after Undo', 1, leaf.referencesCount);

    UndoRedoManager.Redo;

    AssertEquals('undoCount after redo', undoCount+1, UndoRedoManager.undoCount);
    AssertEquals('redoCount after redo', 0, UndoRedoManager.redoCount);

    AssertEquals('list.Count after Redo', 0, list.Count);
    AssertEquals('leaf ref count after Redo', 0, leaf.referencesCount);

  finally
    list.Free;
    leaf.Free;
  end;
end;

procedure TTestOTUndoRedoManager.TestReferringListEditAndUndoRedo;
var
  list: TLeafClassReferenceList;
  undoCount: Integer;
  leaf1: TLeafClass;
  leafOid1: TOID;
  leaf2: TLeafClass;
begin
  //
  // Given a reference list object that contains a leaf object
  //
  // When the existing leaf is replaced with a new leaf
  //  and Undo is called
  // Then the original leaf is put back in the list
  //  and the original leaf reference count is 1
  //  and the new leaf reference is 0
  //
  // When Redo is called
  // Then the new leaf is in the list
  //  and the original leaf reference count is 0
  //  and the new leaf reference count is 1
  //
  leaf1 := nil;
  leaf2 := nil;
  list := TLeafClassReferenceList.Create(nil);
  try
    leaf1 := TLeafClass.Create(nil);
    leafOid1 := leaf1.oid;

    list.Add(leaf1);

    leaf2 := TLeafClass.Create(nil);

    // When
    list[0] := leaf2;
    UndoRedoManager.Undo;

    // Then
    AssertEquals('list[0] after Undo', leafOid1, list[0].oid);
    AssertEquals('leaf1 ref count after Undo', 1, leaf1.referencesCount);
    AssertEquals('leaf2 ref count after Undo', 0, leaf2.referencesCount);

    // When
    UndoRedoManager.Redo;

    // Then
    AssertSame('list[0] after Redo', leaf2, list[0]);
    AssertEquals('leaf1 ref count after Redo', 0, leaf1.referencesCount);
    AssertEquals('leaf2 ref count after Redo', 1, leaf2.referencesCount);

  finally
    list.Free;
    leaf1.Free;
    leaf2.Free;
  end;
end;

initialization
  RegisterTest(TTestOTUndoRedoManager);

end.
