unit OTPersistentListTest;

{$mode Delphi}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testutils,
  testregistry;

type

  { TTestOTPersistentList }

  TTestOTPersistentList = class(TTestCase)
  protected
    procedure Setup; override;
    procedure Teardown; override;

  published
    procedure TestOwningBasicFunctions;
    procedure TestOwningRemove;
    procedure TestOwningExtract;
    procedure TestOwningEdit;
    procedure TestContainerSaveRestoreYaml;

    procedure TestReferringBasicFunctions;
    procedure TestReferringRemove;
    procedure TestReferringExtract;
    procedure TestReferringEdit;
  end;

implementation

uses
  OTPersistent,
  OTPersistentList,
  OTOIDManager,
  OTUndoRedoManager,
  ContainerClass,
  LeafClass,
  OTYaml,
  OTYamlEmitter;

{ TTestOTPersistentList }

procedure TTestOTPersistentList.Setup;
begin
  inherited Setup;

  // clear it out and start anew...
  OIDManager.Free;
  OIDManager := TOTOIDManager.Create;
  UndoRedoManager.Free;
  UndoRedoManager := TOTUndoRedoManager.Create;
end;

procedure TTestOTPersistentList.Teardown;
begin
  // clear it out and start anew...
  OIDManager.Free;
  OIDManager := TOTOIDManager.Create;
  UndoRedoManager.Free;
  UndoRedoManager := TOTUndoRedoManager.Create;

  inherited Teardown;
end;

procedure TTestOTPersistentList.TestOwningBasicFunctions;
var
  list: TLeafClassOwningList;
  leaf: TLeafClass;
  index: Integer;
  oid1: TOID;
  oid2: TOID;
begin
  //
  // Given a new list
  // When constructed
  // Then initial Count is 0
  //
  // When an object is added
  // Then the index of the object is returned
  //  and the Count is 1
  //  and the first item in the list is the added object
  //  and the parent of added object is the list
  //
  // When another object is added
  // Then the index of the object is returned
  //  and the Count is 2
  //  and the second item in the list is the added object
  //  and the parent of the added object is the list
  //
  // When the list is Freed
  // Then the contents are Freed as well
  //

  list := TLeafClassOwningList.Create(nil);
  try
    AssertEquals('initial Count', 0, list.Count);

    leaf := TLeafClass.Create(nil);
    oid1 := leaf.oid;
    index := list.Add(leaf);

    AssertEquals('1 Count', 1, list.Count);
    AssertEquals('index 1', 0, index);
    AssertSame('item 1', leaf, list[index]);
    AssertSame('item1 parent', list, leaf.parent);

    leaf := TLeafClass.Create(nil);
    oid2 := leaf.oid;
    index := list.Add(leaf);

    AssertEquals('2 Count', 2, list.Count);
    AssertEquals('index 2', 1, index);
    AssertSame('item 2', leaf, list[index]);
    AssertSame('item2 parent', list, leaf.parent);

    FreeAndNil(list);

    AssertNull('oid1 null', OIDManager.FromOID(oid1));
    AssertNull('oid2 null', OIDManager.FromOID(oid2));

  finally
    list.Free;
  end;
end;

procedure TTestOTPersistentList.TestOwningRemove;
var
  list: TLeafClassOwningList;
  leaf1: TLeafClass;
  leaf2: TLeafClass;
  index: Integer;
  oid1: TOID;
  oid2: TOID;
begin
  //
  // Given a new list containing several leaf objects
  //
  // When an item is removed
  // Then the list count decreases by 1
  //  and the item is Free'd
  //

  list := TLeafClassOwningList.Create(nil);
  try
    leaf1 := TLeafClass.Create(nil);
    oid1 := leaf1.oid;
    index := list.Add(leaf1);

    leaf2 := TLeafClass.Create(nil);
    oid2 := leaf2.oid;
    index := list.Add(leaf2);

    list.Remove(leaf1);

    AssertEquals('list.Count', 1, list.Count);

    leaf1 := TLeafClass(OIDManager.FromOID(oid1));
    AssertNull('leaf1 null', leaf1);

    leaf2 := list[0];
    AssertEquals('oid2', oid2, leaf2.oid);

  finally
    list.Free;
  end;
end;

procedure TTestOTPersistentList.TestOwningExtract;
var
  list: TLeafClassOwningList;
  leaf: TLeafClass;
  leaf1: TLeafClass;
  leaf2: TLeafClass;
  index: Integer;
  oid1: TOID;
  oid2: TOID;
begin
  //
  // Given a list containing leaf objects
  //
  // When an object is extracted
  // Then the owner of the extracted object is nil
  //
  // When the list if Free'd
  // Then the leaf is not Free'd
  //

  leaf := nil;
  list := TLeafClassOwningList.Create(nil);
  try
    leaf1 := TLeafClass.Create(nil);
    oid1 := leaf1.oid;
    index := list.Add(leaf1);

    leaf2 := TLeafClass.Create(nil);
    oid2 := leaf2.oid;
    index := list.Add(leaf2);

    list.Extract(leaf1);

    AssertEquals('list.Count', 1, list.Count);

    leaf1 := TLeafClass(OIDManager.FromOID(oid1));
    AssertNotNull('leaf1', leaf1);
    AssertNull('leaf1 parent', leaf1.parent);

    FreeAndNil(list);

    leaf := TLeafClass(OIDManager.FromOID(oid1));
    AssertNotNull('leaf', leaf);
    leaf2 := TLeafClass(OIDManager.FromOID(oid2));
    AssertNull('leaf2', leaf2);


  finally
    list.Free;
    leaf.Free;
  end;
end;

procedure TTestOTPersistentList.TestOwningEdit;
var
  leaf1: TLeafClass;
  leaf2: TLeafClass;
  list: TLeafClassOwningList;
  oid1: TOID;
  oid2: TOID;
begin
  //
  // Given an owning list with a leaf object
  //
  // When the leaf is replaced with another leaf
  // Then the original leaf is Free'd
  //  and the list contains the new leaf
  //  and the parent of the new leaf is the containing list
  //

  leaf1 := nil;
  leaf2 := nil;
  list := TLeafClassOwningList.Create(nil);
  try
    leaf1 := TLeafClass.Create(nil);
    oid1 := leaf1.oid;

    list.Add(leaf1);
    leaf1 := nil;

    leaf2 := TLeafClass.Create(nil);
    oid2 := leaf2.oid;

    list[0] := leaf2;

    AssertNull('leaf1 Free', OIDManager.FromOid(oid1));
    AssertSame('leaf2 in list', list[0], leaf2);
    AssertSame('leaf2 parent', list, leaf2.parent);

  finally
    list.Free;
    leaf1.Free;
  end;
end;

procedure TTestOTPersistentList.TestContainerSaveRestoreYaml;
var
  container: TContainerClass;
  newContainer: TContainerClass;
  leaf: TLeafClass;
  newLeaf: TLeafClass;
  i: Integer;
  j: Integer;
  stream: TStringStream;
  yamlVer: TYamlVersionDirective;
  yamlText: String;
  emitter: TYamlEmitter;
  obj: TOTPersistent;
begin
  //
  // Given an instance of TContainerClass
  // And several instances of TLeafClass
  // When the container is saved to Yaml
  // Then a stream of Yaml text is created
  //
  // When the stream of Yaml text is restored
  // Then a new container object is created
  // And the container contains the expected number of TLeafClass instances
  // And each TLeafClass instance is equal to the original instance
  //

  container := TContainerClass.Create(nil);
  try
    for j := 1 to 3 do begin
      leaf := TLeafClass.Create(nil);
      try
        leaf.int1 := 234 + j * 3;
        leaf.int2 := 56789 - j * 3;
        leaf.str1 := 'Happy Birthday!';
        for i := 0 to 20 do begin
          leaf.AddArray1(23.1 + i * i + j * j * j);
        end;
        for i := 0 to 4 do begin
          leaf.array2[i] := 'Fred ' + IntToStr(j) + IntToStr(i);
        end;

        container.leafs.Add(leaf);
        leaf := nil;
      except
        leaf.Free;
      end;
    end;

    // When save...
    stream := nil;
    emitter := TYamlEmitter.Create;
    try
      stream := TStringStream.Create;
      emitter.SetOutput(stream);
      yamlVer.Initialize;

      emitter.StreamStartEvent;
      emitter.DocumentStartEvent(yamlVer, nil, True);

      container.SaveToYaml(emitter);

      emitter.DocumentEndEvent(True);
      emitter.StreamEndEvent;

      yamlText := stream.DataString;
    finally
      emitter.Free;
    end;

    // Then
    AssertTrue('yamlText', yamlText <> '');

    // When restore...
    stream := TStringStream.Create(yamlText);
    obj := TOTPersistent.RestoreYamlFromStream(stream);
    try
      AssertTrue(obj is TContainerClass);
      newContainer := obj as TContainerClass;

      AssertEquals('count', container.leafs.Count, newContainer.leafs.Count);

      for j := 0 to container.leafs.Count - 1 do begin
        leaf := container.leafs[j];
        newLeaf := newContainer.leafs[j];

        AssertEquals('int1', leaf.int1, newLeaf.int1);
        AssertEquals('int2', leaf.int2, newLeaf.int2);
        AssertEquals('str1', leaf.str1, newLeaf.str1);
        AssertEquals('array1Count', leaf.array1Count, newLeaf.array1Count);

        for i := 0 to newLeaf.array1Count - 1 do
          AssertEquals('array1#' + IntToStr(i), leaf.array1[i], newLeaf.array1[i]);

        for i := 0 to 4 do
          AssertEquals('array2#' + IntToStr(i), leaf.array2[i], newLeaf.array2[i]);

      end;
    finally
      obj.Free;
    end;

  finally
    container.Free;
  end;
end;

procedure TTestOTPersistentList.TestReferringBasicFunctions;
var
  list: TLeafClassReferenceList;
  leaf1: TLeafClass;
  leaf2: TLeafClass;
  index: Integer;
  oid1: TOID;
  oid2: TOID;
begin
  //
  // Given a new list
  // When constructed
  // Then initial Count is 0
  //
  // When an object is added
  // Then the index of the object is returned
  //  and the Count is 1
  //  and the first item in the list is the added object
  //  and the parent of added object is not changed
  //  and the item reference count is 1
  //
  // When another object is added
  // Then the index of the object is returned
  //  and the Count is 2
  //  and the second item in the list is the added object
  //  and the parent of the added object is not changed
  //  and the item reference count is 1
  //
  // When the list is Freed
  // Then the contents are not Freed as well
  //

  leaf1 := nil;
  leaf2 := nil;
  list := TLeafClassReferenceList.Create(nil);
  try
    AssertEquals('initial Count', 0, list.Count);

    leaf1 := TLeafClass.Create(nil);
    oid1 := leaf1.oid;
    index := list.Add(leaf1);

    AssertEquals('1 Count', 1, list.Count);
    AssertEquals('index 1', 0, index);
    AssertEquals('item1 refcount', 1, leaf1.referencesCount);
    AssertSame('item 1', leaf1, list[index]);
    AssertNull('item1 parent', leaf1.parent);

    leaf2 := TLeafClass.Create(nil);
    oid2 := leaf2.oid;
    index := list.Add(leaf2);

    AssertEquals('2 Count', 2, list.Count);
    AssertEquals('index 2', 1, index);
    AssertEquals('item2 refcount', 1, leaf2.referencesCount);
    AssertSame('item 2', leaf2, list[index]);
    AssertNull('item2 parent', leaf2.parent);

    FreeAndNil(list);

    AssertNotNull('oid1 null', OIDManager.FromOID(oid1));
    AssertNotNull('oid2 null', OIDManager.FromOID(oid2));

  finally
    list.Free;
    leaf1.Free;
    leaf2.Free;
  end;
end;

procedure TTestOTPersistentList.TestReferringRemove;
var
  list: TLeafClassReferenceList;
  leaf1: TLeafClass;
  leaf2: TLeafClass;
  oid1: TOID;
  oid2: TOID;
begin
  //
  // Given a new list containing several leaf objects
  //
  // When an item is removed
  // Then the list count decreases by 1
  //  and the item is not Free'd
  //  and the item reference count is 0
  //

  list := TLeafClassReferenceList.Create(nil);
  try
    leaf1 := TLeafClass.Create(nil);
    oid1 := leaf1.oid;
    list.Add(leaf1);

    leaf2 := TLeafClass.Create(nil);
    oid2 := leaf2.oid;
    list.Add(leaf2);

    list.Remove(leaf1);

    AssertEquals('list.Count', 1, list.Count);

    leaf1 := TLeafClass(OIDManager.FromOID(oid1));
    AssertNotNull('leaf1', leaf1);
    AssertEquals('leaf1 no refs', 0, leaf1.referencesCount);

    leaf2 := list[0];
    AssertEquals('oid2', oid2, leaf2.oid);

  finally
    list.Free;
    leaf1.Free;
    leaf2.Free;
  end;
end;

procedure TTestOTPersistentList.TestReferringExtract;
var
  list: TLeafClassReferenceList;
  leaf1: TLeafClass;
  leaf2: TLeafClass;
  oid1: TOID;
  oid2: TOID;
begin
  //
  // Given a list containing leaf objects
  //
  // When an object is extracted
  // Then the owner of the extracted object is not changed
  //  and the reference count is 0
  //

  list := TLeafClassReferenceList.Create(nil);
  try
    leaf1 := TLeafClass.Create(nil);
    oid1 := leaf1.oid;
    list.Add(leaf1);

    leaf2 := TLeafClass.Create(nil);
    oid2 := leaf2.oid;
    list.Add(leaf2);

    list.Extract(leaf1);

    AssertEquals('list.Count', 1, list.Count);

    leaf1 := TLeafClass(OIDManager.FromOID(oid1));
    AssertNotNull('leaf1', leaf1);
    AssertNull('leaf1 parent', leaf1.parent);
    AssertEquals('leaf1 refcount', 0, leaf1.referencesCount);

    FreeAndNil(list) ;

    leaf1 := TLeafClass(OIDManager.FromOID(oid1));
    AssertNotNull('leaf1', leaf1);
    leaf2 := TLeafClass(OIDManager.FromOID(oid2));
    AssertNotNull('leaf2', leaf2);
    AssertEquals('leaf2 refcount', 0, leaf2.referencesCount);

  finally
    list.Free;
    leaf1.Free;
    leaf2.Free;
  end;
end;

procedure TTestOTPersistentList.TestReferringEdit;
var
  leaf1: TLeafClass;
  leaf2: TLeafClass;
  list: TLeafClassReferenceList;
  oid1: TOID;
  oid2: TOID;
begin
  //
  // Given a referring list with a leaf object
  //
  // When the leaf is replaced with another leaf
  // Then the original leaf is refernce count is zero
  //  and the list contains the new leaf
  //  and the parent of the new leaf is not changed
  //  and the new leaf reference count is one
  //

  leaf1 := nil;
  leaf2 := nil;
  list := TLeafClassReferenceList.Create(nil);
  try
    leaf1 := TLeafClass.Create(nil);
    oid1 := leaf1.oid;

    list.Add(leaf1);

    AssertSame('leaf1 in list', list[0], leaf1);
    AssertEquals('leaf1 ref count initial', 1, leaf1.referencesCount);

    leaf2 := TLeafClass.Create(nil);
    oid2 := leaf2.oid;

    // When
    list[0] := leaf2;

    // Then
    AssertEquals('leaf1 ref count', 0, leaf1.referencesCount);
    AssertSame('leaf2 in list', list[0], leaf2);
    AssertNull('leaf2 parent', leaf2.parent);
    AssertEquals('leaf2 ref count', 1, leaf2.referencesCount);

  finally
    list.Free;
    leaf1.Free;
    leaf2.Free;
  end;
end;

initialization
  RegisterTest(TTestOTPersistentList);

end.
