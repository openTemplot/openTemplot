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
    procedure TestBasicFunctions;
    procedure TestContainerSaveRestoreYaml;
  end;

implementation

uses
  OTPersistent,
  OTPersistentList,
  OTOIDManager,
  ContainerClass,
  LeafClass,
  OTYaml,
  OTYamlEmitter;

{ TTestOTPersistentList }

procedure TTestOTPersistentList.Setup;
begin
  // clear it out and start anew...
  OIDManager.Free;
  OIDManager := TOTOIDManager.Create;
end;

procedure TTestOTPersistentList.Teardown;
begin
  // clear it out and start anew...
  OIDManager.Free;
  OIDManager := TOTOIDManager.Create;
end;

procedure TTestOTPersistentList.TestBasicFunctions;
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
  // And the Count is 1
  //
  // When another object is added
  // Then the index of the object is returned
  // And the Count is 2
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

    leaf := TLeafClass.Create(nil);
    oid2 := leaf.oid;
    index := list.Add(leaf);

    AssertEquals('2 Count', 2, list.Count);
    AssertEquals('index 2', 1, index);
    AssertSame('item 2', leaf, list[index]);

    FreeAndNil(list);

    AssertNull('oid1 null', OIDManager.FromOID(oid1));
    AssertNull('oid2 null', OIDManager.FromOID(oid2));

  finally
    list.Free;
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

initialization
  RegisterTest(TTestOTPersistentList);

end.
