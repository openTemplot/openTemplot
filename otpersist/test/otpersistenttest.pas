unit otpersistenttest;

{$mode Delphi}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testutils,
  testregistry;

type

  TTestOTPersistent = class(TTestCase)
  published
    procedure TestSimpleSaveRestoreYaml;
    procedure TestSimpleSaveRestoreStream;
    procedure TestSimpleSetModified;

    procedure TestContainerSaveRestoreYaml;
  end;

implementation

uses
  OTYaml,
  OTYamlEmitter,
  OTPersistent,
  ContainerClass,
  LeafClass;

procedure TTestOTPersistent.TestSimpleSaveRestoreYaml;
var
  leaf: TLeafClass;
  newLeaf: TLeafClass;
  i: Integer;
  stream: TStringStream;
  yamlVer: TYamlVersionDirective;
  yamlText: String;
  emitter: TYamlEmitter;
  obj: TOTPersistent;
begin
  //
  // Given an instance of TLeafClass with assigned properties
  // When that instance is saved to Yaml
  // Then a stream of Yaml text is created
  //
  // When the stream of Yaml text is restored
  // Then a new object is created with the same property values as the original
  //
  leaf := TLeafClass.Create(nil);
  try
    leaf.int1 := 234;
    leaf.int2 := 56789;
    leaf.str1 := 'Happy Birthday!';
    for i := 0 to 20 do begin
      leaf.AddArray1(23.1 + i * i);
    end;
    for i := 0 to 4 do begin
      leaf.array2[i] := 'Fred ' + IntToStr(i);
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

      leaf.SaveToYaml(emitter);

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
      AssertTrue(obj is TLeafClass);
      newLeaf := obj as TLeafClass;

      AssertEquals('int1', leaf.int1, newLeaf.int1);
      AssertEquals('int2', leaf.int2, newLeaf.int2);
      AssertEquals('str1', leaf.str1, newLeaf.str1);
      AssertEquals('array1Count', leaf.array1Count, newLeaf.array1Count);

      for i := 0 to newLeaf.array1Count - 1 do
        AssertEquals('array1#' + IntToStr(i), leaf.array1[i], newLeaf.array1[i]);

      for i := 0 to 4 do
        AssertEquals('array2#' + IntToStr(i), leaf.array2[i], newLeaf.array2[i]);
    finally
      obj.Free;
    end;

  finally
    leaf.Free;
  end;
end;

procedure TTestOTPersistent.TestSimpleSaveRestoreStream;
var
  leaf: TLeafClass;
  newLeaf: TLeafClass;
  i: Integer;
  stream: TMemoryStream;
  obj: TOTPersistent;
begin
  //
  // Given an instance of TLeafClass with assigned properties
  // When that instance is saved to a binary stream
  // And the that stream test is restored
  // Then a new object is created with the same property values as the original
  //
  leaf := TLeafClass.Create(nil);
  try
    leaf.int1 := 234;
    leaf.int2 := 56789;
    leaf.str1 := 'Happy Birthday!';
    for i := 0 to 20 do begin
      leaf.AddArray1(23.1 + i * i);
    end;
    for i := 0 to 4 do begin
      leaf.array2[i] := 'Fred ' + IntToStr(i);
    end;

    // When save...
    stream := TMemoryStream.Create;
    try
      leaf.SaveToStream(stream);

      // And restore...
      stream.Seek(0, soFromBeginning);
      obj := TOTPersistent.RestoreStreamedObject(nil, stream);
      try

      // Then
      AssertTrue(obj is TLeafClass);
      newLeaf := obj as TLeafClass;

      AssertEquals('int1', leaf.int1, newLeaf.int1);
      AssertEquals('int2', leaf.int2, newLeaf.int2);
      AssertEquals('str1', leaf.str1, newLeaf.str1);
      AssertEquals('array1Count', leaf.array1Count, newLeaf.array1Count);

      for i := 0 to newLeaf.array1Count - 1 do
        AssertEquals('array1#' + IntToStr(i), leaf.array1[i], newLeaf.array1[i]);

      for i := 0 to 4 do
        AssertEquals('array2#' + IntToStr(i), leaf.array2[i], newLeaf.array2[i]);

      finally
        obj.Free;
      end;
    finally
      stream.Free;
    end;
  finally
    leaf.Free;
  end;
end;

procedure TTestOTPersistent.TestSimpleSetModified;
var
  leaf: TLeafClass;
  value: Integer;
begin
  //
  // When a simple leaf class is created
  // Then the class is *not* calculated
  //
  // When a calculated property is accessed
  // Then the class is calculated
  //
  // When an attribute of the class is modified
  // Then the class is *not* calculated
  //
  // When a calculated property is accessed
  // Then the class is calculated
  //
  // When an attribute is set with it's existing value
  // Then the class is still calculated
  //
  leaf := TLeafClass.Create(nil);
  try
    AssertFalse('initial calculated state', leaf.IsCalculated);

    value := leaf.sum;
    AssertTrue('first sum', leaf.IsCalculated);

    leaf.int1 := 123;
    AssertFalse('int1 = 123', leaf.IsCalculated);

    value := leaf.sum;
    AssertTrue('second sum', leaf.IsCalculated);

    leaf.int1 := 123;
    AssertTrue('second int1 = 123', leaf.IsCalculated);

  finally
    leaf.Free;
  end;
end;

procedure TTestOTPersistent.TestContainerSaveRestoreYaml;
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
        leaf.int1 := 234 + j*3;
        leaf.int2 := 56789 - j*3;
        leaf.str1 := 'Happy Birthday!';
        for i := 0 to 20 do begin
          leaf.AddArray1(23.1 + i*i + j*j*j);
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
  RegisterTest(TTestOTPersistent);
end.
