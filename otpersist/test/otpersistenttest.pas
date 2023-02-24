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

  { TTestOTPersistent }

  TTestOTPersistent = class(TTestCase)
  published
    procedure TestSimpleSaveRestoreYaml;
    //procedure TestSimpleSaveRestoreStream;
    procedure TestSimpleSetModified;

    procedure TestOwningSaveRestoreYaml;
    //procedure TestOwningSaveRestoreStream;
    procedure TestOwningSetModified;

  end;

implementation

uses
  OTYaml,
  OTYamlEmitter,
  OTPersistent,
  ContainerClass,
  OwningClass,
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

{
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
}

procedure TTestOTPersistent.TestSimpleSetModified;
var
  leaf: TLeafClass;
  Value: Integer;
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

    Value := leaf.sum;
    AssertTrue('first sum', leaf.IsCalculated);

    leaf.int1 := 123;
    AssertFalse('int1 = 123', leaf.IsCalculated);

    Value := leaf.sum;
    AssertTrue('second sum', leaf.IsCalculated);

    leaf.int1 := 123;
    AssertTrue('second int1 = 123', leaf.IsCalculated);

  finally
    leaf.Free;
  end;
end;

procedure TTestOTPersistent.TestOwningSaveRestoreYaml;
var
  leaf: TLeafClass;
  owning: TOwningClass;
  newLeaf: TLeafClass;
  newOwning: TOwningClass;
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
  leaf := nil;
  owning := TOwningClass.Create(nil);
  try
    leaf := TLeafClass.Create(nil);
    owning.leaf := leaf;

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

      owning.SaveToYaml(emitter);

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
      AssertTrue(obj is TOwningClass);
      newOwning := obj as TOwningClass;

      newLeaf := newOwning.leaf;

      AssertNotNull('newLeaf', newLeaf);
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
    owning.Free;
  end;
end;

{
procedure TTestOTPersistent.TestOwningSaveRestoreStream;
var
  owning: TOwningClass;
  leaf: TLeafClass;
  i: Integer;
  stream: TMemoryStream;
  obj: TOTPersistent;
begin
  //
  // Given an instance of TOwningClass with an assigned leaf
  // When that instance is saved to a binary stream
  // And that stream is restored
  // Then a new object is created with the same property values as the original
  //
  owning := TOwningClass.Create(nil);
  try
    leaf := TLeafClass.Create(nil);
    owning.leaf := leaf;
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
      owning.SaveToStream(stream);

      // And restore...
      stream.Seek(0, soFromBeginning);
      obj := TOTPersistent.RestoreStreamedObject(nil, stream);
      try

      // Then
      AssertTrue(obj is TOwningClass);
      finally
        obj.Free;
      end;
    finally
      stream.Free;
    end;
  finally
    owning.Free;
  end;
end;
}

procedure TTestOTPersistent.TestOwningSetModified;
var
  owning: TOwningClass;
  leaf: TLeafClass;
  Value: Integer;
begin
  //
  // When an owning class is created
  // Then the class is *not* calculated
  //
  // When a calculated property is accessed
  // Then the class is calculated
  //
  // When a the leaf attribute of the class is modified
  // Then the class is *not* calculated
  //
  // When a calculated property is accessed
  // Then the class is calculated
  //
  // When the leaf is set with it's existing value
  // Then the class is still calculated
  //
  owning := TOwningClass.Create(nil);
  leaf := nil;
  try
    AssertFalse('initial calculated state', owning.IsCalculated);

    Value := owning.leafSum;
    AssertTrue('first sum', owning.IsCalculated);

    leaf := TLeafClass.Create(nil);
    leaf.int1 := 123;
    owning.leaf := leaf;
    AssertFalse('leaf = leaf', owning.IsCalculated);

    Value := owning.leafSum;
    AssertTrue('second sum', owning.IsCalculated);

    owning.leaf := leaf;
    AssertTrue('second leaf = leaf', owning.IsCalculated);

  finally
    leaf.Free;
  end;
end;

initialization
  RegisterTest(TTestOTPersistent);
end.
