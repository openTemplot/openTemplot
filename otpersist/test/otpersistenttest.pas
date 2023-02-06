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
  end;

implementation

uses
  OTYaml,
  OTYamlEmitter,
  OTPersistent,
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
  // When the stream of Yaml test is restored
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

      leaf.SaveYamlObject(emitter);

      emitter.DocumentEndEvent(True);
      emitter.StreamEndEvent;

      yamlText := stream.DataString;
    finally
      emitter.Free;
    end;

    // Then
    AssertTrue('yamlText', yamlText <> '');

    WriteLn('yamlText:');
    WriteLn(yamlText);

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


initialization

  RegisterTest(TTestOTPersistent);
end.
