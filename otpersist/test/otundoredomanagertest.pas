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
    procedure TestSingleObjectEditUndo;
    procedure TestSingleObjectEditUndoCalculatedUpdate;

    procedure TestEditReferenceAndUndo;

  end;

implementation

uses
  OTPersistent,
  OTUndoRedoManager,
  OwningClass,
  ReferringClass,
  LeafClass;

{ TTestOTUndoRedoManager }

procedure TTestOTUndoRedoManager.Setup;
begin
  inherited Setup;
  // clear it out and start anew...
  UndoRedoManager.Free;
  UndoRedoManager := TOTUndoRedoManager.Create;
end;

procedure TTestOTUndoRedoManager.Teardown;
begin
  inherited Teardown;
  // clear it out and start anew...
  UndoRedoManager.Free;
  UndoRedoManager := TOTUndoRedoManager.Create;
end;

procedure TTestOTUndoRedoManager.TestSingleObjectEditUndo;
var
  leaf: TLeafClass;
  undoCount: Integer;
begin
  //
  // Given a leaf object with initial values
  //
  // When the value of an attribute is edited
  // Then a new undo entry is created
  //
  // When Undo is called
  // Then the leaf attribute has its original value restored
  //

  leaf := TLeafClass.Create(nil);
  try
    leaf.int1 := 0;

    undoCount := UndoRedoManager.undoCount;

    leaf.int1 := 32;
    AssertEquals(undoCount + 1, UndoRedoManager.undoCount);

    UndoRedoManager.Undo;
    AssertEquals(0, leaf.int1);
  finally
    leaf.Free;
  end;
end;

procedure TTestOTUndoRedoManager.TestSingleObjectEditUndoCalculatedUpdate;
var
  owning: TOwningClass;
  referring: TReferringClass;
  leaf: TLeafClass;
  Value: Integer;
begin
  //
  // Given a leaf object with initial values
  // And a referring object that references the leaf
  // And an owning object that owns the leaf
  //
  // When the value of a leaf attribute is edited
  // And the referring object is calculated
  // And the owning object is calculated
  // And Undo is called
  //
  // Then the leaf attribute has its original value restored
  // And the leaf object, the owning object and the referring object are marked as not calculated
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
  finally
    referring.Free;
    owning.Free;
  end;
end;

procedure TTestOTUndoRedoManager.TestEditReferenceAndUndo;
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
  finally
    referring.Free;
    owning.Free;
  end;
end;


initialization
  RegisterTest(TTestOTUndoRedoManager);

end.
