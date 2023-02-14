unit OTOIDManagerTest;

{$mode Delphi}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testutils,
  testregistry;

type

  { TTestOTOIDManager }

  TTestOTOIDManager = class(TTestCase)
  public
    procedure Setup; override;
    procedure Teardown; override;

  published
    procedure TestAllocateOID;
    procedure TestFromOID;
    procedure TestFreeOID;
  end;

implementation

uses
  OTPersistent,
  OTOIDManager;

{ TTestOTOIDManager }

procedure TTestOTOIDManager.Setup;
begin
  // clear it out and start anew...
  OIDManager.Free;
  OIDManager := TOTOIDManager.Create;
end;

procedure TTestOTOIDManager.Teardown;
begin
  // clear it out and start anew...
  OIDManager.Free;
  OIDManager := TOTOIDManager.Create;
end;

procedure TTestOTOIDManager.TestAllocateOID;
var
  obj1: TOTPersistent;
  obj2: TOTPersistent;
begin
  //
  // Given...
  // When an TOTPersistent object is constructed
  // Then the object is allocated an OID
  //
  // When another object is constructured
  // Then that object is allocated a different OID
  //
  obj1 := nil;
  obj2 := nil;
  try
    obj1 := TOTPersistent.create(nil);

    AssertNotNull('obj1', obj1);
    AssertTrue('obj1 oid', obj1.oid > 0);

    obj2 := TOTPersistent.Create(nil);

    AssertNotNull('obj2', obj2);
    AssertTrue('obj2 oid', obj2.oid > 0);
    AssertTrue('unique oid', obj1.oid <> obj2.oid);

  finally
    obj1.Free;
    obj2.Free;
  end;
end;

procedure TTestOTOIDManager.TestFromOID;
var
  obj1: TOTPersistent;
  obj2: TOTPersistent;
begin
  //
  // Given an TOTPersistent object is constructed
  // And the object is allocated an OID
  //
  // When FromOID() is called
  // Then the pointer to the object is returned
  //
  // When FromOID() is called with 0 parameter
  // Then a nil pointer is returned
  //
  // When FromOID() is called with a large parameter
  // Then a nil pointer is returned
  //
  obj1 := nil;
  obj2 := nil;
  try
    obj1 := TOTPersistent.create(nil);

    AssertNotNull('obj1', obj1);
    AssertTrue('obj1 oid', obj1.oid > 0);

    obj2 := OIDManager.FromOID(obj1.oid);

    AssertSame('returned same', obj1, obj2);

    obj2 := OIDManager.FromOID(0);
    AssertNull('0 returns nil', obj2);

    obj2 := OIDManager.FromOID(12345);
    AssertNull('invalid returns nil', obj2);

  finally
    obj1.Free;
  end;
end;

procedure TTestOTOIDManager.TestFreeOID;
var
  obj1: TOTPersistent;
  obj2: TOTPersistent;
  oid: TOID;
begin
  //
  // Given an OTPersistent object is constructed
  // And the object is allocated an OID
  //
  // When the object is Freed
  // Then calling FromOID() using the allocated OID returns nil
  //
  obj1 := nil;
  obj2 := nil;
  try
    obj1 := TOTPersistent.create(nil);

    AssertNotNull('obj1', obj1);
    AssertTrue('obj1 oid', obj1.oid > 0);

    oid := obj1.oid;

    FreeAndNil(obj1);

    obj2 := OIDManager.FromOID(oid);

    AssertNull('returns nil', obj2);
  finally
    obj1.Free;
  end;
end;

initialization
  RegisterTest(TTestOTOIDManager);
end.
