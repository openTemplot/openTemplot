
(*  v1
    This file is part of OpenTemplot, a computer program for the design of
    model railway track.

    Copyright (C) 2018  OpenTemplot project contributors

    This program is free software: you may redistribute it and/or modify
    it under the terms of the GNU General Public Licence as published by
    the Free Software Foundation, either version 3 of the Licence, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    See the GNU General Public Licence for more details.

    You should have received a copy of the GNU General Public Licence
    along with this program. See the files: licence.txt or opentemplot.lpr

    Or if not, refer to the web site: https://www.gnu.org/licenses/

====================================================================================
*)

unit shoved_timber_test;

{$mode Delphi}
{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testregistry,
  shoved_timber;

type
  { TTestShovedTimber }

  TTestShovedTimber = class(TTestCase)
  protected
    shovedTimber: TShovedTimber;

    procedure Setup; override;
    procedure TearDown; override;

  published
    procedure TestConstructorDefaults;
    procedure TestCreateFrom;
    procedure TestMakeShoved;
    procedure TestMakeShovedOnOmit;
    procedure TestMakeOmit;

    procedure TestXtbModifier;
    procedure TestAngleModifier;
    procedure TestOffsetModifier;
    procedure TestLengthModifier;
    procedure TestWidthModifier;
    procedure TestCrabModifier;

    procedure TestAdjustXtb;
    procedure TestAdjustAngle;
    procedure TestAdjustOffset;
    procedure TestAdjustLength;
    procedure TestAdjustWidth;
    procedure TestAdjustCrab;

    procedure TestRescale;
    procedure TestCanRestore;
  end;

  { TTestShovedTimberList }

  TTestShovedTimberList = class(TTestCase)
  protected
    list: TShovedTimberList;

    procedure Setup; override;
    procedure TearDown; override;

  published
    procedure TestCopyFromWithNil;
    procedure TestCopyFromAnotherList;

  end;

implementation

{ TTestShovedTimber }

procedure TTestShovedTimber.Setup;
begin
  inherited Setup;

  shovedTimber := TShovedTimber.Create;
end;

procedure TTestShovedTimber.TearDown;
begin
  shovedTimber.Free;

  inherited TearDown;
end;

procedure TTestShovedTimber.TestConstructorDefaults;
begin
  //
  // Given a constructed object
  // When ...
  // Then the object has the expected default values
  //

  CheckEquals(Ord(svcEmpty), Ord(shovedTimber.shoveCode), 'shoveCode');
  CheckEquals(0, shovedTimber.xtbModifier, 'xtbModifier');
  CheckEquals(0, shovedTimber.angleModifier, 'angleModifier');
  CheckEquals(0, shovedTimber.offsetModifier, 'offsetModifier');
  CheckEquals(0, shovedTimber.lengthModifier, 'lengthModifier');
  CheckEquals(0, shovedTimber.widthModifier, 'widthModifier');
  CheckEquals(0, shovedTimber.crabModifier, 'crabModifier');
end;

procedure TTestShovedTimber.TestCreateFrom;
var
  newTimber: TShovedTimber;
begin
  //
  // Given an object with non-default properties
  // When the CreateFrom constructor is called
  // Then the new object has the same properties
  //

  shovedTimber.shoveCode := svcShove;
  shovedTimber.xtbModifier := 1;
  shovedTimber.angleModifier := 2;
  shovedTimber.offsetModifier := 3;
  shovedTimber.lengthModifier := 4;
  shovedTimber.widthModifier := 5;
  shovedTimber.crabModifier := 6;

  newTimber := TShovedTimber.CreateFrom(shovedTimber);
  try
    CheckEquals(Ord(shovedTimber.shoveCode), Ord(newTimber.shoveCode), 'shoveCode');
    CheckEquals(shovedTimber.xtbModifier, newTimber.xtbModifier, 'xtbModifier');
    CheckEquals(shovedTimber.angleModifier, newTimber.angleModifier, 'angleModifier');
    CheckEquals(shovedTimber.offsetModifier, newTimber.offsetModifier, 'offsetModifier');
    CheckEquals(shovedTimber.lengthModifier, newTimber.lengthModifier, 'lengthModifier');
    CheckEquals(shovedTimber.widthModifier, newTimber.widthModifier, 'widthModifier');
    CheckEquals(shovedTimber.crabModifier, newTimber.crabModifier, 'crabModifier');
  finally
    newTimber.Free;
  end;
end;

procedure TTestShovedTimber.TestMakeShoved;
begin
  //
  // Given a default object
  // When MakeShoved is called
  // Then the shoveCode changes to svcShove
  //

  // When
  shovedTimber.MakeShoved;

  // Then
  CheckEquals(Ord(svcShove), Ord(shovedTimber.shoveCode));
end;

procedure TTestShovedTimber.TestMakeShovedOnOmit;
begin
  //
  // Given an ommitted timber
  // When MakeShoved is called
  // Then the shoveCode does not change
  //

  // Given
  shovedTimber.shoveCode := svcOmit;

  // When
  shovedTimber.MakeShoved;

  // Then
  CheckEquals(Ord(svcOmit), Ord(shovedTimber.shoveCode));
end;

procedure TTestShovedTimber.TestMakeOmit;
begin
  //
  // Given a shoved timber
  // When MakeOmit is called
  // Then the shovedCode equals svcOmit
  //  and all the shove parameters revert to defaults
  //

  // Given
  shovedTimber.shoveCode := svcShove;
  shovedTimber.xtbModifier := 1;
  shovedTimber.angleModifier := 2;
  shovedTimber.offsetModifier := 3;
  shovedTimber.lengthModifier := 4;
  shovedTimber.widthModifier := 5;
  shovedTimber.crabModifier := 6;

  // When
  shovedTimber.MakeOmit;

  // Then
  CheckEquals(Ord(svcOmit), Ord(shovedTimber.shoveCode), 'shoveCode');
  CheckEquals(0, shovedTimber.xtbModifier, 'xtbModifier');
  CheckEquals(0, shovedTimber.angleModifier, 'angleModifier');
  CheckEquals(0, shovedTimber.offsetModifier, 'offsetModifier');
  CheckEquals(0, shovedTimber.lengthModifier, 'lengthModifier');
  CheckEquals(0, shovedTimber.widthModifier, 'widthModifier');
  CheckEquals(0, shovedTimber.crabModifier, 'crabModifier');
end;

procedure TTestShovedTimber.TestXtbModifier;
begin
  //
  // Given the default object
  // When the xtbModifier property is set
  // Then the xtbModifier property is updated
  //  and the shoveCode is set to svcShove;
  //

  // Given

  // When
  shovedTimber.xtbModifier := 1;

  // Then
  CheckEquals(Ord(svcShove), Ord(shovedTimber.shoveCode), 'shoveCode');
  CheckEquals(1, shovedTimber.xtbModifier, 'xtbModifier');
end;

procedure TTestShovedTimber.TestAngleModifier;
begin
  //
  // Given the default object
  // When the angleModifier property is set
  // Then the angleModifier property is updated
  //  and the shoveCode is set to svcShove;
  //

  // Given

  // When
  shovedTimber.angleModifier := 1;

  // Then
  CheckEquals(Ord(svcShove), Ord(shovedTimber.shoveCode), 'shoveCode');
  CheckEquals(1, shovedTimber.angleModifier, 'angleModifier');
end;

procedure TTestShovedTimber.TestOffsetModifier;
begin
  //
  // Given the default object
  // When the offsetModifier property is set
  // Then the offsetModifier property is updated
  //  and the shoveCode is set to svcShove;
  //

  // Given

  // When
  shovedTimber.offsetModifier := 1;

  // Then
  CheckEquals(Ord(svcShove), Ord(shovedTimber.shoveCode), 'shoveCode');
  CheckEquals(1, shovedTimber.offsetModifier, 'offsetModifier');
end;

procedure TTestShovedTimber.TestLengthModifier;
begin
  //
  // Given the default object
  // When the lengthModifier property is set
  // Then the lengthModifier property is updated
  //  and the shoveCode is set to svcShove;
  //

  // Given

  // When
  shovedTimber.lengthModifier := 1;

  // Then
  CheckEquals(Ord(svcShove), Ord(shovedTimber.shoveCode), 'shoveCode');
  CheckEquals(1, shovedTimber.lengthModifier, 'lengthModifier');
end;

procedure TTestShovedTimber.TestWidthModifier;
begin
  //
  // Given the default object
  // When the widthModifier property is set
  // Then the widthModifier property is updated
  //  and the shoveCode is set to svcShove;
  //

  // Given

  // When
  shovedTimber.widthModifier := 1;

  // Then
  CheckEquals(Ord(svcShove), Ord(shovedTimber.shoveCode), 'shoveCode');
  CheckEquals(1, shovedTimber.widthModifier, 'widthModifier');
end;

procedure TTestShovedTimber.TestCrabModifier;
begin
  //
  // Given the default object
  // When the crabModifier property is set
  // Then the crabModifier property is updated
  //  and the shoveCode is set to svcShove;
  //

  // Given

  // When
  shovedTimber.crabModifier := 1;

  // Then
  CheckEquals(Ord(svcShove), Ord(shovedTimber.shoveCode), 'shoveCode');
  CheckEquals(1, shovedTimber.crabModifier, 'crabModifier');
end;

procedure TTestShovedTimber.TestAdjustXtb;
begin
  //
  // Given a default object
  // When AdjustXtb is called
  // Then the shoveCode is set to svcShove
  //  and the xtbModifier property is set
  //
  // When AdjustXtb is called again
  // Then the xtbModifier property is adjusted
  //

  // When
  shovedTimber.AdjustXtb(3);

  // Then
  CheckEquals(Ord(svcShove), Ord(shovedTimber.shoveCode), 'shoveCode');
  CheckEquals(3, shovedTimber.xtbModifier, 'xtbModifier #1');

  // When
  shovedTimber.AdjustXtb(-5);

  // Then
  CheckEquals(-2, shovedTimber.xtbModifier, 'xtbModifier #2');
end;

procedure TTestShovedTimber.TestAdjustAngle;
begin
  //
  // Given a default object
  // When AdjustAngle is called
  // Then the shoveCode is set to svcShove
  //  and the angleModifier property is set
  //
  // When AdjustAngle is called again
  // Then the angleModifier property is adjusted
  //

  // When
  shovedTimber.AdjustAngle(3);

  // Then
  CheckEquals(Ord(svcShove), Ord(shovedTimber.shoveCode), 'shoveCode');
  CheckEquals(3, shovedTimber.angleModifier, 'angleModifier #1');

  // When
  shovedTimber.AdjustAngle(-5);

  // Then
  CheckEquals(-2, shovedTimber.angleModifier, 'angleModifier #2');
end;

procedure TTestShovedTimber.TestAdjustOffset;
begin
  //
  // Given a default object
  // When AdjustOffset is called
  // Then the shoveCode is set to svcShove
  //  and the offsetModifier property is set
  //
  // When AdjustOffset is called again
  // Then the offsetModifier property is adjusted
  //

  // When
  shovedTimber.AdjustOffset(3);

  // Then
  CheckEquals(Ord(svcShove), Ord(shovedTimber.shoveCode), 'shoveCode');
  CheckEquals(3, shovedTimber.offsetModifier, 'offsetModifier #1');

  // When
  shovedTimber.AdjustOffset(-5);

  // Then
  CheckEquals(-2, shovedTimber.offsetModifier, 'offsetModifier #2');
end;

procedure TTestShovedTimber.TestAdjustLength;
begin
  //
  // Given a default object
  // When AdjustLength is called
  // Then the shoveCode is set to svcShove
  //  and the lengthModifier property is set
  //
  // When AdjustLength is called again
  // Then the lengthModifier property is adjusted
  //

  // When
  shovedTimber.AdjustLength(3);

  // Then
  CheckEquals(Ord(svcShove), Ord(shovedTimber.shoveCode), 'shoveCode');
  CheckEquals(3, shovedTimber.lengthModifier, 'lengthModifier #1');

  // When
  shovedTimber.AdjustLength(-5);

  // Then
  CheckEquals(-2, shovedTimber.lengthModifier, 'lengthModifier #2');
end;

procedure TTestShovedTimber.TestAdjustWidth;
begin
  //
  // Given a default object
  // When AdjustWidth is called
  // Then the shoveCode is set to svcShove
  //  and the widthModifier property is set
  //
  // When AdjustWidth is called again
  // Then the xtbModifier property is adjusted
  //

  // When
  shovedTimber.AdjustWidth(3);

  // Then
  CheckEquals(Ord(svcShove), Ord(shovedTimber.shoveCode), 'shoveCode');
  CheckEquals(3, shovedTimber.widthModifier, 'widthModifier #1');

  // When
  shovedTimber.AdjustWidth(-5);

  // Then
  CheckEquals(-2, shovedTimber.widthModifier, 'widthModifier #2');
end;

procedure TTestShovedTimber.TestAdjustCrab;
begin
  //
  // Given a default object
  // When AdjustCrab is called
  // Then the shoveCode is set to svcShove
  //  and the crabModifier property is set
  //
  // When AdjustCrab is called again
  // Then the crabModifier property is adjusted
  //

  // When
  shovedTimber.AdjustCrab(3);

  // Then
  CheckEquals(Ord(svcShove), Ord(shovedTimber.shoveCode), 'shoveCode');
  CheckEquals(3, shovedTimber.crabModifier, 'crabModifier #1');

  // When
  shovedTimber.AdjustCrab(-5);

  // Then
  CheckEquals(-2, shovedTimber.crabModifier, 'crabModifier #2');
end;

procedure TTestShovedTimber.TestRescale;
begin
  //
  // Given a shoved timber with non-default values
  // When Rescale is called
  // Then all the dimensions (except angleModifier) are rescaled
  //

  shovedTimber.shoveCode := svcShove;
  shovedTimber.xtbModifier := 1;
  shovedTimber.angleModifier := 2;
  shovedTimber.offsetModifier := 3;
  shovedTimber.lengthModifier := 4;
  shovedTimber.widthModifier := 5;
  shovedTimber.crabModifier := 6;

  // When
  shovedTimber.Rescale(2);

  // Then
  CheckEquals(Ord(svcShove), Ord(shovedTimber.shoveCode), 'shoveCode');
  CheckEquals(2, shovedTimber.xtbModifier, 'xtbModifier');
  CheckEquals(2, shovedTimber.angleModifier, 'angleModifier');
  CheckEquals(6, shovedTimber.offsetModifier, 'offsetModifier');
  CheckEquals(8, shovedTimber.lengthModifier, 'lengthModifier');
  CheckEquals(10, shovedTimber.widthModifier, 'widthModifier');
  CheckEquals(12, shovedTimber.crabModifier, 'crabModifier');

end;

procedure TTestShovedTimber.TestCanRestore;
begin
  //
  // Given a default constructed object
  // Then CanRestore returns false
  //
  // When the object is omitted
  // Then CanRestore returns true
  //
  // When the object is shoved (but no values changed)
  // Then CanRestore returns false
  //
  // When the xtbModifer is changed
  // Then CanRestore returns true
  //
  // When the xtbModifer is set back to 0
  // Then CanRestore returns false
  //
  // When the angleModifier is changed
  // Then CanRestore returns true
  //
  // When the angleModifier is set back to 0
  // Then CanRestore returns false
  //
  // When the offsetModifier is changed
  // Then CanRestore returns true
  //
  // When the offsetModifier is set back to 0
  // Then CanRestore returns false
  //
  // When the lengthModifier is changed
  // Then CanRestore returns true
  //
  // When the lengthModifier is set back to 0
  // Then CanRestore returns false
  //
  // When the widthModifier is changed
  // Then CanRestore returns true
  //
  // When the widthModifier is set back to 0
  // Then CanRestore returns false
  //
  // When the crabModifier is changed
  // Then CanRestore returns true
  //
  // When the crabModifier is set back to 0
  // Then CanRestore returns false
  //

  CheckEquals(False, shovedTimber.CanRestore, 'default');

  shovedTimber.shoveCode := svcOmit;
  CheckEquals(True, shovedTimber.CanRestore, 'svcOmit');

  shovedTimber.shoveCode := svcShove;
  CheckEquals(False, shovedTimber.CanRestore, 'svcShove');

  shovedTimber.xtbModifier := 1;
  CheckEquals(True, shovedTimber.CanRestore, 'xtbModifier Modified');

  shovedTimber.xtbModifier := 0;
  CheckEquals(False, shovedTimber.CanRestore, 'xtbModifier Restored');

  shovedTimber.angleModifier := 1;
  CheckEquals(True, shovedTimber.CanRestore, 'angleModifier Modified');

  shovedTimber.angleModifier := 0;
  CheckEquals(False, shovedTimber.CanRestore, 'angleModifier Restored');

  shovedTimber.offsetModifier := 1;
  CheckEquals(True, shovedTimber.CanRestore, 'offsetModifier Modified');

  shovedTimber.offsetModifier := 0;
  CheckEquals(False, shovedTimber.CanRestore, 'offsetModifier Restored');

  shovedTimber.lengthModifier := 1;
  CheckEquals(True, shovedTimber.CanRestore, 'lengthModifier Modified');

  shovedTimber.lengthModifier := 0;
  CheckEquals(False, shovedTimber.CanRestore, 'lengthModifier Restored');

  shovedTimber.widthModifier := 1;
  CheckEquals(True, shovedTimber.CanRestore, 'widthModifier Modified');

  shovedTimber.widthModifier := 0;
  CheckEquals(False, shovedTimber.CanRestore, 'widthModifier Restored');

  shovedTimber.crabModifier := 1;
  CheckEquals(True, shovedTimber.CanRestore, 'crabModifier Modified');

  shovedTimber.crabModifier := 0;
  CheckEquals(False, shovedTimber.CanRestore, 'crabModifier Restored');

end;

{ TTestShovedTimberList }

procedure TTestShovedTimberList.Setup;
begin
  inherited Setup;

  list := TShovedTimberList.Create;
end;

procedure TTestShovedTimberList.TearDown;
begin
  list.Free;

  inherited TearDown;
end;

procedure TTestShovedTimberList.TestCopyFromWithNil;
begin
  //
  // Given a list containing some items
  // When CopyFrom is called with nil
  // Then the list is cleared
  //

  // Given
  list.Add(TShovedTimber.Create);
  list.Add(TShovedTimber.Create);

  // When
  list.CopyFrom(nil);

  // Then
  CheckEquals(0, list.Count);
end;

procedure TTestShovedTimberList.TestCopyFromAnotherList;
var
  anotherList: TShovedTimberList;
  s: TShovedTimber;
  i: Integer;
begin
  //
  // Given a list containing some items
  //   and another list containing different items
  // When CopyFrom is called with the other list of items
  // Then the list contains copies of all the items from the other list
  //

  // Given
  list.Add(TShovedTimber.Create);
  list.Add(TShovedTimber.Create);

  anotherList := TShovedTimberList.Create;
  try
    s := TShovedTimber.Create;
    anotherList.Add(s);

    s := TShovedTimber.Create;
    s.MakeOmit;
    anotherList.Add(s);

    s := TShovedTimber.Create;
    s.xtbModifier := 1;
    s.angleModifier := 3;
    anotherList.Add(s);

    // When
    list.CopyFrom(anotherList);

    // Then
    CheckEquals(anotherList.Count, list.Count, 'list.Count');
    for i := 0 to list.Count - 1 do begin
      CheckEquals(Ord(anotherList[i].shoveCode), Ord(list[i].shoveCode),
        format('shoveCode[%d]', [i]));
      CheckEquals(anotherList[i].xtbModifier, list[i].xtbModifier, format('xtbModifier[%d]', [i]));
      CheckEquals(anotherList[i].angleModifier, list[i].angleModifier, format('angleModifier[%d]', [i]));
    end;


  finally
    anotherList.Free;
  end;

end;

initialization
  RegisterTest(TTestShovedTimber);
  RegisterTest(TTestShovedTimberList);

end.
