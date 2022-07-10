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
  end;

  { TTestShovedTimberList }

  TTestShovedTimberList = class(TTestCase)
  protected
    list: TShovedTimberList;

    procedure Setup; override;
    procedure TearDown; override;

  published

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

initialization
  RegisterTest(TTestShovedTimber);
  RegisterTest(TTestShovedTimberList);

end.
