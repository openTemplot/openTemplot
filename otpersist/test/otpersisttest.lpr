program otpersisttest;

{$mode objfpc}{$H+}

uses
  Classes, consoletestrunner, otpersistenttest, LeafClass, ContainerClass,
  OTPersistent, TLoggerUnit, otYaml, otYamlEmitter, otYamlEvent, otYamlParser,
  OTPersistentList, OTOIDManager, OTOIDManagerTest, OTPersistentListTest,
OTUndoRedoManager, OTUndoRedoManagerTest;

type

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;

begin
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'OT Persistence Framework test runner';
  Application.Run;
  Application.Free;
end.
