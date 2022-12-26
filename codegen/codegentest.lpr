program codegentest;

{$mode Delphi}{$H+}

uses
  Classes, consoletestrunner, unitgeneratortest, templategeneratortest,
  otunitgenerator, otTemplateGenerator, OTCodeGenUtils, OTClassRegenerator,
  otYaml, otYamlChars, otYamlEvent, otYamlParser, otYamlReader, otYamlScanner,
  otYamlToken, CodeGenUtilsTest, ClassRegeneratorTest;

type

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;

{$R codegen.rc}

begin
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'FPCUnit Console test runner';
  Application.Run;
  Application.Free;
end.
