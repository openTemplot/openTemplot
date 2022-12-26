unit otunitgenerator;

{$mode Delphi}

interface

uses
  Classes,
  SysUtils;

type

  { TOTUnitGenerator }

  TOTUnitGenerator = class
    private
      FTemplateSource : string;
      FTemplateName: string;

    public
      constructor Create(const ATemplateSource: string; const ATemplateName: string);
      procedure Generate;

      property templateName: string read FTemplateName;
      property templateSource: string read FTemplateSource;
  end;

implementation

uses
  OTTemplateGenerator;

{ TOTUnitGenerator }

constructor TOTUnitGenerator.Create(const ATemplateSource: string;const ATemplateName: string);
begin
  inherited Create;

  FTemplateSource := ATemplateSource;
  FTemplateName := ATemplateName;
end;

procedure TOTUnitGenerator.Generate;
var
  generator: TOTTemplateGenerator;
  output: TStrings;
begin
  generator := TOTTemplateGenerator.Create;
  try
    generator.LoadTemplate(FTemplateSource);

    (generator['Name'] as TOTTemplateSubstitution).userValue := FTemplateName;
    (generator['Ancestor'] as TOTTemplateSubstitution).userValue := 'TOTPersistent';

    output := TStringList.Create;
    try
       generator.Generate(output);
       output.SaveToFile(LowerCase(FTemplateName) + '.pas');
    finally
      output.Free;
    end;

  finally
    generator.Free;
  end;
end;

end.

