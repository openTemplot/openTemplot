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
      FTemplateResource : string;
      FTemplateName: string;

    public
      constructor Create(const ATemplateResource: string; const ATemplateName: string);
      procedure Generate;

      property templateName: string read FTemplateName;
      property templateResource: string read FTemplateResource;
  end;

implementation

uses
  OTCodeGenUtils,
  OTTemplateGenerator;

{ TOTUnitGenerator }

constructor TOTUnitGenerator.Create(const ATemplateResource: string;const ATemplateName: string);
begin
  inherited Create;

  FTemplateResource := ATemplateResource;
  FTemplateName := ATemplateName;
end;

procedure TOTUnitGenerator.Generate;
var
  generator: TOTTemplateGenerator;
  output: TStrings;
begin
  generator := TOTTemplateGenerator.Create;
  try
    generator.LoadTemplateFromResource(FTemplateResource);

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

