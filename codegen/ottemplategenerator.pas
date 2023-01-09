unit OTTemplateGenerator;

{$mode Delphi}

interface

uses
  Classes,
  Generics.Collections;

type
  TOTTemplateGenerator = class;

  /// Abstract class representing some form of user input for a template
  TOTTemplateInput = class
  private
    FName: String;

  public
    constructor Create(const AName: String);

    procedure Process(ALines: TStrings); virtual; abstract;

    property Name: String Read FName;
  end;

  TOTTemplateInputList = class(TObjectList<TOTTemplateInput>)
    end;

  // Performs text substitution for a template
  TOTTemplateSubstitution = class(TOTTemplateInput)
  private
    FDefaultValues: TStrings;
    FUserValue: String;

  public
    constructor Create(const AName: String; const ADefaultValue: String);

    procedure Process(ALines: TStrings); override;

    property defaultValues: TStrings Read FDefaultValues;
    property userValue: String Read FUserValue Write FUserValue;
  end;

  // Performs conditional inclusion for a template
  TOTTemplateCondition = class(TOTTemplateInput)
  private
    FDefaultValue: Boolean;
    FUserValue: Boolean;

  public
    constructor Create(const AName: String; const ADefaultValue: Boolean);

    procedure Process(ALines: TStrings); override;

    property defaultValue: Boolean Read FDefaultValue;
    property userValue: Boolean Read FUserValue Write FUserValue;
  end;

  TOTTemplateMultiSubstitution = class(TOTTemplateInput)
  private
    FUserValues: TStrings;

  public
    constructor Create(const AName: String);
    destructor Destroy; override;

    procedure Process(ALines: TStrings); override;

    property userValues: TStrings Read FUserValues;
  end;

  // Main class for handling templates
  TOTTemplateGenerator = class
  private
    FTemplate: TStringList;
    FInputs: TOTTemplateInputList;

    function GetNumberOfInputs: Integer;
    function GetInput(i: Integer): TOTTemplateInput;
    function GetInputByName(const s: String): TOTTemplateInput;

    procedure AddCondition(const AName: String; const ADefaultValue: String);
    procedure AddSub(const AName: String; const ADefaultValue: String);
    procedure AddMultiSub(const AName: String);
    procedure ExtractOptions;

  public
    constructor Create;
    destructor Destroy; override;

    // Clear any template data
    procedure Clear;
    // Load a template and extract user-input fields
    procedure LoadTemplate(const AFilename: String); overload;
    procedure LoadTemplate(AStream: TStream); overload;
    procedure LoadTemplateFromResource(const AResourceName: String); overload;

    // Process the template, applying the user input data
    procedure Generate(ADestination: TStrings);

    property numberOfInputs: Integer Read GetNumberOfInputs;
    property input[i: Integer]: TOTTemplateInput Read GetInput;

    property inputByName[const s: String]: TOTTemplateInput Read GetInputByName; default;
  end;

implementation

uses
  SysUtils,
  LCLType,
  StrUtils,
  OTCodeGenUtils;

const
  DELIM = '|';

{ TOTTemplateGenerator }

constructor TOTTemplateGenerator.Create;
begin
  inherited;
end;

procedure TOTTemplateGenerator.AddCondition(const AName, ADefaultValue: String);
var
  newCond: TOTTemplateCondition;
begin
  newCond := TOTTemplateCondition.Create(AName, (ADefaultValue = 'true'));
  FInputs.Add(newCond);
end;

procedure TOTTemplateGenerator.AddSub(const AName: String; const ADefaultValue: String);
var
  newSub: TOTTemplateSubstitution;
begin
  newSub := TOTTemplateSubstitution.Create(AName, ADefaultValue);
  FInputs.Add(newSub);
end;

procedure TOTTemplateGenerator.AddMultiSub(const AName: String);
var
  newMSub: TOTTemplateMultiSubstitution;
begin
  newMSub := TOTTemplateMultiSubstitution.Create(AName);
  FInputs.Add(newMSub);
end;

procedure TOTTemplateGenerator.Clear;
begin
  FreeAndNil(FTemplate);
  FreeAndNil(FInputs);
end;

destructor TOTTemplateGenerator.Destroy;
begin
  FTemplate.Free;
  FInputs.Free;

  inherited;
end;

procedure TOTTemplateGenerator.ExtractOptions;
var
  options: TStringList;
  endDelim: String;
  i: Integer;
  aLine: String;
  Name: String;
  Value: String;
begin
  //
  // remove all the options from the start of the template
  // and process as name/value pairs
  //
  endDelim := DELIM + DELIM;

  options := TStringList.Create;

  while FTemplate.Count > 0 do begin
    aLine := FTemplate[0];

    FTemplate.Delete(0);

    if aLine <> endDelim then
      options.Add(aLine)
    else
      Break;  // it is the end delimiter so get out of loop
  end;

  // turn our options into a list of names (substitutions) and
  // conditionals...
  for i := 0 to options.Count - 1 do begin
    Name := options.Names[i];
    Value := options.ValueFromIndex[i];

    if StartsStr('sub', Value) then begin
      AddSub(Name, Copy(Value, 5, Length(Value) - 4));
    end
    else
    if StartsStr('condition', Value) then begin
      AddCondition(Name, Copy(Value, 11, Length(Value) - 10));
    end
    else
    if StartsStr('msub', Value) then begin
      AddMultiSub(Name);
    end;
  end;
end;

procedure TOTTemplateGenerator.Generate(ADestination: TStrings);
var
  i: Integer;
  s: TStringList;
begin
  s := TStringList.Create;
  try
    s.AddStrings(FTemplate);
    for i := 0 to numberOfInputs - 1 do
      input[i].Process(s);

    ADestination.AddStrings(s);
  finally
    s.Free;
  end;
end;

function TOTTemplateGenerator.GetInput(i: Integer): TOTTemplateInput;
begin
  Result := FInputs[i] as TOTTemplateInput;
end;

function TOTTemplateGenerator.GetInputByName(const s: String): TOTTemplateInput;
var
  input: TOTTemplateInput;
begin
  for input in FInputs do begin
    if input.Name = s then begin
      Exit(input);
    end;
  end;
  Exit(nil);
end;

function TOTTemplateGenerator.GetNumberOfInputs: Integer;
begin
  if Assigned(FInputs) then
    Result := FInputs.Count
  else
    Result := 0;
end;

procedure TOTTemplateGenerator.LoadTemplate(AStream: TStream);
begin
  FreeAndNil(FTemplate);
  FreeAndNil(FInputs);

  FTemplate := TStringList.Create;
  FTemplate.LoadFromStream(AStream);

  FInputs := TOTTemplateInputList.Create;
  ExtractOptions;
end;

procedure TOTTemplateGenerator.LoadTemplate(const AFilename: String);
var
  stream: TStream;
begin
  stream := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyWrite);
  try
    LoadTemplate(Stream);
  finally
    stream.Free;
  end;
end;

procedure TOTTemplateGenerator.LoadTemplateFromResource(const AResourceName: String);
var
  stream: TStream;
begin
  stream := TResourceStream.Create(HInstance, AResourceName, RT_RCDATA);
  try
    LoadTemplate(Stream);
  finally
    stream.Free;
  end;
end;

{ TOTTemplateInput }

constructor TOTTemplateInput.Create(const AName: String);
begin
  inherited Create;

  FName := AName;
end;

{ TOTTemplateSubstitution }

constructor TOTTemplateSubstitution.Create(const AName: String; const ADefaultValue: String);
begin
  inherited Create(AName);

  FDefaultValues := TStringList.Create;
  FDefaultValues.Delimiter := DELIM;
  FDefaultValues.delimitedText := ADefaultValue;
end;

procedure TOTTemplateSubstitution.Process(ALines: TStrings);

  procedure ProcessSubstitution(ident, replace: String);
  var
    find: String;
    i: Integer;
  begin
    find := DELIM + ident + DELIM;

    for i := 0 to ALines.Count - 1 do
      ALines[i] := StringReplace(ALines[i], find, replace, [rfReplaceAll]);
  end;

begin
  ProcessSubstitution(QuotedValue(Name), userValue);
  ProcessSubstitution(FirstUpper(Name), FirstUpper(userValue));
  ProcessSubstitution(FirstLower(Name), FirstLower(userValue));
  ProcessSubstitution(AllUpper(Name), AllUpper(userValue));
  ProcessSubstitution('_' + LowerCase(Name) + '_', SnakeCase(userValue));
end;

{ TOTTemplateCondition }

constructor TOTTemplateCondition.Create(const AName: String; const ADefaultValue: Boolean);
begin
  inherited Create(AName);

  FDefaultValue := ADefaultValue;
end;

procedure TOTTemplateCondition.Process(ALines: TStrings);
var
  startText,
  startTextNot,
  elseText,
  endText: String;
  i: Integer;
  line: String;
  withinConditional: Boolean;
  removeLine: Boolean;
  include: Boolean;
begin
  startText := DELIM + 'IF ' + Name + DELIM;
  startTextNot := DELIM + 'IFNOT ' + Name + DELIM;
  elseText := DELIM + 'ELSE ' + Name + DELIM;
  endText := DELIM + 'ENDIF ' + Name + DELIM;

  i := 0;
  include := True;
  withinConditional := False;
  while i < ALines.Count do begin
    line := ALines[i];
    removeLine := False;

    if withinConditional then begin
      if line = endText then begin
        withinConditional := False;
        removeLine := True;
      end
      else if line = elseText then begin
        include := not include;
        removeLine := true;
      end
      else begin
        removeLine := (include and (not userValue)) or ((not include) and userValue);
      end;
    end
    else begin
      if line = startText then begin
        withinConditional := True;
        include := True;
        removeLine := True;
      end
      else
      if line = startTextNot then begin
        withinConditional := True;
        include := False;
        removeLine := True;
      end;
    end;

    if removeLine then
      ALines.Delete(i)
    else
      Inc(i);
  end;
end;

constructor TOTTemplateMultiSubstitution.Create(const AName: String);
begin
  inherited Create(AName);

  FUserValues := TStringList.Create;
end;

destructor TOTTemplateMultiSubstitution.Destroy;
begin
  FUserValues.Free;

  inherited;
end;

procedure TOTTemplateMultiSubstitution.Process(ALines: TStrings);
var
  startText,
  endText: String;
  i: Integer;
  line: String;
  withinRepeat: Boolean;
  removeLine: Boolean;
  include: Boolean;
  repeatLines: TStringList;
  temp: TStringList;
  v: String;
  sub: TOTTemplateSubstitution;
  newLine: String;
begin
  startText := DELIM + 'REPEAT ' + Name + DELIM;
  endText := DELIM + 'ENDREPEAT ' + Name + DELIM;

  repeatLines := TStringList.Create;
  try
    i := 0;
    include := True;
    withinRepeat := False;
    while i < ALines.Count do begin
      line := ALines[i];
      removeLine := False;

      if withinRepeat then begin
        removeLine := true;
        if line = endText then begin
          for v in FUserValues do begin
            temp := TStringList.Create;
            try
              temp.AddStrings(repeatLines);
              sub := TOTTemplateSubstitution.Create(FName, '');
              try
                sub.userValue := v;
                sub.Process(temp);

                for newLine in temp do begin
                  ALines.Insert(i, newLine);
                  Inc(i);
                end;

              finally
                sub.Free;
              end;
            finally
              temp.Free;
            end;
          end;
          withinRepeat := False;
        end
        else begin
          repeatLines.Add(line);
        end
      end
      else begin
        if line = startText then begin
          withinRepeat := True;
          removeLine := True;
          repeatLines.Clear;
        end;
      end;

      if removeLine then
        ALines.Delete(i)
      else
        Inc(i);
    end;

  finally
    repeatLines.Free;
  end;
end;

end.
