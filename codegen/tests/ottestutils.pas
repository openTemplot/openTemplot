unit OTTestUtils;

{$mode Delphi}

interface

uses
  Classes, SysUtils;

function FileCompare(const AFile1, AFile2: string): boolean;

implementation

function FileCompare(const AFile1, AFile2: string): boolean;
var
  str1: string;
  str2: string;
  file1: TStringStream;
  file2: TStringStream;
begin
  file1 := TStringStream.Create;
  try
    file1.LoadFromFile(AFile1);
    str1 := file1.DataString;
  finally
    file1.Free;
  end;

  file2 := TStringStream.Create;
  try
    file2.LoadFromFile(AFile2);
    str2 := file2.DataString;
  finally
    file2.Free;
  end;

  Result := (str1 = str2);
end;


end.

