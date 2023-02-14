unit OTPersistentList;

{$mode Delphi}

interface

uses
  Classes,
  SysUtils,
  Generics.Collections,
  OTPersistent;

type
  TOTPersistentList<T: TOTPersistent> = class(TOTPersistent)

    protected
      function GetItem(AIndex: Integer): T;
      procedure SetItem(AIndex: Integer; AValue: T);

    public
      function Add(AValue: T): Integer;
      function Count: Integer;

      property Items[AIndex: Integer]: T read GetItem write SetItem; default;
  end;

  TOTOwningList<T: TOTPersistent> = class(TOTPersistentList<T>)
  end;

  TOTReferenceList<T: TOTPersistent> = class(TOTPersistentList<T>)
  end;

implementation

function TOTPersistentList<T>.Add(AValue: T): Integer;
begin
  Result := 0;
end;

function TOTPersistentList<T>.Count: Integer;
begin
  Result := 0;
end;

function TOTPersistentList<T>.GetItem(AIndex: Integer): T;
begin
  Result := nil;
end;

procedure TOTPersistentList<T>.SetItem(AIndex: Integer; AValue: T);
begin

end;

end.

