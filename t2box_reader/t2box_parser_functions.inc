
function parse_boolean(var box_file: file): boolean;
var
  value: boolean
  number_read: integer;
begin
  BlockRead(box_file, value, SizeOf(boolean), number_read);
  Result := value;
end;

function parse_byte(var box_file: file): byte;
var
  value: byte;
  number_read: integer;
begin
  BlockRead(box_file, value, SizeOf(byte), number_read);
  Result := value;
end;

function parse_integer(var box_file: file): byte;
var
  value: int32;                      // integers are 32 bits in T2 box files
  number_read: integer;
begin
  BlockRead(box_file, value, SizeOf(int32), number_read);
  Result := value;
end;

function parse_string(var box_file: file): string;
begin
Result := ''
end;


