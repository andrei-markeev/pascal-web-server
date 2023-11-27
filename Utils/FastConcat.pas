unit FastConcat;

{$mode objfpc}
{$longstrings on}

interface

function Join(a: array of string): string;

implementation

function Join(a: array of string): string;
var
    i: integer;
    pos: integer;
    len: integer;
begin
    len := 0;
    for i := 0 to Length(a) - 1 do
        inc(len, Length(a[i]));

    SetLength(Join, len);

    pos := 1;
    for i := 0 to Length(a) - 1 do
    begin
        len := Length(a[i]);
        Move(a[i, 1], Join[pos], len);
        inc(pos, len);
    end;
end;

end.