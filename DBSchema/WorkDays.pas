unit WorkDays;

interface

type
    TWorkDays = array of shortstring;

function serializeWorkDays(wd: TWorkDays): string;

implementation

function serializeWorkDays(wd: TWorkDays): string;
var
    i: integer;
    len: integer;
    pos: integer;
begin
    len := 1 + Length(wd) * 3;
    for i := 0 to Length(wd) - 1 do
        inc(len, Length(wd[i]));
    SetLength(serializeWorkDays, len);

    serializeWorkDays[1] := '[';
    pos := 2;
    for i := 0 to Length(wd) - 1 do
    begin
        if i > 0 then
        begin
            serializeWorkDays[pos] := ',';
            inc(pos);
        end;
        serializeWorkDays[pos] := '"';
        len := Length(wd[i]);
        Move(wd[i, 1], serializeWorkDays[pos + 1], len);
        inc(pos, len + 2);
        serializeWorkDays[pos - 1] := '"';
    end;
    serializeWorkDays[pos] := ']';
end;

end.