unit PlaceOptions;

{$mode objfpc}
{$longstrings on}

interface

uses
    sysutils, libmongoc, classes,
    Escape in 'Utils/Escape.pas',
    FastConcat in 'Utils/FastConcat.pas',
    WorkDays in 'DBSchema/WorkDays.pas';

type
    TPlaceOptions = class
    public
        availableFromHour: integer;
        availableUntilHour: integer;
        altSchedule: TWorkDays;
        altScheduleLength: integer;
        altAvailableFromHour: integer;
        altAvailableUntilHour: integer;
        workStartHour: integer;
        workEndHour: integer;

        constructor Create;
        constructor Create(iter: bson_iter_t);
        destructor Destroy; override;
    	function Serialize: string;
    end;

implementation

constructor TPlaceOptions.Create;
begin
end;

constructor TPlaceOptions.Create(iter: bson_iter_t);
var
    key: shortstring;
begin
    while bson_iter_next(@iter) do
    begin
        key := bson_iter_key(@iter);
        with Self do
            case key of
                'availableFromHour': availableFromHour := bson_iter_int32(@iter);
                'availableUntilHour': availableUntilHour := bson_iter_int32(@iter);
                'altAvailableFromHour': altAvailableFromHour := bson_iter_int32(@iter);
                'altAvailableUntilHour': altAvailableUntilHour := bson_iter_int32(@iter);
                'workStartHour': workStartHour := bson_iter_int32(@iter);
                'workEndHour': workEndHour := bson_iter_int32(@iter);
                // TODO: altSchedule
            end;
    end;
end;

destructor TPlaceOptions.Destroy;
begin
end;

function TPlaceOptions.Serialize: string;
var
    n: integer;
    size: integer;
    strings: array of string;
begin
    size := 1;
    if (availableFromHour <> 0) or (availableUntilHour <> 0) then inc(size, 4);
    if Length(altSchedule) > 0 then inc(size, 6);
    if (workStartHour > 0) or (workEndHour > 0) then inc(size, 4);
    SetLength(strings, size);

    n := 0;

    if (availableFromHour <> 0) or (availableUntilHour <> 0) then
    begin
        strings[n] := ',"availableFromHour":';
        strings[n + 1] := IntToStr(availableFromHour);
        strings[n + 2] := ',"availableUntilHour":';
        strings[n + 3] := IntToStr(availableUntilHour);
        inc(n, 4);
    end;

    if Length(altSchedule) > 0 then
    begin
        strings[n] := ',"altSchedule":';
        strings[n + 1] := serializeWorkDays(altSchedule);
        strings[n + 2] := ',"altAvailableFromHour":';
        strings[n + 3] := IntToStr(altAvailableFromHour);
        strings[n + 4] := ',"altAvailableUntilHour":';
        strings[n + 5] := IntToStr(altAvailableUntilHour);
        inc(n, 6);
    end;

    if (workStartHour > 0) or (workEndHour > 0) then begin
        strings[n] := ',"workStartHour":';
        strings[n + 1] := IntToStr(workStartHour);
        strings[n + 2] := ',"workEndHour":';
        strings[n + 3] := IntToStr(workEndHour);
        inc(n, 4);
    end;

    strings[0, 1] := '{';
    strings[n] := '}';

    Serialize := FastConcat.Join(strings);
end;


end.
