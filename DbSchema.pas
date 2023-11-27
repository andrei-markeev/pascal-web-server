unit DbSchema;

{$longstrings on}

interface

uses
    sysutils, libmongoc, Escape;

type
    TWorkDays = array of shortstring;

    PPlaceOptions = ^TPlaceOptions;
    TPlaceOptions = record
        availableFromHour: integer;
        availableUntilHour: integer;
        altSchedule: TWorkDays;
        altScheduleLength: integer;
        altAvailableFromHour: integer;
        altAvailableUntilHour: integer;
        workStartHour: integer;
        workEndHour: integer;
    end;

    TOfficeLocation = record
        _id: string;
        tenantId: string;
        name: string;
        address: string;
        latitude: double;
        longitude: double;
        email: string;
        phone: string;
        notificationEmail: string;
        cateringOrderEmail: string;
        sendICalNotifications: boolean;
        options: PPlaceOptions;
    end;

function serializeOfficeLocation(ol: TOfficeLocation): string;
function parseOfficeLocationFromBson(doc: pbson_t): TOfficeLocation;

implementation

function FastConcat(a: array of string): string;
var
    i: integer;
    pos: integer;
    len: integer;
begin
    len := 0;
    for i := 0 to Length(a) - 1 do
        inc(len, Length(a[i]));

    SetLength(FastConcat, len);

    pos := 1;
    for i := 0 to Length(a) - 1 do
    begin
        len := Length(a[i]);
        Move(a[i, 1], FastConcat[pos], len);
        inc(pos, len);
    end;
end;

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

function serializePlaceOptions(po: TPlaceOptions): string;
var
    n: integer;
    size: integer;
    strings: array of string;
begin

    with po do
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
    end;

    serializePlaceOptions := FastConcat(strings);
end;

function serializeOfficeLocation(ol: TOfficeLocation): string;
var
    n: integer;
    size: integer;
    strings: array of string;
begin
    with ol do
    begin
        size := 12;
        if (latitude > 0) and (longitude > 0) then inc(size, 4);
        if Length(email) <> 0 then inc(size, 3);
        if Length(phone) <> 0 then inc(size, 3);
        if Length(notificationEmail) <> 0 then inc(size, 3);
        if Length(cateringOrderEmail) <> 0 then inc(size, 3);
        if options <> nil then inc(size, 2);
        SetLength(strings, size);

        strings[0] := '{"_id":"';
        strings[1] := _id;
        strings[2] := '","tenantId":"';
        strings[3] := tenantId;
        strings[4] := '","name":"';
        strings[5] := EscapeJsonString(name);
        strings[6] := '","address":"';
        strings[7] := EscapeJsonString(address);
        strings[8] := '"';

        n := 9;
        if (latitude > 0) and (longitude > 0) then
        begin
            strings[n] := ',"latitude":';
            strings[n + 1] := FloatToStr(latitude);
            strings[n + 2] := ',"longitude":';
            strings[n + 3] := FloatToStr(longitude);
            inc(n, 4);
        end;

        if email <> '' then
        begin
            strings[n] := ',"email":"';
            strings[n + 1] := EscapeJsonString(email);
            strings[n + 2] := '"';
            inc(n, 3)
        end;

        if phone <> '' then
        begin
            strings[n] := ',"phone":"';
            strings[n + 1] := EscapeJsonString(phone);
            strings[n + 2] := '"';
            inc(n, 3)
        end;

        if notificationEmail <> '' then
        begin
            strings[n] := ',"notificationEmail":"';
            strings[n + 1] := EscapeJsonString(notificationEmail);
            strings[n + 2] := '"';
            inc(n, 3)
        end;

        if cateringOrderEmail <> '' then
        begin
            strings[n] := ',"cateringOrderEmail":"';
            strings[n + 1] := EscapeJsonString(cateringOrderEmail);
            strings[n + 2] := '"';
            inc(n, 3)
        end;

        if options <> nil then
        begin
            strings[n] := ',"options":';
            strings[n + 1] := serializePlaceOptions(options^);
            inc(n, 2)
        end;

        strings[n] := ',"sendICalNotifications":';
        strings[n + 1] := BoolToStr(sendICalNotifications,'true','false');
        strings[n + 2] := '}';

        serializeOfficeLocation := FastConcat(strings);
    end;
end;

function parseWorkDaysFromBson(doc: pbson_t): TWorkDays;
begin
end;

function parsePlaceOptionsFromBson(iter: bson_iter_t): TPlaceOptions;
var
    key: shortstring;
begin
    FillChar(parsePlaceOptionsFromBson, SizeOf(TPlaceOptions), 0);
    while bson_iter_next(@iter) do
    begin
        key := bson_iter_key(@iter);
        case key of
            'availableFromHour': parsePlaceOptionsFromBson.availableFromHour := bson_iter_int32(@iter);
            'availableUntilHour': parsePlaceOptionsFromBson.availableUntilHour := bson_iter_int32(@iter);
            'altAvailableFromHour': parsePlaceOptionsFromBson.altAvailableFromHour := bson_iter_int32(@iter);
            'altAvailableUntilHour': parsePlaceOptionsFromBson.altAvailableUntilHour := bson_iter_int32(@iter);
            'workStartHour': parsePlaceOptionsFromBson.workStartHour := bson_iter_int32(@iter);
            'workEndHour': parsePlaceOptionsFromBson.workEndHour := bson_iter_int32(@iter);
            // TODO: altSchedule
        end;
    end;
end;

function parseOfficeLocationFromBson(doc: pbson_t): TOfficeLocation;
var
    iter: bson_iter_t;
    optionsIter: bson_iter_t;
    key: shortstring;
    bsonType: bson_type_t;
begin
    FillChar(parseOfficeLocationFromBson, SizeOf(TOfficeLocation), 0);
    if not bson_iter_init(@iter, doc) then exit;

    while bson_iter_next(@iter) do
    begin
        key := bson_iter_key(@iter);
        //bsonType := bson_iter_type(@iter);
        //WriteLn('key=', key, ' type=', HexStr(bsonType, 2));

        with parseOfficeLocationFromBson do
            case key of
                '_id': _id := bson_iter_utf8(@iter, nil);
                'tenantId': tenantId := bson_iter_utf8(@iter, nil);
                'name': name := bson_iter_utf8(@iter, nil);
                'address': address := bson_iter_utf8(@iter, nil);
                'email':
                    if bson_iter_type(@iter) = BSON_TYPE_NULL then
                        email := ''
                    else
                        email := bson_iter_utf8(@iter, nil);
                'phone':
                    if bson_iter_type(@iter) = BSON_TYPE_NULL then
                        phone := ''
                    else
                        phone := bson_iter_utf8(@iter, nil);
                'latitude':
                    if bson_iter_type(@iter) = BSON_TYPE_NULL then
                        latitude := 0
                    else
                        latitude := bson_iter_double(@iter);
                'longitude':
                    if bson_iter_type(@iter) = BSON_TYPE_NULL then
                        longitude := 0
                    else
                        longitude := bson_iter_double(@iter);
                'options':
                    if bson_iter_type(@iter) = BSON_TYPE_DOCUMENT then
                    begin
                        if bson_iter_recurse(@iter, @optionsIter) then
                            parsePlaceOptionsFromBson(optionsIter);
                    end
            end;
    end;
end;

end.
