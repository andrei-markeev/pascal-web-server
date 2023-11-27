unit OfficeLocation;

{$mode objfpc}
{$longstrings on}

interface

uses
    sysutils, libmongoc, classes,
    Escape in 'Utils/Escape.pas',
    FastConcat in 'Utils/FastConcat.pas',
    PlaceOptions in 'DBSchema/PlaceOptions.pas';

type
    TOfficeLocation = class
    public
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
        options: TPlaceOptions;

        constructor Create;
        constructor Create(doc: pbson_t);
        destructor Destroy; override;
    	function Serialize: string;
    end;


implementation

constructor TOfficeLocation.Create;
begin
end;

destructor TOfficeLocation.Destroy;
begin
    if options <> nil then
        options.Free;
end;

constructor TOfficeLocation.Create(doc: pbson_t);
var
    iter: bson_iter_t;
    optionsIter: bson_iter_t;
    key: shortstring;
begin
    if not bson_iter_init(@iter, doc) then exit;

    while bson_iter_next(@iter) do
    begin
        key := bson_iter_key(@iter);

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
                        options := TPlaceOptions.Create(optionsIter);
                end
        end;
    end;
end;

function TOfficeLocation.Serialize: string;
var
    n: integer;
    size: integer;
    strings: array of string;
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
        strings[n + 1] := options.Serialize;
        inc(n, 2)
    end;

    strings[n] := ',"sendICalNotifications":';
    strings[n + 1] := BoolToStr(sendICalNotifications,'true','false');
    strings[n + 2] := '}';

    Serialize := FastConcat.Join(strings);
end;

end.
