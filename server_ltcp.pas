program server_ltcp;

{$mode objfpc}
{$longstrings on}

uses
    {$ifdef UNIX}cthreads,{$endif}
    crt, sysutils, lnet, libmongoc, RequestHandler, DeskMeDb,
    OfficeLocation in 'DBSchema/OfficeLocation.pas',
    PlaceOptions in 'DBSchema/PlaceOptions.pas';

var
    pool: TDeskMeDbPool;

function Max(a, b: integer): integer;
begin
    if a > b then Max := a else Max := b;
end;
function Min(a, b: integer): integer;
begin
    if a < b then Min := a else Min := b;
end;

procedure ProcessRequest(request: TRequest; socket: TLSocket);
var
    html: string;
    json: string;
    body: string;
    db: TDeskMeDatabase;
    officeLocation: TOfficeLocation;
    query: pbson_t;
    p: integer;
begin
    case request.url of
        '/hello':
        begin
            html :=
                '<!DOCTYPE html><html><h1>Hello worlde!</h1></html>';

            body := 'HTTP/1.1 200' + CRLF
                + 'Content-type: text/html' + CRLF
                + 'Content-length: ' + IntToStr(Length(html)) + CRLF
                + CRLF
                + html;

            socket.SendMessage(body);
        end;
        '/json':
        begin
            officeLocation := TOfficeLocation.Create;

            with officeLocation do
            begin
                _id := 'id12356';
                tenantId := 'tenant12346';
                name := 'New \" location""';
                address := 'Very Long Address String that contains a lot of information and even instructions, which might be relevant but might not be relevant - who knows, but they''re still there, because the company is trying to use address field for providing additional information in the notificaiton emails.';
                email := 'newLocation@myCompany.com';
                latitude := 12.34;
                longitude := 43.21;
                sendICalNotifications := false;
                notificationEmail := '"notify'#13#10'@myCompany.com"';
                options := TPlaceOptions.Create;
                with options do
                begin
                    availableFromHour := 8;
                    availableUntilHour := 17;
                    altSchedule := ['Sat', 'Sun'];
                    altAvailableFromHour := 10;
                    altAvailableUntilHour := 15;
                end;
            end;

            json := officeLocation.Serialize;

            officeLocation.Free;

            body := 'HTTP/1.1 200' + CRLF
                + 'Content-type: application/json' + CRLF
                + 'Content-length: ' + IntToStr(Length(json)) + CRLF
                + CRLF
                + json;

            socket.SendMessage(body);
        end;
        '/mongo':
        begin
            db := TDeskMeDatabase.Create(pool);
            query := bson_new;
            bson_append_utf8(query, 'name', length('name'), 'Demo location', length('Demo location'));
            officeLocation := db.Locations.findOne(query);
            bson_destroy(query);
            db.Free;

            if officeLocation = nil then
            begin
                socket.SendMessage('HTTP/1.1 404' + CRLF + 'Content-length: 0' + CRLF + CRLF);
                exit;
            end;

            json := officeLocation.Serialize;

            officeLocation.Free;

            body := 'HTTP/1.1 200' + CRLF
                + 'Content-type: application/json' + CRLF
                + 'Content-length: ' + IntToStr(Length(json)) + CRLF
                + CRLF
                + json;

            socket.SendMessage(body);
        end;
        '/mongohtml':
        begin
            db := TDeskMeDatabase.Create(pool);
            query := bson_new;
            bson_append_utf8(query, 'name', length('name'), 'Demo location', length('Demo location'));
            officeLocation := db.Locations.findOne(query);
            bson_destroy(query);
            db.Free;

            if officeLocation = nil then
            begin
                socket.SendMessage('HTTP/1.1 404' + CRLF + 'Content-length: 0' + CRLF + CRLF);
                exit;
            end;

            html := '<!DOCTYPE html><html><h1>' + officeLocation.name + '</h1><p>This location''s address is: ' + officeLocation.address + '</p></html>';

            officeLocation.Free;

            body := 'HTTP/1.1 200' + CRLF
                + 'Content-type: text/html' + CRLF
                + 'Content-length: ' + IntToStr(Length(html)) + CRLF
                + CRLF
                + html;

            socket.SendMessage(body);
        end;
        '/ipsum':
        begin
            html := '<html><body><h1>Ipsum finder</h1><ol>';
            p := 1;
            repeat
                p := pos('ipsum', request.body, p + 1);
                if p > 0 then
                    html := html + '<li>Found "ipsum" at ' + IntToStr(p) + ', near "' + copy(request.body, Max(1, p - 15), Min(25, Length(request.body))) + '</li>';
            until p = 0;
            html := html + '</ol></body></html>';

            body := 'HTTP/1.1 200' + CRLF
                + 'Content-type: text/html' + CRLF
                + 'Content-length: ' + IntToStr(Length(html)) + CRLF
                + CRLF
                + html;

            socket.SendMessage(body);
        end;
    else
        socket.SendMessage('HTTP/1.1 404' + CRLF + 'Content-length: 0' + CRLF + CRLF);
    end;
end;

var
    canQuit: Boolean = false;
    handler: TRequestHandler;
    server: TLTcp;
begin

    handler := TRequestHandler.Create;
    handler.OnRequestParsed := @ProcessRequest;

    server := TLTcp.Create(nil);
    server.OnError := @handler.Error;
    server.OnReceive := @handler.Receive;
    server.Timeout := 1000;

    if not server.Listen(3000) then exit else Writeln('Listening on port 3000');

    pool := TDeskMeDbPool.Create;

    repeat
        server.CallAction;
    until canQuit or KeyPressed;

    pool.Free;
    server.Free;
    handler.Free;

end.
