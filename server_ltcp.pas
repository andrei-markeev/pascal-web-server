program server_ltcp;

{$longstrings on}

uses
    crt, sysutils, lnet, libmongoc, RequestHandler, DbSchema, DeskMeDb;

var
    pool: TDeskDbPool;

procedure ProcessRequest(request: TRequest; socket: TLSocket);
var
    html: string;
    json: string;
    body: string;
    db: TDeskMeDatabase;
    officeLocation: TOfficeLocation;
    query: pbson_t;
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
            FillChar(officeLocation, SizeOf(officeLocation), 0);
            with officeLocation do
            begin
                _id := 'id12356';
                tenantId := 'tenant12346';
                name := 'New \" location""';
                address := 'Very Long Address String that contains a lot of information and even instructions, which might be relevant but might not be relevant - who knows, but they''re still there, because the company is trying to use address field for providing additional information in the notificaiton emails.';
                email := 'newLocation@myCompany.com';
                latitude := 1234;
                longitude := 4321;
                sendICalNotifications := false;
                notificationEmail := '"notify'#13#10'@myCompany.com"';
                with options do
                begin
                    availableFromHour := 8;
                    availableUntilHour := 17;
                    altSchedule := ['Sat', 'Sun'];
                    altAvailableFromHour := 10;
                    altAvailableUntilHour := 15;
                end;
            end;

            json := serializeOfficeLocation(officeLocation);

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

            json := serializeOfficeLocation(officeLocation);

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

            html := serializeOfficeLocation(officeLocation);

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

    pool := TDeskDbPool.Create;

    repeat
        server.CallAction;
    until canQuit or KeyPressed;

    pool.Free;
    server.Free;
    handler.Free;

end.
