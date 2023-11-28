program server_ltcp;

{$mode objfpc}
{$longstrings on}

uses
    {$ifdef UNIX}cthreads,{$endif}
    crt, sysutils, classes, lnet, libmongoc,
    RequestHandler, DeskMeDb, TaskManager,
    OfficeLocation in 'DBSchema/OfficeLocation.pas',
    PlaceOptions in 'DBSchema/PlaceOptions.pas',
    LocationAsJson in 'Tasks/LocationAsJson.pas',
    LocationAsHtml in 'Tasks/LocationAsHtml.pas';


var
    pool: TDeskMeDbPool;
    asyncTaskManager: TTaskManager;

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
    officeLocation: TOfficeLocation;
    p: integer;
    task: TTask;
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
            task := TLocationAsJsonTask.Create(pool, socket);
            socket.UserData := task;
            asyncTaskManager.Enqueue(task);
        end;
        '/mongohtml':
        begin
            task := TLocationAsHtmlTask.Create(pool, socket);
            socket.UserData := task;
            asyncTaskManager.Enqueue(task);
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

    asyncTaskManager := TTaskManager.Create;

    server := TLTcp.Create(nil);
    server.OnError := @handler.Error;
    server.OnReceive := @handler.Receive;
    server.OnDisconnect := @asyncTaskManager.SocketDisconnected;
    server.Timeout := 25;

    if not server.Listen(3000) then exit else Writeln('Listening on port 3000');

    pool := TDeskMeDbPool.Create;

    repeat
        server.CallAction;
        asyncTaskManager.FinalizeTasks;
    until canQuit or KeyPressed;

    asyncTaskManager.Free;
    pool.Free;
    server.Free;
    handler.Free;

end.
