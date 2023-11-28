unit LocationAsHtml;

{$mode objfpc}
{$longstrings on}

interface

uses
    sysutils, classes, lnet, libmongoc, TaskManager, MongoDbPool, MyDb, OfficeLocation;

const
    CRLF = #13#10;

type
    TLocationAsHtmlTask = class(TTask)
    public
        pool: TMongoDbPool;
        socket: TLSocket;
        location: TOfficeLocation;
        constructor Create(dbPool: TMongoDbPool; lSocket: TLSocket);
        destructor Destroy; override;
        procedure Execute; override;
        procedure Finalize; override;
    end;

implementation

constructor TLocationAsHtmlTask.Create(dbPool: TMongoDbPool; lSocket: TLSocket);
begin
    inherited Create(lSocket);

    pool := dbPool;
    socket := lSocket;
end;

destructor TLocationAsHtmlTask.Destroy;
begin
    inherited;
end;

procedure TLocationAsHtmlTask.Execute;
var
    db: TMyDatabase;
    query: pbson_t;
begin
    db := TMyDatabase.Create(pool);
    query := bson_new;
    bson_append_utf8(query, 'name', length('name'), 'Demo Office', length('Demo Office'));
    location := db.Locations.findOne(query);
    bson_destroy(query);
    db.Free;
end;

procedure TLocationAsHtmlTask.Finalize;
var
    html: string;
    body: string;
begin
    if location = nil then
    begin
        if (status <> tsCancelled) and (socket.ConnectionStatus = scConnected) then
            socket.SendMessage('HTTP/1.1 404' + CRLF + 'Content-length: 0' + CRLF + CRLF);
        exit;
    end;

    html := '<!DOCTYPE html><html><h1>' + location.name + '</h1><p>This location''s address is: ' + location.address + '</p></html>';

    location.Free;

    body := 'HTTP/1.1 200' + CRLF
        + 'Content-type: text/html' + CRLF
        + 'Content-length: ' + IntToStr(Length(html)) + CRLF
        + CRLF
        + html;

    if (status <> tsCancelled) and (socket.ConnectionStatus = scConnected) then
        socket.SendMessage(body);

end;

end.