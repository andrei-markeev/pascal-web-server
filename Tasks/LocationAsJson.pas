unit LocationAsJson;

{$mode objfpc}
{$longstrings on}

interface

uses
    sysutils, classes, lnet, libmongoc, TaskManager, DeskMeDb, OfficeLocation;

const
    CRLF = #13#10;

type
    TLocationAsJsonTask = class(TTask)
    public
        pool: TDeskMeDbPool;
        socket: TLSocket;
        location: TOfficeLocation;
        constructor Create(dbPool: TDeskMeDbPool; lSocket: TLSocket);
        destructor Destroy; override;
        procedure Execute; override;
        procedure Finalize; override;
    end;

implementation

constructor TLocationAsJsonTask.Create(dbPool: TDeskMeDbPool; lSocket: TLSocket);
begin
    pool := dbPool;
    socket := lSocket;
end;

destructor TLocationAsJsonTask.Destroy;
begin
    inherited;
end;

procedure TLocationAsJsonTask.Execute;
var
    db: TDeskMeDatabase;
    query: pbson_t;
begin
    db := TDeskMeDatabase.Create(pool);
    query := bson_new;
    bson_append_utf8(query, 'name', length('name'), 'Demo Office', length('Demo Office'));
    location := db.Locations.findOne(query);
    bson_destroy(query);
    db.Free;
end;

procedure TLocationAsJsonTask.Finalize;
var
    json: string;
    body: string;
begin
    if location = nil then
    begin
        if (status <> tsCancelled) and (socket.ConnectionStatus = scConnected) then
            socket.SendMessage('HTTP/1.1 404' + CRLF + 'Content-length: 0' + CRLF + CRLF);
        exit;
    end;

    json := location.Serialize;

    location.Free;

    body := 'HTTP/1.1 200' + CRLF
        + 'Content-type: application/json' + CRLF
        + 'Content-length: ' + IntToStr(Length(json)) + CRLF
        + CRLF
        + json;

    if (status <> tsCancelled) and (socket.ConnectionStatus = scConnected) then
        socket.SendMessage(body);
end;

end.