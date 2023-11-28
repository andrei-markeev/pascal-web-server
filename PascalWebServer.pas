unit PascalWebServer;

{$mode objfpc}
{$longstrings on}

interface

uses
    crt, classes, lnet, RequestHandler, TaskManager, MongoDbPool;

type
    TProcessRequestEvent = procedure(request: TRequest; socket: TLSocket);

    TPascalWebServer = class
    private
        handler: TRequestHandler;
        asyncTaskManager: TTaskManager;
        tcpServer: TLTcp;
    public
        pool: TMongoDbPool;
        constructor Create(ProcessRequest: TProcessRequestEvent);
        destructor Destroy; override;
        procedure Listen(port: integer);
        procedure EnqueueTask(task: TTask);
    end;

implementation

constructor TPascalWebServer.Create(ProcessRequest: TProcessRequestEvent);
begin
    handler := TRequestHandler.Create;
    handler.OnRequestParsed := ProcessRequest;

    asyncTaskManager := TTaskManager.Create;

    tcpServer := TLTcp.Create(nil);
    tcpServer.OnError := @handler.Error;
    tcpServer.OnReceive := @handler.Receive;
    tcpServer.OnDisconnect := @asyncTaskManager.SocketDisconnected;
    tcpServer.Timeout := 25;
end;

procedure TPascalWebServer.Listen(port: integer);
var
    canQuit: Boolean = false;
begin
    if not tcpServer.Listen(port) then exit else Writeln('Listening on port 3000');

    pool := TMongoDbPool.Create;

    repeat
        tcpServer.CallAction;
        asyncTaskManager.FinalizeTasks;
    until canQuit or KeyPressed;
end;

procedure TPascalWebServer.EnqueueTask(task: TTask);
begin
    asyncTaskManager.Enqueue(task);
end;

destructor TPascalWebServer.Destroy;
begin
    asyncTaskManager.Free;
    pool.Free;
    tcpServer.Free;
    handler.Free;
end;

end.