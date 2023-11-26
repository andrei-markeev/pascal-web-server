program server_syn;

{$ifdef fpc}
  {$mode delphi}
{$endif}

{$apptype console}

uses
  sysutils, blcksock, Synautil;

procedure AttendConnection(ASocket: TTCPBlockSocket);
var
  timeout: integer;
  s: string;
  method, uri: string;
  html: string;
begin
  timeout := 120000;

  //read request line
  s := ASocket.RecvString(timeout);
  //WriteLn(s);
  method := fetch(s, ' ');
  uri := fetch(s, ' ');
  //protocol := fetch(s, ' ');

  //read request headers
  repeat
    s := ASocket.RecvString(Timeout);
    //WriteLn(s);
  until s = '';

  // Now write the document to the output stream

  if uri = '/test1' then
  begin

    // Write the output document to the stream
    html :=
      '<!DOCTYPE html>' + CRLF
      + '<html><h1>Hello worlde!</h1></html>' + CRLF;

    // Write the headers back to the client
    ASocket.SendString('HTTP/1.0 200' + CRLF);
    ASocket.SendString('Content-type: Text/Html' + CRLF);
    ASocket.SendString('Content-length: ' + IntToStr(Length(html)) + CRLF);
    ASocket.SendString('Connection: close' + CRLF);
    ASocket.SendString('Server: Servidor do Felipe usando Synapse' + CRLF);
    ASocket.SendString('' + CRLF);

  //  if ASocket.lasterror <> 0 then HandleError;

    // Write the document back to the browser
    ASocket.SendString(html);
  end
  else
    ASocket.SendString('HTTP/1.0 404' + CRLF);
end;

var
  ListenerSocket, ConnectionSocket: TTCPBlockSocket;

begin
  ListenerSocket := TTCPBlockSocket.Create;
  ConnectionSocket := TTCPBlockSocket.Create;

  ListenerSocket.CreateSocket;
  ListenerSocket.setLinger(true,10);
  ListenerSocket.bind('0.0.0.0','3000');
  ListenerSocket.listen;

  repeat
    if ListenerSocket.CanRead(1000) then
    begin
      ConnectionSocket.Socket := ListenerSocket.accept;
      AttendConnection(ConnectionSocket);
      ConnectionSocket.CloseSocket;
    end;
  until false;

  ListenerSocket.Free;
  ConnectionSocket.Free;
end.
