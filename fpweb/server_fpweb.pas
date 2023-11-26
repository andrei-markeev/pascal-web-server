program server_fpweb;

{$mode objfpc}{$H+}
{$define UseCThreads}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  sysutils, Classes, fphttpserver;

Type

  TTestHTTPServer = Class(TFPHTTPServer)
  public
    procedure HandleRequest(var ARequest: TFPHTTPConnectionRequest;
                            var AResponse : TFPHTTPConnectionResponse); override;
  end;

procedure TTestHTTPServer.HandleRequest(var ARequest: TFPHTTPConnectionRequest; var AResponse: TFPHTTPConnectionResponse);
var
    html: string;
begin
  if ARequest.Url = '/test1' then
    begin
      html := '<!DOCTYPE html><html><h1>Hello worlde!</h1></html>';
      AResponse.ContentLength:=Length(html);
      AResponse.Content := html;
      AResponse.SendContent;
    end
  else
    begin
        AResponse.Code:=404;
        AResponse.ContentLength:=0;
        AResponse.SendContent;
    end;
end;

var
    Serv: TTestHTTPServer;

begin
    Serv:=TTestHTTPServer.Create(Nil);
    try
        Serv.Port:=3000;
        Serv.AcceptIdleTimeout:=10;
        // with threaded = true, this server has a very high resource
        // consumption because it has to create a thread for every request
        // with threaded = false, it becomes blocking i.e. it can handle
        // only one request at a time, which is even slower
        Serv.Threaded:=True;
        Serv.Active:=True;
    finally
        Serv.Free;
    end;
end.
