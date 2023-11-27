unit RequestHandler;

{$mode objfpc}
{$longstrings on}
{$T-}

interface

uses classes, lnet, sysutils;

const
    CRLF = #13#10;
    BUFFER_SIZE_BYTES = 8096;
    MAX_BODY_SIZE_BYTES = 65536;

type
    TBufferPos = -1..BUFFER_SIZE_BYTES;
    THeader = record
        name: string;
        value: string;
    end;

    TRequest = record
        state: (reqParsingStart, reqParsingHeaders, reqReadingBody, reqFinalized);
        url: string;
        method: (methodUnknown, methodGET, methodPOST, methodPUT, methodPATCH, methodDELETE);
        contentLength: integer;
        body: string;
        bodyBytesRead: integer;
        headers: array of THeader;
    end;

    TRequestParsedEvent = procedure(request: TRequest; socket: TLSocket);

    TRequestHandler = class
    private
        buffer: array [0..BUFFER_SIZE_BYTES] of char;
        parseFrom: TBufferPos;
        freeFrom: TBufferPos;
        request: TRequest;
        requestParsedCallback: TRequestParsedEvent;
        procedure ParseBuffer(socket: TLSocket);
    public
        constructor Create;
        procedure Error(const msg: string; socket:TLSocket);
        procedure Receive(socket: TLSocket);
        property OnRequestParsed: TRequestParsedEvent read requestParsedCallback write requestParsedCallback;
    end;

implementation

constructor TRequestHandler.Create;
begin
    parseFrom := 0;
    freeFrom := 0;
    request.state := reqParsingStart;
    request.contentLength := 0;
    request.bodyBytesRead := 0;
end;

procedure TRequestHandler.Error(const msg: string; socket:TLSocket);
begin
    WriteLn('TRequestHandler.Error ', msg);
end;

procedure TRequestHandler.Receive(socket: TLSocket);
var
    availableSize: TBufferPos;
    bytesRead: TBufferPos;
begin
    repeat

        if parseFrom > 0 then
        begin
            move(buffer[parseFrom], buffer, freeFrom - parseFrom);
            dec(freeFrom, parseFrom);
            parseFrom := 0;
        end;

        availableSize := BUFFER_SIZE_BYTES - freeFrom;

        if availableSize > 0 then
        begin
            bytesRead := socket.Get(buffer[freeFrom], availableSize);
            if bytesRead = 0 then exit;
            inc(freeFrom, bytesRead);
        end;

        ParseBuffer(socket);

    until (bytesRead < availableSize) and (parseFrom = freeFrom) or (i > 10);
end;

procedure TRequestHandler.ParseBuffer(socket: TLSocket);
var
    i: TBufferPos;
    colonPos: TBufferPos;
    firstSpacePos: integer;
    secondSpacePos: integer;
    lastCharWasSpace: boolean;
    method: string;
    headerName: string;
    headerValue: string;
    headersLength: integer;
    needToRead: integer;
begin

    if request.state = reqReadingBody then
    begin

        if request.contentLength - request.bodyBytesRead < freeFrom - parseFrom then
            needToRead := request.contentLength - request.bodyBytesRead
        else
            needToRead := freeFrom - parseFrom;

        Move(buffer[parseFrom], request.body[request.bodyBytesRead], needToRead - 1);
        inc(request.bodyBytesRead, needToRead);
        inc(parseFrom, needToRead);

        if request.bodyBytesRead >= request.contentLength then
        begin
            if assigned(requestParsedCallback) then
                requestParsedCallback(request, socket);
            request.state := reqParsingStart;
            request.contentLength := 0;
            request.bodyBytesRead := 0;
            SetLength(request.headers, 0);
        end;

        exit;
    end;

    headersLength := Length(request.headers);

    colonPos := -1;
    firstSpacePos := -1;
    secondSpacePos := -1;
    lastCharWasSpace := false;

    for i := parseFrom to freeFrom - 1 do
    begin
        if (buffer[i] = ':') and (colonPos = -1) then
            colonPos := i;

        if buffer[i] = ' ' then
        begin
            if firstSpacePos = -1 then firstSpacePos := i
            else
                if not lastCharWasSpace and (secondSpacePos = -1) then secondSpacePos := i;
            lastCharWasSpace := true;
        end
        else
            lastCharWasSpace := false;

        if (buffer[i] = #10) and (i > 0) and (buffer[i - 1] = #13) then
        begin
            case request.state of

                reqParsingStart:
                begin
                    if (firstSpacePos = -1) or (secondSpacePos = -1) then
                    begin
                        request.state := reqParsingStart;
                        socket.SendMessage('HTTP/1.0 400 Bad Request'#13#10);
                        socket.Disconnect(true);
                        exit;
                    end;

                    SetString(method, PChar(@buffer[parseFrom]), firstSpacePos);
                    case UpperCase(method) of
                        'GET': request.method := methodGET;
                        'POST': request.method := methodPOST;
                        'PUT': request.method := methodPUT;
                        'PATCH': request.method := methodPATCH;
                        'DELETE': request.method := methodDELETE;
                    else
                        request.method := methodUnknown;
                    end;
                    SetString(request.url, PChar(@buffer[firstSpacePos + 1]), secondSpacePos - (firstSpacePos + 1));
                    request.state := reqParsingHeaders;
                end;

                reqParsingHeaders:
                begin
                    if i - parseFrom = 1 then
                    begin
                        if request.contentLength < freeFrom - i + 1 then
                        begin
                            SetString(request.body, PChar(@buffer[i + 1]), request.contentLength);
                            if assigned(requestParsedCallback) then
                                requestParsedCallback(request, socket);
                            request.state := reqParsingStart;
                            request.contentLength := 0;
                            SetLength(request.headers, 0);
                            parseFrom := i + 1;
                            exit;
                        end
                        else
                        begin
                            SetLength(request.body, request.contentLength);
                            Move(buffer[i + 1], request.body[1], freeFrom - (i + 1));
                            request.bodyBytesRead := freeFrom - (i + 1);
                            request.state := reqReadingBody;
                            parseFrom := freeFrom;
                            exit;
                        end;
                    end
                    else if colonPos > -1 then
                    begin
                        SetString(headerName, PChar(@buffer[parseFrom]), colonPos - parseFrom);
                        SetString(headerValue, PChar(@buffer[colonPos + 1]), i - colonPos - 2);
                        if (LowerCase(headerName) = 'content-length') then
                        begin
                            request.contentLength := StrToIntDef(headerValue, 0);
                            if (request.contentLength > MAX_BODY_SIZE_BYTES) then
                            begin
                                request.state := reqParsingStart;
                                request.contentLength := 0;
                                SetLength(request.headers, 0);
                                socket.SendMessage('HTTP/1.0 417 Entity Too Large'#13#10);
                                socket.Disconnect(true);
                                exit;
                            end;
                        end;

                        SetLength(request.headers, headersLength + 1);
                        with request.headers[headersLength] do
                        begin
                            name := headerName;
                            value := headerValue;
                        end;
                        inc(headersLength);

                    end;
                end;

            end;
            parseFrom := i + 1;
            colonPos := -1;
            firstSpacePos := -1;
            secondSpacePos := -1;
        end;

    end;
end;

end.
