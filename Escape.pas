unit Escape;

interface

function EscapeJsonString(s: ansistring): ansistring;

implementation

const
    HEX_CHARS: array[0..15] of char = '0123456789ABCDEF';

function EscapeJsonString(s: ansistring): ansistring;
var
    i, j: integer;
    savedPos: integer;
    len: integer;
    resLen: integer;
    ch: char;
    ordCh: byte;
    bufPos: integer;
    size: integer;
begin
    savedPos := 1;
    bufPos := 1;
    len := Length(s);
    resLen := len;

    for i := 1 to len do
        case s[i] of 
            '"', '\', #0..#31: if (s[i] in ['"', '\', #8, #9, #10, #12, #13]) then inc(resLen) else inc(resLen, 5);
        end;
    if len = resLen then exit;

    SetLength(EscapeJsonString, resLen);

    i := 1;
    while i <= len do
    begin
        if s[i] in ['"', '\', #0..#31] then
        begin
            size := i - savedPos;
            if size > 0 then
            begin
                Move(s[savedPos], EscapeJsonString[bufPos], size);
                inc(bufPos, size);
            end;

            ch := s[i];
            case ch of
                '\': begin EscapeJsonString[bufPos] := '\'; EscapeJsonString[bufPos + 1] := '\'; inc(bufPos, 2); end;
                '"': begin EscapeJsonString[bufPos] := '\'; EscapeJsonString[bufPos + 1] := '"'; inc(bufPos, 2); end;
                #8: begin EscapeJsonString[bufPos] := '\'; EscapeJsonString[bufPos + 1] := 'b'; inc(bufPos, 2); end;
                #9: begin EscapeJsonString[bufPos] := '\'; EscapeJsonString[bufPos + 1] := 't'; inc(bufPos, 2); end;
                #10: begin EscapeJsonString[bufPos] := '\'; EscapeJsonString[bufPos + 1] := 'n'; inc(bufPos, 2); end;
                #12: begin EscapeJsonString[bufPos] := '\'; EscapeJsonString[bufPos + 1] := 'f'; inc(bufPos, 2); end;
                #13: begin EscapeJsonString[bufPos] := '\'; EscapeJsonString[bufPos + 1] := 'r'; inc(bufPos, 2); end;
            else
                EscapeJsonString[bufPos] := '\';
                EscapeJsonString[bufPos + 1] := 'u';
                inc(bufPos, 6);
                ordCh := Ord(ch);
                for j := 1 to 4 do
                begin
                    EscapeJsonString[bufPos - j] := HEX_CHARS[ordCh and $f];
                    ordCh := ordCh shr 4;
                end;
            end;
            savedPos := i + 1;
        end;
        inc(i);
    end;

    Move(s[savedPos], EscapeJsonString[bufPos], i - savedPos);

end;

end.