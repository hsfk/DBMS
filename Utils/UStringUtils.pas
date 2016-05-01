unit UStringUtils;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes;

procedure DeleteLastSymbols(var AString: string; n: integer);
procedure DeleteFirstSymbols(var AString: string; n: integer);
function Param(Index: integer): string;
function FindInd(Str: string; Strings: TStrings): integer;

implementation

function Param(Index: integer): string;
begin
  Exit('Param' + IntToStr(Index));
end;

function FindInd(Str: string; Strings: TStrings): integer;
var
  i: integer;
begin
  for i := 0 to Strings.Count - 1 do
    if Str = Strings[i] then
      Exit(i);
  Exit(-1);
end;

procedure DeleteLastSymbols(var AString: string; n: integer);
begin
  SetLength(AString, Length(AString) - n);
end;

procedure DeleteFirstSymbols(var AString: string; n: integer);
begin
  AString := Copy(AString, n + 1, Length(AString) - n);
end;

end.

