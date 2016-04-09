unit UStringUtils;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

function Param(Index: integer): string;
procedure DeleteLastSymbols(var AString: string; n: integer);
procedure DeleteFirstSymbols(var AString: string; n: integer);

implementation

function Param(Index: integer): string;
begin
  Exit('Param' + IntToStr(Index));
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

