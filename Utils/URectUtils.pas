unit URectUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, UPointUtils;

function Squeeze(Rect: TRect; Amount: integer): TRect;
function Shift(Rect: TRect; Offset: TPoint): TRect;
function Width(Rect: TRect): integer;

implementation

function Squeeze(Rect: TRect; Amount: integer): TRect;
begin
  Rect.Right -= Amount;
  Rect.Left += Amount;
  Rect.Top += Amount;
  Rect.Bottom -= Amount;
  Exit(Rect);
end;

function Shift(Rect: TRect; Offset: TPoint): TRect;
begin
  Rect.TopLeft += Offset;
  Rect.BottomRight += Offset;
  Exit(Rect);
end;

function Width(Rect: TRect): integer;
begin
  Exit(Rect.Right - Rect.Left);
end;

end.

