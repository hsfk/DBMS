unit UPointUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  NULLP: TPoint = (X: 0; Y: 0);
  MINP: TPoint = (X: -2147483648; Y: -2147483648);
  MAXP: TPoint = (X: 2147483647; Y: 2147483647);

type
  TPoints = array of TPoint;

operator +(a, b: TPoint): TPoint; overload; inline;
operator +(a: TPoint; b: integer): TPoint; overload; inline;
operator -(a, b: TPoint): TPoint; overload; inline;
operator -(a: TPoint; b: integer): TPoint; overload; inline;
operator / (a: TPoint; b: integer): TPoint; overload; inline;
operator * (a: TPoint; b: integer): TPoint; overload; inline;
operator >= (a, b: TPoint): boolean; overload; inline;
operator <= (a, b: TPoint): boolean; overload; inline;
function PointInRect(Point: TPoint; Rect: TRect): boolean; inline;
function ToPoint(x, y: integer): TPoint; inline;
function Distance(a, b: TPoint): double; inline;

implementation

operator +(a, b: TPoint): TPoint; overload; inline;
begin
  Result.x := a.x + b.x;
  Result.y := a.y + b.y;
end;

operator +(a: TPoint; b: integer): TPoint; overload; inline;
begin
  Result.x := a.x + b;
  Result.y := a.y + b;
end;

operator -(a, b: TPoint): TPoint; overload; inline;
begin
  Result.x := a.x - b.x;
  Result.y := a.y - b.y;
end;

operator -(a: TPoint; b: integer): TPoint; overload; inline;
begin
  Result.x := a.x - b;
  Result.y := a.y - b;
end;

operator / (a: TPoint; b: integer): TPoint; overload; inline;
begin
  Result.x := a.x div b;
  Result.y := a.y div b;
end;

operator * (a: TPoint; b: integer): TPoint; overload; inline;
begin
  Result.x := a.x * b;
  Result.y := a.y * b;
end;

operator >= (a, b: TPoint): boolean; overload; inline;
begin
  Result := False;
  if (a.x >= b.x) and (a.y >= b.y) then
    Result := True;
end;

operator <= (a, b: TPoint): boolean; overload; inline;
begin
  Result := False;
  if (a.x <= b.x) and (a.y <= b.y) then
    Result := True;
end;

function PointInRect(Point: TPoint; Rect: TRect): boolean; inline;
begin
  if (Point.x < Rect.Right) and (Point.x > Rect.Left) then
    if (Point.y > Rect.Top) and (Point.y < Rect.Bottom) then
      Exit(True);
  Exit(False);
end;

function ToPoint(x, y: integer): TPoint; inline;
begin
  Result.x := x;
  Result.y := y;
end;

function Distance(a, b: TPoint): double; inline;
var
  dx: integer;
  dy: integer;
begin
  dx := Abs(a.x - b.x);
  dy := Abs(a.y - B.y);
  Exit(Sqrt(dx * dx + dy * dy));
end;

end.
