unit UCanvasUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, UStack, URectUtils;

type

  TCanvasDrawState = record
    PColor: TColor;
    PStyle: TPenStyle;
    PWidth: integer;
    BColor: TColor;
    BStyle: TBrushStyle;
  end;
  __TStateSt = specialize TStack<TCanvasDrawState>;

//SS suffix means Save State
procedure LoadState(Canvas: TCanvas; State: TCanvasDrawState);
procedure SaveState(Canvas: TCanvas);
procedure RestoreState(Canvas: TCanvas);
procedure Fill(Color: TColor; Rect: TRect; Canvas: TCanvas);
procedure FillSS(Color: TColor; Rect: TRect; Canvas: TCanvas);
procedure Border(Color: TColor; Width: integer; Rect: TRect; Canvas: TCanvas);
procedure BorderSS(Color: TColor; Width: integer; Rect: TRect; Canvas: TCanvas);
function GetState(Canvas: TCanvas): TCanvasDrawState;
function Fit(Text: string; Rect: TRect; Canvas: TCanvas): boolean;
function ToState(PColor, BColor: TColor; PWidth: integer = 1;
  PStyle: TPenStyle = psSolid; BStyle: TBrushStyle = bsClear): TCanvasDrawState;

var
  __CStates: __TStateSt;

implementation

procedure LoadState(Canvas: TCanvas; State: TCanvasDrawState);
begin
  with Canvas do begin
    Pen.Color := State.PColor;
    Pen.Width := State.PWidth;
    Pen.Style := State.PStyle;
    Brush.Color := State.BColor;
    Brush.Style := State.BStyle;
  end;
end;

procedure SaveState(Canvas: TCanvas);
begin
  __CStates.Push(GetState(Canvas));
end;

procedure RestoreState(Canvas: TCanvas);
begin
  LoadState(Canvas, __CStates.Front);
  __CStates.Pop;
end;

procedure Fill(Color: TColor; Rect: TRect; Canvas: TCanvas);
begin
  Canvas.Pen.Color := Color;
  Canvas.Brush.Color := Color;
  Canvas.Rectangle(Rect);
end;

procedure FillSS(Color: TColor; Rect: TRect; Canvas: TCanvas);
begin
  SaveState(Canvas);
  Fill(Color, Rect, Canvas);
  RestoreState(Canvas);
end;

procedure Border(Color: TColor; Width: integer; Rect: TRect; Canvas: TCanvas);
begin
  Canvas.Pen.Color := Color;
  Canvas.Pen.Width := Width;
  Canvas.Brush.Style := bsClear;
  Canvas.Rectangle(Rect);
end;

procedure BorderSS(Color: TColor; Width: integer; Rect: TRect; Canvas: TCanvas);
begin
  SaveState(Canvas);
  Border(Color, Width, Rect, Canvas);
  RestoreState(Canvas);
end;

function GetState(Canvas: TCanvas): TCanvasDrawState;
begin
  with Result do begin
    PColor := Canvas.Pen.Color;
    PWidth := Canvas.Pen.Width;
    PStyle := Canvas.Pen.Style;
    BColor := Canvas.Brush.Color;
    BStyle := Canvas.Brush.Style;
  end;
end;

function Fit(Text: string; Rect: TRect; Canvas: TCanvas): boolean;
begin
  Exit(Canvas.TextWidth(Text) < Width(Rect));
end;

function ToState(PColor, BColor: TColor; PWidth: integer = 1;
  PStyle: TPenStyle = psSolid; BStyle: TBrushStyle = bsClear): TCanvasDrawState;
begin
  Result.PColor := PColor;
  Result.PWidth := PWidth;
  Result.PStyle := PStyle;
  Result.BColor := BColor;
  Result.BStyle := BStyle;
end;

initialization

  __CStates := __TStateSt.Create;

end.
