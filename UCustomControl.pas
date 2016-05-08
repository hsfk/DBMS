unit UCustomControl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, StdCtrls, Controls, UVector;

type
  TEvent = procedure of object;
  TParamEvent = procedure(ControlIndex: integer) of object;
  TCustomControlType = class of TCustomControl;

  TCustomControl = class(TPanel)
  private
    FIndex: integer;
    FDelBtn: TButton;
    FOnDelete: TParamEvent;
  protected
    procedure InitComponent(Component, AParent: TWinControl;
      ATop, ALeft, AWidth: integer);
    procedure FDelBtnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer); virtual;
  public
    constructor Create; virtual;
    constructor Create(AParent: TWinControl; AWidth, ATop, ALeft: integer); virtual;
  published
    property OnDelete: TParamEvent write FOnDelete;
    property Index: integer read FIndex write FIndex;
    property DelBtn: TButton read FDelBtn write FDelBtn;
  end;

  generic TCustomControls<T: TCustomControl> = class(specialize TObjVector<T>)
  private
    FParent: TWinControl;
    FTop: integer;
    FLeft: integer;
    FYOffset: integer;
    FOnDelete: TEvent;
    //this procedure is called by deleted control panel
    procedure OnControlDelete(ControlIndex: integer);
    procedure UpdateControls;
  public
    constructor Create(AParent: TWinControl; ATop, ALeft: integer); virtual;
    procedure DeleteAll; virtual;
    procedure AddControlPanel(CustomControl: T); virtual;
  published
    property OnDelete: TEvent write FOnDelete;
    property Parent: TWinControl read FParent;
    property Top: integer read FTop;
    property Left: integer read FLeft;
  end;

implementation

procedure TCustomControl.InitComponent(Component, AParent: TWinControl;
  ATop, ALeft, AWidth: integer);
begin
  Component.Parent := AParent;
  Component.Top := ATop;
  Component.Left := ALeft;
  Component.Width := AWidth;
end;

procedure TCustomControl.FDelBtnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if FOnDelete <> nil then
    if FIndex <> -1 then
      FOnDelete(FIndex);
  Self.Free;
end;

constructor TCustomControl.Create;
begin
  FIndex := -1;
  FOnDelete := nil;
end;

constructor TCustomControl.Create(AParent: TWinControl; AWidth, ATop, ALeft: integer);
begin
  Create;
  inherited Create(AParent);
  InitComponent(Self, AParent, ATop - 1, ALeft, AWidth);
  Self.Height := 25;
  Self.BevelInner := bvNone;
  Self.BevelOuter := bvNone;

  FDelBtn := TButton.Create(Self);
  InitComponent(FDelBtn, Self, ATop, Left + Width, 20);
  FDelBtn.OnMouseUp := @FDelBtnMouseUp;
  FDelBtn.Caption := 'X';
end;

procedure TCustomControls.OnControlDelete(ControlIndex: integer);
begin
  if Size > 0 then begin
    DeleteIndS(ControlIndex);
    UpdateControls;
    if FOnDelete <> nil then
      FOnDelete;
  end;
end;

procedure TCustomControls.UpdateControls;
var
  i: integer;
begin
  FYOffset := 0;
  for i := 0 to Size - 1 do begin
    FYOffset := i * Items[i].Height;
    Items[i].Top := FYOffset;
    Items[i].Index := i;
  end;
end;

constructor TCustomControls.Create(AParent: TWinControl; ATop, ALeft: integer);
begin
  FTop := ATop;
  FLeft := ALeft;
  FParent := AParent;
  FYOffset := 0;
  FOnDelete := nil;
end;

procedure TCustomControls.DeleteAll;
var
  i: integer = 0;
  Amount: integer;
begin
  if Size > 0 then begin
    Amount := Size;
    while i < Amount do begin
      if Items[i].Enabled then begin
        Items[i].Free;
        DeleteIndS(i);
        Amount -= 1;
      end
      else
        i += 1;
    end;
    FYOffset := 0;
    if FOnDelete <> nil then
      FOnDelete;
  end;
end;

procedure TCustomControls.AddControlPanel(CustomControl: T);
begin
  FYOffset := Size * CustomControl.Height;
  CustomControl.Index := Size;
  CustomControl.OnDelete := @OnControlDelete;
  CustomControl.Top := CustomControl.Top + FYOffset;
  PushBack(CustomControl);
end;

end.
