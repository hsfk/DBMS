unit UCardControls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Controls, DB, UDBObjects, UVector;

type

  TControls = specialize TVector<TDBControl>;

  TDBEditControl = class(TDBControl)
  private
    FID: integer;
    FEdit: TEdit;
  protected
    function GetData: TParam; override;
    procedure SetCaption(AData: string); override;
  public
    function UpdateTable: TQueryContainer; override;
    procedure CreateGUI(AParent: TWinControl; ATop, ALeft: integer); override;
    procedure Clear; override;
    function Correct: boolean; override;
    procedure LoadData(AData: string; ID: integer); override;
  end;

  TDBCBoxControl = class(TDBControl)
  private
    type
    TCBoxData = record
      Data: string;
      ID: integer;
    end;
  private
    FID: integer;
    FCBox: TComboBox;
    FData: array of TCBoxData;
    procedure OnChange(Sender: TObject);
  protected
    function GetData: TParam; override;
    procedure OnNotificationRecieve(Sender: TObject); override;
    procedure SetCaption(AData: string); override;
  public
    function UpdateTable: TQueryContainer; override;
    procedure CreateGUI(AParent: TWinControl; ATop, ALeft: integer); override;
    procedure Clear; override;
    function Correct: boolean; override;
    procedure LoadData(AData: string; ID: integer); override;
  end;

implementation

function TDBEditControl.UpdateTable: TQueryContainer;
var
  NewData: TParam;
begin
  NewData := TParam.Create(nil, ptInput);
  NewData.DataType := DataType;
  NewData.Value := FEdit.Text;
  Exit(Query.Update(FID, NewData));
end;

procedure TDBEditControl.CreateGUI(AParent: TWinControl; ATop, ALeft: integer);
begin
  inherited CreateGUI(AParent, ATop, ALeft);
  FEdit := TEdit.Create(AParent);
  FEdit.Parent := AParent;
  FEdit.Top := ATop;
  FEdit.Left := ALeft + 100;
  FEdit.Width := 250;
  FEdit.Anchors := [akRight, akLeft];
  if DataType = ftInteger then
    FEdit.NumbersOnly := True;
end;

procedure TDBEditControl.Clear;
begin
  FEdit.Text := '';
end;

function TDBEditControl.Correct: boolean;
begin
  if FEdit.Text = '' then
    Exit(False);
  Exit(True);
end;

procedure TDBEditControl.LoadData(AData: string; ID: integer);
begin
  FEdit.Text := AData;
  if FID = 0 then
    FID := ID;
end;

function TDBEditControl.GetData: TParam;
begin
  Result := TParam.Create(nil, ptInput);
  Result.DataType := DataType;
  Result.Name := NativeName;
  Result.Value := FEdit.Text;
end;

procedure TDBEditControl.SetCaption(AData: string);
begin
  FEdit.Text := AData;
end;

function TDBCBoxControl.GetData: TParam;
begin
  Result := TParam.Create(nil, ptInput);
  Result.DataType := DataType;
  Result.Name := NativeName;
  Result.Value := FData[FCBox.ItemIndex].ID;
end;

procedure TDBCBoxControl.OnChange(Sender: TObject);
begin
  Subscriber.CreateNotification(FCBox, Subscriber.NClass);
end;

function TDBCBoxControl.UpdateTable: TQueryContainer;
var
  NewData: TParam;
begin
  NewData := TParam.Create(nil, ptInput);
  NewData.DataType := ftInteger;
  NewData.Value := FData[FCBox.ItemIndex].ID;
  Exit(Query.Update(FID, NewData));
end;

procedure TDBCBoxControl.OnNotificationRecieve(Sender: TObject);
begin
  FCBox.ItemIndex := TCombobox(Sender).ItemIndex;
end;

procedure TDBCBoxControl.CreateGUI(AParent: TWinControl; ATop, ALeft: integer);
begin
  inherited CreateGUI(AParent, ATop, ALeft);
  FCBox := TComboBox.Create(AParent);
  FCBox.Parent := AParent;
  FCBox.ReadOnly := True;
  FCBox.Top := ATop;
  FCBox.Left := ALeft + 100;
  FCBox.Width := 250;
  FCBox.Anchors := [akRight, akLeft];
  FCBox.OnChange := @OnChange;
  Subscriber.OnNotificationRecieve := @OnNotificationRecieve;
end;

function TDBCBoxControl.Correct: boolean;
begin
  if FCBox.ItemIndex = -1 then
    Exit(False);
  Exit(True);
end;

procedure TDBCBoxControl.Clear;
begin
  FCBox.ItemIndex := -1;
end;

procedure TDBCBoxControl.LoadData(AData: string; ID: integer);
begin
  SetLength(FData, Length(FData) + 1);
  FData[High(FData)].Data := AData;
  FData[High(FData)].ID := ID;
  FCBox.Items.Add(AData);
end;

procedure TDBCBoxControl.SetCaption(AData: string);
var
  i: integer;
begin
  for i := 0 to High(FData) do
    if AData = FData[i].Data then begin
      FCBox.ItemIndex := i;
      FId := FData[FCBox.ItemIndex].ID;
      Exit;
    end;
end;

end.
