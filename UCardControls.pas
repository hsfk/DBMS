unit UCardControls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Controls, UCustomControl, DB, UDBObjects,
  UVector, UNotifications, UStringUtils;

type

  TCardControl = class(TCustomControl)
  private
    FLabel: TLabel;
    FField: TDBField;
    FSubscriber: TSubscriber;
    FRecID: integer;
    procedure SetCaption(AData: string); virtual; abstract;
    procedure OnNotificationRecieve(Sender: TObject); virtual;
    function GetData: TParam; virtual; abstract;
  public
    constructor Create(RecID: integer);
    procedure CreateGUI(AParent: TWinControl; ATop, ALeft: integer); virtual;
    procedure Deselect; virtual; abstract;
    procedure Clear; virtual; abstract;
    procedure LoadData(Data: string; ID: integer); virtual; abstract;
    function UpdateTable: TQueryContainer; virtual; abstract;
    function Correct: boolean; virtual; abstract;
  published
    property Subscriber: TSubscriber read FSubscriber write FSubscriber;
    property Caption: string write SetCaption;
    property Data: TParam read GetData;
    property Field: TDBField read FField write FField;
  end;

  TFieldControl = class(TCardControl)
  private
    FEdit: TEdit;
    procedure SetCaption(AData: string); override;
    function GetData: TParam; override;
  public
    procedure CreateGUI(AParent: TWinControl; ATop, ALeft: integer); override;
    procedure Clear; override;
    procedure Deselect; override;
    procedure LoadData(AData: string; ID: integer); override;
    function UpdateTable: TQueryContainer; override;
    function Correct: boolean; override;
  end;

  TRefFieldControl = class(TCardControl)
  private
    FCBox: TComboBox;
    FData: array of TDBData;
    procedure OnChange(Sender: TObject);
    procedure OnNotificationRecieve(Sender: TObject); override;
    procedure SetCaption(AData: string); override;
    function GetData: TParam; override;
  public
    procedure CreateGUI(AParent: TWinControl; ATop, ALeft: integer); override;
    procedure Clear; override;
    procedure Deselect; override;
    procedure LoadData(AData: string; ID: integer); override;
    function UpdateTable: TQueryContainer; override;
    function Correct: boolean; override;
  end;

  TCustomCardControls = specialize TCustomControls<TCardControl>;

  TCardControls = class(TCustomCardControls)
  private
    FTable: TDBTable;
    FRecID: integer;
    FCboxNotifications: TSubscriber;
    procedure AddControl(Control: TCardControl);
  public
    constructor Create(RecID: integer; Table: TDBTable; AParent: TWinControl;
      ATop, ALeft: integer);
    procedure CreateGUIControls;
    procedure SubscribeControlsFromSameTable;
    function Correct: boolean;
  end;

implementation

procedure TCardControl.OnNotificationRecieve(Sender: TObject);
begin
  { Do nothing }
end;

constructor TCardControl.Create(RecID: integer);
begin
  FRecID := RecID;
end;

procedure TCardControl.CreateGUI(AParent: TWinControl; ATop, ALeft: integer);
begin
  inherited Create(AParent, 370, ATop, ALeft);
  FLabel := TLabel.Create(Self);
  FLabel.Parent := Self;
  FLabel.Caption := FField.Name + ':';
  FLabel.Top := 0;
  FLabel.Left := 0;
  DelBtn.Free;
end;

function TFieldControl.GetData: TParam;
begin
  Result := TParam.Create(nil, ptInput);
  Result.DataType := FField.DataType;
  Result.Name := FField.NativeName;
  Result.Value := FEdit.Text;
end;

procedure TFieldControl.SetCaption(AData: string);
begin
  FEdit.Text := AData;
end;

function TFieldControl.UpdateTable: TQueryContainer;
var
  NewData: TParam;
begin
  NewData := TParam.Create(nil, ptInput);
  NewData.DataType := FField.DataType;
  NewData.Value := FEdit.Text;
  Exit(FField.Query.Update(FRecID, NewData));
end;

function TFieldControl.Correct: boolean;
begin
  Exit(FEdit.Text <> '');
end;

procedure TFieldControl.CreateGUI(AParent: TWinControl; ATop, ALeft: integer);
begin
  inherited CreateGUI(AParent, ATop, ALeft);
  FEdit := TEdit.Create(Self);
  InitComponent(FEdit, Self, 0, 120, 250);
  if FField.DataType = ftInteger then
    FEdit.NumbersOnly := True;
end;

procedure TFieldControl.Clear;
begin
  Deselect;
end;

procedure TFieldControl.Deselect;
begin
  FEdit.Text := '';
end;

procedure TFieldControl.LoadData(AData: string; ID: integer);
begin
  FEdit.Text := AData;
end;

procedure TRefFieldControl.OnChange(Sender: TObject);
begin
  Subscriber.CreateNotification(FCBox, Subscriber.NClass);
end;

function TRefFieldControl.GetData: TParam;
begin
  Result := TParam.Create(nil, ptInput);
  Result.DataType := FField.DataType;
  Result.Name := FField.NativeName;
  Result.Value := FData[FCBox.ItemIndex].ID;
end;

procedure TRefFieldControl.OnNotificationRecieve(Sender: TObject);
begin
  FCBox.ItemIndex := TComboBox(Sender).ItemIndex;
end;

procedure TRefFieldControl.SetCaption(AData: string);
var
  i: integer;
begin
  for i := 0 to High(FData) do
    if AData = FData[i].Data then begin
      FCBox.ItemIndex := i;
      Subscriber.CreateNotification(FCBox, Subscriber.NClass);
      Exit;
    end;
end;

function TRefFieldControl.UpdateTable: TQueryContainer;
var
  NewData: TParam;
begin
  NewData := TParam.Create(nil, ptInput);
  NewData.DataType := ftInteger;
  NewData.Value := FData[FCBox.ItemIndex].ID;
  Exit(FField.Query.Update(FRecID, NewData));
end;

function TRefFieldControl.Correct: boolean;
begin
  Exit(FCBox.ItemIndex <> -1);
end;

procedure TRefFieldControl.CreateGUI(AParent: TWinControl; ATop, ALeft: integer);
begin
  inherited CreateGUI(AParent, ATop, ALeft);
  FCBox := TComboBox.Create(AParent);
  InitComponent(FCBox, Self, 0, 120, 250);
  FCBox.ReadOnly := True;
  FCBox.OnChange := @OnChange;
  Subscriber.OnNotificationRecieve := @OnNotificationRecieve;
end;

procedure TRefFieldControl.Clear;
begin
  FCBox.Items.Clear;
end;

procedure TRefFieldControl.Deselect;
begin
  FCBox.ItemIndex := -1;
end;

procedure TRefFieldControl.LoadData(AData: string; ID: integer);
begin
  SetLength(FData, Length(FData) + 1);
  FData[High(FData)].Data := AData;
  FData[High(FData)].ID := ID;
  FCBox.Items.Add(AData);
end;

constructor TCardControls.Create(RecID: integer; Table: TDBTable;
  AParent: TWinControl; ATop, ALeft: integer);
begin
  FTable := Table;
  FRecID := RecID;
  inherited Create(AParent, ATop, ALeft);
end;

procedure TCardControls.AddControl(Control: TCardControl);
begin
  Control.CreateGUI(Parent, Top, Left);
  AddControlPanel(Control);
end;

procedure TCardControls.CreateGUIControls;
var
  i: integer;
begin
  for i := 1 to FTable.Count - 1 do
    AddControl(TCardControl(FTable.Fields[i].CreateControln(FRecID)));
end;

procedure TCardControls.SubscribeControlsFromSameTable;
var
  SameTable: TDBTable = nil;
  NClass: integer = 1;
  i: integer = 1;
begin
  FCboxNotifications.Free;
  FCboxNotifications := TSubscriber.Create(True);
  with FTable do
    while i < Count - 1 do begin
      if Fields[i].ParentTable = Fields[i + 1].ParentTable then begin
        SameTable := Fields[i].ParentTable;
        while (i < Count) and (Fields[i].ParentTable = SameTable) do begin
          Items[i - 1].Subscriber.NClass := ToNClass([NClass]);
          FCboxNotifications.Subscribe(Items[i - 1].Subscriber);
          i += 1;
        end;
        NClass += 1;
      end
      else
        i += 1;
    end;
end;

function TCardControls.Correct: boolean;
var
  i: integer;
begin
  for i := 0 to Size - 1 do
    if Items[i].Correct = False then
      Exit(False);
  Exit(True);
end;

end.
