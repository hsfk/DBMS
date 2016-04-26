unit UFilterPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Controls, ExtCtrls, Graphics, DB,
  UDBObjects, Dialogs, UVector, UStringUtils;

type

  TFilterPanel = class;
  TEvent = procedure of object;
  TParamEvent = procedure(FilterIndex: integer) of object;
  TFilterPanels = specialize TVector<TFilterPanel>;

  TFilterPanel = class(TPanel)
  private
    type
    TOperator = record
      Name: string;
      COperator: string;
    end;
    TOperators = array of TOperator;
  private
    FIndex: integer;
    FEnabled: boolean;
    FFilter: TDBFilter;
    FFieldsCBox: TComboBox;
    FOpsCBox: TComboBox;
    FDelBtn: TButton;
    FEdit: TEdit;
    FCurOps: TOperators;
    FNumOps: TOperators;
    FStrOps: TOperators;
    FOnChangeEvent: TEvent;
    FBeforeDelete: TParamEvent;
    procedure Init(Component, AParent: TWinControl; ATop, ALeft, AWidth: integer);
    procedure InitOperators;
    procedure AddOperator(var Operators: TOperators; AName, ACOperator: string);
    procedure LoadFieldsCBox;
    procedure LoadOperators(Ops: TOperators);
    procedure OnChangeEvent(Sender: TObject);
    procedure OnFieldsCBoxChange(Sender: TObject);
    procedure LoadOpsFromDataType(DataType: TFieldType);
    procedure SetState(AEnabled: boolean);
    procedure FDelBtnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    function FindOpsInd(Op: string; Ops: TOperators): integer;
  public
    constructor Create(Table: TDBTable);
    constructor Create(Table: TDBTable; AParent: TWinControl; ATop, ALeft: integer);
    procedure InitControls(AParent: TWinControl; ATop, ALeft: integer);
    procedure SetFilterData(Field, COp, Param: string);
    function Correct: boolean;
  published
    property BeforeDelete: TParamEvent write FBeforeDelete;
    property OnChange: TEvent write FOnChangeEvent;
    property Filter: TDBFilter read FFilter;
    property Index: integer read FIndex write FIndex;
    property Enabled: boolean read FEnabled write SetState;
  end;

implementation

constructor TFilterPanel.Create(Table: TDBTable);
begin
  FEnabled := True;
  FFilter := TDBFilter.Create;
  FFilter.Assign(Table.Fields[0]);
  InitOperators;
end;

constructor TFilterPanel.Create(Table: TDBTable; AParent: TWinControl;
  ATop, ALeft: integer);
begin
  Create(Table);
  InitControls(AParent, ATop, ALeft);
end;

function TFilterPanel.Correct: boolean;
begin
  if FEdit.Text <> '' then
    Exit(True);
  Exit(False);
end;

procedure TFilterPanel.InitControls(AParent: TWinControl; ATop, ALeft: integer);
const
  SPACE = 5;
  FIELDS_CBOX_WIDTH = 150;
  OPS_CBOX_WIDTH = 100;
  EDIT_WIDTH = 150;
  TOTAL_WIDTH = SPACE * 4 + FIELDS_CBOX_WIDTH + OPS_CBOX_WIDTH + EDIT_WIDTH + 20;
begin
  inherited Create(AParent);
  Init(Self, AParent, ATop - 1, ALeft, TOTAL_WIDTH);
  Self.Height := 25;
  Self.BevelInner := bvNone;
  Self.BevelOuter := bvNone;
  FFieldsCBox := TComboBox.Create(Self);
  FOpsCBox := TComboBox.Create(Self);
  FEdit := TEdit.Create(Self);
  FDelBtn := TButton.Create(Self);
  FFieldsCBox.ReadOnly := True;
  FOpsCBox.ReadOnly := True;
  Init(FFieldsCBox, Self, 1, 0, FIELDS_CBOX_WIDTH);
  Init(FOpsCBox, Self, 1, FIELDS_CBOX_WIDTH + SPACE, OPS_CBOX_WIDTH);
  Init(FEdit, Self, 1, FOpsCBox.Left + OPS_CBOX_WIDTH + SPACE, EDIT_WIDTH);
  Init(FDelBtn, Self, 0, FEdit.Left + FEdit.Width, 20);

  FFieldsCBox.OnSelect := @OnFieldsCBoxChange;
  FOpsCBox.OnChange := @OnChangeEvent;
  FEdit.OnChange := @OnChangeEvent;
  FDelBtn.OnMouseUp := @FDelBtnMouseUp;
  FDelBtn.Caption := 'X';

  LoadFieldsCBox;
end;

procedure TFilterPanel.SetFilterData(Field, COp, Param: string);
var
  OpsIndex: integer;
begin
  FFieldsCBox.ItemIndex := FindInd(Field, FFieldsCBox.Items);
  OpsIndex := FindOpsInd(COp, FNumOps);
  if OpsIndex <> -1 then begin
    LoadOperators(FNumOps);
  end
  else begin
    OpsIndex := FindOpsInd(COp, FStrOps);
    LoadOperators(FStrOps);
  end;
  FOpsCBox.ItemIndex := OpsIndex;
  FEdit.Text := Param;
end;

procedure TFilterPanel.InitOperators;
begin
  AddOperator(FNumOps, 'Равно', ' = ');
  AddOperator(FNumOps, 'Больше', ' > ');
  AddOperator(FNumOps, 'Меньше', ' < ');
  AddOperator(FStrOps, 'Содержит', ' Containing ');
  AddOperator(FStrOps, 'Не содержит', ' Not Containing ');
  AddOperator(FStrOps, 'Начинается с', ' Starting With ');
end;

procedure TFilterPanel.Init(Component, AParent: TWinControl;
  ATop, ALeft, AWidth: integer);
begin
  Component.Parent := AParent;
  Component.Top := ATop;
  Component.Left := ALeft;
  Component.Width := AWidth;
end;

procedure TFilterPanel.LoadFieldsCBox;
var
  i: integer;
begin
  for i := 0 to FFilter.ParentTable.Count - 1 do
    FFieldsCBox.Items.Add(FFilter.ParentTable.Fields[i].Name);
  FFieldsCBox.ItemIndex := 0;
  LoadOperators(FNumOps);
end;

procedure TFilterPanel.LoadOperators(Ops: TOperators);
var
  i: integer;
begin
  FOpsCBox.Items.Clear;
  for i := 0 to High(Ops) do
    FOpsCBox.Items.Add(Ops[i].Name);
  FCurOps := Ops;
  FOpsCBox.ItemIndex := 0;
  FEdit.NumbersOnly := True;
end;

procedure TFilterPanel.OnChangeEvent(Sender: TObject);
begin
  FFilter.Assign(FFilter.ParentTable.Fields[FFieldsCBox.ItemIndex]);
  FFilter.ConditionalOperator := FCurOps[FOpsCBox.ItemIndex].COperator;
  FFilter.Param := FEdit.Text;
  if FOnChangeEvent <> nil then
    FOnChangeEvent;
end;

procedure TFilterPanel.OnFieldsCBoxChange(Sender: TObject);
begin
  LoadOpsFromDataType(FFilter.ParentTable.Fields[FFieldsCBox.ItemIndex].DataType);
  if FOnChangeEvent <> nil then
    FOnChangeEvent;
end;

procedure TFilterPanel.LoadOpsFromDataType(DataType: TFieldType);
begin
  if DataType = ftInteger then begin
    LoadOperators(FNumOps);
    FEdit.NumbersOnly := True;
  end;
  if DataType = ftString then begin
    LoadOperators(FStrOps);
    FEdit.NumbersOnly := False;
  end;
end;

procedure TFilterPanel.SetState(AEnabled: boolean);
begin
  FEnabled := AEnabled;
  FEdit.Enabled := AEnabled;
  FFieldsCBox.Enabled := AEnabled;
  FOpsCBox.Enabled := AEnabled;
  FDelBtn.Enabled := AEnabled;
end;

procedure TFilterPanel.AddOperator(var Operators: TOperators; AName, ACOperator: string);
begin
  SetLength(Operators, Length(Operators) + 1);
  Operators[High(Operators)].Name := AName;
  Operators[High(Operators)].COperator := ACOperator;
end;

procedure TFilterPanel.FDelBtnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if FBeforeDelete <> nil then
    FBeforeDelete(FIndex);
  Self.Free;
end;

function TFilterPanel.FindOpsInd(Op: string; Ops: TOperators): integer;
var
  i: integer;
begin
  Op := UpCase(Op);
  for i := 0 to High(Ops) do
    if (Op = UpCase(Ops[i].COperator)) or (Op = UpCase(Ops[i].Name)) then
      Exit(i);
  Exit(-1);
end;

end.
