unit UFilters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Controls, ExtCtrls, Graphics, DB,
  UDBObjects, UVector, UStringUtils;

type

  TFilterPanel = class;
  TEvent = procedure of object;
  TParamEvent = procedure(FilterIndex: integer) of object;

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
    FTable: TDBTable;
    FFieldsCBox: TComboBox;
    FOpsCBox: TComboBox;
    FDelBtn: TButton;
    FEdit: TEdit;
    FCurOps: TOperators;
    FNumOps: TOperators;
    FStrOps: TOperators;
    FOnChange: TEvent;
    FOnDelete: TParamEvent;
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
    property OnDelete: TParamEvent write FOnDelete;
    property OnChange: TEvent write FOnChange;
    property Filter: TDBFilter read FFilter;
    property Index: integer read FIndex write FIndex;
    property Enabled: boolean read FEnabled write SetState;
  end;

  TFilterPanelV = specialize TObjVector<TFilterPanel>;

  TFilterPanels = class(TFilterPanelV)
  private
    FParent: TWinControl;
    FTop: integer;
    FLeft: integer;
    FYOffset: integer;
    FOnDelete: TEvent;
    FOnChange: TEvent;
    FTable: TDBTable;
    procedure UpdateFilters;
    //this procedure is called by deleted filter panel
    procedure OnFilterDelete(FilterIndex: integer);
    procedure SetOnChange(AOnChange: TEvent);
  public
    constructor Create(Table: TDBTable; AParent: TWinControl; ATop, ALeft: integer);
    procedure DeleteAll;
    procedure AddFilterPanel;
    procedure AddFilterPanel(AFilterPanel: TFilterPanel);
    function FiltersCorrect: boolean;
    function Apply: TQueryContainer;
  published
    property OnDelete: TEvent write FOnDelete;
    property OnChange: TEvent write SetOnChange;
  end;

implementation

constructor TFilterPanel.Create(Table: TDBTable);
begin
  FOnChange := nil;
  FOnDelete := nil;
  FEnabled := True;
  FFilter := TDBFilter.Create;
  FTable := Table;
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
  FFilter.Assign(FTable.Fields[FFieldsCBox.ItemIndex]);
  FFilter.ConditionalOperator := FCurOps[FOpsCBox.ItemIndex].COperator;
  FFilter.Param := FEdit.Text;
  if FOnChange <> nil then
    FOnChange;
end;

procedure TFilterPanel.OnFieldsCBoxChange(Sender: TObject);
begin
  LoadOpsFromDataType(FFilter.ParentTable.Fields[FFieldsCBox.ItemIndex].DataType);
  if FOnChange <> nil then
    FOnChange;
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
  if FOnDelete <> nil then
    FOnDelete(FIndex);
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

constructor TFilterPanels.Create(Table: TDBTable; AParent: TWinControl;
  ATop, ALeft: integer);
begin
  FTable := Table;
  FParent := AParent;
  FTop := ATop;
  FLeft := ALeft;
  FOnDelete := nil;
  FOnChange := nil;
  FYOffset := 0;
end;

procedure TFilterPanels.AddFilterPanel;
begin
  AddFilterPanel(TFilterPanel.Create(FTable, FParent, FTop, FLeft));
end;

procedure TFilterPanels.AddFilterPanel(AFilterPanel: TFilterPanel);
begin
  FYOffset := Size * AFilterPanel.Height;
  AFilterPanel.Index := Size;
  AFilterPanel.OnChange := FOnChange;
  AFilterPanel.OnDelete := @OnFilterDelete;
  AFilterPanel.Top := AFilterPanel.Top + FYOffset;
  PushBack(AFilterPanel);
end;

procedure TFilterPanels.UpdateFilters;
var
  i: integer;
begin
  FYOffset := 0;
  for i := 0 to Size - 1 do begin
    FYOffset := i* Items[i].Height;
    Items[i].Top := FYOffset;
    Items[i].Index := i;
  end;
end;

procedure TFilterPanels.OnFilterDelete(FilterIndex: integer);
begin
  if Size > 0 then begin
    DeleteIndS(FilterIndex);
    UpdateFilters;
    if FOnDelete <> nil then
      FOnDelete;
  end;
end;

procedure TFilterPanels.SetOnChange(AOnChange: TEvent);
var
  i: integer;
begin
  FOnChange := AOnChange;
  for i := 0 to Size - 1 do
    Items[i].OnChange := AOnChange;
end;

procedure TFilterPanels.DeleteAll;
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

function TFilterPanels.FiltersCorrect: boolean;
var
  i: integer;
begin
  for i := 0 to Size - 1 do
    if not Items[i].Correct then
      Exit(False);
  Exit(True);
end;

function TFilterPanels.Apply: TQueryContainer;
var
  Filters: TDBFilters;
  i: integer;
begin
  if Size > 0 then begin
    Filters := TDBFilters.Create;
    for i := 0 to Size - 1 do
      Filters.PushBack(Items[i].Filter);
    Result := FTable.Query.Select(Filters);
    Filters.Free;
    Exit(Result);
  end;
  Exit(FTable.Query.Select(nil));
end;

end.
