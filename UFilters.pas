unit UFilters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Controls, ExtCtrls, Graphics, DB,
  UDBObjects, UVector, UStringUtils, UCustomControl;

type

  TFilterPanel = class;
  TEvent = procedure of object;
  TParamEvent = procedure(FilterIndex: integer) of object;

  TFilterPanel = class(TCustomControl)
  private
    type
    TOperator = record
      Name: string;
      COperator: string;
    end;
    TOperators = array of TOperator;
  private
    FEnabled: boolean;
    FFilter: TDBFilter;
    FTable: TDBTable;
    FFieldsCBox: TComboBox;
    FOpsCBox: TComboBox;
    FEdit: TEdit;
    FCurOps: TOperators;
    FNumOps: TOperators;
    FStrOps: TOperators;
    FOnChange: TEvent;
    procedure InitOperators;
    procedure AddOperator(var Operators: TOperators; AName, ACOperator: string);
    procedure LoadFieldsCBox;
    procedure LoadOperators(Ops: TOperators);
    procedure OnChangeEvent(Sender: TObject);
    procedure OnFieldsCBoxChange(Sender: TObject);
    procedure LoadOpsFromDataType(DataType: TFieldType);
    procedure SetState(AEnabled: boolean);
    function FindOpsInd(Op: string; Ops: TOperators): integer;
  public
    constructor Create(Table: TDBTable);
    constructor Create(Table: TDBTable; AParent: TWinControl; ATop, ALeft: integer);
    procedure InitControls(AParent: TWinControl; ATop, ALeft: integer);
    procedure SetFilterData(Field, COp, Param: string);
    function Correct: boolean;
  published
    property OnChange: TEvent write FOnChange;
    property Filter: TDBFilter read FFilter;
    property Enabled: boolean read FEnabled write SetState;
  end;

  TCustomFilterControls = specialize TCustomControls<TFilterPanel>;

  TFilterPanels = class(TCustomFilterControls)
  private
    FOnChange: TEvent;
    FTable: TDBTable;
    procedure SetOnChange(AOnChange: TEvent);
  public
    constructor Create(Table: TDBTable; AParent: TWinControl; ATop, ALeft: integer);
    procedure AddFilterPanel;
    procedure AddFilterPanel(AFilterPanel: TFilterPanel);
    function FiltersCorrect: boolean;
    function Apply: TQueryContainer;
    function Correct: boolean;
  published
    property OnDelete: TEvent write FOnDelete;
    property OnChange: TEvent write SetOnChange;
  end;

implementation

constructor TFilterPanel.Create(Table: TDBTable);
begin
  inherited Create;
  FOnChange := nil;
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
begin
  inherited Create(AParent, 402 + 20, ATop, ALeft);
  FFieldsCBox := TComboBox.Create(Self);
  FOpsCBox := TComboBox.Create(Self);
  FEdit := TEdit.Create(Self);

  FFieldsCBox.ReadOnly := True;
  FOpsCBox.ReadOnly := True;
  InitComponent(FFieldsCBox, Self, 1, 0, 150);
  InitComponent(FOpsCBox, Self, 1, 151, 100);
  InitComponent(FEdit, Self, 1, 252, 150);
  DelBtn.Left := 402;
  DelBtn.Top := 0;

  FFieldsCBox.OnSelect := @OnFieldsCBoxChange;
  FOpsCBox.OnChange := @OnChangeEvent;
  FEdit.OnChange := @OnChangeEvent;

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
  LoadOpsFromDataType(FTable.Fields[FFieldsCBox.ItemIndex].DataType);
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
  DelBtn.Enabled := AEnabled;
end;

procedure TFilterPanel.AddOperator(var Operators: TOperators; AName, ACOperator: string);
begin
  SetLength(Operators, Length(Operators) + 1);
  Operators[High(Operators)].Name := AName;
  Operators[High(Operators)].COperator := ACOperator;
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
  inherited Create(AParent, ATop, ALeft);
  FTable := Table;
  FOnChange := nil;
end;

procedure TFilterPanels.AddFilterPanel;
begin
  AddFilterPanel(TFilterPanel.Create(FTable, FParent, FTop, FLeft));
end;

procedure TFilterPanels.AddFilterPanel(AFilterPanel: TFilterPanel);
begin
  inherited AddControlPanel(AFilterPanel);
  AFilterPanel.OnChange := FOnChange;
end;

procedure TFilterPanels.SetOnChange(AOnChange: TEvent);
var
  i: integer;
begin
  FOnChange := AOnChange;
  for i := 0 to Size - 1 do
    Items[i].OnChange := AOnChange;
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
    if not Correct then
      Exit(FTable.Query.Select(nil));
    Filters := TDBFilters.Create;
    for i := 0 to Size - 1 do
      Filters.PushBack(Items[i].Filter);
    Result := FTable.Query.Select(Filters);
    Filters.Free;
    Exit(Result);
  end;
  Exit(FTable.Query.Select(nil));
end;

function TFilterPanels.Correct: boolean;
var
  i: integer;
begin
  for i := 0 to Size - 1 do
    if not Items[i].Correct then
      Exit(False);
  Exit(True);
end;

end.
