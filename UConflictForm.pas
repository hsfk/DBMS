unit UConflictForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, UCustomControl, UDBObjects, UVector, UConflicts, DB, UElementaryFunctions;

type

  TParamControl = class;
  TParamControls = class;
  TExprControl = class;
  TExprControls = class;

  TConflictForm = class(TForm)
  private
    FParamControls: TParamControls;
    FExprControls: TExprControls;
    FTable: TDBTable;
    FEditedConflict: TConflictPanel;
    FConflicts: TConflictPanels;
    function BuildConflict: TConflictType;
    procedure LoadConflict(Conflict: TConflictType);
  public
    procedure Load(Table: TDBTable; Conflicts: TConflictPanels;
      EditedConflict: TConflictPanel);
  published
    FNameEdit: TEdit;
    FApplyBtn: TButton;
    FCancelBtn: TButton;
    FParamsGBox: TGroupBox;
    FNameLabel: TLabel;
    FBtnsPanel: TPanel;
    FParamsSBox: TScrollBox;
    FAddParamBtn: TButton;
    FDelParamsBtn: TButton;
    FAddExprBtn: TButton;
    FDelExprBtn: TButton;
    FExprSBox: TScrollBox;
    FExprGBox: TGroupBox;
    procedure FAddExprBtnClick(Sender: TObject);
    procedure FDelExprBtnClick(Sender: TObject);
    procedure FApplyBtnClick(Sender: TObject);
    procedure FCancelBtnClick(Sender: TObject);
    procedure FAddParamBtnClick(Sender: TObject);
    procedure FDelParamsBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;

  TParamControl = class(TCustomControl)
  private
    FTable: TDBTable;
    FRecCBox: TComboBox;
    FParamCBox: TComboBox;
    FOps: TStringV;
    function GetFieldN: integer;
  public
    constructor Create(Table: TDBTable; AParent: TWinControl; ATop, ALeft: integer);
    procedure InitControls(AParent: TWinControl; ATop, ALeft: integer);
    function Correct: boolean;
    function IsEQ: boolean;
  published
    property FieldN: integer read GetFieldN;
  end;

  TCustomParamControls = specialize TCustomControls<TParamControl>;

  TParamControls = class(TCustomParamControls)
  private
    FTable: TDBTable;
  public
    constructor Create(Table: TDBTable; AParent: TWinControl; ATop, ALeft: integer);
    procedure AddParam(RecN: integer; IsEQ: boolean);
    procedure AddParam;
    function BuildFilters: TDataFilters;
    function EQFieldsCount: integer;
    function Correct: boolean;
  end;

  TExprControl = class(TCustomControl)
  private
    FTable: TDBTable;
    FRecACbox: TComboBox;
    FRecBCBox: TComboBox;
    FCompareFCBox: TComboBox;
    FAggregateFCBox: TComboBox;
    FIntRecIDs: TIntegerV;
    FExprFunctions: TExprFunctions;
    procedure InitCBoxData;
  public
    constructor Create(Table: TDBTable; AParent: TWinControl; ATop, ALeft: integer);
    procedure InitControls(AParent: TWinControl; ATop, ALeft: integer);
    function Correct: boolean;
    function BuildExpr: TExpression;
  end;

  TCustomExprControls = specialize TCustomControls<TExprControl>;

  TExprControls = class(TCustomExprControls)
  private
    FTable: TDBTable;
  public
    constructor Create(Table: TDBTable; AParent: TWinControl; ATop, ALeft: integer);
    procedure AddExpr(RecA, RecB: integer; ECompareF: EnumIntCompareFunctions;
      EAggregateF: EnumIntAggregateFunctions);
    procedure AddExpr;
    function BuildExpr: TExpressions;
    function Correct: boolean;
  end;

implementation

{$R *.lfm}

constructor TExprControls.Create(Table: TDBTable; AParent: TWinControl;
  ATop, ALeft: integer);
begin
  inherited Create(AParent, ATop, ALeft);
  FTable := Table;
end;

procedure TExprControls.AddExpr(RecA, RecB: integer; ECompareF: EnumIntCompareFunctions;
  EAggregateF: EnumIntAggregateFunctions);
var
  ExprControl: TExprControl;
begin
  ExprControl := TExprControl.Create(FTable, Parent, Top, Left);
  ExprControl.FRecACbox.ItemIndex := ExprControl.FIntRecIDs.FindInd(RecA);
  ExprControl.FRecBCBox.ItemIndex := ExprControl.FIntRecIDs.FindInd(RecB);
  ExprControl.FAggregateFCBox.ItemIndex :=
    ExprControl.FExprFunctions.FindInd(EAggregateF);
  ExprControl.FCompareFCBox.ItemIndex := ExprControl.FExprFunctions.FindInd(ECompareF);
  AddControlPanel(ExprControl);
end;

procedure TExprControls.AddExpr;
begin
  AddControlPanel(TExprControl.Create(FTable, Parent, Top, Left));
end;

function TExprControls.BuildExpr: TExpressions;
var
  i: integer;
begin
  Result := TExpressions.Create;
  for i := 0 to Size - 1 do
    Result.PushBack(Items[i].BuildExpr);
end;

function TExprControls.Correct: boolean;
var
  i: integer;
begin
  for i := 0 to Size - 1 do
    if not Items[i].Correct then
      Exit(False);
  Exit(True);
end;

procedure TConflictForm.FAddExprBtnClick(Sender: TObject);
begin
  FExprControls.AddExpr;
end;

procedure TConflictForm.FDelExprBtnClick(Sender: TObject);
begin
  FExprControls.DeleteAll;
end;

function TConflictForm.BuildConflict: TConflictType;
begin
  Result := TConflictType.Create(FNameEdit.Text);
  Result.Filters.Free;
  Result.Filters := FParamControls.BuildFilters;
  if FExprControls.Size > 0 then begin
    Result.Expressions.Free;
    Result.Expressions := FExprControls.BuildExpr;
  end;
end;

procedure TConflictForm.Load(Table: TDBTable; Conflicts: TConflictPanels;
  EditedConflict: TConflictPanel);
begin
  FTable := Table;
  FParamControls.DeleteAll;
  FParamControls.FTable := FTable;
  FExprControls.DeleteAll;
  FExprControls.FTable := FTable;
  FConflicts := Conflicts;
  FEditedConflict := EditedConflict;
  FNameEdit.Text := '';

  if FEditedConflict <> nil then begin
    Caption := 'Редактор конфликтов - ' + FEditedConflict.Conflict.Name;
    FNameEdit.Text := FEditedConflict.Conflict.Name;
    LoadConflict(FEditedConflict.Conflict);
  end
  else
    Caption := 'Редактор конфликтов';
end;

procedure TConflictForm.LoadConflict(Conflict: TConflictType);
var
  i: integer;
  j: integer;
  IsEQ: boolean;
begin
  with Conflict do begin
    for i := 0 to Filters.Size - 1 do begin
      IsEQ := Filters[i].Priority = 0;
      for j := 0 to Filters[i].FilteredRecs.Size - 1 do
        FParamControls.AddParam(Filters[i].FilteredRecs[j], IsEQ);
    end;
    for i := 0 to Expressions.Size - 1 do
      with Expressions[i] do
        FExprControls.AddExpr(FieldA, FieldB, ECompareFunc, EAggregateFunc);
  end;
end;

procedure TConflictForm.FormCreate(Sender: TObject);
begin
  FParamControls := TParamControls.Create(FTable, FParamsSBox, 5, 5);
  FExprControls := TExprControls.Create(FTable, FExprSBox, 5, 5);
  FEditedConflict := nil;
end;

procedure TConflictForm.FApplyBtnClick(Sender: TObject);
begin
  if FParamControls.EQFieldsCount = 0 then begin
    ShowMessage('Хотя бы одно поле должно совпадать');
    Exit;
  end;
  if not FParamControls.Correct or not FExprControls.Correct then begin
    ShowMessage('Нужно заполнить все поля.');
    Exit;
  end;
  if FEditedConflict = nil then
    FConflicts.AddConflictPanel(BuildConflict)
  else begin
    FEditedConflict.Conflict.Free;
    FEditedConflict.Conflict := BuildConflict;
    FEditedConflict.Edit.Text := FNameEdit.Text;
  end;
  Close;
end;

procedure TConflictForm.FCancelBtnClick(Sender: TObject);
begin
  if FParamControls.Correct and (FParamControls.Size > 0) then
    if MessageDlg('Вы действительно хотите выйти?', mtConfirmation, mbOKCancel, 0) =
      mrCancel then
      Exit;
  Close;
end;

procedure TConflictForm.FAddParamBtnClick(Sender: TObject);
begin
  FParamControls.AddParam;
end;

procedure TConflictForm.FDelParamsBtnClick(Sender: TObject);
begin
  FParamControls.DeleteAll;
end;

constructor TParamControl.Create(Table: TDBTable; AParent: TWinControl;
  ATop, ALeft: integer);
begin
  FTable := Table;
  InitControls(AParent, ATop, ALeft);
end;

procedure TParamControl.InitControls(AParent: TWinControl; ATop, ALeft: integer);
var
  i: integer;
begin
  inherited Create(AParent, 251 + 20, ATop, ALeft);
  FRecCBox := TComboBox.Create(Self);
  FParamCBox := TComboBox.Create(Self);
  InitComponent(FRecCBox, Self, 1, 0, 150);
  InitComponent(FParamCBox, Self, 1, 151, 100);
  DelBtn.Top := 0;
  Delbtn.Left := 251;
  FOps := TStringV.Create(['Совпадает', 'Не совпадает']);
  for i := 0 to FOps.Size - 1 do
    FParamCBox.Items.Add(FOps[i]);
  for i := 0 to FTable.Count - 1 do
    FRecCBox.Items.Add(FTable.Fields[i].Name);
  FRecCBox.ReadOnly := True;
  FParamCBox.ReadOnly := True;
end;

function TParamControl.GetFieldN: integer;
begin
  Exit(FRecCBox.ItemIndex);
end;

function TParamControl.Correct: boolean;
begin
  Exit((FParamCBox.ItemIndex <> -1) and (FRecCBox.ItemIndex <> -1));
end;

function TParamControl.IsEQ: boolean;
begin
  Exit(FParamCBox.ItemIndex = 0);
end;

constructor TParamControls.Create(Table: TDBTable; AParent: TWinControl;
  ATop, ALeft: integer);
begin
  inherited Create(AParent, ATop, ALeft);
  FTable := Table;
end;

procedure TParamControls.AddParam(RecN: integer; IsEQ: boolean);
var
  ParamControl: TParamControl;
begin
  ParamControl := TParamControl.Create(FTable, FParent, FTop, FLeft);
  ParamControl.FRecCBox.ItemIndex := RecN;
  if IsEQ then
    ParamControl.FParamCBox.ItemIndex := 0
  else
    ParamControl.FParamCBox.ItemIndex := 1;
  AddControlPanel(ParamControl);
end;

procedure TParamControls.AddParam;
begin
  AddControlPanel(TParamControl.Create(FTable, FParent, FTop, FLeft));
end;

function TParamControls.BuildFilters: TDataFilters;
var
  i: integer;
  EQFilter: TSeparateEqualRecsFilter;
  NEQFilter: TDeleteNotEqualRecsFilter;
  EQRecs: TIntegerV;
  NEQRecs: TIntegerV;
begin
  EQRecs := TIntegerV.Create;
  NEQRecs := TIntegerV.Create;
  for i := 0 to Size - 1 do
    if Items[i].IsEQ then
      EQRecs.PushBack(Items[i].FieldN)
    else
      NEQRecs.PushBack(Items[i].FieldN);
  EQFilter := TSeparateEqualRecsFilter.Create(EQRecs);
  NEQFilter := TDeleteNotEqualRecsFilter.Create(NEQRecs);
  Exit(TDataFilters.Create([EQFilter, NEQFilter]));
end;

function TParamControls.EQFieldsCount: integer;
var
  i: integer;
begin
  Result := 0;
  if Size = 0 then
    Exit;
  for i := 0 to Size - 1 do
    if Items[i].ISEq then
      Result += 1;
end;

function TParamControls.Correct: boolean;
var
  i: integer;
begin
  for i := 0 to Size - 1 do
    if not Items[i].Correct then
      Exit(False);
  Exit(True);
end;

constructor TExprControl.Create(Table: TDBTable; AParent: TWinControl;
  ATop, ALeft: integer);
begin
  FTable := Table;
  FIntRecIDs := TIntegerV.Create;
  FExprFunctions := TExprFunctions.Create;
  InitControls(AParent, ATop, ALeft);
end;

procedure TExprControl.InitControls(AParent: TWinControl; ATop, ALeft: integer);
begin
  inherited Create(AParent, 423 + 20, ATop, ALeft);
  FRecACbox := TComboBox.Create(Self);
  FRecBCBox := TComboBox.Create(Self);
  FCompareFCBox := TComboBox.Create(Self);
  FAggregateFCBox := TComboBox.Create(Self);

  InitComponent(FRecACbox, Self, 1, 0, 110);
  InitComponent(FCompareFCBox, Self, 1, 111, 120);
  InitComponent(FAggregateFCBox, Self, 1, 232, 80);
  InitComponent(FRecBCBox, Self, 1, 313, 110);
  DelBtn.Top := 0;
  DelBtn.Left := 423;

  InitCBoxData;
end;

procedure TExprControl.InitCBoxData;
var
  i: integer;
begin
  for i := 0 to FTable.Count - 1 do
    if FTable.Fields[i].DataType = ftInteger then begin
      FRecACbox.Items.Add(FTable.Fields[i].Name);
      FRecBCbox.Items.Add(FTable.Fields[i].Name);
      FIntRecIDs.PushBack(i);
    end;

  for i := 0 to FExprFunctions.CompareFCount - 1 do
    FCompareFCBox.Items.Add(FExprFunctions.CompareFs[i].Name);
  for i := 0 to FExprFunctions.AggregateFCount - 1 do
    FAggregateFCBox.Items.Add(FExprFunctions.AggregateFs[i].Name);

  FRecACbox.ReadOnly := True;
  FRecBCbox.ReadOnly := True;
  FCompareFCBox.ReadOnly := True;
  FAggregateFCBox.ReadOnly := True;

  FRecACbox.ItemIndex := -1;
  FRecBCbox.ItemIndex := -1;
  FCompareFCBox.ItemIndex := -1;
  FAggregateFCBox.ItemIndex := -1;
end;

function TExprControl.Correct: boolean;
begin
  Exit(
    (FCompareFCBox.ItemIndex <> -1) and (FRecACbox.ItemIndex <> -1) and
    (FRecBCBox.ItemIndex <> -1) and (FAggregateFCBox.ItemIndex <> -1));
end;

function TExprControl.BuildExpr: TExpression;
begin
  Exit(
    TExpression.Create(FIntRecIDs[FRecACbox.ItemIndex],
    FIntRecIDs[FRecBCbox.ItemIndex],
    FExprFunctions.CompareFs[FCompareFCBox.ItemIndex].EFunc,
    FExprFunctions.AggregateFs[FAggregateFCBox.ItemIndex].EFunc));
end;

end.
