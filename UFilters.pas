unit UFilters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Controls, ExtCtrls, Graphics, DB,
  UDBObjects, UVector, UStringUtils, UCustomControl, DBExtCtrls;

type

  TInsertFieldFactory = class;
  TInsertFieldType = class of TInsertFieldFactory;

  TOperator = record
    Name: string;
    COperator: string;
    OnSelect: TEvent;
  end;

  _TOperatorV = specialize TCustomVector<TOperator>;

  TOperatorV = class(_TOperatorV)
  private
    FOnLoad: TEvent;
    FDataType: TFieldType;
    function Equal(A, B: TOperator): boolean; override;
  public
    property OnLoad: TEvent read FOnLoad write FOnLoad;
    property DataType: TFieldType read FDataType write FDataType;
  end;

  TFilterOps = class(TComboBox)
  private
    type
    TOperators = specialize TObjVector<TOperatorV>;
  private
    FOps: TOperators;
    FCurOps: integer;
    FOnSelect: TNotifyEvent;
    procedure InitOps;
    procedure OnSelectEvent(Sender: TObject);
    function GetOps: TOperatorV;
  public
    constructor Create(AParent: TWinControl; ATop, ALeft: integer);
    procedure Load(Index: integer);
    procedure AddOperator(Ops: TOperatorV; AName, COperator: string;
      AOnSelect: TEvent = nil);
    procedure LoadOpsFromDataType(DataType: TFieldType);
    function FindAndLoad(COperator: string): integer;

    property OnSelect: TNotifyEvent write FOnSelect;
    property Ops: TOperators read FOps;
    property DataOps: TOperatorV read GetOps;
  end;

  TInsertFieldFactory = class
  private
    FEnabled: boolean;
    procedure InitComponent(Component, AParent: TWinControl;
      ATop, ALeft, AWidth: integer);
    procedure SetOnChange(Event: TNotifyEvent); virtual; abstract;
    procedure SetEnabled(AEnabled: boolean); virtual;
    procedure SetData(Data: string); virtual; abstract;
    function GetData: string; virtual; abstract;
  public
    constructor Create(Parent: TWinControl; Top, Left: integer); virtual;
    destructor Destroy; virtual;
    function Correct: boolean; virtual; abstract;

    property Enabled: boolean read FEnabled write SetEnabled;
    property Data: string read GetData write SetData;
    property OnChange: TNotifyEvent write SetOnChange;
  end;

  TStrEditField = class(TInsertFieldFactory)
  private
    FEdit: TEdit;
    procedure SetOnChange(Event: TNotifyEvent); override;
    procedure SetEnabled(AEnabled: boolean); override;
    procedure SetData(AData: string); override;
    function GetData: string; override;
  public
    constructor Create(Parent: TWinControl; Top, Left: integer); override;
    destructor Destroy; override;
    function Correct: boolean; override;
  end;

  TNumEditField = class(TStrEditField)
  public
    constructor Create(Parent: TWinControl; Top, Left: integer); override;
  end;

  TDateEditField = class(TStrEditField)
  private
    FDateEdit: TDBDateEdit;
    procedure EditingDoneEvent(Sender: TObject);
    procedure SetEnabled(AEnabled: boolean); override;
  public
    constructor Create(Parent: TWinControl; Top, Left: integer); override;
    destructor Destroy; override;
  end;

  TFilterPanel = class(TCustomControl)
  private
    FEnabled: boolean;
    FFilter: TDBFilter;
    FTable: TDBTable;
    FFieldsCBox: TComboBox;
    FOps: TFilterOps;
    FInsertField: TInsertFieldFactory;
    FOnChange: TEvent;
    procedure LoadFieldsCBox;
    procedure OnChangeEvent(Sender: TObject);
    procedure OnFieldsCBoxChange(Sender: TObject);
    procedure SetState(AEnabled: boolean);
    procedure NumOpsLoad;
    procedure StrOpsLoad;
    procedure DateOpsLoad;
    procedure OnNumDefault;
    procedure OnStrDefault;
    procedure OnDateDefault;
    procedure InitOpsData;
    procedure LoadInsertField(InsertFieldType: TInsertFieldType);
  public
    constructor Create(Table: TDBTable);
    constructor Create(Table: TDBTable; AParent: TWinControl; ATop, ALeft: integer);
    procedure InitControls(AParent: TWinControl; ATop, ALeft: integer);
    procedure SetFilterData(Field, COp, Param: string);
    function Correct: boolean;
  published
    property Filter: TDBFilter read FFilter;
    property OnChange: TEvent write FOnChange;
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

function ToOperator(Name, COperator: string; OnSelect: TEvent = nil): TOperator;
begin
  Result.Name := Name;
  Result.COperator := COperator;
  Result.OnSelect := OnSelect;
end;

function TOperatorV.Equal(A, B: TOperator): boolean;
begin
  Exit(A.COperator = B.COperator);
end;

procedure TFilterOps.AddOperator(Ops: TOperatorV; AName, COperator: string;
  AOnSelect: TEvent = nil);
begin
  Ops.PushBack(ToOperator(AName, COperator, AOnSelect));
end;

procedure TFilterOps.LoadOpsFromDataType(DataType: TFieldType);
var
  i: integer;
begin
  for i := 0 to FOps.Size - 1 do
    if DataType = FOps[i].DataType then begin
      Load(i);
      Exit;
    end;
end;

procedure TFilterOps.InitOps;
var
  i: integer;
begin
  FOps := TOperators.Create;
  FOps.Resize(3);
  for i := 0 to 2 do
    FOps[i] := TOperatorV.Create;

  FOps[0].DataType := ftInteger;
  FOps[1].DataType := ftString;
  FOps[2].DataType := ftDate;
end;

function TFilterOps.GetOps: TOperatorV;
begin
  Exit(FOps[FCurOps]);
end;

procedure TFilterOps.OnSelectEvent(Sender: TObject);
begin
  if FOnSelect <> nil then
    FOnSelect(Self);
  if DataOps[ItemIndex].OnSelect <> nil then
    DataOps[ItemIndex].OnSelect;
end;

constructor TFilterOps.Create(AParent: TWinControl; ATop, ALeft: integer);
begin
  inherited Create(AParent);
  OnChange := @OnSelectEvent;
  OnSelect := @OnSelectEvent;
  FOnSelect := nil;
  Parent := AParent;
  Top := ATop;
  Left := ALeft;
  Width := 100;
  InitOps;
  Load(0);
end;

procedure TFilterOps.Load(Index: integer);
var
  i: integer;
begin
  FCurOps := Index;
  Items.Clear;
  for i := 0 to FOps[Index].Size - 1 do
    Items.Add(FOps[Index][i].Name);
  if FOps[Index].OnLoad <> nil then
    FOps[Index].OnLoad;
  ItemIndex := 0;
end;

function TFilterOps.FindAndLoad(COperator: string): integer;
var
  i: integer;
  Index: integer;
begin
  for i := 0 to FOps.Size - 1 do begin
    Index := FOps[i].FindInd(ToOperator('', COperator));
    if Index <> -1 then begin
      Load(i);
      ItemIndex := Index;
      Exit(Index);
    end;
  end;
end;


procedure TInsertFieldFactory.SetEnabled(AEnabled: boolean);
begin
  FEnabled := AEnabled;
end;

constructor TInsertFieldFactory.Create(Parent: TWinControl; Top, Left: integer);
begin
  FEnabled := True;
end;

destructor TInsertFieldFactory.Destroy;
begin
  inherited Destroy;
end;

procedure TInsertFieldFactory.InitComponent(Component, AParent: TWinControl;
  ATop, ALeft, AWidth: integer);
begin
  Component.Parent := AParent;
  Component.Top := ATop;
  Component.Left := ALeft;
  Component.Width := AWidth;
end;

procedure TStrEditField.SetOnChange(Event: TNotifyEvent);
begin
  FEdit.OnChange := Event;
end;

procedure TStrEditField.SetEnabled(AEnabled: boolean);
begin
  inherited SetEnabled(AEnabled);
  FEdit.Enabled := AEnabled;
end;

procedure TStrEditField.SetData(AData: string);
begin
  FEdit.Text := AData;
end;

function TStrEditField.GetData: string;
begin
  Exit(FEdit.Text);
end;

constructor TStrEditField.Create(Parent: TWinControl; Top, Left: integer);
begin
  FEdit := TEdit.Create(Parent);
  InitComponent(FEdit, Parent, Top, Left, 150);
end;

destructor TStrEditField.Destroy;
begin
  FEdit.Free;
  inherited Destroy;
end;

function TStrEditField.Correct: boolean;
begin
  Exit(FEdit.Text <> '');
end;

constructor TNumEditField.Create(Parent: TWinControl; Top, Left: integer);
begin
  inherited Create(Parent, Top, Left);
  FEdit.NumbersOnly := True;
end;

procedure TDateEditField.SetEnabled(AEnabled: boolean);
begin
  FDateEdit.Enabled := AEnabled;
end;

procedure TDateEditField.EditingDoneEvent(Sender: TObject);
begin
  if FDateEdit.Text <> '' then
    FEdit.Text := FDateEdit.Text;
end;

constructor TDateEditField.Create(Parent: TWinControl; Top, Left: integer);
begin
  inherited Create(Parent, Top, Left);
  FEdit.Width := 130;
  FEdit.Enabled := False;
  FDateEdit := TDBDateEdit.Create(Parent);
  FDateEdit.OnChange := @EditingDoneEvent;
  InitComponent(FDateEdit, Parent, Top, Left + 130, 23);
end;

destructor TDateEditField.Destroy;
begin
  FDateEdit.Free;
  inherited Destroy;
end;

constructor TFilterPanel.Create(Table: TDBTable);
begin
  inherited Create;
  FOnChange := nil;
  FInsertField := nil;
  FEnabled := True;
  FFilter := TDBFilter.Create;
  FTable := Table;
  FFilter.Assign(Table.Fields[0]);
end;

constructor TFilterPanel.Create(Table: TDBTable; AParent: TWinControl;
  ATop, ALeft: integer);
begin
  Create(Table);
  InitControls(AParent, ATop, ALeft);
end;

function TFilterPanel.Correct: boolean;
begin
  Exit(FInsertField.Correct);
end;

procedure TFilterPanel.InitControls(AParent: TWinControl; ATop, ALeft: integer);
begin
  inherited Create(AParent, 402 + 20, ATop, ALeft);
  FFieldsCBox := TComboBox.Create(Self);
  FOps := TFilterOps.Create(Self, 1, 151);

  FFieldsCBox.ReadOnly := True;
  FOps.ReadOnly := True;
  InitComponent(FFieldsCBox, Self, 1, 0, 150);
  DelBtn.Left := 402;
  DelBtn.Top := 0;

  FFieldsCBox.OnSelect := @OnFieldsCBoxChange;
  FOps.OnSelect := @OnChangeEvent;
  FOps.Ops[0].OnLoad := @NumOpsLoad;
  FOps.Ops[1].OnLoad := @StrOpsLoad;
  FOps.Ops[2].OnLoad := @DateOpsLoad;
  InitOpsData;

  LoadFieldsCBox;
end;

procedure TFilterPanel.SetFilterData(Field, COp, Param: string);
begin
  FFieldsCBox.ItemIndex := FindInd(Field, FFieldsCBox.Items);
  FOps.FindAndLoad(COp);
  FInsertField.Data := Param;
end;

procedure TFilterPanel.LoadFieldsCBox;
var
  i: integer;
begin
  for i := 0 to FFilter.ParentTable.Count - 1 do
    FFieldsCBox.Items.Add(FFilter.ParentTable.Fields[i].Name);
  FFieldsCBox.ItemIndex := 0;
  FOps.Load(0);
end;

procedure TFilterPanel.OnChangeEvent(Sender: TObject);
begin
  FFilter.Assign(FTable.Fields[FFieldsCBox.ItemIndex]);
  FFilter.ConditionalOperator := FOps.DataOps[FOps.ItemIndex].COperator;
  FFilter.Param := FInsertField.Data;
  if FOnChange <> nil then
    FOnChange;
end;

procedure TFilterPanel.OnFieldsCBoxChange(Sender: TObject);
begin
  FOps.LoadOpsFromDataType(FTable.Fields[FFieldsCBox.ItemIndex].DataType);
  if FOnChange <> nil then
    FOnChange;
end;

procedure TFilterPanel.SetState(AEnabled: boolean);
begin
  FEnabled := AEnabled;
  FInsertField.Enabled := AEnabled;
  FFieldsCBox.Enabled := AEnabled;
  FOps.Enabled := AEnabled;
  DelBtn.Enabled := AEnabled;
end;

procedure TFilterPanel.LoadInsertField(InsertFieldType: TInsertFieldType);
begin
  if FInsertField <> nil then
    FInsertField.Destroy;
  FInsertField := InsertFieldType.Create(Self, 1, 252);
  FInsertField.OnChange := @OnChangeEvent;
end;

procedure TFilterPanel.NumOpsLoad;
begin
  LoadInsertField(TNumEditField);
end;

procedure TFilterPanel.StrOpsLoad;
begin
  LoadInsertField(TStrEditField);
end;

procedure TFilterPanel.DateOpsLoad;
begin
  LoadInsertField(TDateEditField);
end;

procedure TFilterPanel.OnNumDefault;
begin
  LoadInsertField(TNumEditField);
end;

procedure TFilterPanel.OnStrDefault;
begin
  LoadInsertField(TStrEditField);
end;

procedure TFilterPanel.OnDateDefault;
begin
  LoadInsertField(TDateEditField);
end;

procedure TFilterPanel.InitOpsData;
begin
  with FOps do begin
    AddOperator(Ops[0], 'Равно', ' = ');
    AddOperator(Ops[0], 'Больше', ' > ');
    AddOperator(Ops[0], 'Меньше', ' < ');

    AddOperator(Ops[1], 'Содержит', ' Containing ');
    AddOperator(Ops[1], 'Не содержит', ' Not Containing ');
    AddOperator(Ops[1], 'Начинается с', ' Starting With ');

    AddOperator(Ops[2], 'Равно', ' = ');
    AddOperator(Ops[2], 'Больше', ' > ');
    AddOperator(Ops[2], 'Меньше', ' < ');
  end;
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
      Filters.PushBackA(Items[i].Filter);
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
