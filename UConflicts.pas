unit UConflicts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UVector, Dialogs, Controls, ExtCtrls,
  StdCtrls, UCustomControl, UElementaryFunctions;

type

  TConflictType = class;
  TConflictPanel = class;
  TDataFilter = class;
  TExpression = class;
  TDataFilterType = class of TDataFilter;

  TEditEvent = procedure(Conflict: TConflictPanel) of object;

  TExpressions = specialize TObjVector<TExpression>;
  TDataFilters = specialize TObjVector<TDataFilter>;
  TDataTuple = specialize TVector<TStringV>;
  TData = specialize TVector<TDataTuple>;
  TConflictTypeV = specialize TVector<TConflictType>;

  TResult = record
    RecID: integer;
    ConflictType: TConflictType;
  end;
  TResultTuple = array of TResult;

  TDataFilter = class
  private
    FPriority: integer;
    FFilteredRecs: TIntegerV;
    procedure DeleteOneElTuples(AData: TData);
    function FilteredData(AData: TData; RecID: integer): TData; virtual;
  public
    constructor Create(RecIDs: TIntegerV); virtual;
    destructor Destroy; override;
    procedure Filter(var AData: TData);
    function FilteredTuple(ADataTuple: TDataTuple; RecID: integer): TData; virtual;
  published
    property FilteredRecs: TIntegerV read FFilteredRecs;
    property Priority: integer read FPriority;
  end;

  // separates data tuple into set of vectors, each vector represents
  // set of data with the same equal records
  TSeparateEqualRecsFilter = class(TDataFilter)
  private
    function FilteredData(AData: TData; RecID: integer): TData; override;
  public
    constructor Create(FieldIDs: TIntegerV); override;
    function FilteredTuple(ADataTuple: TDataTuple; RecID: integer): TData; override;
  end;

  // deletes data tuple item if its records occures once within data tuple
  TDeleteNotEqualRecsFilter = class(TDataFilter)
  private
    function FilteredData(AData: TData; RecID: integer): TData; override;
  public
    constructor Create(FieldIDs: TIntegerV); override;
    function FilteredTuple(ADataTuple: TDataTuple; RecID: integer): TData; override;
  end;

  // checks conflicts within fields of a record
  TInnerFilter = class(TDataFilter)

  end;

  TExpression = class
  private
    FCompareF: TIntCompareFunction;
    FECompareF: EnumIntCompareFunctions;
    FAggregateF: TIntAggregateFunction;
    FEAggregateF: EnumIntAggregateFunctions;
    FFieldA: integer;
    FFieldB: integer;
  public
    constructor Create(FieldA, FieldB: integer; ECompareF: EnumIntCompareFunctions;
      EAgregateF: EnumIntAggregateFunctions);
    procedure Filter(var AData: TData);
  published
    property FieldA: integer read FFieldA;
    property FieldB: integer read FFieldB;
    property ECompareFunc: EnumIntCompareFunctions read FECompareF write FECompareF;
    property EAggregateFunc: EnumIntAggregateFunctions
      read FEAggregateF write FEAggregateF;
  end;

  TExprFunctions = class
  private
    FCompareFs: array of TIntCompareFunctionData;
    FAggregateFs: array of TIntAggregateFunctionData;
    procedure AddCmp(AName: string; Cmp: EnumIntCompareFunctions);
    procedure AddOperation(AName: string; Operation: EnumIntAggregateFunctions);
    function GetCompareFCount: integer;
    function GetAggregateFCount: integer;
    function GetCompareF(AIndex: integer): TIntCompareFunctionData;
    function GetAggregateF(AIndex: integer): TIntAggregateFunctionData;
  public
    constructor Create;
    destructor Destroy; override;
    function FindInd(ECompareF: EnumIntCompareFunctions): integer;
    function FindInd(EAggregateF: EnumIntAggregateFunctions): integer;

    property CompareFs[Index: integer]: TIntCompareFunctionData read GetCompareF;
    property AggregateFs[Index: integer]: TIntAggregateFunctionData read GetAggregateF;
  published
    property CompareFCount: integer read GetCompareFCount;
    property AggregateFCount: integer read GetAggregateFCount;
  end;

  TConflictType = class
  private
    FName: string;
    FFilters: TDataFilters;
    FExpressions: TExpressions;
    FData: TData;
  public
    constructor Create(AName: string);
    destructor Destroy; override;
    procedure SelectConflicted(DataTuple: TDataTuple);
    procedure AddFilter(Filter: TDataFilter);
    procedure AddFilter(Fields: TIntegerV; FilterType: TDataFilterType);
    function ResultTuple: TResultTuple;
  published
    property Name: string read FName;
    property Data: TData read FData;
    property Filters: TDataFilters read FFilters write FFilters;
    property Expressions: TExpressions read FExpressions write FExpressions;
  end;

  TConflictPanel = class(TCustomControl)
  private
    FConflict: TConflictType;
    FEditBtn: TButton;
    FEdit: TEdit;
    FOnEdit: TEditEvent;
    procedure EditClick(Sender: TObject);
  public
    constructor Create(AName: string);
    constructor Create(AName: string; AParent: TWinControl; ATop, ALeft: integer);
    procedure InitControls(AParent: TWinControl; ATop, ALeft: integer);
  published
    property Conflict: TConflictType read FConflict write FConflict;
    property Edit: TEdit read FEdit write FEdit;
  end;

  TCustomConflictPanels = specialize TCustomControls<TConflictPanel>;

  TConflictPanels = class(TCustomConflictPanels)
  private
    FOnEdit: TEditEvent;
    procedure SetEvent(Event: TEditEvent);
  public
    constructor Create(AParent: TWinControl; ATop, ALeft: integer); override;
    procedure AnalyzeData(Data: TDataTuple);
    procedure AddConflictPanel(AName: string; Filters: TDataFilters;
      Expressions: TExpressions = nil);
    function AddConflictPanel(Conflict: TConflictType): TConflictPanel;
    function GetResult: TResultTuple;
  published
    property OnEditClick: TEditEvent write SetEvent;
  end;

implementation

procedure FreeData(var AData: TData);
var
  i: integer;
begin
  for i := 0 to AData.Size - 1 do
    AData[i].Free;
  AData.Free;
  AData := nil;
end;

procedure ShowTuple(ATuple: TDataTuple);
var
  i: integer;
  j: integer;
  Temp: TStringV;
  Text: string;
begin
  Text := '';
  for i := 0 to ATuple.Size - 1 do begin
    Temp := ATuple[i];
    for j := 0 to Temp.Size - 1 do begin
      Text += Temp[j];
      Text += #13#10;
    end;
    Text += #13#10;
  end;
  ShowMessage(Text);
end;

procedure ShowData(AData: TData);
var
  i: integer;
begin
  for i := 0 to AData.Size - 1 do
    ShowTuple(AData[i]);
end;

procedure TDataFilter.DeleteOneElTuples(AData: TData);
var
  i: integer = 0;
  Size: integer;
begin
  Size := AData.Size;
  while i < Size do
    if AData[i].Size <= 1 then begin
      AData[i].Free;
      AData.DeleteInd(i);
      Size -= 1;
    end
    else
      i += 1;
end;

function TDataFilter.FilteredTuple(ADataTuple: TDataTuple; RecID: integer): TData;
begin
  Exit(nil);
end;

function TDataFilter.FilteredData(AData: TData; RecID: integer): TData;
begin
  Exit(nil);
end;

constructor TDataFilter.Create(RecIDs: TIntegerV);
begin
  FPriority := -1;
  FFilteredRecs := RecIDs;
end;

destructor TDataFilter.Destroy;
begin
  FFilteredRecs.Free;
  inherited Destroy;
end;

procedure TDataFilter.Filter(var AData: TData);
var
  i: integer;
begin
  for i := 0 to FFilteredRecs.Size - 1 do begin
    AData := FilteredData(AData, FFilteredRecs[i]);
    if AData = nil then
      Exit;
  end;
end;

function TSeparateEqualRecsFilter.FilteredTuple(ADataTuple: TDataTuple;
  RecID: integer): TData;
var
  i: integer = 1;
  j: integer;
  Index: integer;
begin
  Result := TData.Create(TDataTuple.Create(ADataTuple[0]));
  for i := 1 to ADataTuple.Size - 1 do begin
    Index := -1;
    for j := 0 to Result.Size - 1 do
      if Result[j][0][RecID] = ADataTuple[i][RecID] then begin
        Index := j;
        break;
      end;
    if Index <> -1 then
      Result[Index].PushBack(ADataTuple[i])
    else
      Result.PushBack(TDataTuple.Create(ADataTuple[i]));
  end;
  DeleteOneElTuples(Result);
  Exit(Result);
end;

function TSeparateEqualRecsFilter.FilteredData(AData: TData; RecID: integer): TData;
var
  i: integer = 0;
  j: integer;
  Buffer: TData;
begin
  Result := TData.Create;
  for i := 0 to AData.Size - 1 do begin
    Buffer := FilteredTuple(AData[i], RecID);
    for j := 0 to Buffer.Size - 1 do
      Result.PushBack(Buffer[j]);
    Buffer.Free;
  end;
  FreeData(AData);
end;

constructor TSeparateEqualRecsFilter.Create(FieldIDs: TIntegerV);
begin
  inherited Create(FieldIDs);
  FPriority := 0;
end;

function TDeleteNotEqualRecsFilter.FilteredTuple(ADataTuple: TDataTuple;
  RecID: integer): TData;
var
  i: integer = 1;
  j: integer;
  Counter: integer;
begin
  Result := TData.Create(TDataTuple.Create);
  for i := 0 to ADataTuple.Size - 1 do begin
    Counter := 0;
    for j := 0 to ADataTuple.Size - 1 do
      if (ADataTuple[i][RecID] = ADataTuple[j][RecID]) then begin
        Counter += 1;
        if Counter > 1 then
          break;
      end;
    if Counter = 1 then
      Result[0].PushBack(ADataTuple[i]);
  end;
  if Result[0].Empty then begin
    FreeData(Result);
    Exit(nil);
  end;
  Exit(Result);
end;

function TDeleteNotEqualRecsFilter.FilteredData(AData: TData; RecID: integer): TData;
var
  i: integer;
  j: integer;
  Buffer: TData;
begin
  Result := TData.Create;
  for i := 0 to AData.Size - 1 do begin
    Buffer := FilteredTuple(AData[i], RecID);
    if Buffer <> nil then
      for j := 0 to Buffer.Size - 1 do
        Result.PushBack(Buffer[j]);
    Buffer.Free;
  end;
  if Result.Empty then begin
    Result.Free;
    Exit(nil);
  end;
  FreeData(AData);
  DeleteOneElTuples(Result);
end;

constructor TDeleteNotEqualRecsFilter.Create(FieldIDs: TIntegerV);
begin
  inherited Create(FieldIDs);
  FPriority := 1;
end;

constructor TExpression.Create(FieldA, FieldB: integer; ECompareF: EnumIntCompareFunctions;
  EAgregateF: EnumIntAggregateFunctions);
begin
  FFieldA := FieldA;
  FFieldB := FieldB;
  FECompareF := ECompareF;
  FEAggregateF := EAgregateF;
  FCompareF := TIntCompareFunctions.GetFunc(ECompareF);
  FAggregateF := TIntAggregateFunctions.GetFunc(EAgregateF);
end;

procedure TExpression.Filter(var AData: TData);
var
  AggregateResult: integer;
  Size: integer;
  i: integer = 0;
  j: integer;
begin
  if AData.Size = 0 then
    Exit;

  Size := AData.Size;
  while i < Size do begin
    AggregateResult := 0;
    for j := 0 to AData[i].Size - 1 do
      AggregateResult := FAggregateF(AggregateResult, StrToInt(AData[i][j][FFieldB]));
    if not FCompareF(StrToInt(AData[i][0][FFieldA]), AggregateResult) then begin
      AData[i].Free;
      AData.DeleteInd(i);
      Size -= 1;
    end
    else
      i += 1;
  end;

  if AData.Size = 0 then begin
    AData.Free;
    AData := nil;
  end;
end;

procedure TConflictPanels.SetEvent(Event: TEditEvent);
var
  i: integer;
begin
  FOnEdit := Event;
  for i := 0 to Size - 1 do
    Items[i].FOnEdit := Event;
end;

constructor TConflictPanels.Create(AParent: TWinControl; ATop, ALeft: integer);
begin
  FOnEdit := nil;
  inherited Create(AParent, ATop, ALeft);
end;

procedure TConflictPanels.AddConflictPanel(AName: string; Filters: TDataFilters;
  Expressions: TExpressions = nil);
var
  Conflict: TConflictType;
  i: integer;
begin
  Conflict := TConflictType.Create(AName);
  for i := 0 to Filters.Size - 1 do
    Conflict.AddFilter(Filters[i]);
  if Expressions <> nil then begin
    Conflict.Expressions.Free;
    Conflict.Expressions := Expressions;
  end;
  AddConflictPanel(Conflict);
end;

function TConflictPanels.AddConflictPanel(Conflict: TConflictType): TConflictPanel;
begin
  Result := TConflictPanel.Create(Conflict.Name, FParent, FTop, FLeft);
  Result.Conflict := Conflict;
  Result.FOnEdit := FOnEdit;
  AddControlPanel(Result);
end;

procedure TConflictPanels.AnalyzeData(Data: TDataTuple);
var
  i: integer;
begin
  for i := 0 to Size - 1 do
    Items[i].Conflict.SelectConflicted(Data);
  Data.Free;
end;

function TConflictPanels.GetResult: TResultTuple;
var
  Temp: TResultTuple = nil;
  i: integer;
  j: integer;
begin
  Result := nil;
  for i := 0 to Size - 1 do begin
    Temp := Items[i].Conflict.ResultTuple;
    if Temp <> nil then
      for j := 0 to High(Temp) do begin
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := Temp[j];
      end;
    Temp := nil;
  end;
  if Length(Result) = 0 then
    Result := nil;
end;

constructor TConflictType.Create(AName: string);
begin
  FName := AName;
  FFilters := TDataFilters.Create;
  FExpressions := TExpressions.Create;
end;

destructor TConflictType.Destroy;
begin
  FFilters.Free;
  FExpressions.Free;
  FData.Free;
  inherited Destroy;
end;

procedure TConflictType.AddFilter(Filter: TDataFilter);
var
  i: integer;
  j: integer;
begin
  if FFilters.Empty then begin
    FFilters.PushBack(Filter);
    Exit;
  end;

  for i := 0 to FFilters.Size - 1 do
    if Filter.Priority < FFilters[i].Priority then begin
      FFilters.Resize(FFilters.Size + 1);
      for j := FFilters.Size - 1 to i + 1 do
        FFilters[j] := FFilters[j + 1];
      FFilters[i] := Filter;
      Exit;
    end;

  FFilters.PushBack(Filter);
end;

procedure TConflictType.AddFilter(Fields: TIntegerV; FilterType: TDataFilterType);
var
  Filter: TDataFilter;
begin
  Filter := FilterType.Create(Fields);
  AddFilter(Filter);
end;

function TConflictType.ResultTuple: TResultTuple;
var
  i: integer;
  j: integer;
begin
  if FData = nil then
    Exit(nil);

  Result := nil;
  for i := 0 to FData.Size - 1 do
    for j := 0 to FData[i].Size - 1 do begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)].RecID := StrToInt(FData[i][j][0]);
      Result[High(Result)].ConflictType := Self;
    end;
end;

procedure TConflictType.SelectConflicted(DataTuple: TDataTuple);
var
  i: integer;
begin
  if FData <> nil then
    FreeData(FData);
  if DataTuple.Empty or FFilters.Empty then
    Exit;

  FData := FFilters[0].FilteredTuple(DataTuple, FFilters[0].FFilteredRecs[0]);
  for i := 0 to FFilters.Size - 1 do begin
    FFilters[i].Filter(FData);
    if FData = nil then
      Exit;
  end;
  for i := 0 to FExpressions.Size - 1 do begin
    FExpressions[i].Filter(FData);
    if FData = nil then
      Exit;
  end;
end;

procedure TConflictPanel.EditClick(Sender: TObject);
begin
  if FOnEdit <> nil then
    FOnEdit(Self);
end;

constructor TConflictPanel.Create(AName: string);
begin
  FConflict := TConflictType.Create(AName);
  FOnEdit := nil;
end;

constructor TConflictPanel.Create(AName: string; AParent: TWinControl;
  ATop, ALeft: integer);
begin
  Create(AName);
  InitControls(AParent, ATop, ALeft);
end;

procedure TConflictPanel.InitControls(AParent: TWinControl; ATop, ALeft: integer);
begin
  inherited Create(AParent, 250 + 20, ATop, ALeft);
  FEdit := TEdit.Create(Self);
  FEditBtn := TButton.Create(Self);
  FEditBtn.Caption := 'Ред.';
  InitComponent(FEdit, Self, 1, 0, 200);
  InitComponent(FEditBtn, Self, 0, 200, 50);
  FEdit.Text := FConflict.FName;
  FEdit.Enabled := False;
  FEditBtn.OnClick := @EditClick;
  DelBtn.Left := 250;
  DelBtn.Top := 0;
end;

function TExprFunctions.GetCompareFCount: integer;
begin
  Exit(Length(FCompareFs));
end;

function TExprFunctions.GetAggregateFCount: integer;
begin
  Exit(Length(FAggregateFs));
end;

function TExprFunctions.GetCompareF(AIndex: integer): TIntCompareFunctionData;
begin
  Exit(FCompareFs[AIndex]);
end;

function TExprFunctions.GetAggregateF(AIndex: integer): TIntAggregateFunctionData;
begin
  Exit(FAggregateFs[AIndex]);
end;

procedure TExprFunctions.AddCmp(AName: string; Cmp: EnumIntCompareFunctions);
begin
  SetLength(FCompareFs, Length(FCompareFs) + 1);
  FCompareFs[High(FCompareFs)].Name := AName;
  FCompareFs[High(FCompareFs)].EFunc := Cmp;
end;

procedure TExprFunctions.AddOperation(AName: string;
  Operation: EnumIntAggregateFunctions);
begin
  SetLength(FAggregateFs, Length(FAggregateFs) + 1);
  FAggregateFs[High(FAggregateFs)].Name := AName;
  FAggregateFs[High(FAggregateFs)].EFunc := Operation;
end;

constructor TExprFunctions.Create;
begin
  AddCmp('Больше', cfGr);
  AddCmp('Меньше', cfLe);
  AddCmp('Равно', cfEq);
  AddCmp('Больше либо равно', cfGrEq);
  AddCmp('Меньше либо равно', cfLeEq);

  AddOperation('Сумма', afSum);
  AddOperation('Максимум', afMax);
  AddOperation('Минимум', afMin);
end;

destructor TExprFunctions.Destroy;
begin
  FCompareFs := nil;
  FAggregateFs := nil;
  inherited Destroy;
end;

function TExprFunctions.FindInd(ECompareF: EnumIntCompareFunctions): integer;
var
  i: integer;
begin
  for i := 0 to CompareFCount - 1 do
    if FCompareFs[i].EFunc = ECompareF then
      Exit(i);
  Exit(-1);
end;

function TExprFunctions.FindInd(EAggregateF: EnumIntAggregateFunctions): integer;
var
  i: integer;
begin
  for i := 0 to AggregateFCount - 1 do
    if FAggregateFs[i].EFunc = EAggregateF then
      Exit(i);
  Exit(-1);
end;

end.
