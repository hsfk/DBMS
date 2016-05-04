unit UConflicts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UVector, UMatrix, Dialogs, Controls, ExtCtrls,
  StdCtrls, UCustomControl;

type

  TConflictType = class;
  TConflictPanel = class;
  TDataTuple = specialize TVector<TStringM>;
  TConflictTypes = specialize TObjVector<TConflictType>;
  TData = specialize TVector<TDataTuple>;
  TEditEvent = procedure(Conflict: TConflictPanel) of object;

  TResult = record
    RecID: integer;
    ConflictType: TConflictType;
  end;
  TResultTuple = array of TResult;

  TConflictType = class
  private
    FName: string;
    FEQRecIDs: TIntegerV;
    FNEQRecIDs: TIntegerV;
    FData: TData;
  public
    constructor Create(AName: string);
    destructor Destroy; override;
    procedure AddConditions(EQRecIDs, NEQRecIDs: TIntegerV);
    procedure ShowData(AData: TData);
    procedure ShowTuple(ATuple: TDataTuple);
    procedure SelectConflicted(DataTuple: TDataTuple);
    procedure DeleteOneElTuples(Data: TData);
    function ResultTuple: TResultTuple;
    // separates data tuple into set of vectors, each vector represents
    // set of data with the same equal records
    function SeparateEQ(Data: TDataTuple; EQRec: integer): TData;
    function SeparateEQ(Data: TData; EQRec: integer): TData;
    // deletes data tuple item if its NEQRec record occures once
    function DeleteNEQ(Data: TDataTuple; NEQRec: integer): TData;
    function DeleteNEQ(Data: TData; NEQRec: integer): TData;
  published
    property EQRecIDs: TIntegerV read FEQRecIDs write FEQRecIDs;
    property NEQRecIDs: TIntegerV read FNEQRecIDs write FNEQRecIDs;
    property Name: string read FName;
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
    constructor Create(AParent: TWinControl; ATop, ALeft: integer);
    procedure AddConflict(AName: string; EQRecIDs, NEQRecIDs: TIntegerV);
    procedure AddConflict(Conflict: TConflictType);
    procedure AnalyzeData(Data: TDataTuple);
    function GetResult: TResultTuple;
  published
    property OnEditClick: TEditEvent write SetEvent;
  end;

implementation

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
  with TIntegerV do begin
    AddConflict('Разрыв группы', Create([9, 8, 6]), Create([7]));
    AddConflict('Разрыв преподавателя', Create([3, 8, 9]), Create([7]));
    AddConflict('Больше 1 преподавателя в аудитории', Create([7, 8, 9, 6]), Create([3]));
    AddConflict('Больше 1 группы в аудитории', Create([7, 8, 9]), Create([6]));
  end;
end;

procedure TConflictPanels.AddConflict(AName: string; EQRecIDs, NEQRecIDs: TIntegerV);
var
  Panel: TConflictPanel;
begin
  Panel := TConflictPanel.Create(AName, FParent, FTop, FLeft);
  Panel.Conflict.AddConditions(EQRecIDs, NEQRecIDs);
  Panel.FOnEdit := FOnEdit;
  AddControlPanel(Panel);
end;

procedure TConflictPanels.AddConflict(Conflict: TConflictType);
var
  Panel: TConflictPanel;
begin
  Panel := TConflictPanel.Create(Conflict.Name, FParent, FTop, FLeft);
  Panel.Conflict := Conflict;
  Panel.FOnEdit := FOnEdit;
  AddControlPanel(Panel);
end;

procedure TConflictPanels.AnalyzeData(Data: TDataTuple);
var
  i: integer;
begin
  for i := 0 to Size - 1 do
    Items[i].Conflict.SelectConflicted(Data);
end;

function TConflictPanels.GetResult: TResultTuple;
var
  Temp: TResultTuple;
  i: integer;
  j: integer;
begin
  SetLength(Result, 0);
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
end;

destructor TConflictType.Destroy;
begin
  FEQRecIDs.Free;
  FNEQRecIDs.Free;
  FData.Free;
  inherited Destroy;
end;

procedure TConflictType.AddConditions(EQRecIDs, NEQRecIDs: TIntegerV);
begin
  FEQRecIDs := EQRecIDs;
  FNEQRecIDs := NEQRecIDs;
end;

procedure TConflictType.DeleteOneElTuples(Data: TData);
var
  i: integer = 0;
  Size: integer;
begin
  Size := Data.Size;
  while i < Size do
    if Data[i].Size <= 1 then begin
      Data.DeleteInd(i);
      Size -= 1;
    end
    else
      i += 1;
end;

function TConflictType.ResultTuple: TResultTuple;
var
  i: integer;
  j: integer;
begin
  if FData = nil then
    Exit(nil);
  SetLength(Result, 0);
  for i := 0 to FData.Size - 1 do
    for j := 0 to FData[i].Size - 1 do begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)].RecID := StrToInt(FData[i][j][0, 0]);
      Result[High(Result)].ConflictType := Self;
    end;
end;

procedure TConflictType.SelectConflicted(DataTuple: TDataTuple);
var
  i: integer;
begin
  if DataTuple.Size = 0 then
    Exit;
  FData := SeparateEQ(DataTuple, FEQRecIDs[0]);
  for i := 0 to FEQRecIDs.Size - 1 do
    FData := SeparateEQ(FData, FEQRecIDs[i]);
  for i := 0 to FNEQRecIDs.Size - 1 do begin
    FData := DeleteNEQ(FData, FNEQRecIDs[i]);
    if FData = nil then
      Exit; // no conflicts found
  end;
end;

function TConflictType.SeparateEQ(Data: TDataTuple; EQRec: integer): TData;
var
  i: integer = 1;
  j: integer;
  Index: integer;
begin
  Result := TData.Create(TDataTuple.Create(Data[0]));
  for i := 1 to Data.Size - 1 do begin
    Index := -1;
    for j := 0 to Result.Size - 1 do
      if Result[j][0][0, EQRec] = Data[i][0, EQRec] then begin
        Index := j;
        break;
      end;
    if Index <> -1 then
      Result[Index].PushBack(Data[i])
    else
      Result.PushBack(TDataTuple.Create(Data[i]));
  end;
  DeleteOneElTuples(Result);
  Exit(Result);
end;

function TConflictType.SeparateEQ(Data: TData; EQRec: integer): TData;
var
  i: integer = 0;
  j: integer;
  Buffer: TData;
begin
  Result := TData.Create;
  for i := 0 to Data.Size - 1 do begin
    Buffer := SeparateEQ(Data[i], EQRec);
    for j := 0 to Buffer.Size - 1 do
      Result.PushBack(Buffer[j]);
    Buffer.Free;
  end;
  Data.Free;
end;

function TConflictType.DeleteNEQ(Data: TDataTuple; NEQRec: integer): TData;
var
  i: integer = 1;
  j: integer;
  Counter: integer;
begin
  Result := TData.Create(TDataTuple.Create);
  for i := 0 to Data.Size - 1 do begin
    Counter := 0;
    for j := 0 to Data.Size - 1 do
      if (Data[i][0, NEQRec] = Data[j][0, NEQRec]) then begin
        Counter += 1;
        if Counter > 1 then
          break;
      end;
    if Counter = 1 then
      Result[0].PushBack(Data[i]);
  end;
  if Result[0].Size = 0 then begin
    Data.Free;
    Result.Free;
    Exit(nil);
  end;
  Exit(Result);
end;

function TConflictType.DeleteNEQ(Data: TData; NEQRec: integer): TData;
var
  i: integer;
  j: integer;
  Buffer: TData;
begin
  Result := TData.Create;
  for i := 0 to Data.Size - 1 do begin
    Buffer := DeleteNEQ(Data[i], NEQRec);
    if Buffer <> nil then
      for j := 0 to Buffer.Size - 1 do
        Result.PushBack(Buffer[j]);
    Buffer.Free;
  end;
  if Result.Size = 0 then begin
    Result.Free;
    Exit(nil);
  end;
end;

procedure TConflictType.ShowTuple(ATuple: TDataTuple);
var
  i: integer;
  j: integer;
  TempM: TStringM;
  Text: string;
begin
  Text := '';
  for i := 0 to ATuple.Size - 1 do begin
    TempM := ATuple[i];
    for j := 0 to TempM.Height - 1 do begin
      Text += TempM[0, j];
      Text += #13#10;
    end;
    Text += #13#10;
  end;
  ShowMessage(Text);
end;

procedure TConflictType.ShowData(AData: TData);
var
  i: integer;
begin
  for i := 0 to AData.Size - 1 do
    ShowTuple(AData[i]);
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

end.
