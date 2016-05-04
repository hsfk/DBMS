unit UConflicts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UVector, UMatrix, Dialogs, Controls, ExtCtrls, StdCtrls;

type

  TConflictType = class;
  TDataTuple = specialize TVector<TStringM>;
  TConflictTypes = specialize TObjVector<TConflictType>;
  TData = specialize TVector<TDataTuple>;

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
  end;

  TConflicts = class
  private
    FConflicts: TConflictTypes;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Conflict: TConflictType);
    procedure AnalyzeData(Data: TDataTuple);
    function GetResult: TResultTuple;
  end;

implementation

constructor TConflicts.Create;
var
  Conflict: TConflictType;
begin
  FConflicts := TConflictTypes.Create;

  Conflict := TConflictType.Create('Разрыв группы');
  with TIntegerV do
    Conflict.AddConditions(Create([9, 8, 6]), Create([7]));
  Add(Conflict);
end;

destructor TConflicts.Destroy;
begin
  FConflicts.Free;
  inherited Destroy;
end;

procedure TConflicts.Add(Conflict: TConflictType);
begin
  FConflicts.PushBack(Conflict);
end;

procedure TConflicts.AnalyzeData(Data: TDataTuple);
var
  i: integer;
begin
  for i := 0 to FConflicts.Size - 1 do
    FConflicts[i].SelectConflicted(Data);
end;

function TConflicts.GetResult: TResultTuple;
var
  Temp: TResultTuple;
  i: integer;
  j: integer;
begin
  SetLength(Result, 0);
  for i := 0 to FConflicts.Size - 1 do begin
    Temp := FConflicts[i].ResultTuple;
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

end.
