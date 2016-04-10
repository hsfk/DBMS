unit UQuery;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UDBObjects, DB, UVector, UStringUtils;

type

  TDBFieldType = class of TDBField;

  TDBFieldQuery = class(TDBField, IFieldQuery, IFieldConstructor)
  private
    function RealName: string;
    function InnerJoin: string;
    function SelectedFields: string;
  public
    function Update(RecordID: integer; Data: TParam): TQueryContainer;
    constructor Create(Field: TDBField);
    function QConstructor: IFieldConstructor;
  end;

  TDBRefFieldQuery = class(TDBReferenceField, IFieldQuery, IFieldConstructor)
  private
    function RealName: string;
    function InnerJoin: string;
    function SelectedFields: string;
  public
    function Update(RecordID: integer; Data: TParam): TQueryContainer;
    constructor Create(Field: TDBReferenceField);
    function QConstructor: IFieldConstructor;
  end;

  TDBTableQuery = class(TDBTable, ITableQuery)
  public
    function Insert(Values: TParams): TQueryContainer;
    function Delete(RecordID: integer): TQueryContainer;
    function Select(Filters: TDBFilters): TQueryContainer;
    constructor Create(Table: TDBTable);
  end;

implementation

function TDBFieldQuery.RealName: string;
begin
  Exit(NativeName);
end;

function TDBFieldQuery.InnerJoin: string;
begin
  Exit('');
end;

function TDBFieldQuery.SelectedFields: string;
begin
  Exit(
    Format('%s.%s, ', [ParentTable.NativeName, NativeName]));
end;

function TDBFieldQuery.Update(RecordID: integer; Data: TParam): TQueryContainer;
begin
  Result.Query := Format('Update %s Set %s.%s = :Param0 Where %s.Id = %s ',
    [ParentTable.NativeName, ParentTable.NativeName, NativeName,
    ParentTable.NativeName, IntToStr(RecordID)]);
  Result.Params := TParams.Create;
  Result.Params.AddParam(Data);
  Result.Params.Items[0].Name := 'Param0';
end;

constructor TDBFieldQuery.Create(Field: TDBField);
begin
  inherited Assign(Field);
end;

function TDBFieldQuery.QConstructor: IFieldConstructor;
begin
  Exit(Self);
end;

function TDBRefFieldQuery.RealName: string;
begin
  Exit(FRefFieldName);
end;

function TDBRefFieldQuery.InnerJoin: string;
begin
  Exit(
    Format('Inner Join %s On %s.%s = %s.%s ', [FParentTable.NativeName,
    FParentTable.NativeName, FJoinOn, FRefTable.NativeName, FRefFieldName]));
end;

function TDBRefFieldQuery.SelectedFields: string;
begin
  Exit(
    Format('%s.%s, ', [FParentTable.NativeName, NativeName]));
end;

function TDBRefFieldQuery.Update(RecordID: integer; Data: TParam): TQueryContainer;
begin
  Result.Query := Format('Update %s Set %s.%s = :Param0 Where %s.Id = %s ',
    [FRefTable.NativeName, FRefTable.NativeName, FRefFieldName,
    FRefTable.NativeName, IntToStr(RecordID)]);
  Result.Params := TParams.Create;
  Result.Params.AddParam(Data);
  Result.Params.Items[0].Name:='Param0';
end;

constructor TDBRefFieldQuery.Create(Field: TDBReferenceField);
begin
  inherited Assign(Field);
end;

function TDBRefFieldQuery.QConstructor: IFieldConstructor;
begin
  Exit(Self);
end;

function TDBTableQuery.Insert(Values: TParams): TQueryContainer;
var
  i: integer;
begin
  Result.Query := Format('Insert Into %s Values(0, ', [NativeName]);
  Result.Params := TParams.Create;
  for i := 1 to Count - 1 do begin
    Result.Query += Format(':%s, ', [Param(i)]);
    Result.Params.AddParam(Values.ParamByName(Fields[i].NativeName));
    Result.Params.ParamByName(Fields[i].NativeName).Name := Param(i);
  end;
  DeleteLastSymbols(Result.Query, 2);
  Result.Query += ') ';
end;

function TDBTableQuery.Delete(RecordID: integer): TQueryContainer;
begin
  Result.Params := nil;
  Result.Query := Format('Delete From %s Where %s = %s ',
    [NativeName, Fields[0].NativeName, IntToStr(RecordID)]);
end;

function TDBTableQuery.Select(Filters: TDBFilters): TQueryContainer;
var
  i: integer;
begin
  Result.Query := 'Select ';
  Result.Params := nil;
  for i := 0 to Count - 1 do
    Result.Query += Format('%s.%s, ', [Fields[i].ParentTable.NativeName,
      Fields[i].NativeName]);
  DeleteLastSymbols(Result.Query, 2);
  Result.Query += Format(' From %s ', [NativeName]);

  for i := 1 to Count - 1 do
    if (Fields[i].Query.QConstructor.InnerJoin <>
        Fields[i - 1].Query.QConstructor.InnerJoin) then
      Result.Query += Fields[i].Query.QConstructor.InnerJoin;

  if Filters <> nil then begin
    Result.Params := TParams.Create;
    Result.Query += 'Where ';
    for i := 0 to Filters.Size - 1 do begin
      with Filters do begin
        Result.Query += Format('%s.%s %s :%s And ',
          [Items[i].ParentTable.NativeName, Items[i].NativeName,
          Items[i].ConditionalOperator, Param(i)]);
        Result.Params.CreateParam(Items[i].DataType, Param(i), ptInput);
        Result.Params.ParamByName(Param(i)).Value := Items[i].Param;
      end;
    end;
    DeleteLastSymbols(Result.Query, 4);
  end;
end;

constructor TDBTableQuery.Create(Table: TDBTable);
begin
  inherited Assign(Table);
end;

end.
