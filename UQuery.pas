unit UQuery;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UDBObjects, DB, UStringUtils;

type

  TDBFieldType = class of TDBField;

  TDBFieldQuery = class(TInterfacedObject, IFieldQuery, IFieldConstructor)
  private
    FField: TDBField;
    function RealName: string;
    function InnerJoin: string;
    function SelectedFields: string;
  public
    function Update(RecordID: integer; Data: TParam): TQueryContainer;
    constructor Create(Field: TDBField);
    function __QConstructor: IFieldConstructor;
  end;

  TDBRefFieldQuery = class(TInterfacedObject, IFieldQuery, IFieldConstructor)
  private
    FField: TDBRefField;
    function RealName: string;
    function InnerJoin: string;
    function SelectedFields: string;
  public
    function Update(RecordID: integer; Data: TParam): TQueryContainer;
    constructor Create(Field: TDBRefField);
    function __QConstructor: IFieldConstructor;
  end;

  TDBTableQuery = class(TInterfacedObject, ITableQuery)
  public
    FTable: TDBTable;
    function Insert(Values: TParams): TQueryContainer;
    function Delete(RecordID: integer): TQueryContainer;
    function Select(Filters: TDBFilters): TQueryContainer;
    constructor Create(Table: TDBTable);
  end;

implementation

function TDBFieldQuery.RealName: string;
begin
  Exit(FField.NativeName);
end;

function TDBFieldQuery.InnerJoin: string;
begin
  Exit('');
end;

function TDBFieldQuery.SelectedFields: string;
begin
  with FField do
    Exit(Format('%s.%s, ', [ParentTable.NativeName, NativeName]));
end;

function TDBFieldQuery.Update(RecordID: integer; Data: TParam): TQueryContainer;
begin
  with FField do
    Result.Query := Format('Update %s Set %s.%s = :Param0 Where %s.Id = %s ',
      [ParentTable.NativeName, ParentTable.NativeName, NativeName,
      ParentTable.NativeName, IntToStr(RecordID)]);
  Result.Params := TParams.Create;
  Result.Params.AddParam(Data);
  Result.Params.Items[0].Name := 'Param0';
end;

constructor TDBFieldQuery.Create(Field: TDBField);
begin
  FField := Field;
end;

function TDBFieldQuery.__QConstructor: IFieldConstructor;
begin
  Exit(Self);
end;

function TDBRefFieldQuery.RealName: string;
begin
  Exit(FField.RefFieldName);
end;

function TDBRefFieldQuery.InnerJoin: string;
begin
  with FField do
    Exit(
      Format('Inner Join %s On %s.%s = %s.%s ',
      [ParentTable.NativeName, ParentTable.NativeName,
      ParentTable.IDField.NativeName, RefTable.NativeName,
      RefFieldName]));
end;

function TDBRefFieldQuery.SelectedFields: string;
begin
  with FField do
    Exit(Format('%s.%s, ', [ParentTable.NativeName, NativeName]));
end;

function TDBRefFieldQuery.Update(RecordID: integer; Data: TParam): TQueryContainer;
begin
  with FField do
    Result.Query := Format('Update %s Set %s.%s = :Param0 Where %s.Id = %s ',
      [RefTable.NativeName, RefTable.NativeName, RefFieldName,
      RefTable.NativeName, IntToStr(RecordID)]);
  Result.Params := TParams.Create;
  Result.Params.AddParam(Data);
  Result.Params.Items[0].Name := 'Param0';
end;

constructor TDBRefFieldQuery.Create(Field: TDBRefField);
begin
  FField := Field;
end;

function TDBRefFieldQuery.__QConstructor: IFieldConstructor;
begin
  Exit(Self);
end;

function TDBTableQuery.Insert(Values: TParams): TQueryContainer;
var
  i: integer = 1;
  SameTable: TDBTable = nil;
begin
  with FTable do begin
    Result.Query := Format('Insert Into %s Values(0, ', [NativeName]);
    Result.Params := TParams.Create;

    while i < Count do begin
      if SameTable <> Fields[i].ParentTable then begin
        Result.Query += Format(':%s, ', [Param(i)]);
        Result.Params.AddParam(Values.ParamByName(Fields[i].NativeName));
        Result.Params.ParamByName(Fields[i].NativeName).Name := Param(i);
        SameTable := Fields[i].ParentTable;
      end;
      i += 1;
    end;
    DeleteLastSymbols(Result.Query, 2);
    Result.Query += ') ';
  end;
end;

function TDBTableQuery.Delete(RecordID: integer): TQueryContainer;
begin
  Result.Params := nil;
  with FTable do
    Result.Query := Format('Delete From %s Where %s = %s ',
      [NativeName, Fields[0].NativeName, IntToStr(RecordID)]);
end;

function TDBTableQuery.Select(Filters: TDBFilters): TQueryContainer;
var
  i: integer;
begin
  with FTable do begin
    Result.Query := 'Select ';
    Result.Params := nil;
    for i := 0 to Count - 1 do
      Result.Query += Format('%s.%s, ', [Fields[i].ParentTable.NativeName,
        Fields[i].NativeName]);
    DeleteLastSymbols(Result.Query, 2);
    Result.Query += Format(' From %s ', [NativeName]);

    for i := 1 to Count - 1 do
      if (Fields[i].Query.__QConstructor.InnerJoin <>
        Fields[i - 1].Query.__QConstructor.InnerJoin) then
        Result.Query += Fields[i].Query.__QConstructor.InnerJoin;

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
end;

constructor TDBTableQuery.Create(Table: TDBTable);
begin
  FTable := Table;
end;

end.
