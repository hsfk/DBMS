unit UDBObjects;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DBGrids, DB, UMetaData, UVector, UNotifications,
  StdCtrls, Controls;

type

  {TODO: refactor RefFields
  tdbfielddata ???
  }

  TDBField = class;
  TDBReferenceField = class;
  TDBFilter = class;
  TDBControl = class;
  TDBTable = class;
  TDBMetaData = class;
  TDBFilters = specialize TVector<TDBFilter>;

  TQueryContainer = record
    Query: string;
    Params: TParams;
  end;

  IFieldConstructor = interface
    function RealName: string;
    function InnerJoin: string;
    function SelectedFields: string;
  end;

  IFieldQuery = interface
    function Update(RecordID: integer; Data: TParam): TQueryContainer;
    function __QConstructor: IFieldConstructor;
  end;

  ITableQuery = interface
    function Insert(Values: TParams): TQueryContainer;
    function Delete(RecordID: integer): TQueryContainer;
    function Select(Filters: TDBFilters): TQueryContainer;
  end;

  TDBField = class(TField)
  protected
    FQuery: IFieldQuery;
    FParentTable: TDBTable;
  public
    constructor Create;
    constructor Create(AName, ANativeName: string; AWidth: integer;
      ADataType: TFieldType; AParentTable: TDBTable = nil);
    procedure Load(Column: TColumn);
    function CreateControl: TDBControl; virtual;
    procedure Assign(Field: TDBField); virtual;
  published
    property ParentTable: TDBTable read FParentTable write FParentTable;
    property Query: IFieldQuery read FQuery;
  end;

  TDBReferenceField = class(TDBField)
  protected
    FJoinOn: string;
    FJoinedOnField: TDBField;
    FRefFieldName: string;
    FRefTable: TDBTable;
  public
    constructor Create(RefTable, RefFieldName: string; JoinedTable: TDBTable;
      JoinedOnFieldName, FieldName: string);
    function CreateControl: TDBControl; override;
    procedure Assign(Field: TDBReferenceField);
  published
    property JoinOn: string read FJoinOn;
    property JoinedOnField: TDBField read FJoinedOnField;
    property RefFieldName: string read FRefFieldName;
    property RefTable: TDBTable read FRefTable;
  end;

  TDBFilter = class(TDBField)
  private
    FParam: string;
    FOperator: string;
  public
    property Param: string read FParam write FParam;
    property ConditionalOperator: string read FOperator write FOperator;
    procedure Assign(Field: TDBField); override;
    constructor Create; overload;
    constructor Create(AField: TDBField; COperator, AParam: string); overload;
  end;

  TDBOrder = class(TDBField)
  private
    FOrder: string;
  public
    procedure Assign(Field: TDBField); override;
  published
    property Order: string write FOrder;
  end;

  TDBControl = class(TDBField)
  private
    FLabel: TLabel;
    FSubscriber: TSubscriber;
  protected
    function GetData: TParam; virtual; abstract;
    procedure SetCaption(AData: string); virtual; abstract;
    procedure OnNotificationRecieve(Sender: TObject); virtual;
  public
    function UpdateTable: TQueryContainer; virtual; abstract;
    procedure CreateGUI(AParent: TWinControl; ATop, ALeft: integer); virtual;
    procedure Clear; virtual; abstract;
    function Correct: boolean; virtual; abstract;
    procedure LoadData(Data: string; ID: integer); virtual; abstract;
  published
    property Subscriber: TSubscriber read FSubscriber write FSubscriber;
    property Caption: string write SetCaption;
    property Data: TParam read GetData;
  end;

  _TTable = specialize _TGData<TDBField>;

  TDBTable = class(_TTable)
  protected
    FQuery: ITableQuery;
    function CmpItemName(AField: TDBField; AName: string): boolean; override;
  public
    property Fields[AIndex: integer]: TDBField read GetItem write SetItem;
    property FieldsByName[AName: string]: TDBField read GetItemByName;
    procedure SetFields(AFields: array of TDBField);
    constructor Create(AName, ANativeName: string; AIndex: integer = -1);
    procedure Assign(Table: TDBTable);
  published
    property Query: ITableQuery read FQuery;
  end;

  _TMetaData = specialize _TGData<TDBTable>;

  TDBMetaData = class(_TMetaData)
  protected
    function CmpItemName(ATable: TDBTable; AName: string): boolean; override;
  public
    property Tables[AIndex: integer]: TDBTable read GetItem write SetItem;
    property TablesByName[AName: string]: TDBTable read GetItemByName;
    procedure SetTables(ATables: array of TDBTable);
  end;

var
  DBData: TDBMetaData;

implementation

uses UCardControls, UQuery;

constructor TDBField.Create;
begin
 { An empty constructor for descendant classes }
end;

constructor TDBField.Create(AName, ANativeName: string; AWidth: integer;
  ADataType: TFieldType; AParentTable: TDBTable = nil);
begin
  inherited Create(AName, ANativeName, AWidth, ADataType);
  FQuery := TDBFieldQuery.Create(Self);
end;

procedure TDBField.Load(Column: TColumn);
begin
  Column.Title.Caption := Name;
  Column.Width := Width;
end;

function TDBField.CreateControl: TDBControl;
begin
  Result := TDBEditControl.Create;
  Result.Subscriber := TSubscriber.Create(False);
  Result.Assign(Self);
end;

procedure TDBField.Assign(Field: TDBField);
begin
  inherited Assign(Field);
  FParentTable := Field.ParentTable;
end;

constructor TDBReferenceField.Create(RefTable, RefFieldName: string;
  JoinedTable: TDBTable; JoinedOnFieldName, FieldName: string);
var
  AJoinedField: TDBField;
  AJoinedOnField: TDBField;
begin
  FRefTable := DBData.TablesByName[RefTable];
  FRefFieldName := RefFieldName;
  AJoinedField := JoinedTable.FieldsByName[FieldName];
  AJoinedOnField := JoinedTable.FieldsByName[JoinedOnFieldName];
  FJoinOn := AJoinedOnField.NativeName;
  with AJoinedField do
    inherited Create(Name, NativeName, Width, DataType);
  FParentTable := AJoinedField.ParentTable;
  FJoinedOnField := AJoinedOnField;
  FQuery := TDBRefFieldQuery.Create(Self);
end;

function TDBReferenceField.CreateControl: TDBControl;
begin
  Result := TDBCBoxControl.Create;
  Result.Subscriber := TSubscriber.Create(False);
  Result.Assign(Self);
end;

procedure TDBReferenceField.Assign(Field: TDBReferenceField);
begin
  inherited Assign(Field);
  FJoinOn := Field.FJoinOn;
  FJoinedOnField := Field.FJoinedOnField;
  FRefFieldName := Field.FRefFieldName;
  FRefTable := Field.FRefTable;
end;

procedure TDBFilter.Assign(Field: TDBField);
begin
  inherited Assign(Field);
  FOperator := '';
  FParam := '';
end;

constructor TDBFilter.Create;
begin
  { Default empty constructor }
end;

constructor TDBFilter.Create(AField: TDBField; COperator, AParam: string);
begin
  Assign(AField);
  FOperator := COperator;
  FParam := AParam;
end;

procedure TDBOrder.Assign(Field: TDBField);
begin
  inherited Assign(Field);
  FOrder := '';
end;

procedure TDBControl.OnNotificationRecieve(Sender: TObject);
begin
  { Do nothing }
end;

procedure TDBControl.CreateGUI(AParent: TWinControl; ATop, ALeft: integer);
begin
  FLabel := TLabel.Create(AParent);
  FLabel.Parent := AParent;
  FLabel.Top := ATop;
  FLabel.Left := ALeft;
  FLabel.Caption := Name;
end;

function TDBTable.CmpItemName(AField: TDBField; AName: string): boolean;
begin
  AName := UpCase(AName);
  if (UpCase(AField.Name) = AName) or (UpCase(AField.NativeName) = AName) then
    Exit(True);
  Exit(False);
end;

procedure TDBTable.SetFields(AFields: array of TDBField);
var
  i: integer;
begin
  inherited SetItems(AFields);
  for i := 0 to Count - 1 do begin
    if Fields[i].ParentTable = nil then
      Fields[i].ParentTable := Self;
    Fields[i].Index := i;
  end;
end;

constructor TDBTable.Create(AName, ANativeName: string; AIndex: integer = -1);
begin
  inherited Create(AName, ANativeName, AIndex);
  FQuery := TDBTableQuery.Create(Self);
end;

procedure TDBTable.Assign(Table: TDBTable);
begin
  inherited Assign(Table);
  FQuery := Table.Query;
end;

function TDBMetaData.CmpItemName(ATable: TDBTable; AName: string): boolean;
begin
  AName := UpCase(AName);
  if (UpCase(ATable.Name) = AName) or (UpCase(ATable.NativeName) = AName) then
    Exit(True);
  Exit(False);
end;

procedure TDBMetaData.SetTables(ATables: array of TDBTable);
var
  i: integer;
begin
  inherited SetItems(ATables);
  for i := 0 to Count - 1 do
    Tables[i].Index := i;
end;

initialization

  DBData := TDBMetaData.Create('MetaData', 'MetaData');

  with DBData do begin

    with TDBTable do begin
      SetTables([
         Create( 'Расписание'     ,'Time_Table'    )
        ,Create( 'Группы'         ,'Groups'        )
        ,Create( 'Предметы'       ,'Lessons'       )
        ,Create( 'Преподаватели'  ,'Teachers'      )
        ,Create( 'Аудитории'      ,'Class_Rooms'   )
        ,Create( 'Времена занятий','Lessons_Times' )
        ,Create( 'Дни недели'     ,'Week_Days'     )
        ,Create( 'Виды предметов' ,'Lessons_Types' )
      ]);
    end;

    with TDBField do begin
      TablesByName
      ['Groups'].SetFields([
         Create( 'ID'     ,'Id'   ,40  ,ftInteger )
        ,Create( 'Группа' ,'Name' ,100 ,ftString  )
      ]);
      TablesByName
      ['Lessons'].SetFields([
         Create( 'ID'      ,'Id'   ,40  ,ftInteger )
        ,Create( 'Предмет' ,'Name' ,300 ,ftString  )
      ]);
      TablesByName
      ['Teachers'].SetFields([
         Create( 'ID'       ,'Id'          ,40  ,ftInteger )
        ,Create( 'Фамилия'  ,'Last_Name'   ,100 ,ftString  )
        ,Create( 'Имя'      ,'First_Name'  ,100 ,ftString  )
        ,Create( 'Отчество' ,'Middle_Name' ,100 ,ftString  )
      ]);
      TablesByName
      [ 'Class_Rooms'].SetFields([
         Create( 'ID'        ,'Id'   ,40  ,ftInteger )
        ,Create( 'Аудитория' ,'Name' ,100 ,ftString  )
      ]);
      TablesByName
      ['Lessons_Times'].SetFields([
         Create( 'ID'     ,'Id'     ,40  ,ftInteger )
        ,Create( 'Начало' ,'Begin_' ,100 ,ftString  )
        ,Create( 'Конец'  ,'End_'   ,100 ,ftString  )
      ]);
      TablesByName
      ['Week_Days'].SetFields([
         Create( 'ID'          ,'Id'   ,40  ,ftInteger )
        ,Create( 'День недели' ,'Name' ,100 ,ftString  )
      ]);
      TablesByName
      ['Lessons_Types'].SetFields([
         Create( 'ID'  ,'Id'   ,40  ,ftInteger )
        ,Create( 'Тип' ,'Name' ,130 ,ftString  )
      ]);
    end;

    With TDBReferenceField do begin
      TablesByName['Time_Table'].SetFields([
        TDBField.Create('ID','Id', 40, ftInteger),
        Create('Time_Table' ,'Lesson_Id'      ,TablesByName['Lessons'],       'Id', 'Name'),
        Create('Time_Table' ,'Lesson_Type_Id' ,TablesByName['Lessons_Types'], 'Id', 'Name'),
        Create('Time_Table' ,'Teacher_Id'     ,TablesByName['Teachers'],      'Id', 'Last_Name'),
        Create('Time_Table' ,'Teacher_Id'     ,TablesByName['Teachers'],      'Id', 'First_Name'),
        Create('Time_Table' ,'Teacher_Id'     ,TablesByName['Teachers'],      'Id', 'Middle_Name'),
        Create('Time_Table' ,'Group_Id'       ,TablesByName['Groups'],        'Id', 'Name'),
        Create('Time_Table' ,'Class_Room_Id'  ,TablesByName['Class_Rooms'],   'Id', 'Name'),
        Create('Time_Table' ,'Week_Day_Id'    ,TablesByName['Week_Days'],     'Id', 'Name'),
        Create('Time_Table' ,'Lesson_Time_Id' ,TablesByName['Lessons_Times'], 'Id', 'Begin_'),
        Create('Time_Table' ,'Lesson_Time_Id' ,TablesByName['Lessons_Times'], 'Id', 'End_')
      ]);
    end;

  end;

end.
