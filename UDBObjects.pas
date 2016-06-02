unit UDBObjects;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DBGrids, DB, UMetaDataItems, UVector, UNotifications,
  StdCtrls, Controls, UCustomControl, UPair;

type

  TDBField = class;
  TDBRefField = class;
  TDBFilter = class;
  TDBTable = class;
  TDBMetaData = class;
  TDBFilters = specialize TVector<TDBFilter>;

  TQueryContainer = record
    Query: string;
    Params: TParams;
  end;

  TFieldData = record
    Data: string;
    ID: integer;
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

  _TFieldDataV = specialize TCustomVector<TFieldData>;

  TFieldDataV = class(_TFieldDataV)
  private
    function Equal(A, B: TFieldData): boolean; override;
  end;

  TDBField = class(TField)
  protected
    FQuery: IFieldQuery;
    FParentTable: TDBTable;
  public
    constructor Create;
    constructor Create(AName, ANativeName: string; AWidth: integer;
      ADataType: TFieldType; AVisible: boolean = True; AParentTable: TDBTable = nil);
    function CreateControln(RecID: integer): TCustomControl; virtual;
    procedure Load(Column: TColumn);
    procedure Assign(Field: TDBField); virtual;
  published
    property ParentTable: TDBTable read FParentTable write FParentTable;
    property Query: IFieldQuery read FQuery;
  end;

  TDBRefField = class(TDBField)
  protected
    FRefFieldName: string;
    FRefTable: TDBTable;
  public
    constructor Create(Field: TDBField; RefTable: TDBTable; RefFieldName: string);
    function CreateControln(RecID: integer): TCustomControl; override;
    procedure Assign(Field: TDBRefField);
  published
    property RefFieldName: string read FRefFieldName;
    property RefTable: TDBTable read FRefTable;
  end;

  TDBFilter = class(TDBField)
  private
    FParam: string;
    FOperator: string;
    FConnection: string;
  public
    constructor Create;
    constructor Create(AField: TDBField; COperator, AParam: string);
    procedure Assign(Field: TDBField); override;
  published
    property Param: string read FParam write FParam;
    property ConditionalOperator: string read FOperator write FOperator;
    property Connection: string read FConnection write FConnection;
  end;

  TDBOrder = class(TDBField)
  private
    FOrder: string;
  public
    procedure Assign(Field: TDBField); override;
  published
    property Order: string write FOrder;
  end;

  _TTable = specialize TGData<TDBField>;

  TDBTable = class(_TTable)
  protected
    FQuery: ITableQuery;
    function GetIDField: TDBField;
    function CmpItemName(AField: TDBField; AName: string): boolean; override;
  public
    constructor Create;
    constructor Create(AName, ANativeName: string; AIndex: integer = -1);
    procedure SetFields(AFields: array of TDBField);
    procedure Assign(Table: TDBTable);
    property Fields[AIndex: integer]: TDBField read GetItem write SetItem;
    property FieldsByName[AName: string]: TDBField read GetItemByName;
  published
    property Query: ITableQuery read FQuery;
    property IDField: TDBField read GetIDField;
  end;

  _TMetaData = specialize TGData<TDBTable>;

  TDBMetaData = class(_TMetaData)
  protected
    function CmpItemName(ATable: TDBTable; AName: string): boolean; override;
  public
    procedure SetTables(ATables: array of TDBTable);
    property Tables[AIndex: integer]: TDBTable read GetItem write SetItem;
    property TablesByName[AName: string]: TDBTable read GetItemByName;
  end;

function ToFieldData(ID: integer; Data: string): TFieldData;

var
  DBData: TDBMetaData;

implementation

uses UQuery, UCardControls;

function ToFieldData(ID: integer; Data: string): TFieldData;
begin
  Result.ID := ID;
  Result.Data := Data;
end;

function TFieldDataV.Equal(A, B: TFieldData): boolean;
begin
  Exit(A.Data = B.Data);
end;

constructor TDBField.Create;
begin
  { An empty constructor for descendant classes }
end;

constructor TDBField.Create(AName, ANativeName: string; AWidth: integer;
  ADataType: TFieldType; AVisible: boolean = True; AParentTable: TDBTable = nil);
begin
  inherited Create(AName, ANativeName, AWidth, ADataType);
  FQuery := TDBFieldQuery.Create(Self);
  Visible := AVisible;
end;

procedure TDBField.Load(Column: TColumn);
begin
  Column.Title.Caption := Name;
  Column.Width := Width;
end;

function TDBField.CreateControln(RecID: integer): TCustomControl;
var
  Control: TEditControl;
begin
  Control := TEditControl.Create(RecID);
  Control.Subscriber := TSubscriber.Create;
  Control.Field := Self;
  Exit(Control);
end;

procedure TDBField.Assign(Field: TDBField);
begin
  inherited Assign(Field);
  FParentTable := Field.ParentTable;
  FQuery := Field.Query;
end;

constructor TDBRefField.Create(Field: TDBField; RefTable: TDBTable;
  RefFieldName: string);
begin
  inherited Assign(Field);
  FRefTable := RefTable;
  FRefFieldName := RefFieldName;
  FQuery := TDBRefFieldQuery.Create(Self);
end;

function TDBRefField.CreateControln(RecID: integer): TCustomControl;
var
  Control: TCBoxControl;
begin
  Control := TCBoxControl.Create(RecID);
  Control.Subscriber := TSubscriber.Create(False);
  Control.Field := Self;
  Exit(Control);
end;

procedure TDBRefField.Assign(Field: TDBRefField);
begin
  inherited Assign(Field);
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
  FConnection := 'AND';
  { Default empty constructor }
end;

constructor TDBFilter.Create(AField: TDBField; COperator, AParam: string);
begin
  Create;
  Assign(AField);
  FOperator := COperator;
  FParam := AParam;
end;

procedure TDBOrder.Assign(Field: TDBField);
begin
  inherited Assign(Field);
  FOrder := '';
end;

function TDBTable.GetIDField: TDBField;
begin
  Exit(Fields[0]);
end;

function TDBTable.CmpItemName(AField: TDBField; AName: string): boolean;
begin
  AName := UpCase(AName);
  if (UpCase(AField.Name) = AName) or (UpCase(AField.NativeName) = AName) then
    Exit(True);
  Exit(False);
end;

constructor TDBTable.Create;
begin
  inherited Create;
  FQuery := TDBTableQuery.Create(Self);
end;

procedure TDBTable.SetFields(AFields: array of TDBField);
var
  i: integer;
begin
  inherited SetItems(AFields);
  for i := 0 to Count - 1 do begin
    if Fields[i].ParentTable = nil then
      Fields[i].ParentTable := Self;
    if Fields[i].Index = -1 then
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
         Create('Расписание'     ,'TimeTable'   )
        ,Create('Группы'         ,'Groups'      )
        ,Create('Предметы'       ,'Lessons'     )
        ,Create('Преподаватели'  ,'Teachers'    )
        ,Create('Аудитории'      ,'ClassRooms'  )
        ,Create('Времена занятий','LessonTimes' )
        ,Create('Дни недели'     ,'WeekDays'    )
        ,Create('Виды предметов' ,'LessonTypes' )
      ]);
    end;

    with TDBField do begin
      TablesByName
      ['Groups'].SetFields([
         Create('ID'              ,'ID'           ,40  ,ftInteger       )
        ,Create('Группа'          ,'Name'         ,100 ,ftString        )
        ,Create('Размер группы'   ,'Size'         ,90  ,ftInteger ,False)
        ,Create('Начало обучения' ,'StartingDate' ,150 ,ftDate    ,False)
        ,Create('Конец обучения'  ,'EndingDate'   ,150 ,ftDate    ,False)
      ]);
      TablesByName
      ['Lessons'].SetFields([
         Create('ID'                      ,'ID'           ,40  ,ftInteger       )
        ,Create('Предмет'                 ,'Name'         ,300 ,ftString        )
        ,Create('Начало преподавания (П)' ,'StartingDate' ,150 ,ftDate    ,False)
        ,Create('Конец преподавания (П)'  ,'EndingDate'   ,150 ,ftDate    ,False)
      ]);
      TablesByName
      ['Teachers'].SetFields([
         Create('ID'                      ,'ID'           ,40  ,ftInteger       )
        ,Create('Фамилия'                 ,'LastName'     ,100 ,ftString        )
        ,Create('Имя'                     ,'FirstName'    ,100 ,ftString        )
        ,Create('Отчество'                ,'MiddleName'   ,100 ,ftString        )
        ,Create('Начало преподавания (У)' ,'StartingDate' ,150 ,ftDate    ,False)
        ,Create('Конец преподавания (У)'  ,'EndingDate'   ,150 ,ftDate    ,False)
      ]);
      TablesByName
      ['ClassRooms'].SetFields([
         Create('ID'                  ,'ID'   ,40  ,ftInteger )
        ,Create('Аудитория'           ,'Name' ,100 ,ftString  )
        ,Create('Размер аудитории'    ,'Size' ,110 ,ftInteger )
      ]);
      TablesByName
      ['LessonTimes'].SetFields([
         Create('ID'     ,'ID'           ,40  ,ftInteger )
        ,Create('Начало' ,'StartingTime' ,100 ,ftString  )
        ,Create('Конец'  ,'EndingTime'   ,100 ,ftString  )
      ]);
      TablesByName
      ['WeekDays'].SetFields([
         Create('ID'          ,'ID'   ,40  ,ftInteger )
        ,Create('День недели' ,'Name' ,100 ,ftString  )
      ]);
      TablesByName
      ['LessonTypes'].SetFields([
         Create('ID'  ,'ID'   ,40  ,ftInteger )
        ,Create('Тип' ,'Name' ,130 ,ftString  )
      ]);
    end;

    With TDBRefField do begin
      TablesByName['TimeTable'].SetFields([
         TDBField.Create('ID','ID', 40, ftInteger)
        ,Create( TablesByName['Lessons'     ].FieldsByName['Name'         ]
                ,TablesByName['TimeTable'   ],'LessonID'                  )
        ,Create( TablesByName['Lessons'     ].FieldsByName['StartingDate' ]
                ,TablesByName['TimeTable'   ],'LessonID'                  )
        ,Create( TablesByName['Lessons'     ].FieldsByName['EndingDate'   ]
                ,TablesByName['TimeTable'   ],'LessonID'                  )

        ,Create( TablesByName['LessonTypes' ].FieldsByName['Name'         ]
                ,TablesByName['TimeTable'   ],'LessonTypeID'              )

        ,Create( TablesByName['Teachers'    ].FieldsByName['LastName'     ]
                ,TablesByName['TimeTable'   ],'TeacherID'                 )
        ,Create( TablesByName['Teachers'    ].FieldsByName['FirstName'    ]
                ,TablesByName['TimeTable'   ],'TeacherID'                 )
        ,Create( TablesByName['Teachers'    ].FieldsByName['MiddleName'   ]
                ,TablesByName['TimeTable'   ],'TeacherID'                 )
        ,Create( TablesByName['Teachers'    ].FieldsByName['StartingDate' ]
                ,TablesByName['TimeTable'   ],'TeacherID'                 )
        ,Create( TablesByName['Teachers'    ].FieldsByName['EndingDate'   ]
                ,TablesByName['TimeTable'   ],'TeacherID'                 )

        ,Create( TablesByName['Groups'      ].FieldsByName['Name'         ]
                ,TablesByName['TimeTable'   ],'GroupID'                   )
        ,Create( TablesByName['Groups'      ].FieldsByName['Size'         ]
                ,TablesByName['TimeTable'   ],'GroupID'                   )
        ,Create( TablesByName['Groups'      ].FieldsByName['StartingDate' ]
                ,TablesByName['TimeTable'   ],'GroupID'                   )
        ,Create( TablesByName['Groups'      ].FieldsByName['EndingDate'   ]
                ,TablesByName['TimeTable'   ],'GroupID'                   )

        ,Create( TablesByName['ClassRooms'  ].FieldsByName['Name'         ]
                ,TablesByName['TimeTable'   ],'ClassRoomID'               )
        ,Create( TablesByName['ClassRooms'  ].FieldsByName['Size'         ]
                ,TablesByName['TimeTable'   ],'ClassRoomID'               )

        ,Create( TablesByName['WeekDays'    ].FieldsByName['Name'         ]
                ,TablesByName['TimeTable'   ],'WeekDayID'                 )

        ,Create( TablesByName['LessonTimes' ].FieldsByName['StartingTime' ]
                ,TablesByName['TimeTable'   ],'LessonTimeID'              )
        ,Create( TablesByName['LessonTimes' ].FieldsByName['EndingTime'   ]
                ,TablesByName['TimeTable'   ],'LessonTimeID'              )
      ]);
    end;
  end;

end.
