unit UDBObjects;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DBGrids, DB, UMetaData, UVector, UNotifications,
  StdCtrls, Controls;

type

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
    function QConstructor: IFieldConstructor;  //< incapsulation needed
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
    constructor Create(AName, ANativeName: string; AWidth: integer;
      ADataType: TFieldType; AParentTable: TDBTable = nil);
    procedure Load(Column: TColumn);
    function CreateControl: TDBControl; virtual;
    procedure Assign(Field: TDBField);
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
  end;

  TDBFieldData = class(TDBField)
  public
    constructor Create;
    procedure Assign(Field: TDBField); virtual;
  end;

  TDBFilter = class(TDBFieldData)
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

  TDBOrder = class(TDBFieldData)
  private
    FOrder: string;
  public
    property Order: string write FOrder;
    procedure Assign(Field: TDBField); override;
  end;

  TDBControl = class(TDBFieldData)
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

  TGenericTable = specialize TGenericData<TDBField>;

  TDBTable = class(TGenericTable)
  protected
    FQuery: ITableQuery;
    function CmpItemName(AField: TDBField; AName: string): boolean; override;
  public
    property Back: TDBField read GetBack write SetBack;
    property Front: TDBField read GetFront write SetFront;
    property Fields[AIndex: integer]: TDBField read GetItem write SetItem;
    property FieldsByName[AName: string]: TDBField read GetItemByName;
    procedure SetItems(AFields: array of TDBField); override;
    constructor Create(AName, ANativeName: string; AIndex: integer = -1);
    procedure Assign(Table: TDBTable);
  published
    property Count: integer read GetLength;
    property Query: ITableQuery read FQuery;
  end;

  TGenericMetaData = specialize TGenericData<TDBTable>;

  TDBMetaData = class(TGenericMetaData)
  protected
    function CmpItemName(ATable: TDBTable; AName: string): boolean; override;
  public
    property Back: TDBTable read GetBack write SetBack;
    property Front: TDBTable read GetFront write SetFront;
    property Tables[AIndex: integer]: TDBTable read GetItem write SetItem;
    property TablesByName[AName: string]: TDBTable read GetItemByName;
    procedure Add(Table: TDBTable); override;
  published
    property Count: integer read GetLength;
  end;

var
  DBData: TDBMetaData;

implementation

uses UCardControls, UQuery;

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

{ Create Self-Assigned Control }
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
  JoinedField: TDBField;
  JoinedOnField: TDBField;
begin
  FRefTable := DBData.TablesByName[RefTable];
  FRefFieldName := RefFieldName;
  JoinedField := JoinedTable.FieldsByName[FieldName];
  JoinedOnField := JoinedTable.FieldsByName[JoinedOnFieldName];
  FJoinOn := JoinedOnField.NativeName;
  inherited Create(JoinedField.Name, JoinedField.NativeName,
    JoinedField.Width, JoinedField.DataType);
  FParentTable := JoinedField.ParentTable;
  FJoinedOnField := JoinedOnField;
  FQuery := TDBRefFieldQuery.Create(Self);
end;

{ Create Self-Assigned Control }
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

constructor TDBFieldData.Create;
begin
  { Default empty constructor }
end;

procedure TDBFieldData.Assign(Field: TDBField);
begin
  NativeName := Field.NativeName;
  Name := Field.Name;
  ParentTable := Field.ParentTable;
  DataType := Field.DataType;
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
  { do nothing }
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

procedure TDBTable.SetItems(AFields: array of TDBField);
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
end;

function TDBMetaData.CmpItemName(ATable: TDBTable; AName: string): boolean;
begin
  AName := UpCase(AName);
  if (UpCase(ATable.Name) = AName) or (UpCase(ATable.NativeName) = AName) then
    Exit(True);
  Exit(False);
end;

procedure TDBMetaData.Add(Table: TDBTable);
begin
  inherited Add(Table);
  Back.Index := Count - 1;
end;

initialization

  DBData := TDBMetaData.Create('MetaData', 'MetaData');
  with DBData do begin
    Add(TDBTable.Create('Расписание', 'Time_Table'));
    Add(TDBTable.Create('Группы', 'Groups'));
    Add(TDBTable.Create('Предметы', 'Lessons'));
    Add(TDBTable.Create('Преподаватели', 'Teachers'));
    Add(TDBTable.Create('Аудитории', 'Class_Rooms'));
    Add(TDBTable.Create('Времена занятий', 'Lessons_Times'));
    Add(TDBTable.Create('Дни недели', 'Week_Days'));
    Add(TDBTable.Create('Виды предметов', 'Lessons_Types'));

    TablesByName['Groups'].SetItems([
      TDBField.Create('ID',     'Id',   40,  ftInteger),
      TDBField.Create('Группа', 'Name', 100, ftString)
    ]);
    TablesByName['Lessons'].SetItems([
      TDBField.Create('ID',      'Id',   40,  ftInteger),
      TDBField.Create('Предмет', 'Name', 300, ftString)
    ]);
    TablesByName['Teachers'].SetItems([
      TDBField.Create('ID',       'Id',          40,  ftInteger),
      TDBField.Create('Фамилия',  'Last_Name',   100, ftString),
      TDBField.Create('Имя',      'First_Name',  100, ftString),
      TDBField.Create('Отчество', 'Middle_Name', 100, ftString)
    ]);
    TablesByName['Class_Rooms'].SetItems([
      TDBField.Create('ID',        'Id',   40,  ftInteger),
      TDBField.Create('Аудитория', 'Name', 100, ftString)
    ]);
    TablesByName['Lessons_Times'].SetItems([
      TDBField.Create('ID',     'Id',     40,  ftInteger),
      TDBField.Create('Начало', 'Begin_', 100, ftString),
      TDBField.Create('Конец',  'End_',   100, ftString)
    ]);
    TablesByName['Week_Days'].SetItems([
      TDBField.Create('ID',          'Id',   40,  ftInteger),
      TDBField.Create('День недели', 'Name', 100, ftString)
    ]);
    TablesByName['Lessons_Types'].SetItems([
      TDBField.Create('ID',  'Id',   40,  ftInteger),
      TDBField.Create('Тип', 'Name', 130, ftString)
    ]);
    With TDBReferenceField do begin
      TablesByName['Time_Table'].SetItems([
        TDBField.Create('ID', 'Id', 40, ftInteger),
        Create('Time_Table', 'Lesson_Id',      TablesByName['Lessons'],       'Id', 'Name'),
        Create('Time_Table', 'Lesson_Type_Id', TablesByName['Lessons_Types'], 'Id', 'Name'),
        Create('Time_Table', 'Teacher_Id',     TablesByName['Teachers'],      'Id', 'Last_Name'),
        Create('Time_Table', 'Teacher_Id',     TablesByName['Teachers'],      'Id', 'First_Name'),
        Create('Time_Table', 'Teacher_Id',     TablesByName['Teachers'],      'Id', 'Middle_Name'),
        Create('Time_Table', 'Group_Id',       TablesByName['Groups'],        'Id', 'Name'),
        Create('Time_Table', 'Class_Room_Id',  TablesByName['Class_Rooms'],   'Id', 'Name'),
        Create('Time_Table', 'Week_Day_Id',    TablesByName['Week_Days'],     'Id', 'Name'),
        Create('Time_Table', 'Lesson_Time_Id', TablesByName['Lessons_Times'], 'Id', 'Begin_'),
        Create('Time_Table', 'Lesson_Time_Id', TablesByName['Lessons_Times'], 'Id', 'End_')
      ]);
    end;
  end;

end.
