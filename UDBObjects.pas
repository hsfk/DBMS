unit UDBObjects;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DBGrids, DB, UMetaDataItems, UVector, UNotifications,
  StdCtrls, Controls;

type

  TDBField = class;
  TDBRefField = class;
  TDBFilter = class;
  TDBCardControl = class;
  TDBTable = class;
  TDBMetaData = class;
  TDBFilters = specialize TVector<TDBFilter>;
  TDBControls = specialize TVector<TDBCardControl>;

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
    function CreateControl(RecID: integer): TDBCardControl; virtual;
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
    function CreateControl(RecID: integer): TDBCardControl; override;
    procedure Assign(Field: TDBRefField);
  published
    property RefFieldName: string read FRefFieldName;
    property RefTable: TDBTable read FRefTable;
  end;

  TDBFilter = class(TDBField)
  private
    FParam: string;
    FOperator: string;
  public
    constructor Create;
    constructor Create(AField: TDBField; COperator, AParam: string);
    procedure Assign(Field: TDBField); override;
  published
    property Param: string read FParam write FParam;
    property ConditionalOperator: string read FOperator write FOperator;
  end;

  TDBOrder = class(TDBField)
  private
    FOrder: string;
  public
    procedure Assign(Field: TDBField); override;
  published
    property Order: string write FOrder;
  end;

  TDBCardControl = class(TDBField)
  private
    FLabel: TLabel;
    FSubscriber: TSubscriber;
  protected
    FRecID: integer;
    function GetData: TParam; virtual; abstract;
    procedure SetCaption(AData: string); virtual; abstract;
    procedure OnNotificationRecieve(Sender: TObject); virtual;
  public
    constructor Create(RecID: integer);
    function UpdateTable: TQueryContainer; virtual; abstract;
    function Correct: boolean; virtual; abstract;
    procedure CreateGUI(AParent: TWinControl; ATop, ALeft: integer); virtual;
    procedure Deselect; virtual; abstract;
    procedure Clear; virtual; abstract;
    procedure LoadData(Data: string; ID: integer); virtual; abstract;
  published
    property Subscriber: TSubscriber read FSubscriber write FSubscriber;
    property Caption: string write SetCaption;
    property Data: TParam read GetData;
  end;

  TDBEditControl = class(TDBCardControl)
  private
    FEdit: TEdit;
  protected
    function GetData: TParam; override;
    procedure SetCaption(AData: string); override;
  public
    function UpdateTable: TQueryContainer; override;
    function Correct: boolean; override;
    procedure CreateGUI(AParent: TWinControl; ATop, ALeft: integer); override;
    procedure Clear; override;
    procedure Deselect; override;
    procedure LoadData(AData: string; ID: integer); override;
  end;

  TDBCBoxControl = class(TDBCardControl)
  private
    type
    TCBoxData = record
      Data: string;
      ID: integer;
    end;
  private
    FCBox: TComboBox;
    FData: array of TCBoxData;
    procedure OnChange(Sender: TObject);
  protected
    function GetData: TParam; override;
    procedure OnNotificationRecieve(Sender: TObject); override;
    procedure SetCaption(AData: string); override;
  public
    function UpdateTable: TQueryContainer; override;
    function Correct: boolean; override;
    procedure CreateGUI(AParent: TWinControl; ATop, ALeft: integer); override;
    procedure Clear; override;
    procedure Deselect; override;
    procedure LoadData(AData: string; ID: integer); override;
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

var
  DBData: TDBMetaData;

implementation

uses UQuery;

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

function TDBField.CreateControl(RecID: integer): TDBCardControl;
begin
  Result := TDBEditControl.Create(RecID);
  Result.Subscriber := TSubscriber.Create(False);
  Result.Assign(Self);
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

function TDBRefField.CreateControl(RecID: integer): TDBCardControl;
begin
  Result := TDBCBoxControl.Create(RecID);
  Result.Subscriber := TSubscriber.Create(False);
  Result.Assign(Self);
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

procedure TDBCardControl.OnNotificationRecieve(Sender: TObject);
begin
  { Do nothing }
end;

constructor TDBCardControl.Create(RecID: integer);
begin
  FRecID := RecID;
end;

function TDBEditControl.UpdateTable: TQueryContainer;
var
  NewData: TParam;
begin
  NewData := TParam.Create(nil, ptInput);
  NewData.DataType := DataType;
  NewData.Value := FEdit.Text;
  Exit(Query.Update(FRecID, NewData));
end;

procedure TDBEditControl.CreateGUI(AParent: TWinControl; ATop, ALeft: integer);
begin
  inherited CreateGUI(AParent, ATop, ALeft);
  FEdit := TEdit.Create(AParent);
  FEdit.Parent := AParent;
  FEdit.Top := ATop;
  FEdit.Left := ALeft + 100;
  FEdit.Width := 250;
  FEdit.Anchors := [akRight, akLeft];
  FEdit.Align := alCustom;
  if DataType = ftInteger then
    FEdit.NumbersOnly := True;
end;

procedure TDBEditControl.Clear;
begin
  Deselect;
end;

procedure TDBEditControl.Deselect;
begin
  FEdit.Text := '';
end;

function TDBEditControl.Correct: boolean;
begin
  if FEdit.Text = '' then
    Exit(False);
  Exit(True);
end;

procedure TDBEditControl.LoadData(AData: string; ID: integer);
begin
  FEdit.Text := AData;
end;

function TDBEditControl.GetData: TParam;
begin
  Result := TParam.Create(nil, ptInput);
  Result.DataType := DataType;
  Result.Name := NativeName;
  Result.Value := FEdit.Text;
end;

procedure TDBEditControl.SetCaption(AData: string);
begin
  FEdit.Text := AData;
end;

function TDBCBoxControl.GetData: TParam;
begin
  Result := TParam.Create(nil, ptInput);
  Result.DataType := DataType;
  Result.Name := NativeName;
  Result.Value := FData[FCBox.ItemIndex].ID;
end;

procedure TDBCBoxControl.OnChange(Sender: TObject);
begin
  Subscriber.CreateNotification(FCBox, Subscriber.NClass);
end;

function TDBCBoxControl.UpdateTable: TQueryContainer;
var
  NewData: TParam;
begin
  NewData := TParam.Create(nil, ptInput);
  NewData.DataType := ftInteger;
  NewData.Value := FData[FCBox.ItemIndex].ID;
  Exit(Query.Update(FRecID, NewData));
end;

procedure TDBCBoxControl.OnNotificationRecieve(Sender: TObject);
begin
  FCBox.ItemIndex := TCombobox(Sender).ItemIndex;
end;

procedure TDBCBoxControl.CreateGUI(AParent: TWinControl; ATop, ALeft: integer);
begin
  inherited CreateGUI(AParent, ATop, ALeft);
  FCBox := TComboBox.Create(AParent);
  FCBox.Parent := AParent;
  FCBox.ReadOnly := True;
  FCBox.Top := ATop;
  FCBox.Left := ALeft + 100;
  FCBox.Width := 250;
  FCBox.Anchors := [akRight, akLeft];
  FCBox.Align := alCustom;
  FCBox.OnChange := @OnChange;
  Subscriber.OnNotificationRecieve := @OnNotificationRecieve;
end;

function TDBCBoxControl.Correct: boolean;
begin
  if FCBox.ItemIndex = -1 then
    Exit(False);
  Exit(True);
end;

procedure TDBCBoxControl.Clear;
begin
  FCBox.Items.Clear;
end;

procedure TDBCBoxControl.Deselect;
begin
  FCBox.ItemIndex := -1;
end;

procedure TDBCBoxControl.LoadData(AData: string; ID: integer);
begin
  SetLength(FData, Length(FData) + 1);
  FData[High(FData)].Data := AData;
  FData[High(FData)].ID := ID;
  FCBox.Items.Add(AData);
end;

procedure TDBCBoxControl.SetCaption(AData: string);
var
  i: integer;
begin
  for i := 0 to High(FData) do
    if AData = FData[i].Data then begin
      FCBox.ItemIndex := i;
      Exit;
    end;
end;

procedure TDBCardControl.CreateGUI(AParent: TWinControl; ATop, ALeft: integer);
begin
  FLabel := TLabel.Create(AParent);
  FLabel.Parent := AParent;
  FLabel.Top := ATop;
  FLabel.Left := ALeft;
  FLabel.Caption := Name;
  FLabel.Align := alCustom;
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

    With TDBRefField do begin
      TablesByName['Time_Table'].SetFields([
         TDBField.Create('ID','Id', 40, ftInteger)
        ,Create( TablesByName[ 'Lessons'      ].FieldsByName[ 'Name'       ]
                ,TablesByName[ 'Time_Table'   ],'Lesson_Id'                )
        ,Create( TablesByName[ 'Lessons_Types'].FieldsByName[ 'Name'       ]
                ,TablesByName[ 'Time_Table'   ],'Lesson_Type_Id'           )
        ,Create( TablesByName[ 'Teachers'     ].FieldsByName[ 'Last_Name'  ]
                ,TablesByName[ 'Time_Table'   ],'Teacher_Id'               )
        ,Create( TablesByName[ 'Teachers'     ].FieldsByName[ 'First_Name' ]
                ,TablesByName[ 'Time_Table'   ],'Teacher_Id'               )
        ,Create( TablesByName[ 'Teachers'     ].FieldsByName[ 'Middle_Name']
                ,TablesByName[ 'Time_Table'   ],'Teacher_Id'               )
        ,Create( TablesByName[ 'Groups'       ].FieldsByName[ 'Name'       ]
                ,TablesByName[ 'Time_Table'   ],'Group_Id'                 )
        ,Create( TablesByName[ 'Class_Rooms'  ].FieldsByName[ 'Name'       ]
                ,TablesByName[ 'Time_Table'   ],'Class_Room_Id'            )
        ,Create( TablesByName[ 'Week_Days'    ].FieldsByName[ 'Name'       ]
                ,TablesByName[ 'Time_Table'   ],'Week_Day_Id'              )
        ,Create( TablesByName[ 'Lessons_Times'].FieldsByName[ 'Begin_'     ]
                ,TablesByName[ 'Time_Table'   ],'Lesson_Time_Id'           )
        ,Create( TablesByName[ 'Lessons_Times'].FieldsByName[ 'End_'       ]
                ,TablesByName[ 'Time_Table'   ],'Lesson_Time_Id'           )
      ]);
    end;
  end;

end.
