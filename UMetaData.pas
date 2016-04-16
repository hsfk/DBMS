unit UMetaData;

{$mode objfpc}{$H+}

interface

uses DB, UVector;

type

  TPrimaryObject = class(TInterfacedObject)
  private
    FIndex: integer;
    FName: string;
    FNativeName: string;
  public
    constructor Create(AName, ANativeName: string; AIndex: integer = -1);
    procedure Assign(PObject: TPrimaryObject);
  published
    property Index: integer read FIndex write FIndex;
    property Name: string read FName write FName;
    property NativeName: string read FNativeName write FNativeName;
  end;

  TField = class(TPrimaryObject)
  private
    FDataType: TFieldType;
    FWidth: integer;
  public
    constructor Create(AName, ANativeName: string; AWidth: integer;
      DataType: TFieldType);
    procedure Assign(Field: TField);
  published
    property DataType: TFieldType read FDataType write FDataType;
    property Width: integer read FWidth write FWidth;
  end;

  {TODO Refactor \|/ }
  generic TGenericData<T> = class(TPrimaryObject)
  private
    type
    TItems = specialize TVector<T>;
  var
    FItems: TItems;
  protected
    function CmpItemName(AItem: T; AName: string): boolean; virtual;
    function GetItem(AIndex: integer): T;
    function GetItemByName(AName: string): T;
    procedure SetItem(AIndex: integer; const Item: T);
    procedure SetBack(AItem: T);
    function GetBack: T;
    procedure SetFront(AItem: T);
    function GetFront: T;
    function GetLength: integer;
  public
    procedure SetItems(AItems: array of T); virtual;
    procedure Add(Item: T); virtual;
    constructor Create(AName, ANativeName: string; AIndex: integer = -1); overload;
    procedure Assign(Data: TGenericData);
  end;

implementation

constructor TPrimaryObject.Create(AName, ANativeName: string; AIndex: integer = -1);
begin
  FName := AName;
  FNativeName := ANativeName;
  FIndex := AIndex;
end;

procedure TPrimaryObject.Assign(PObject: TPrimaryObject);
begin
  FIndex := PObject.FIndex;
  FName := PObject.FName;
  FNativeName := PObject.FNativeName;
end;

constructor TField.Create(AName, ANativeName: string; AWidth: integer;
  DataType: TFieldType);
begin
  inherited Create(AName, ANativeName);
  FWidth := AWidth;
  FDataType := DataType;
end;

procedure TField.Assign(Field: TField);
begin
  inherited Assign(Field);
  FDataType := Field.DataType;
  FWidth := Field.Width;
end;

function TGenericData.CmpItemName(AItem: T; AName: string): boolean;
begin
  Exit(True);
end;

function TGenericData.GetItem(AIndex: integer): T;
begin
  Exit(FItems.Items[AIndex]);
end;

function TGenericData.GetItemByName(AName: string): T;
var
  i: integer;
begin
  AName := UpCase(AName);
  for i := 0 to FItems.Size - 1 do
    if CmpItemName(FItems.Items[i], AName) then
      Exit(FItems.Items[i]);
end;

procedure TGenericData.SetItem(AIndex: integer; const Item: T);
begin
  FItems.Items[AIndex] := Item;
end;

procedure TGenericData.SetBack(AItem: T);
begin
  FItems.Back := AItem;
end;

function TGenericData.GetBack: T;
begin
  Exit(FItems.Back);
end;

procedure TGenericData.SetFront(AItem: T);
begin
  FItems.Front := AItem;
end;

function TGenericData.GetFront: T;
begin
  Exit(FItems.Front);
end;

function TGenericData.GetLength: integer;
begin
  Exit(FItems.Size);
end;

procedure TGenericData.SetItems(AItems: array of T);
begin
  FItems.Free;
  FItems := TItems.Create;
  FItems.APushBack(AItems);
end;

procedure TGenericData.Add(Item: T);
begin
  FItems.PushBack(Item);
end;

constructor TGenericData.Create(AName, ANativeName: string; AIndex: integer = -1);
begin
  FItems := TItems.Create;
  inherited Create(AName, ANativeName, AIndex);
end;

procedure TGenericData.Assign(Data: TGenericData);
begin
  inherited Assign(Data);
  FItems := Data.FItems;
end;

end.
