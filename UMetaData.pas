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

  generic _TGData<T: TObject> = class(TPrimaryObject)
  private type
    _TGDataItems = specialize TVector<T>;
  private
    FItems: _TGDataItems;
    function Cast(AItem: TObject): T;
    function CmpItemName(AItem: T; AName: string): boolean; virtual;
    function GetItem(AIndex: integer): T;
    function GetItemByName(AName: string): T;
    function GetBack: T;
    procedure SetItem(AIndex: integer; const AItem: T);
    procedure SetBack(AItem: T);
    procedure SetFront(AItem: T);
    function GetFront: T;
    function GetLength: integer;
  public
    constructor Create(AName, ANativeName: string; AIndex: integer = -1);
    procedure Assign(AData: _TGData);
    procedure SetItems(AItems: array of T); virtual;
    procedure Add(AItem: T);
  published
    property Back: T read GetBack write SetBack;
    property Front: T read GetFront write SetFront;
    property Count: integer read GetLength;
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

function _TGData.Cast(AItem: TObject): T;
begin
  Exit(T(AItem));
end;

function _TGData.CmpItemName(AItem: T; AName: string): boolean;
begin
  Exit(True);
end;

function _TGData.GetItem(AIndex: integer): T;
begin
  Exit(Cast(FItems[AIndex]));
end;

function _TGData.GetItemByName(AName: string): T;
var
  i: integer;
begin
  AName := UpCase(AName);
  for i := 0 to FItems.Size - 1 do
    if CmpItemName(FItems[i], AName) then
      Exit(Cast(FItems[i]));
end;

procedure _TGData.SetItem(AIndex: integer; const AItem: T);
begin
  FItems[AIndex] := AItem;
end;

procedure _TGData.SetBack(AItem: T);
begin
  FItems.Back := AItem;
end;

function _TGData.GetBack: T;
begin
  Exit(Cast(FItems.Back));
end;

procedure _TGData.SetFront(AItem: T);
begin
  FItems.Front := AItem;
end;

function _TGData.GetFront: T;
begin
  Exit(Cast(FItems.Front));
end;

function _TGData.GetLength: integer;
begin
  Exit(FItems.Size);
end;

constructor _TGData.Create(AName, ANativeName: string; AIndex: integer = -1);
begin
  FItems := _TGDataItems.Create;
  inherited Create(AName, ANativeName, AIndex);
end;

procedure _TGData.Assign(AData: _TGData);
begin
  inherited Assign(AData);
  FItems := AData.FItems;
end;

procedure _TGData.SetItems(AItems: array of T);
begin
  FItems.APushBack(AItems);
end;

procedure _TGData.Add(AItem: T);
begin
  FItems.PushBack(AItem);
end;


end.
