unit UArray;

{$mode objfpc}{$H+}

interface

type

  generic TArray<T> = class
  protected type
    TSelf = specialize TArray<T>;
    TTArray = array of T;
  private
    FItems: TTArray;
    procedure SetBack   (Item:   T);                      virtual;
    procedure SetFront  (Item:   T);                      virtual;
    procedure SetItem   (Index:  integer; const Item: T); virtual;
    procedure ChangeSize(Amount: integer);                virtual;
    function GetItem    (Index:  integer): T;             virtual;
    function GetLength: integer;                          virtual;
    function GetFront:  T;                                virtual;
    function GetBack:   T;                                virtual;
  public
    constructor Create;                            virtual;
    constructor Create  (FirstItem: T);            virtual;
    constructor Create  (Items: TTArray);          virtual;
    constructor Create  (Items: array of T);       virtual;
    destructor Destroy;                            override;
    procedure Resize    (ASize: integer);          virtual;
    procedure DeleteInd (Index: integer);          virtual;
    procedure DeleteIndS(Index: integer);          virtual;
    procedure Swap      (IndexA, IndexB: integer); virtual;
    procedure Fill      (Item: T);                 virtual;
    procedure PushBack  (Item: T);                 virtual;
    procedure PushBackA (Items: TTArray);          virtual;
    procedure PushBackA (Items: array of T);       virtual;
    procedure PushBackA (Items: TSelf);            virtual;
    procedure PushFrontS(Item: T);                 virtual;
    function Empty:      boolean;                  virtual;

    property FPCArray: TTArray read FItems;
    property Back:  T read GetBack  write SetBack;
    property Front: T read GetFront write SetFront;
    property Size:  integer read GetLength;
    property Items[Index: integer]: T read GetItem write SetItem; default;
  end;

implementation

constructor TArray.Create;
begin

end;

constructor TArray.Create(FirstItem: T);
begin
  PushBack(FirstItem);
  Create;
end;

constructor TArray.Create(Items: TTArray);
begin
  PushBackA(Items);
  Create;
end;

constructor TArray.Create(Items: array of T);
begin
  PushBackA(Items);
  Create;
end;

destructor TArray.Destroy;
begin
  FItems := nil;
  inherited Destroy;
end;

procedure TArray.Resize(ASize: integer);
begin
  SetLength(FItems, ASize);
end;

function TArray.Empty: boolean;
begin
  Exit(Size = 0);
end;

procedure TArray.Fill(Item: T);
var
  i: integer;
begin
  for i := 0 to Size - 1 do
    FItems[i] := Item;
end;

procedure TArray.PushBack(Item: T);
begin
  ChangeSize(1);
  SetBack(Item);
end;

procedure TArray.PushBackA(Items: TTArray);
var
  i: integer;
begin
  for i := 0 to High(Items) do
    PushBack(Items[i]);
end;

procedure TArray.PushBackA(Items: array of T);
var
  i: integer;
begin
  for i := 0 to High(Items) do
    PushBack(Items[i]);
end;

procedure TArray.PushBackA(Items: TSelf);
begin
  PushBackA(Items.FPCArray);
end;

procedure TArray.PushFrontS(Item: T);
var
  i: integer;
begin
  ChangeSize(1);
  for i := High(FItems) downto 1 do
    Swap(i, i - 1);
  SetFront(Item);
end;

procedure TArray.DeleteInd(Index: integer);
begin
  FItems[Index] := Back;
  ChangeSize(-1);
end;

procedure TArray.DeleteIndS(Index: integer);
var
  i: integer;
begin
  for i := Index to High(FItems) - 1 do
    FItems[i] := FItems[i + 1];
  ChangeSize(-1);
end;

procedure TArray.Swap(IndexA, IndexB: integer);
var
  Temp: T;
begin
  Temp := FItems[IndexA];
  FItems[IndexA] := FItems[IndexB];
  FItems[IndexB] := Temp;
end;

function TArray.GetLength: integer;
begin
  Exit(Length(FItems));
end;

function TArray.GetBack: T;
begin
  Exit(FItems[High(FItems)]);
end;

procedure TArray.SetBack(Item: T);
begin
  FItems[High(FItems)] := Item;
end;

function TArray.GetFront: T;
begin
  Exit(FItems[0]);
end;

procedure TArray.SetFront(Item: T);
begin
  FItems[0] := Item;
end;

function TArray.GetItem(Index: integer): T;
begin
  Exit(FItems[Index]);
end;

procedure TArray.SetItem(Index: integer; const Item: T);
begin
  FItems[Index] := Item;
end;

procedure TArray.ChangeSize(Amount: integer);
begin
  SetLength(FItems, Size + Amount);
end;

end.

