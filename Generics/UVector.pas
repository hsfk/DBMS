unit UVector;

{$mode objfpc}{$H+}

interface

type

  generic TVector<T> = class
  private
    FItems: array of T;
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
    destructor Destroy;                            override;
    procedure Resize    (ASize: integer);          virtual;
    procedure DeleteInd (Index: integer);          virtual;
    procedure DeleteIndS(Index: integer);          virtual;
    procedure Swap      (IndexA, IndexB: integer); virtual;
    procedure Fill      (Item: T);                 virtual;
    procedure Delete    (Item: T);                 virtual;
    procedure DeleteS   (Item: T);                 virtual;
    procedure PushBack  (Item: T);                 virtual;
    procedure PushBackA (Items: array of T);       virtual;
    procedure PushFrontS(Item: T);                 virtual;
    function Containing (Item: T): boolean;        virtual;
    function Find       (Item: T): T;              virtual;
    function FindInd    (Item: T): integer;        virtual;

    property Back:  T read GetBack  write SetBack;
    property Front: T read GetFront write SetFront;
    property Size:  integer read GetLength;
    property Items[Index: integer]: T read GetItem write SetItem; default;
  end;

  TStringV  = specialize TVector<string>;
  TIntegerV = specialize TVector<integer>;

  generic TObjVector<T: TObject> = class(specialize TVector<T>)
  public
    destructor Destroy; override;
    procedure  FreeItems;
  end;

implementation

constructor TVector.Create;
begin

end;

constructor TVector.Create(FirstItem: T);
begin
  PushBack(FirstItem);
end;

destructor TVector.Destroy;
begin
  FItems := nil;
  inherited Destroy;
end;

procedure TVector.Resize(ASize: integer);
begin
  SetLength(FItems, ASize);
end;

function TVector.Containing(Item: T): boolean;
begin
  if FindInd(Item) <> -1 then
    Exit(True);
  Exit(False);
end;

function TVector.Find(Item: T): T;
var
  Index: integer;
begin
  Index := FindInd(Item);
  if Index = -1 then
    Exit(Back)
  else
    Exit(FItems[Index]);
end;

function TVector.FindInd(Item: T): integer;
var
  i: integer;
begin
  for i := 0 to High(FItems) do
    if Item = FItems[i] then
      Exit(i);
  Exit(-1);
end;

procedure TVector.Fill(Item: T);
var
  i: integer;
begin
  for i := 0 to Size - 1 do
    FItems[i] := Item;
end;

procedure TVector.PushBack(Item: T);
begin
  ChangeSize(1);
  SetBack(Item);
end;

procedure TVector.PushBackA(Items: array of T);
var
  i: integer;
begin
  for i := 0 to High(Items) do
    PushBack(Items[i]);
end;

procedure TVector.PushFrontS(Item: T);
var
  i: integer;
begin
  ChangeSize(1);
  for i := High(FItems) downto 1 do
    Swap(i, i - 1);
  SetFront(Item);
end;

procedure TVector.Delete(Item: T);
var
  Index: integer;
begin
  Index := FindInd(Item);
  if Index <> -1 then
    DeleteInd(Index);
end;

procedure TVector.DeleteInd(Index: integer);
begin
  FItems[Index] := Back;
  ChangeSize(-1);
end;

procedure TVector.DeleteS(Item: T);
var
  Index: integer;
begin
  Index := FindInd(Item);
  if Index <> -1 then
    DeleteIndS(Index);
end;

procedure TVector.DeleteIndS(Index: integer);
var
  i: integer;
begin
  for i := Index to High(FItems) - 1 do
    FItems[i] := FItems[i + 1];
  ChangeSize(-1);
end;

procedure TVector.Swap(IndexA, IndexB: integer);
var
  Temp: T;
begin
  Temp := FItems[IndexA];
  FItems[IndexA] := FItems[IndexB];
  FItems[IndexB] := Temp;
end;

function TVector.GetLength: integer;
begin
  Exit(Length(FItems));
end;

function TVector.GetBack: T;
begin
  Exit(FItems[High(FItems)]);
end;

procedure TVector.SetBack(Item: T);
begin
  FItems[High(FItems)] := Item;
end;

function TVector.GetFront: T;
begin
  Exit(FItems[0]);
end;

procedure TVector.SetFront(Item: T);
begin
  FItems[0] := Item;
end;

function TVector.GetItem(Index: integer): T;
begin
  Exit(FItems[Index]);
end;

procedure TVector.SetItem(Index: integer; const Item: T);
begin
  FItems[Index] := Item;
end;

procedure TVector.ChangeSize(Amount: integer);
begin
  SetLength(FItems, Size + Amount);
end;

destructor TObjVector.Destroy;
begin
  FreeItems;
  inherited Destroy;
end;

procedure TObjVector.FreeItems;
var
  i: integer;
begin
  for i := 0 to Size - 1 do
    if Items[i] <> nil then begin
      Items[i].Free;
      Items[i] := nil
    end;
end;

end.
