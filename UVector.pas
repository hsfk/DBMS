unit UVector;

{$mode objfpc}{$H+}

interface

type

  generic TVector<T> = class
  private
    FItems: array of T;
    function GetLength: integer;
    function GetBack: T;
    procedure SetBack(Item: T);
    function GetFront: T;
    procedure SetFront(Item: T);
    function GetItem(Index: integer): T;
    procedure SetItem(Index: integer; const Item: T);
    procedure ChangeSize(Amount: integer);
  public
    constructor Create;
    constructor Create(FirstItem: T);
    function Containing(Item: T): boolean;
    function Find(Item: T): T;
    function FindInd(Item: T): integer;
    procedure Fill(Item: T);
    procedure Resize(ASize: integer);
    procedure PushBack(Item: T);
    procedure APushBack(Items: array of T);
    procedure PushFrontS(Item: T);
    procedure Delete(Item: T);
    procedure DeleteInd(Index: integer);
    procedure DeleteS(Item: T);
    procedure DeleteIndS(Index: integer);
    procedure Swap(IndexA, IndexB: integer);
    property Items[Index: integer]: T read GetItem write SetItem; default;
    property Back: T read GetBack write SetBack;
    property Front: T read GetFront write SetFront;
    property Size: integer read GetLength;
  end;

implementation

constructor TVector.Create;
begin

end;

constructor TVector.Create(FirstItem: T);
begin
  PushBack(FirstItem);
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
  i: integer;
begin
  for i := 0 to High(FItems) do
    if Item = FItems[i] then
      Exit(FItems[i]);
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

procedure TVector.APushBack(Items: array of T);
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
  Swap(Index, High(FItems));
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
    Swap(i, i + 1);
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

end.
