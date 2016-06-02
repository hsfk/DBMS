unit UVector;

{$mode objfpc}{$H+}

interface

uses UArray;

type

  generic TCustomVector<T> = class(specialize TArray<T>)
  public type
    TCmp = function(A, B: T): boolean of object;

  private
    FAfterLast: T;
    function Equal(A, B: T): boolean;              virtual;
  public
    procedure Delete    (Item: T);                 virtual;
    procedure DeleteS   (Item: T);                 virtual;
    function Containing (Item: T): boolean;        virtual;
    function Find       (Item: T): T;              virtual;
    function FindInd    (Item: T): integer;        virtual;

    property AfterLast: T read FAfterLast;
  end;

  generic TVector<T> = class(specialize TCustomVector<T>)
  private
    function Equal(A, B: T): boolean; override;
  end;

  generic TObjVector<T: TObject> = class(specialize TVector<T>)
  public
    destructor Destroy; override;
    procedure  FreeItems;
  end;

  TStringV  = specialize TVector<string>;
  TIntegerV = specialize TVector<integer>;

implementation

function TCustomVector.Equal(A, B: T): boolean;
begin
  Exit(False);
end;

procedure TCustomVector.Delete(Item: T);
var
  Index: integer;
begin
  Index := FindInd(Item);
  if Index <> -1 then
    DeleteInd(Index);
end;

procedure TCustomVector.DeleteS(Item: T);
var
  Index: integer;
begin
  Index := FindInd(Item);
  if Index <> -1 then
    DeleteIndS(Index);
end;

function TCustomVector.Containing(Item: T): boolean;
begin
  if FindInd(Item) <> -1 then
    Exit(True);
  Exit(False);
end;

function TCustomVector.Find(Item: T): T;
var
  Index: integer;
begin
  Index := FindInd(Item);
  if Index = -1 then
    Exit(FAfterLast);
  Exit(FItems[Index]);
end;

function TCustomVector.FindInd(Item: T): integer;
var
  i: integer;
begin
  for i := 0 to High(FItems) do
    if Equal(Item, FItems[i]) then
      Exit(i);
   Exit(-1);
end;

function TVector.Equal(A, B: T): boolean;
begin
  Exit(A = B);
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
