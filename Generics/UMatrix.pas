unit UMatrix;

{$mode objfpc}{$H+}

interface

uses UVector;

type

  generic TMatrix<T> = class
  private type
    TCol = specialize TVector<T>;
    TItems = specialize TVector<TCol>;
  private
    FMatrix: TItems;
    function GetItem(i, j: integer): T;
    function GetWidth: integer;
    function GetHeight: integer;
    procedure SetItem(i, j:integer; const Item: T);
  public
    procedure SwapCols(IndexA, IndexB: integer);
    procedure SwapRows(IndexA, IndexB: integer);
    procedure Swap(XA, YA, XB, YB: integer);
  public
    constructor Create;
    procedure Resize(AWidth, AHeight: integer);
    procedure AddColumns(Amount: integer);
    procedure Fill(Item: T);
    procedure DeleteRow(Index: integer);
    procedure DeleteCol(Index: integer);
    property Items[i, j: integer]: T read GetItem write SetItem; default;
  published
    property Width: integer read GetWidth;
    property Height: integer read GetHeight;
  end;

  TStringM = specialize TMatrix<string>;
  TIntegerM = specialize TMatrix<integer>;
  TDoubleM = specialize TMatrix<double>;

implementation

constructor TMatrix.Create;
begin
  FMatrix := TItems.Create;
end;

function TMatrix.GetItem(i, j: integer): T;
begin
  Exit(FMatrix[i][j]);
end;

function TMatrix.GetWidth: integer;
begin
  Exit(FMatrix.Size);
end;

function TMatrix.GetHeight: integer;
begin
  if FMatrix.Size > 0 then
    Exit(FMatrix[0].Size)
  else
    Exit(0);
end;

procedure TMatrix.SetItem(i, j:integer; const Item: T);
begin
  FMatrix[i][j] := Item;
end;

procedure TMatrix.Resize(AWidth, AHeight: integer);
var
  i: integer;
begin
  FMatrix.Resize(AWidth);
  for i := 0 to AWidth - 1 do begin
    FMatrix[i].Free;
    FMatrix[i] := TCol.Create;
    FMatrix[i].Resize(AHeight);
  end;
end;

procedure TMatrix.AddColumns(Amount: integer);
var
  i: integer;
begin
  i := Width;
  FMatrix.Resize(Width + Amount);
  for i := i to Width - 1 do begin
    FMatrix[i] := TCol.Create;
    FMatrix[i].Resize(Height);
  end;
end;

procedure TMatrix.Fill(Item: T);
var
  i: integer;
begin
  for i := 0 to Width - 1 do
    FMatrix[i].Fill(Item);
end;

procedure TMatrix.SwapCols(IndexA, IndexB: integer);
var
  Tmp: TCol;
begin
  Tmp := FMatrix[IndexA];
  FMatrix[IndexA] := FMatrix[IndexB];
  FMatrix[IndexB] := Tmp;
end;

procedure TMatrix.SwapRows(IndexA, IndexB: integer);
var
  i: integer;
begin
  for i := 0 to FMatrix.Size - 1 do
    Swap(i, IndexA, i, IndexB);
end;

procedure TMatrix.Swap(XA, YA, XB, YB: integer);
var
  Tmp: T;
begin
  Tmp := FMatrix[XA][YA];
  FMatrix[XA][YA] := FMatrix[XB][YB];
  FMatrix[XB][YB] := Tmp;
end;

procedure TMatrix.DeleteRow(Index: integer);
var
  i: integer;
begin
  SwapRows(Index, Height - 1);
  for i := 0 to FMatrix.Size - 1 do
    FMatrix[i].Resize(FMatrix[i].Size - 1);
end;

procedure TMatrix.DeleteCol(Index: integer);
begin
  SwapCols(Index, Width - 1);
  FMatrix.Resize(FMatrix.Size - 1);
end;

end.
