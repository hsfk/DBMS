unit UMatrix;

{$mode objfpc}{$H+}

interface

uses UVector;

type

  generic TMatrix<T> = class
  private type
    TRow = specialize TVector<T>;
    TItems = specialize TVector<TRow>;
  private
    FMatrix: TItems;
    function GetItem(i, j: integer): T;
    function GetWidth: integer;
    function GetHeight: integer;
    procedure SetItem(i, j:integer; const Item: T);
  public
    constructor Create;
    procedure Resize(AWidth, AHeight: integer);
    procedure AddColumns(Amount: integer);
    procedure Fill(Item: T);
    property Items[i, j: integer]: T read GetItem write SetItem; default;
  published
    property Width: integer read GetWidth;
    property Height: integer read GetHeight;
  end;

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
    FMatrix[i] := TRow.Create;
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
    FMatrix[i] := TRow.Create;
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

end.

