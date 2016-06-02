unit UPair;

{$mode objfpc}{$H+}

interface

type

  generic TPairContainer<F, S> = class
  public type
    TSelfType = specialize TPairContainer<F, S>;

  private
    FFirst:  F;
    FSecond: S;
    procedure SetFirst (Item: F);              virtual;
    procedure SetSecond(Item: S);              virtual;
    function GetFirst:  F;                     virtual;
    function GetSecond: S;                     virtual;
  public
    constructor Create;                        virtual;
    constructor Create(AFirst: F; ASecond: S); virtual;
    procedure Assign(Item: TSelfType);         virtual;
    function Copy: TSelfType;                  virtual;

    property First:  F read GetFirst  write SetFirst;
    property Second: S read GetSecond write SetSecond;
  end;

  generic TCustomPair<F, S> = class(specialize TPairContainer<F, S>)
  private
    class function FirstEqual (A, B: F): boolean;           virtual;
    class function SecondEqual(A, B: S): boolean;           virtual;
  public
    class function EqualByFirst (A, B: TSelfType): boolean; virtual;
    class function EqualBySecond(A, B: TSelfType): boolean; virtual;
    class function Equal        (A, B: TSelfType): boolean; virtual;
  end;

  generic TPair<F, S> = class(specialize TCustomPair<F, S>)
  private
    class function FirstEqual (A, B: F): boolean; override;
    class function SecondEqual(A, B: S): boolean; override;
  end;

implementation

constructor TPairContainer.Create;
begin

end;

constructor TPairContainer.Create(AFirst: F; ASecond: S);
begin
  First := AFirst;
  Second := ASecond;
  Create;
end;

procedure TPairContainer.SetFirst(Item: F);
begin
  FFirst := Item;
end;

procedure TPairContainer.SetSecond(Item: S);
begin
  FSecond := Item;
end;

function TPairContainer.GetFirst: F;
begin
  Exit(FFirst);
end;

function TPairContainer.GetSecond: S;
begin
  Exit(FSecond);
end;

procedure TPairContainer.Assign(Item: TSelfType);
begin
  First := Item.First;
  Second := Item.Second;
end;

function TPairContainer.Copy: TSelfType;
begin
  Exit(Result.Create(First, Second));
end;

class function TCustomPair.FirstEqual(A, B: F): boolean;
begin
  Exit(False);
end;

class function TCustomPair.SecondEqual(A, B: S): boolean;
begin
  Exit(False);
end;

class function TCustomPair.EqualByFirst(A, B: TSelfType): boolean;
begin
  Exit(FirstEqual(A.First, B.First));
end;

class function TCustomPair.EqualBySecond(A, B: TSelfType): boolean;
begin
  Exit(SecondEqual(A.Second, B.Second));
end;

class function TCustomPair.Equal(A, B: TSelfType): boolean;
begin
  Exit(EqualByFirst(A, B) and EqualBySecond(A, B));
end;

class function TPair.FirstEqual (A, B: F): boolean;
begin
  Exit(A.First = B.First);
end;

class function TPair.SecondEqual(A, B: S): boolean;
begin
  Exit(A.Second = B.Second);
end;

end.
