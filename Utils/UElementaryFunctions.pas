unit UElementaryFunctions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UVector;

type

  TIntCompareFunction = function(a, b: integer): boolean of object;
  TIntAggregateFunction = function(a, b: integer): integer of object;
  TIntCompareFunctionV = specialize TVector<TIntCompareFunction>;
  TIntAggregateFunctionV = specialize TVector<TIntAggregateFunction>;

  EnumIntCompareFunctions = (cfEq, cfGr, cfLe, cfGrEq, cfLeEq);
  EnumIntAggregateFunctions = (afSum, afMin, afMax);

  TIntCompareFunctionData = record
    Name: string;
    EFunc: EnumIntCompareFunctions;
  end;

  TIntAggregateFunctionData = record
    Name: string;
    EFunc: EnumIntAggregateFunctions;
  end;

  TIntCompareFunctions = class
  public
    class function GetFunc(F: EnumIntCompareFunctions): TIntCompareFunction;
    class function Eq(a, b: integer): boolean;
    class function Gr(a, b: integer): boolean;
    class function Le(a, b: integer): boolean;
    class function GrEq(a, b: integer): boolean;
    class function LeEq(a, b: integer): boolean;
  end;

  TIntAggregateFunctions = class
  public
    class function GetFunc(F: EnumIntAggregateFunctions): TIntAggregateFunction;
    class function Sum(a, b: integer): integer;
    class function Min(a, b: integer): integer;
    class function Max(a, b: integer): integer;
  end;

implementation

class function TIntCompareFunctions.GetFunc(F: EnumIntCompareFunctions):
TIntCompareFunction;
begin
  case F of
    cfEq: Exit(@Eq);
    cfGr: Exit(@Gr);
    cfLe: Exit(@Le);
    cfGrEq: Exit(@GrEq);
    cfLeEq: Exit(@LeEq);
  end;
  Exit(nil);
end;

class function TIntAggregateFunctions.GetFunc(F: EnumIntAggregateFunctions):
TIntAggregateFunction;
begin
  case F of
    afSum: Exit(@Sum);
    afMin: Exit(@Min);
    afMax: Exit(@Max);
  end;
  Exit(nil);
end;

class function TIntCompareFunctions.Eq(a, b: integer): boolean;
begin
  Exit(a = b);
end;

class function TIntCompareFunctions.Gr(a, b: integer): boolean;
begin
  Exit(a > b);
end;

class function TIntCompareFunctions.Le(a, b: integer): boolean;
begin
  Exit(a < b);
end;

class function TIntCompareFunctions.GrEq(a, b: integer): boolean;
begin
  Exit(not Le(a, b));
end;

class function TIntCompareFunctions.LeEq(a, b: integer): boolean;
begin
  Exit(not Gr(a, b));
end;

class function TIntAggregateFunctions.Sum(a, b: integer): integer;
begin
  Exit(a + b);
end;

class function TIntAggregateFunctions.Min(a, b: integer): integer;
begin
  if a < b then
    Exit(a);
  Exit(b);
end;

class function TIntAggregateFunctions.Max(a, b: integer): integer;
begin
  if a > b then
    Exit(a);
  Exit(b);
end;

end.
