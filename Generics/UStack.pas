unit UStack;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  generic TStack<T> = class
  private
    type
    Nodeptr = ^TNode;

    TNode = record
      Item: T;
      Next: Nodeptr;
    end;
  private
    FSize: integer;
    FHead: Nodeptr;
    function CreateNode(Item: T): Nodeptr; virtual;
    function GetFront: T;                  virtual;
  public
    constructor Create;                    virtual;
    destructor  Destroy;                   override;
    procedure   Push(Item: T);             virtual;
    procedure   Pop;                       virtual;

    property Front: T read GetFront;
  published
    property Size: integer read FSize;
  end;

  TIntegerSt = specialize TStack<integer>;
  TStringSt  = specialize TStack<string>;

implementation

constructor TStack.Create;
begin
  FHead := nil;
  FSize := 0;
end;

destructor TStack.Destroy;
begin
  while Size > 0 do
    Pop;
  inherited Destroy;
end;

function TStack.CreateNode(Item: T): Nodeptr;
begin
  Result := new(Nodeptr);
  Result^.Item := Item;
  Result^.Next := nil;
end;

function TStack.GetFront: T;
begin
  Exit(FHead^.Item);
end;

procedure TStack.Push(Item: T);
var
  NewNode: Nodeptr;
begin
  FSize += 1;
  NewNode := CreateNode(Item);
  NewNode^.Next := FHead;
  FHead := NewNode;
end;

procedure TStack.Pop;
var
  Node: Nodeptr;
begin
  FSize -= 1;
  Node := FHead;
  FHead := FHead^.Next;
  Dispose(Node);
end;

end.
