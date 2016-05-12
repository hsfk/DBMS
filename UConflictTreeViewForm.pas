unit UConflictTreeViewForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ComCtrls, Menus, UConflicts, UVector, UDBObjects;

type

  TConflictTreeViewForm = class(TForm)
  private
    FTable: TDBTable;
    FRoot: TTreeNode;
    FConflicts: TConflictPanels;
    procedure MakeBranch(ConflictPanel: TConflictPanel);
    procedure MakeLVL(AParent: TTreeNode; SameRecs, Recs: TStringV);
    function FindSameRecs(AData: TDataTuple): TStringV;
  public
    procedure Load(Table: TDBTable; Conflicts: TConflictPanels);
  published
    FTreeView: TTreeView;
    FActions: TPopupMenu;
    FExpandChild: TMenuItem;
    FCollapseChild: TMenuItem;
    procedure FCollapseChildClick(Sender: TObject);
    procedure FExpandChildClick(Sender: TObject);
    procedure FTreeViewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
  end;

implementation

{$R *.lfm}

procedure TConflictTreeViewForm.Load(Table: TDBTable; Conflicts: TConflictPanels);
var
  i: integer;
begin
  FConflicts := Conflicts;
  FTable := Table;
  FTreeView.Items.Clear;
  FRoot := FTreeView.Items.Add(nil, 'Конфликты');
  FTreeView.Selected := FRoot;
  for i := 0 to FConflicts.Size - 1 do
    MakeBranch(FConflicts[i]);
end;

procedure TConflictTreeViewForm.FCollapseChildClick(Sender: TObject);
begin
  FTreeView.Selected.Collapse(True);
end;

procedure TConflictTreeViewForm.FExpandChildClick(Sender: TObject);
begin
  FTreeView.Selected.Expand(True);
end;

procedure TConflictTreeViewForm.FTreeViewMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if Button = mbRight then
    FActions.PopUp(X + Left, Y + Top);
end;

procedure TConflictTreeViewForm.MakeLVL(AParent: TTreeNode; SameRecs, Recs: TStringV);
var
  AName: string = '';
  i: integer;
begin
  for i := 0 to Recs.Size - 1 do
    if SameRecs[i] = '' then
      AName += FTable.Fields[i].Name + ': ' + Recs[i] + ', ';
  FTreeView.Items.AddChild(AParent, AName);
end;

procedure TConflictTreeViewForm.MakeBranch(ConflictPanel: TConflictPanel);
var
  Branch: TTreeNode;
  Cell: TTreeNode;
  SameRecs: TStringV;
  AName: string;
  i: integer;
  j: integer;
begin
  with ConflictPanel do begin
    if Conflict.Data = nil then
      Exit;

    Branch := FTreeView.Items.AddChild(FRoot, Conflict.Name);
    Branch.Data := Conflict;
    for i := 0 to Conflict.Data.Size - 1 do begin
      SameRecs := FindSameRecs(Conflict.Data[i]);
      AName := '';
      for j := 0 to SameRecs.Size - 1 do
        if SameRecs[j] <> '' then
          AName += FTable.Fields[j].Name + ': ' + SameRecs[j] + ', ';
      Cell := FTreeView.Items.AddChild(Branch, AName);
      for j := 0 to Conflict.Data[i].Size - 1 do
        MakeLVL(Cell, SameRecs, Conflict.Data[i][j]);
    end;
  end;
end;

function TConflictTreeViewForm.FindSameRecs(AData: TDataTuple): TStringV;
var
  i: integer;
  j: integer;
begin
  Result := TStringV.Create;
  Result.Resize(AData[0].Size);
  for i := 0 to Result.Size - 1 do
    Result[i] := AData[0][i];

  for i := 0 to AData.Size - 1 do
    for j := 0 to AData[i].Size - 1 do
      if Result[j] <> AData[i][j] then
        Result[j] := '';
end;

end.
