unit UFormManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Menus, UDBForm;

type

  TFormManager = class(TForm)
  private
    FRoot: TDBForm;
  public
    procedure LoadForms(Root: TDBForm; AParent: TTreeNode);
    procedure UpDateTree;
  published
    FDBFormsView: TTreeView;
    FShowItem: TMenuItem;
    TreeActionsMenu: TPopupMenu;
    FCloseChildsMenu: TMenuItem;
    FCloseMenu: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FCloseChildsMenuClick(Sender: TObject);
    procedure FCloseMenuClick(Sender: TObject);
    procedure FShowItemClick(Sender: TObject);
    procedure FDBFormsViewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
  end;

var
  FormManager: TFormManager = nil;

implementation

{$R *.lfm}

procedure TFormManager.FDBFormsViewMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if Button = mbRight then
    if FDBFormsView.Selected <> nil then
      TreeActionsMenu.PopUp(X + Left, Y + Top);
end;

procedure TFormManager.FShowItemClick(Sender: TObject);
begin
  TDBForm(FDBFormsView.Selected.Data).BringToFront;
end;

procedure TFormManager.UpDateTree;
begin
  FDBFormsView.Items.Clear;
  LoadForms(FRoot, nil);
  FDBFormsView.Invalidate;
end;

procedure TFormManager.FCloseMenuClick(Sender: TObject);
begin
  TDBForm(FDBFormsView.Selected.Data).Close;
end;

procedure TFormManager.FCloseChildsMenuClick(Sender: TObject);
begin
  TDBForm(FDBFormsView.Selected.Data).CloseChildForms;
end;

procedure TFormManager.FormCreate(Sender: TObject);
begin
  Caption := 'Менеджер окон';
end;

procedure TFormManager.LoadForms(Root: TDBForm; AParent: TTreeNode);
var
  i: integer;
  CurNode: TTreeNode;
begin
  if (FDBFormsView.Items.Count = 0) or (AParent = nil) then begin
    FDBFormsView.Items.Add(nil, 'Root');
    FRoot := Root;
  end
  else
    FDBFormsView.Items.AddChild(AParent, Root.Caption);

  CurNode := FDBFormsView.Items[FDBFormsView.Items.Count - 1];
  CurNode.ExpandParents;
  CurNode.Data := Pointer(Root);
  for i := 0 to Root.ChildForms.Size - 1 do
    LoadForms(Root.ChildForms[i], CurNode);
end;

end.
