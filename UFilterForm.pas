unit UFilterForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, UDBForm, UFilterPanel, UDBObjects;

type

  TFilterForm = class(TDBForm)
  protected
    FFilterPanels: TFilterPanels;
    procedure BeforeFilterDelete(FilterIndex: integer); virtual;
    procedure UpdateFilters;
    procedure DeleteFilters;
  public
    procedure AddFilterPanel(AFilterPanel: TFilterPanel); virtual;
    function FiltersCorrect: boolean;
    function FilterCount: integer;
    function ApplyFilters: boolean;
  published
    procedure FormCreate(Sender: TObject); override;
  end;

implementation

{$R *.lfm}

procedure TFilterForm.FormCreate(Sender: TObject);
begin
  inherited FormCreate(Sender);
  FFilterPanels := TFilterPanels.Create;
end;

procedure TFilterForm.AddFilterPanel(AFilterPanel: TFilterPanel);
begin
  AFilterPanel.Index := FFilterPanels.Size;
  FFilterPanels.PushBack(AFilterPanel);
  AFilterPanel.BeforeDelete := @BeforeFilterDelete;
end;

procedure TFilterForm.UpdateFilters;
var
  i: integer;
begin
  for i := 0 to FFilterPanels.Size - 1 do begin
    FFilterPanels[i].Top := 5 + i * 25;
    FFilterPanels[i].Index := i;
  end;
end;

procedure TFilterForm.DeleteFilters;
var
  i: integer = 0;
  Size: integer;
begin
  if FFilterPanels.Size > 0 then begin
    if FiltersCorrect then
      if MessageDlg('Вы действительно хотите удалить все фильтры?',
        mtConfirmation, mbOKCancel, 0) = mrCancel then
        Exit;
    Size := FFilterPanels.Size;
    while i < Size do begin
      if FFilterPanels[i].Enabled then begin
        FFilterPanels[i].Free;
        FFilterPanels.DeleteIndS(i);
        i -= 1;
        Size -= 1;
      end;
      i += 1;
    end;
  end;
end;

function TFilterForm.FiltersCorrect: boolean;
var
  i: integer;
begin
  for i := 0 to FFilterPanels.Size - 1 do
    if not FFilterPanels[i].Correct then
      Exit(False);
  Exit(True);
end;

function TFilterForm.FilterCount: integer;
begin
  Exit(FFilterPanels.Size);
end;

function TFilterForm.ApplyFilters: boolean;
var
  Filters: TDBFilters;
  i: integer;
begin
  if FFilterPanels.Size > 0 then begin
    if not FiltersCorrect then begin
      ShowMessage('Нужно заполнить все фильтры');
      Exit;
    end;
    Filters := TDBFilters.Create;
    for i := 0 to FFilterPanels.Size - 1 do
      Filters.PushBack(FFilterPanels.Items[i].Filter);
    PerformQuery(Table.Query.Select(Filters));
    Exit(True);
  end
  else
    PerformQuery(FSelectAll);
  Exit(False);
end;

procedure TFilterForm.BeforeFilterDelete(FilterIndex: integer);
begin
  if FFilterPanels.Size > 0 then begin
    FFilterPanels.DeleteIndS(FilterIndex);
    UpdateFilters;
  end;
end;

end.

