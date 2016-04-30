unit UDirectory;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, DBGrids, ComCtrls, UAbout, UFilters, StdCtrls, UCard, UDBForm,
  UDBConnection, DB, UDBObjects, Dialogs, Controls, Classes;

type

  TDirectory = class(TDBForm)
  private
    FFocus: TPoint;
    FFilters: TFilterPanels;
    procedure EnableBtn;
    procedure UpdateColumns;
    procedure CreateCard(CardType: TDBFormType);
    procedure NotificationRecieve(Sender: TObject);
    procedure MemFocus;
    procedure RestoreFocus;
    function FormHash: integer;
  public
    procedure InitConnection(DBConnection: TDbConnection); override;
    procedure Load(ANClass: TNClass; ATable: TDBTable; Params: TParams = nil); override;
    procedure ApplyFilters;
    procedure AddFilterPanel(AFilterPanel: TFilterPanel);
    procedure AddFilterPanel(Field, COp, Param: string; AEnabled: boolean);
    function FilterCount: integer;
  published
    FFiltersGBox: TGroupBox;
    FTableGBox: TGroupBox;
    FStatusBar: TStatusBar;
    FDBGrid: TDBGrid;
    FFiltersSBox: TScrollBox;
    FAddFilterBtn: TButton;
    FApplyBtn: TButton;
    FDelAllFiltersBtn: TButton;
    FAddElement: TButton;
    FDelElement: TButton;
    procedure FDBGridCellClick(Column: TColumn);
    procedure FAddElementClick(Sender: TObject);
    procedure FDBGridDblClick(Sender: TObject);
    procedure FDelElementClick(Sender: TObject);
    procedure FDelAllFiltersBtnClick(Sender: TObject);
    procedure FAddFilterBtnClick(Sender: TObject);
    procedure FApplyBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject); override;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction); override;
  end;

implementation

{$R *.lfm}

procedure TDirectory.FormCreate(Sender: TObject);
begin
  inherited FormCreate(Sender);
  Constraints.MinWidth := 350;
  Constraints.MinHeight := 20;
  DoubleBuffered := True;
  FApplyBtn.Enabled := True;
  OnPerformQuery := @UpdateColumns;
  FFocus.X := 0;
  FFocus.Y := 1;
end;

procedure TDirectory.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FFilters.Free;
  inherited FormClose(Sender, CloseAction);
end;

procedure TDirectory.Load(ANClass: TNClass; ATable: TDBTable; Params: TParams = nil);
begin
  inherited Load(ANClass, ATable);
  FFilters := TFilterPanels.Create(ATable, FFiltersSBox, 5, 5);
  FFilters.OnChange := @EnableBtn;
  FFilters.OnDelete := @EnableBtn;
  ThisSubscriber.OnNotificationRecieve := @NotificationRecieve;
  Caption := APP_CAPTION + ' - ' + Table.Name;
  PerformQuery(FSelectAll);
  MemFocus;
end;

procedure TDirectory.ApplyFilters;
var
  Filtered: TQueryContainer;
begin
  Filtered := FFilters.Apply;
  PerformQuery(Filtered);
  if Filtered.Query <> FSelectAll.Query then
    FApplyBtn.Enabled := False;
end;

procedure TDirectory.FAddFilterBtnClick(Sender: TObject);
begin
  FFilters.AddFilterPanel;
end;

procedure TDirectory.AddFilterPanel(AFilterPanel: TFilterPanel);
begin
  FFilters.AddFilterPanel(AFilterPanel);
end;

procedure TDirectory.AddFilterPanel(Field, COp, Param: string; AEnabled: boolean);
var
  FilterPanel: TFilterPanel;
begin
  FilterPanel := TFilterPanel.Create(Table, FFiltersSBox, 5, 5);
  FFilters.AddFilterPanel(FilterPanel);
  FilterPanel.SetFilterData(Field, COp, Param);
  FilterPanel.Enabled := AEnabled;
end;

function TDirectory.FilterCount: integer;
begin
  Exit(FFilters.Size);
end;

procedure TDirectory.FApplyBtnClick(Sender: TObject);
begin
  ApplyFilters;
end;

procedure TDirectory.FDelAllFiltersBtnClick(Sender: TObject);
begin
  FFilters.DeleteAll;
end;

procedure TDirectory.FDelElementClick(Sender: TObject);
var
  Index: integer;
begin
  Index := FormQuery.Fields[0].Value;
  ExecQuery(Table.Query.Delete(Index));
  ThisSubscriber.CreateNotification(nil, ThisSubscriber.NClass);
end;

procedure TDirectory.FAddElementClick(Sender: TObject);
begin
  CreateCard(TInsertCard);
end;

procedure TDirectory.FDBGridDblClick(Sender: TObject);
begin
  CreateCard(TEditCard);
end;

procedure TDirectory.FDBGridCellClick(Column: TColumn);
begin
  MemFocus;
end;

procedure TDirectory.EnableBtn;
begin
  FApplyBtn.Enabled := True;
end;

procedure TDirectory.UpdateColumns;
var
  i: integer;
begin
  for i := 0 to FDBGrid.Columns.Count - 1 do
    Table.Fields[i].Load(FDBGrid.Columns[i]);
end;

procedure TDirectory.CreateCard(CardType: TDBFormType);
var
  RowIndex: integer;
  Params: TParams;
begin
  RowIndex := FormQuery.Fields[0].Value;
  Params := TParams.Create;
  Params.CreateParam(ftInteger, 'Target', ptUnknown);
  Params.ParamByName('Target').AsInteger := RowIndex;
  CreateChildForm(ThisSubscriber.NClass, Table, CardType, Params, FormHash);
end;

procedure TDirectory.NotificationRecieve(Sender: TObject);
begin
  PerformQuery(FFilters.Apply);
  RestoreFocus;
end;

procedure TDirectory.MemFocus;
begin
  FFocus.X := FDBGrid.SelectedIndex;
  FFocus.Y := FDBGrid.DataSource.DataSet.RecNo;
end;

procedure TDirectory.RestoreFocus;
var
  RecCount: integer;
begin
  RecCount := FDBGrid.DataSource.DataSet.RecordCount;
  if RecCount > 0 then begin
    if FFocus.Y > RecCount then
      FFocus.Y := RecCount;
    FDBGrid.DataSource.DataSet.RecNo := FFocus.Y;
    FDBGrid.SelectedIndex := FFocus.X;
  end;
end;

function TDirectory.FormHash: integer;
begin
  Result := (FormQuery.Fields[0].AsInteger + 3);
  Result *= Result * (Result + 1);
  Result *= (FDBGrid.SelectedIndex + 11);
  Result *= Result;
end;

procedure TDirectory.InitConnection(DBConnection: TDbConnection);
begin
  inherited InitConnection(DBConnection);
  FDBGrid.DataSource := DataSource;
  FDBGrid.ReadOnly := True;
  FStatusBar.SimpleText := DBConnection.CurrentConnection;
end;

end.
