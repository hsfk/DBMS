unit UDirectory;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, DBGrids, ComCtrls, UAbout, UFilterPanel, StdCtrls, UCard, UDBForm,
  UDBConnection, DB, UDBObjects, UVector, Dialogs;

type

  { TDirectory }

  TDirectory = class(TDBForm)
  private
    type
    TFilterPanels = specialize TVector<TFilterPanel>;

    TFocus = record
      X: integer;
      Y: integer;
    end;
  private
    FFocus: TFocus;
    FFilterPanels: TFilterPanels;
    procedure OnFiltersChange;
    procedure UpdateColumns;
    procedure CreateCard(CardType: TDBFormType);
    procedure NotificationRecieve(Sender: TObject);
    procedure MemFocus;
    procedure RestoreFocus;
    procedure AddFilterPanel(AFilterPanel: TFilterPanel);
    function FormHash: integer;
  public
    procedure InitConnection(DBConnection: TDbConnection); override;
    procedure Load(ANClass: TNClass; ATable: TDBTable; Params: TParams = nil); override;
    procedure AddFilter(Field, COperator: integer; Param: string;
      AEnabled: boolean = True);
    procedure ApplyFilters;
  published
    FFiltersGBox: TGroupBox;
    FTableGBox: TGroupBox;
    FStatusBar: TStatusBar;
    FDBGrid: TDBGrid;
    FFiltersScrollBox: TScrollBox;
    FAddFilterBtn: TButton;
    FApplyFilterBtn: TButton;
    FDelFilterBtn: TButton;
    FAddElement: TButton;
    FDelElement: TButton;
    procedure FDBGridCellClick(Column: TColumn);
    procedure FAddElementClick(Sender: TObject);
    procedure FDBGridDblClick(Sender: TObject);
    procedure FDelElementClick(Sender: TObject);
    procedure FDelFilterBtnClick(Sender: TObject);
    procedure FAddFilterBtnClick(Sender: TObject);
    procedure FApplyFilterBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject); override;
  end;

implementation

{$R *.lfm}

procedure TDirectory.FormCreate(Sender: TObject);
begin
  inherited FormCreate(Sender);
  Constraints.MinWidth := 350;
  Constraints.MinHeight := 20;
  DoubleBuffered := True;
  FFilterPanels := TFilterPanels.Create;
  FApplyFilterBtn.Enabled := True;
  OnPerformQuery := @UpdateColumns;
  FFocus.X := 0;
  FFocus.Y := 1;
end;

procedure TDirectory.Load(ANClass: TNClass; ATable: TDBTable; Params: TParams = nil);
begin
  inherited Load(ANClass, ATable);
  ThisSubscriber.OnNotificationRecieve := @NotificationRecieve;
  Caption := APP_CAPTION + ' - ' + Table.Name;
  FSelectAll := Table.Query.Select(nil);
  PerformQuery(FSelectAll);
  MemFocus;
end;

procedure TDirectory.AddFilter(Field, COperator: integer; Param: string;
  AEnabled: boolean = True);
var
  FilterPanel: TFilterPanel;
begin
  FilterPanel := TFilterPanel.Create(Table, FFiltersScrollBox, 5 +
    FFilterPanels.Size * 25, 5);
  FilterPanel.FieldIndex := Field;
  FilterPanel.OpsIndex := COperator;
  FilterPanel.EditText := Param;
  FilterPanel.Enabled := AEnabled;
  AddFilterPanel(FilterPanel);
end;

procedure TDirectory.FAddFilterBtnClick(Sender: TObject);
begin
  AddFilterPanel(TFilterPanel.Create(Table, FFiltersScrollBox, 5 +
    FFilterPanels.Size * 25, 5));
end;

procedure TDirectory.AddFilterPanel(AFilterPanel: TFilterPanel);
begin
  AFilterPanel.OnChange := @OnFiltersChange;
  AFilterPanel.Index := FFilterPanels.Size;
  FFilterPanels.PushBack(AFilterPanel);
  FApplyFilterBtn.Enabled := True;
end;

procedure TDirectory.ApplyFilters;
var
  Filters: TDBFilters;
  i: integer;
begin
  if FFilterPanels.Size > 0 then begin
    for i := 0 to FFilterPanels.Size - 1 do
      if not FFilterPanels.Items[i].Correct then begin
        ShowMessage('Нужно заполнить все фильтры');
        Exit;
      end;
    Filters := TDBFilters.Create;
    for i := 0 to FFilterPanels.Size - 1 do
      Filters.PushBack(FFilterPanels.Items[i].Filter);
    FApplyFilterBtn.Enabled := False;
    PerformQuery(Table.Query.Select(Filters));
  end
  else
    PerformQuery(FSelectAll);
end;

procedure TDirectory.FApplyFilterBtnClick(Sender: TObject);
begin
  ApplyFilters;
end;

procedure TDirectory.FDelFilterBtnClick(Sender: TObject);
begin
  if FFilterPanels.Size > 0 then begin
    FFilterPanels.Back.Free;
    FFilterPanels.DeleteInd(FFilterPanels.Size - 1);
    FApplyFilterBtn.Enabled := True;
  end;
  if FFilterPanels.Size = 0 then
    FApplyFilterBtn.Enabled := True;
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

procedure TDirectory.OnFiltersChange;
begin
  FApplyFilterBtn.Enabled := True;
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
  TargetIndex: integer;
  Params: TParams;
begin
  TargetIndex := FormQuery.Fields[0].Value;
  Params := TParams.Create;
  Params.CreateParam(ftInteger, 'Target', ptUnknown);
  Params.ParamByName('Target').AsInteger := TargetIndex;
  CreateChildForm(ThisSubscriber.NClass, Table, CardType, Params, FormHash);
end;

procedure TDirectory.NotificationRecieve(Sender: TObject);
begin
  PerformQuery(FSelectAll);
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
