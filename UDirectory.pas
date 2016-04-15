unit UDirectory;

{$mode objfpc}{$H+}

interface

uses
  Forms, DBGrids, ComCtrls, UAbout, UFilterPanel, StdCtrls, UCard, UDBForm,
  UDB, DB, UDBObjects, UVector, Dialogs;

type
  {TODO: Focus}
  TDirectory = class(TDBForm)
  private
    type
    TFilterPanels = specialize TVector<TFilterPanel>;
  private
    FFilterPanels: TFilterPanels;
    procedure OnFiltersChange;
    procedure UpdateColumns;
    procedure CreateCard(CardType: TDBFormType);
    procedure NotificationRecieve(Sender: TObject);
  public
    procedure InitConnection(DBConnection: TDbConnection); override;
    procedure Load(ANotificationClass: TNClass; ATable: TDBTable;
      Params: TParams = nil); override;
  published
    FFiltersGroupBox: TGroupBox;
    FTableGroupBox: TGroupBox;
    FStatusBar: TStatusBar;
    FDBGrid: TDBGrid;
    FFiltersScrollBox: TScrollBox;
    FAddFilterBtn: TButton;
    FApplyFilterBtn: TButton;
    FDelFilterBtn: TButton;
    FAddElement: TButton;
    FDelElement: TButton;
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
{ TDirectory }

procedure TDirectory.FormCreate(Sender: TObject);
begin
  inherited FormCreate(Sender);
  FFilterPanels := TFilterPanels.Create;
  Constraints.MinHeight := Self.Height;
  Constraints.MinWidth := Self.Width;
  FApplyFilterBtn.Enabled := False;
  OnPerformQuery := @UpdateColumns;
end;

procedure TDirectory.Load(ANotificationClass: TNClass;
  ATable: TDBTable; Params: TParams = nil);
begin
  inherited Load(ANotificationClass, ATable);
  ThisSubscriber.OnNotificationRecieve := @NotificationRecieve;
  Caption := APP_NAME + CURRENT_VERSION + ' - ' + Table.Name;
  FSelectAll := Table.Query.Select(nil);
  PerformQuery(FSelectAll);
end;

procedure TDirectory.FAddFilterBtnClick(Sender: TObject);
var
  FilterPanel: TFilterPanel;
begin
  FilterPanel := TFilterPanel.Create
  (
    Table, FFiltersScrollBox, 5 + FFilterPanels.Size * 25, 5
  );
  FilterPanel.OnChange := @OnFiltersChange;
  FilterPanel.Index := FFilterPanels.Size;
  FFilterPanels.PushBack(FilterPanel);
  FApplyFilterBtn.Enabled := True;
end;

procedure TDirectory.FApplyFilterBtnClick(Sender: TObject);
var
  Filters: TDBFilters;
  i: integer;
begin
  for i := 0 to FFilterPanels.Size - 1 do
    if not FFilterPanels.Items[i].Correct then begin
      ShowMessage('Нужно заполнить все фильтры');
      Exit;
    end;
  Filters := TDBFilters.Create;
  for i := 0 to FFilterPanels.Size - 1 do
    Filters.PushBack(FFilterPanels.Items[i].Filter);
  PerformQuery(Table.Query.Select(Filters));
end;

procedure TDirectory.FDelFilterBtnClick(Sender: TObject);
begin
  if FFilterPanels.Size > 0 then begin
    FFilterPanels.Back.Free;
    FFilterPanels.DeleteInd(FFilterPanels.Size - 1);
    FApplyFilterBtn.Enabled := True;
  end;
  if FFilterPanels.Size = 0 then
    FApplyFilterBtn.Enabled := False;
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
  CreateChildForm(ThisSubscriber.NClass, Table, CardType, Params);
end;

procedure TDirectory.NotificationRecieve(Sender: TObject);
begin
  PerformQuery(FSelectAll);
end;

procedure TDirectory.InitConnection(DBConnection: TDbConnection);
begin
  inherited InitConnection(DBConnection);
  FDBGrid.DataSource := DataSource;
  FDBGrid.ReadOnly := True;
  FStatusBar.SimpleText := 'USER: ' + DbConnection.Connection.UserName +
    ' | ' + DbConnection.Connection.DatabaseName;
end;

end.
