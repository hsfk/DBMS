unit UDirectory;

{$mode objfpc}{$H+}

interface

uses
  Forms, DBGrids, ComCtrls, UAbout, UFilter, StdCtrls, UCard, UDBForm,
  UDB, DB, UDBObjects;

type
  {TODO: Focus}
  TDirectory = class(TDBForm)
  private
    procedure OnFiltersChange;
    procedure UpdateColumns;
    procedure CreateCard(CardType: TDBFormType);
    procedure NotificationRecieve(Sender: TObject);
  public
    procedure InitConnection(DBConnection: TDbConnection); override;
    procedure Load(ANotificationClass: TNotificationClass; ATable: TDBTable;
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
  Constraints.MinHeight := Self.Height;
  Constraints.MinWidth := Self.Width;
  FApplyFilterBtn.Enabled := False;
  OnPerformQuery := @UpdateColumns;
end;

procedure TDirectory.FAddFilterBtnClick(Sender: TObject);
var
  Counter: integer;
  Filter: TFilterPanel;
begin
  Counter := FFiltersScrollBox.ControlCount;
  Filter := TFilterPanel.Create(Table, FFiltersScrollBox, 5 + Counter * 25, 5);
  Filter.OnChange := @OnFiltersChange;
  FApplyFilterBtn.Enabled := True;
end;

procedure TDirectory.FApplyFilterBtnClick(Sender: TObject);
var
  Filters: TDBFilters;
  i: integer;
begin
  Filters := TDBFilters.Create;
  for i := 0 to FFiltersScrollBox.ControlCount - 1 do
    Filters.PushBack(TFilterPanel(FFiltersScrollBox.Controls[i]).Filter);
  PerformQuery(Table.Select(Filters));
end;

procedure TDirectory.FDelFilterBtnClick(Sender: TObject);
begin
  if FFiltersScrollBox.ControlCount > 0 then begin
    FFiltersScrollBox.Controls[FFiltersScrollBox.ControlCount - 1].Free;
    FApplyFilterBtn.Enabled := True;
  end;
  if FFiltersScrollBox.ControlCount = 0 then
    FApplyFilterBtn.Enabled := False;
end;

procedure TDirectory.FDelElementClick(Sender: TObject);
var
  Index: integer;
begin
  Index := Query.Fields[0].Value;
  ExecQuery(Table.Delete(Index));
  ThisSubscriber.CreateNotification(nil, ThisSubscriber.NotificationClass);
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
  TargetIndex := Query.Fields[0].Value;
  Params := TParams.Create;
  Params.CreateParam(ftInteger, 'Target', ptUnknown);
  Params.ParamByName('Target').AsInteger := TargetIndex;
  CreateChildForm(ThisSubscriber.NotificationClass, Table, CardType, Params);
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

procedure TDirectory.Load(ANotificationClass: TNotificationClass;
  ATable: TDBTable; Params: TParams = nil);
begin
  inherited Load(ANotificationClass, ATable);
  ThisSubscriber.OnNotificationRecieve := @NotificationRecieve;
  Self.Caption := APP_NAME + CURRENT_VERSION + ' - ' + Table.Name;
  FSelectAll := Table.Select(nil);
  PerformQuery(FSelectAll);
end;

end.
