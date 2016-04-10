unit UDBForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  UNotifications, sqldb, DB, UDB, UDBObjects;

type

  TDBField = UDBObjects.TDBField;
  TDBTable = UDBObjects.TDBTable;
  TDbConnection = UDB.TDbConnection;
  TDBFormType = class of TDBForm;
  TNotificationClass = UNotifications.TNotificationClass;
  TParams = DB.TParams;

  {TODO: Child forms}
  TDBForm = class(TForm)
  private
    FOnPerform: TEvent;
    FOnExec: TEvent;
    FQuery: TSQLQuery;
    FDataSource: TDataSource;
    FTable: TDBTable;
    FThisSubscriber: TSubscriber;
    //FLastSuccesfulQuery <-
    procedure EmptyEvent;
  protected
    //FChildForms //<= Vector
    FSelectAll: TQueryContainer;
    function PerformQuery(QContainer: TQueryContainer): boolean; virtual;
    function ExecQuery(QContainer: TQueryContainer): boolean; virtual;
    function CreateChildForm(ANotificationClass: TNotificationClass;
      ATable: TDBTable; FormType: TDBFormType; Params: TParams = nil): TDBForm;
  public
    procedure InitConnection(DBConnection: TDbConnection); virtual;
    procedure Load(ANotificationClass: TNotificationClass; ATable: TDBTable;
      Params: TParams = nil); virtual;
  published
    property OnPerformQuery: TEvent write FOnPerform;
    property OnExecQuery: TEvent write FOnExec;
    property ThisSubscriber: TSubscriber read FThisSubscriber write FThisSubscriber;
    property FormQuery: TSQLQuery read FQuery;
    property Table: TDBTable read FTable write FTable;
    property DataSource: TDataSource read FDataSource;
    procedure FormCreate(Sender: TObject); virtual;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction); virtual;
  end;

function ToNotificationClass(A: array of integer): TNotificationClass;

implementation

{$R *.lfm}

function ToNotificationClass(A: array of integer): TNotificationClass;
var
  i: integer;
begin
  SetLength(Result, Length(A));
  for i := 0 to High(A) do
    Result[i] := A[i];
end;

procedure TDBForm.EmptyEvent;
begin
end;

procedure TDBForm.FormCreate(Sender: TObject);
begin
  FSelectAll.Query := '';
  FSelectAll.Params := nil;
  FTable := nil;
  FThisSubscriber := nil;
  FOnExec := @EmptyEvent;
  FOnPerform := @EmptyEvent;
end;

procedure TDBForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if FThisSubscriber <> nil then
    if FThisSubscriber.Parent <> nil then
      FThisSubscriber.Parent.UnSubscribe(FThisSubscriber);
end;

procedure TDBForm.InitConnection(DBConnection: TDbConnection);
begin
  FQuery := TSQLQuery.Create(FQuery);
  FDataSource := TDataSource.Create(FDataSource);
  FQuery.DataBase := DBConnection.Connection;
  FQuery.Transaction := DBConnection.Transaction;
  FDataSource.DataSet := FQuery;
end;

procedure TDBForm.Load(ANotificationClass: TNotificationClass;
  ATable: TDBTable; Params: TParams = nil);
begin
  FTable := ATable;
  FThisSubscriber := TSubscriber.Create;
  FThisSubscriber.NotificationClass := ANotificationClass;
end;

function TDBForm.PerformQuery(QContainer: TQueryContainer): boolean;
begin
  Result := True;
  FQuery.Close;
  FQuery.SQL.Clear;
  FQuery.SQL.Text := QContainer.Query;
  if QContainer.Params <> nil then
    FQuery.Params := QContainer.Params;
  try
    FQuery.Open;
    FOnPerform;
  except
    Result := False;
  end;
end;

function TDBForm.ExecQuery(QContainer: TQueryContainer): boolean;
begin
  Result := True;
  FQuery.Close;
  FQuery.SQL.Clear;
  FQuery.SQL.Text := QContainer.Query;
  if QContainer.Params <> nil then
    FQuery.Params := QContainer.Params;
  try
    FQuery.ExecSQL;
    DbConnection.Transaction.Commit;
    FOnExec;
  except
    Result := False;
  end;
end;

function TDBForm.CreateChildForm(ANotificationClass: TNotificationClass;
  ATable: TDBTable; FormType: TDBFormType; Params: TParams = nil): TDBForm;
begin
  Application.CreateForm(FormType, Result);
  Result.InitConnection(DbConnection);
  Result.Load(ANotificationClass, ATable, Params);
  ThisSubscriber.Subscribe(Result.ThisSubscriber);
  Result.Show;
end;

end.
