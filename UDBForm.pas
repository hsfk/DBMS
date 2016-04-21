unit UDBForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  UNotifications, sqldb, DB, UDBConnection, UDBObjects, UVector;

type

  TDBField = UDBObjects.TDBField;
  TDBTable = UDBObjects.TDBTable;
  TDbConnection = UDBConnection.TDBConnection;
  TDBFormType = class of TDBForm;
  TNClass = UNotifications.TNClass;
  TParams = DB.TParams;

  {TODO: Child forms}
  TDBForm = class(TForm)
  private
    type
    TDBForms = specialize TVector<TDBForm>;
  private
    FID: integer;
    FOnPerform: TEvent;
    FOnExec: TEvent;
    FQuery: TSQLQuery;
    FDataSource: TDataSource;
    FTable: TDBTable;
    FThisSubscriber: TSubscriber;
    FParentForm: TDBForm;
    FDBConnection: TDbConnection;
    procedure RemoveChildForm(Form: TDBForm);
    procedure EmptyEvent;
  protected
    FCForms: TDBForms;
    FSelectAll: TQueryContainer;
    procedure ShowQuery(QContainer: TQueryContainer);
    function PerformQuery(QContainer: TQueryContainer): boolean; virtual;
    function ExecQuery(QContainer: TQueryContainer): boolean; virtual;
    function CreateForm(ANClass: TNClass; ATable: TDBTable;
      FormType: TDBFormType; Params: TParams = nil): TDBForm;
    function CreateChildForm(ANClass: TNClass; ATable: TDBTable;
      FormType: TDBFormType; Params: TParams; ID: integer): TDBForm;
  public
    procedure InitConnection(DBConnection: TDbConnection); virtual;
    procedure Load(ANClass: TNClass; ATable: TDBTable; Params: TParams = nil); virtual;
  published
    property Connection: TDbConnection read FDBConnection;
    property OnPerformQuery: TEvent write FOnPerform;
    property OnExecQuery: TEvent write FOnExec;
    property ThisSubscriber: TSubscriber read FThisSubscriber write FThisSubscriber;
    property FormQuery: TSQLQuery read FQuery;
    property Table: TDBTable read FTable write FTable;
    property DataSource: TDataSource read FDataSource;
    procedure FormCreate(Sender: TObject); virtual;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction); virtual;
  end;

function ToNClass(A: array of integer): TNClass;

implementation

{$R *.lfm}

function ToNClass(A: array of integer): TNClass;
var
  i: integer;
begin
  SetLength(Result, Length(A));
  for i := 0 to High(A) do
    Result[i] := A[i];
end;

procedure TDBForm.RemoveChildForm(Form: TDBForm);
var
  Index: integer;
begin
  Index := FCForms.FindInd(Form);
  if Index <> -1 then
    FCForms.DeleteInd(Index);
end;

procedure TDBForm.EmptyEvent;
begin
end;

procedure TDBForm.ShowQuery(QContainer: TQueryContainer);
var
  Str: string;
  i: integer;
begin
  Str := QContainer.Query + #13#10 + '__________' + #13#10;
  with QContainer do
    if Params <> nil then
      for i := 0 to Params.Count - 1 do begin
        Str += Params.Items[i].Name + ' ' + Params.Items[i].AsString + #13#10;
      end;
  ShowMessage(Str);
end;

procedure TDBForm.FormCreate(Sender: TObject);
begin
  FSelectAll.Query := '';
  FSelectAll.Params := nil;
  FTable := nil;
  FThisSubscriber := nil;
  FOnExec := @EmptyEvent;
  FOnPerform := @EmptyEvent;
  FCForms := TDBForms.Create;
  FParentForm := nil;
end;

procedure TDBForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if FThisSubscriber <> nil then
    if FThisSubscriber.Parent <> nil then
      FThisSubscriber.Parent.UnSubscribe(FThisSubscriber);
  if FParentForm <> nil then
    FParentForm.RemoveChildForm(Self);
end;

procedure TDBForm.InitConnection(DBConnection: TDbConnection);
begin
  FDBConnection := DBConnection;
  FQuery := TSQLQuery.Create(FQuery);
  FDataSource := TDataSource.Create(FDataSource);
  FQuery.DataBase := DBConnection.Connection;
  FQuery.Transaction := DBConnection.Transaction;
  FDataSource.DataSet := FQuery;
end;

procedure TDBForm.Load(ANClass: TNClass; ATable: TDBTable; Params: TParams = nil);
begin
  FTable := ATable;
  FThisSubscriber := TSubscriber.Create;
  FThisSubscriber.NClass := ANClass;
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
    FDBConnection.Transaction.Commit;
    FOnExec;
  except
    Result := False;
  end;
end;

function TDBForm.CreateForm(ANClass: TNClass; ATable: TDBTable; FormType: TDBFormType;
  Params: TParams = nil): TDBForm;
begin
  Application.CreateForm(FormType, Result);
  Result.InitConnection(FDBConnection);
  Result.Load(ANClass, ATable, Params);
  ThisSubscriber.Subscribe(Result.ThisSubscriber);
  Result.Show;
end;

function TDBForm.CreateChildForm(ANClass: TNClass; ATable: TDBTable;
  FormType: TDBFormType; Params: TParams; ID: integer): TDBForm;
var
  i: integer;
begin
  for i := 0 to FCForms.Size - 1 do
    if FCForms[i].FID = ID then begin
      FCForms[i].BringToFront;
      Exit(FCForms[i]);
    end;
  Result := CreateForm(ANClass, ATable, FormType, Params);
  Result.FID := ID;
  Result.FParentForm := Self;
  FCForms.PushBack(Result);
  Exit(Result);
end;

end.
