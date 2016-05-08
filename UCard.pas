unit UCard;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, StdCtrls, ComCtrls, sqldb, DB, Forms,
  UDBForm, Dialogs, UDBObjects, UNotifications, Controls, ExtCtrls, UCardControls;

type

  TCard = class(TDBForm)
  private
    FTop: integer;
    FLeft: integer;
    FControls: TCardControls;
    FRecordIndex: integer;
    procedure LoadGUIData;
    procedure LoadInterface; virtual; abstract;
    procedure OnNotificationRecieve(Sender: TObject);
  public
    procedure Load(ANClass: TNClass; ATable: TDBTable; Params: TParams = nil); override;
    procedure Select(ControlIndex: integer; Data: string);
  published
    FStatusBar: TStatusBar;
    FApplyBtn: TButton;
    FBtnsPanel: TPanel;
    FCancelBtn: TButton;
    FControlsSBox: TScrollBox;
    procedure FormCreate(Sender: TObject); override;
    procedure FApplyBtnClick(Sender: TObject); virtual; abstract;
    procedure FCancelBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction); override;
  end;

  TEditCard = class(TCard)
  private
    procedure UpdateTable;
    procedure LoadInterface; override;
  public
    procedure Load(ANClass: TNClass; ATable: TDBTable; Params: TParams = nil); override;
  published
    procedure FApplyBtnClick(Sender: TObject); override;
  end;

  TInsertCard = class(TCard)
  private
    procedure TableInsert;
    procedure LoadInterface; override;
  public
    procedure Load(ANClass: TNClass; ATable: TDBTable; Params: TParams = nil); override;
  published
    procedure FApplyBtnClick(Sender: TObject); override;
  end;

implementation

{$R *.lfm}

procedure TCard.FormCreate(Sender: TObject);
begin
  inherited FormCreate(Sender);
end;

procedure TCard.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FControls.Free;
  inherited FormClose(Sender, CloseAction);
end;

procedure TCard.Load(ANClass: TNClass; ATable: TDBTable; Params: TParams = nil);
begin
  Table := ATable;
  FStatusBar.SimpleText := Connection.CurrentConnection;
  FTop := 5;
  FLeft := 5;
  FRecordIndex := Params.ParamByName('Target').AsInteger;
  FControls := TCardControls.Create(FRecordIndex, Table, FControlsSBox, 5, 5);
  inherited Load(ANClass, ATable);
  ThisSubscriber.OnNotificationRecieve := @OnNotificationRecieve;
  FSelectAll := Table.Query.Select(TDBFilters.Create(
    TDBFilter.Create(Table.Front, ' = ', IntToStr(FRecordIndex))));
  FControls.CreateGUIControls;
  LoadGUIData;
  FControls.SubscribeControlsFromSameTable;
  LoadInterface;
end;

procedure TCard.Select(ControlIndex: integer; Data: string);
begin
  FControls[ControlIndex].Caption := Data;
end;

procedure TCard.LoadGUIData;
var
  i: integer;
  FieldIndex: integer = 0;
  SameFieldLeft: boolean = False;
begin
  for i := 0 to FControls.Size - 1 do begin
    FControls[i].Clear;
    PerformQuery(FControls[i].Field.ParentTable.Query.Select(nil));
    if i > 0 then
      SameFieldLeft := FControls[i - 1].Field.ParentTable = FControls[i].Field.ParentTable;
    if SameFieldLeft then
      FieldIndex += 1
    else
      FieldIndex := 1;
    while not FormQuery.EOF do begin
      FControls[i].LoadData(
        FormQuery.Fields[FieldIndex].AsString
       ,FormQuery.Fields[0].AsInteger
        );
      FormQuery.Next;
    end;
  end;
end;

procedure TCard.OnNotificationRecieve(Sender: TObject);
begin
  try
    LoadGUIData;
    LoadInterface;
  except
    Close;
  end;
end;

procedure TCard.FCancelBtnClick(Sender: TObject);
begin
  if FControls.Correct then
    if MessageDlg('Вы действительно хотите выйти?', mtConfirmation, mbOKCancel, 0) =
      mrCancel then
      Exit;
  ThisSubscriber.Parent.UnSubscribe(ThisSubscriber);
  Close;
end;

procedure TEditCard.UpdateTable;
var
  i: integer;
begin
  for i := 0 to FControls.Size - 1 do
    ExecQuery(FControls.Items[i].UpdateTable);
end;

procedure TEditCard.LoadInterface;
var
  i: integer;
begin
  PerformQuery(FSelectAll);
  for i := 0 to FControls.Size - 1 do
    FControls.Items[i].Caption := FormQuery.Fields[i + 1].Value;
end;

procedure TEditCard.Load(ANClass: TNClass; ATable: TDBTable; Params: TParams = nil);
begin
  inherited Load(ANClass, ATable, Params);
  Caption := ATable.Name + ', ' + 'Редактирование, ID = ' + IntToStr(FRecordIndex);
end;

procedure TEditCard.FApplyBtnClick(Sender: TObject);
begin
  if not FControls.Correct then begin
    ShowMessage('Нужно заполнить все поля.');
    Exit;
  end;
  UpdateTable;
  ThisSubscriber.CreateNotification(nil, ThisSubscriber.NClass);
  Close;
end;

procedure TInsertCard.Load(ANClass: TNClass; ATable: TDBTable; Params: TParams = nil);
begin
  inherited Load(ANClass, ATable, Params);
  Caption := ATable.Name + ', ' + 'Вставка';
end;

procedure TInsertCard.TableInsert;
var
  Values: TParams;
  i: integer = 0;
  SameTable: TDBTable = nil;
begin
  Values := TParams.Create;
  while i < FControls.Size do begin
    if SameTable <> FControls[i].Field.ParentTable then begin
      Values.AddParam(FControls[i].Data);
      SameTable := FControls[i].Field.ParentTable;
    end;
    i += 1;
  end;
  ExecQuery(Table.Query.Insert(Values));
end;

procedure TInsertCard.LoadInterface;
var
  i: integer;
begin
  for i := 0 to FControls.Size - 1 do
    FControls.Items[i].Deselect;
end;

procedure TInsertCard.FApplyBtnClick(Sender: TObject);
begin
  if not FControls.Correct then begin
    ShowMessage('Нужно заполнить все поля.');
    Exit;
  end;
  TableInsert;
  ThisSubscriber.CreateNotification(nil, ThisSubscriber.NClass);
  Close;
end;

end.
