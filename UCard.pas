unit UCard;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, StdCtrls, ComCtrls, sqldb, UDb, UCardControls, DB,
  UAbout, UDBForm, Dialogs, UDBObjects, UNotifications, Controls;

type

  { TCard }

  TCard = class(TDBForm)
  private
    FTop: integer;
    FLeft: integer;
    FControls: TControls;
    FRecordIndex: integer;
    FCboxNotifications: TSubscriber;
    function Correct: boolean;
    procedure CreateGUIControls;
    procedure LoadGUIData;
    procedure SubscribeCBoxes;
    procedure LoadInterface; virtual; abstract;
    procedure OnNotificationRecieve(Sender: TObject);
  public
    procedure Load(ANClass: TNClass; ATable: TDBTable; Params: TParams = nil); override;
  published
    FStatusBar: TStatusBar;
    FApplyBtn: TButton;
    FCancelBtn: TButton;
    procedure FormCreate(Sender: TObject); override;
    procedure FApplyBtnClick(Sender: TObject); virtual; abstract;
    procedure FCancelBtnClick(Sender: TObject);
  end;

  TEditCard = class(TCard)
  private
    procedure UpdateTable;
    procedure LoadInterface; override;
  published
    procedure FApplyBtnClick(Sender: TObject); override;
  end;

  TInsertCard = class(TCard)
  private
    procedure TableInsert;
    procedure LoadInterface; override;
  published
    procedure FApplyBtnClick(Sender: TObject); override;
  end;

implementation

{$R *.lfm}

{ TCard }

procedure TCard.FormCreate(Sender: TObject);
begin
  inherited FormCreate(Sender);
  Self.Constraints.MinHeight := Self.Height;
  Self.Constraints.MaxHeight := Self.Height;
  Self.Constraints.MinWidth := Self.Width;
end;

procedure TCard.Load(ANClass: TNClass; ATable: TDBTable; Params: TParams = nil);
begin
  Table := ATable;
  Self.Caption := APP_NAME + CURRENT_VERSION + ' - ' + Table.Name;
  FTop := 20;
  FLeft := 32;
  FControls := TControls.Create;
  FRecordIndex := Params.ParamByName('Target').AsInteger;
  inherited Load(ANClass, ATable);
  ThisSubscriber.OnNotificationRecieve := @OnNotificationRecieve;
  FSelectAll := Table.Query.Select(
    TDBFilters.Create(
      TDBFilter.Create(Table.Front, ' = ', IntToStr(FRecordIndex))));
  CreateGUIControls;
  LoadGUIData;
  SubscribeCBoxes;
  LoadInterface;
end;

procedure TCard.SubscribeCBoxes;
var
  SameTable: TDBTable = nil;
  NClass: integer = 1;
  i: integer = 1;
begin
  FCboxNotifications.Free;
  FCboxNotifications := TSubscriber.Create(True);
  with Table do
    while i < Count - 1 do begin
      if Fields[i].ParentTable = Fields[i + 1].ParentTable then begin
        SameTable := Fields[i].ParentTable;
        while (i < Count) and (Fields[i].ParentTable = SameTable) do begin
          FControls.Items[i - 1].Subscriber.NClass :=
            ToNClass([NClass]);
          FCboxNotifications.Subscribe(FControls.Items[i - 1].Subscriber);
          i += 1;
        end;
        NClass += 1;
      end;
      i += 1;
    end;
end;

procedure TCard.CreateGUIControls;
var
  i: integer;
  Control: TDBControl;
begin
  with Table do
    for i := 1 to Count - 1 do begin
      Control := Fields[i].CreateControl(FRecordIndex);
      Control.CreateGUI(Self, FTop, FLeft);
      FControls.PushBack(Control);
      FTop += 25;
    end;
end;

procedure TCard.LoadGUIData;
var
  i: integer;
  FieldIndex: integer = 1;
  SameFieldLeft: boolean = False;
begin
  for i := 0 to FControls.Size - 1 do begin
    FControls[i].Clear;
    PerformQuery(FControls[i].ParentTable.Query.Select(nil));
    if i > 0 then
      SameFieldLeft := FControls[i - 1].ParentTable = FControls[i].ParentTable;
    if SameFieldLeft then
      FieldIndex += 1
    else
      FieldIndex := 2;
    while not FormQuery.EOF do begin
      FControls[i].LoadData(
         FormQuery.Fields.FieldByNumber(FieldIndex).AsString
        ,FormQuery.Fields.FieldByNumber(1).AsInteger
        );
      FormQuery.Next;
    end;
  end;
end;

function TCard.Correct: boolean;
var
  i: integer;
begin
  for i := 0 to FControls.Size - 1 do
    if FControls.Items[i].Correct = False then
      Exit(False);
  Exit(True);
end;

procedure TCard.OnNotificationRecieve(Sender: TObject);
begin
  LoadGUIData;
  LoadInterface;
end;

procedure TCard.FCancelBtnClick(Sender: TObject);
begin
  if Correct then
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

procedure TEditCard.FApplyBtnClick(Sender: TObject);
begin
  if not Correct then begin
    ShowMessage('Нужно заполнить все поля.');
    Exit;
  end;
  UpdateTable;
  ThisSubscriber.CreateNotification(nil, ThisSubscriber.NClass);
  Close;
end;

procedure TInsertCard.TableInsert;
var
  Values: TParams;
  i: integer;
begin
  Values := TParams.Create;
  for i := 0 to FControls.Size - 1 do
    Values.AddParam(FControls.Items[i].Data);
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
  if not Correct then begin
    ShowMessage('Нужно заполнить все поля.');
    Exit;
  end;
  TableInsert;
  ThisSubscriber.CreateNotification(nil, ThisSubscriber.NClass);
  Close;
end;

end.
