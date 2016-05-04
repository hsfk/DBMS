unit UConflictForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, UCustomControl, UDBObjects, UVector, UConflicts;

type

  TParamControl = class;
  TParamControls = class;

  TConflictForm = class(TForm)
    FParamsSBox: TScrollBox;
  private
    FControls: TParamControls;
    FTable: TDBTable;
    FEditedConflict: TConflictPanel;
    FConflicts: TConflictPanels;
    function MakeConflictPanel: TConflictPanel;
  public
    procedure Load(Table: TDBTable; Conflicts: TConflictPanels;
      EditedConflict: TConflictPanel);
  published
    FNameEdit: TEdit;
    FAddParamBtn: TButton;
    FDelParamsBtn: TButton;
    FApplyBtn: TButton;
    FCancelBtn: TButton;
    FParamsGBox: TGroupBox;
    FNameLabel: TLabel;
    FBtnsPanel: TPanel;
    procedure FApplyBtnClick(Sender: TObject);
    procedure FCancelBtnClick(Sender: TObject);
    procedure FAddParamBtnClick(Sender: TObject);
    procedure FDelParamsBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;

  TParamControl = class(TCustomControl)
  private
    FTable: TDBTable;
    FFieldCBox: TComboBox;
    FParamCBox: TComboBox;
    FOps: TStringV;
    function GetFieldN: integer;
  public
    constructor Create(Table: TDBTable; AParent: TWinControl; ATop, ALeft: integer);
    procedure InitControls(AParent: TWinControl; ATop, ALeft: integer);
    function Correct: boolean;
    function IsEQ: boolean;
  published
    property FieldN: integer read GetFieldN;
  end;

  TCustomParamControl = specialize TCustomControls<TParamControl>;

  TParamControls = class(TCustomParamControl)
  private
    FTable: TDBTable;
  public
    constructor Create(Table: TDBTable; AParent: TWinControl; ATop, ALeft: integer);
    procedure AddParam(FieldN: integer; IsEQ: boolean);
    procedure AddParam;
    function BuildConflict(Name: string): TConflictType;
    function Correct: boolean;
  end;

implementation

{$R *.lfm}

function TConflictForm.MakeConflictPanel: TConflictPanel;
var
  Conflict: TConflictType;
begin
  Conflict := FControls.BuildConflict(FNameEdit.Text);
  Result := TConflictPanel.Create(Conflict.Name);
  Result.Conflict := Conflict;
end;

procedure TConflictForm.Load(Table: TDBTable; Conflicts: TConflictPanels;
  EditedConflict: TConflictPanel);
var
  i: integer;
begin
  FTable := Table;
  FControls.DeleteAll;
  FControls.FTable := FTable;
  FConflicts := Conflicts;
  FEditedConflict := EditedConflict;
  FNameEdit.Text := '';
  if FEditedConflict <> nil then begin
    Caption := 'Редактор конфликтов - ' + FEditedConflict.Conflict.Name;
    FNameEdit.Text := FEditedConflict.Conflict.Name;
    for i := 0 to FEditedConflict.Conflict.EQRecIDs.Size - 1 do
      FControls.AddParam(FEditedConflict.Conflict.EQRecIDs[i], True);
    for i := 0 to FEditedConflict.Conflict.NEQRecIDs.Size - 1 do
      FControls.AddParam(FEditedConflict.Conflict.NEQRecIDs[i], False);
  end
  else
    Caption := 'Редактор конфликтов';
end;

procedure TConflictForm.FormCreate(Sender: TObject);
begin
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;
  Constraints.MaxWidth := Width;
  Constraints.MaxHeight := Height;
  FControls := TParamControls.Create(FTable, FParamsSBox, 5, 5);
  FEditedConflict := nil;
end;

procedure TConflictForm.FApplyBtnClick(Sender: TObject);
begin
  if not FControls.Correct then begin
    ShowMessage('Нужно заполнить все поля.');
    Exit;
  end;
  if FEditedConflict = nil then
    FConflicts.AddConflict(FControls.BuildConflict(FNameEdit.Text))
  else begin
    FEditedConflict.Conflict.Free;
    FEditedConflict.Conflict := FControls.BuildConflict(FNameEdit.Text);
    FEditedConflict.Edit.Text := FEditedConflict.Conflict.Name;
  end;
  Close;
end;

procedure TConflictForm.FCancelBtnClick(Sender: TObject);
begin
  if FControls.Correct then
    if MessageDlg('Вы действительно хотите выйти?', mtConfirmation, mbOKCancel, 0) =
      mrCancel then
      Exit;
  Close;
end;

procedure TConflictForm.FAddParamBtnClick(Sender: TObject);
begin
  FControls.AddParam;
end;

procedure TConflictForm.FDelParamsBtnClick(Sender: TObject);
begin
  FControls.DeleteAll;
end;

constructor TParamControl.Create(Table: TDBTable; AParent: TWinControl;
  ATop, ALeft: integer);
begin
  FTable := Table;
  InitControls(AParent, ATop, ALeft);
end;

procedure TParamControl.InitControls(AParent: TWinControl; ATop, ALeft: integer);
var
  i: integer;
begin
  inherited Create(AParent, 250 + 20, ATop, ALeft);
  FFieldCBox := TComboBox.Create(Self);
  FParamCBox := TComboBox.Create(Self);
  InitComponent(FFieldCBox, Self, 1, 0, 150);
  InitComponent(FParamCBox, Self, 1, 150, 100);
  DelBtn.Top := 0;
  Delbtn.Left := 250;
  FOps := TStringV.Create(['Равно', 'Не равно']);
  for i := 0 to FOps.Size - 1 do
    FParamCBox.Items.Add(FOps[i]);
  for i := 0 to FTable.Count - 1 do
    FFieldCBox.Items.Add(FTable.Fields[i].Name);
  FFieldCBox.ReadOnly := True;
  FParamCBox.ReadOnly := True;
end;

function TParamControl.GetFieldN: integer;
begin
  Exit(FFieldCBox.ItemIndex);
end;

function TParamControl.Correct: boolean;
begin
  Exit((FParamCBox.ItemIndex <> -1) and (FFieldCBox.ItemIndex <> -1));
end;

function TParamControl.IsEQ: boolean;
begin
  Exit(FParamCBox.ItemIndex = 0);
end;

constructor TParamControls.Create(Table: TDBTable; AParent: TWinControl;
  ATop, ALeft: integer);
begin
  inherited Create(AParent, ATop, ALeft);
  FTable := Table;
end;

procedure TParamControls.AddParam(FieldN: integer; IsEQ: boolean);
var
  ParamControl: TParamControl;
begin
  ParamControl := TParamControl.Create(FTable, FParent, FTop, FLeft);
  ParamControl.FFieldCBox.ItemIndex := FieldN;
  if IsEQ then
    ParamControl.FParamCBox.ItemIndex := 0
  else
    ParamControl.FParamCBox.ItemIndex := 1;
  AddControlPanel(ParamControl);
end;

procedure TParamControls.AddParam;
begin
  AddControlPanel(TParamControl.Create(FTable, FParent, FTop, FLeft));
end;

function TParamControls.BuildConflict(Name: string): TConflictType;
var
  i: integer;
begin
  if not Correct then
    Exit(nil);
  Result := TConflictType.Create(Name);
  Result.EQRecIDs := TIntegerV.Create;
  Result.NEQRecIDs := TIntegerV.Create;
  for i := 0 to Size - 1 do
    if Items[i].IsEQ then
      Result.EQRecIDs.PushBack(Items[i].FieldN)
    else
      Result.NEQRecIDs.PushBack(Items[i].FieldN);
end;

function TParamControls.Correct: boolean;
var
  i: integer;
begin
  if Size = 0 then
    Exit(False);
  for i := 0 to Size - 1 do
    if not Items[i].Correct then
      Exit(False);
  Exit(True);
end;

end.
