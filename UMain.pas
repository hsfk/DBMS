unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  Menus, ComCtrls, UDBConnection, UDirectory, UAbout, UNotifications,
  UDBObjects, USchedule, UDBForm, UFormManager;

type

  TMainForm = class(TDBForm)
  private
    procedure CreateForm(ATable: TDBTable; FormType: TDBFormType);
    procedure LoadDirectoryMenyItems;
  published
    FMainMenu: TMainMenu;
    FFileMenu: TMenuItem;
    FDirectoryMenu: TMenuItem;
    FAboutMenu: TMenuItem;
    FOpenMenuItem: TMenuItem;
    FStatusBar: TStatusBar;
    FScheduleMenu: TMenuItem;
    FFormMenu: TMenuItem;
    procedure FFormMenuClick(Sender: TObject);
    procedure FScheduleMenuClick(Sender: TObject);
    procedure FAboutMenuClick(Sender: TObject);
    procedure DirectoryMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject); override;
    procedure FOpenMenuItemClick(Sender: TObject);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  inherited FormCreate(Sender);
  Connection := TDBConnection.Create;
  Connection.Connect('DataBase.fdb', 'sysdba', 'masterkey');
  Constraints.MinHeight := 40;
  Constraints.MinWidth := Width;
  Caption := APP_CAPTION;
  FStatusBar.SimpleText := Connection.CurrentConnection;
  ThisSubscriber := TSubscriber.Create(True);
  LoadDirectoryMenyItems;
end;

procedure TMainForm.FScheduleMenuClick(Sender: TObject);
begin
  CreateForm(DBData.TablesByName['TimeTable'], TSchedule);
end;

procedure TMainForm.FFormMenuClick(Sender: TObject);
begin
  if FormManager = nil then begin
    Application.CreateForm(TFormManager, FormManager);
    FormManager.LoadForms(Self, nil);
  end;
  FormManager.Show;
end;

procedure TMainForm.CreateForm(ATable: TDBTable; FormType: TDBFormType);
var
  NClass: TNClass;
  i: integer;
begin
  if Connection.Connected then begin
    SetLength(NClass, 0);
    for i := 0 to ATable.Count - 1 do begin
      SetLength(NClass, Length(NClass) + 1);
      NClass[High(NClass)] := ATable.Fields[i].ParentTable.Index;
    end;
    CreateChildForm(NClass, ATable, FormType, nil, -1);
  end
  else
    ShowMessage('Необходимо подключение к базе данных.');
end;

procedure TMainForm.LoadDirectoryMenyItems;
var
  i: integer;
  MenuItem: TMenuItem;
begin
  for i := 0 to DBData.Count - 1 do begin
    MenuItem := TMenuItem.Create(FDirectoryMenu);
    MenuItem.Tag := i;
    MenuItem.OnClick := @DirectoryMenuItemClick;
    MenuItem.Caption := DBData.Tables[i].Name;
    FDirectoryMenu.Add(MenuItem);
  end;
end;

procedure TMainForm.FOpenMenuItemClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
begin
  OpenDialog := TOpenDialog.Create(Self);
  OpenDialog.Filter := 'Firebird Database|*.fdb|All Files|*.*';
  if OpenDialog.Execute then begin
    Connection.Connect(OpenDialog.FileName, 'sysdba', 'masterkey');
    FStatusBar.SimpleText := Connection.CurrentConnection;
  end;
  OpenDialog.Free;
end;

procedure TMainForm.DirectoryMenuItemClick(Sender: TObject);
begin
  CreateForm(DBData.Tables[TMenuItem(Sender).Tag], TDirectory);
end;

procedure TMainForm.FAboutMenuClick(Sender: TObject);
var
  About: TAboutForm;
begin
  Application.CreateForm(TAboutForm, About);
  About.Show;
end;

end.
