unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  Menus, ComCtrls, UDBConnection, UDirectory, UAbout, UNotifications,
  UDBObjects, USchedule, UDBForm;

type

  TMainForm = class(TForm)
  private
    FNotifications: TSubscriber;
    FDBConnection: TDBConnection;
    procedure CreateForm(Table: TDBTable; FormType: TDBFormType);
    procedure LoadDirectoryMenyItems;
  published
    FMainMenu: TMainMenu;
    FFileMenu: TMenuItem;
    FDirectoryMenu: TMenuItem;
    FAboutMenu: TMenuItem;
    FOpenMenuItem: TMenuItem;
    FStatusBar: TStatusBar;
    FScheduleMenu: TMenuItem;
    procedure FScheduleMenuClick(Sender: TObject);
    procedure FAboutMenuClick(Sender: TObject);
    procedure DirectoryMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FOpenMenuItemClick(Sender: TObject);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FDBConnection := TDBConnection.Create;
  FDBConnection.Connect('DataBase.fdb', 'sysdba', 'masterkey');
  Constraints.MinHeight := 40;
  Constraints.MinWidth := Width;
  Caption := APP_CAPTION;
  FStatusBar.SimpleText := FDBConnection.CurrentConnection;
  FNotifications := TSubscriber.Create(True);
  LoadDirectoryMenyItems;
end;

procedure TMainForm.FScheduleMenuClick(Sender: TObject);
begin
  CreateForm(DBData.TablesByName['Time_Table'], TSchedule);
end;

procedure TMainForm.CreateForm(Table: TDBTable; FormType: TDBFormType);
var
  Form: TDBForm;
  NClass: TNClass;
  i: integer;
begin
  if FDBConnection.Connected then begin
    SetLength(NClass, 0);
    for i := 0 to Table.Count - 1 do begin
      SetLength(NClass, Length(NClass) + 1);
      NClass[High(NClass)] := Table.Fields[i].ParentTable.Index;
    end;
    Application.CreateForm(FormType, Form);
    Form.InitConnection(FDBConnection);
    Form.Load(NClass, Table);
    FNotifications.Subscribe(Form.ThisSubscriber);
    Form.Show;
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
    FDBConnection.Connect(OpenDialog.FileName, 'sysdba', 'masterkey');
    FStatusBar.SimpleText := FDBConnection.CurrentConnection;
  end;
  OpenDialog.Free;
end;

procedure TMainForm.DirectoryMenuItemClick(Sender: TObject);
begin
  CreateForm(DBData.Tables[TMenuItem(Sender).Tag], TDirectory)
end;

procedure TMainForm.FAboutMenuClick(Sender: TObject);
var
  About: TAboutForm;
begin
  Application.CreateForm(TAboutForm, About);
  About.Show;
end;

end.
