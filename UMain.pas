unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  Menus, ComCtrls, UDBConnection, UDirectory, UAbout, UNotifications,
  UDBObjects, USchedule;

type

  TMainForm = class(TForm)
  private
    FNotifications: TSubscriber;
    FDBConnection: TDBConnection;
    procedure CreateDirectoryForm(Table: TDBTable);
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
var
  Schedule: TSchedule;
  NClass: TNClass;
begin
  if FDBConnection.Connected then begin
    SetLength(NClass, 1);
    NClass[0] := 0;
    Application.CreateForm(TSchedule, Schedule);
    Schedule.InitConnection(FDBConnection);
    Schedule.Load(NClass, DBData.TablesByName['Time_Table']);
    Schedule.Show;
  end
  else
    ShowMessage('Необходимо подключение к базе данных.');
end;

procedure TMainForm.CreateDirectoryForm(Table: TDBTable);
var
  Directory: TDirectory;
  NClass: TNClass;
  i: integer;
begin
  SetLength(NClass, 0);
  for i := 0 to Table.Count - 1 do begin
    SetLength(NClass, Length(NClass) + 1);
    NClass[High(NClass)] := Table.Fields[i].ParentTable.Index;
  end;
  Application.CreateForm(TDirectory, Directory);
  Directory.InitConnection(FDBConnection);
  Directory.Load(NClass, Table);
  FNotifications.Subscribe(Directory.ThisSubscriber);
  Directory.Show;
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
  if FDBConnection.Connected then
    CreateDirectoryForm(DBData.Tables[TMenuItem(Sender).Tag])
  else
    ShowMessage('Необходимо подключение к базе данных.');
end;

procedure TMainForm.FAboutMenuClick(Sender: TObject);
var
  About: TAboutForm;
begin
  Application.CreateForm(TAboutForm, About);
  About.Show;
end;

end.
