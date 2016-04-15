unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  Menus, ComCtrls, UDb, UDirectory, UAbout, UNotifications, UDBObjects;

type

  { TMainForm }

  TMainForm = class(TForm)
  private
    FIsConnected: boolean;
    FNotifications: TSubscriber;
    procedure CreateDirectoryForm(Table: TDBTable);
  published
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    DirectoryMenu: TMenuItem;
    AboutMenu: TMenuItem;
    Groups: TMenuItem;
    Lessons: TMenuItem;
    ClassRooms: TMenuItem;
    LessonTypes: TMenuItem;
    LessonTimes: TMenuItem;
    OpenMenuItem: TMenuItem;
    FStatusBar: TStatusBar;
    WeekDays: TMenuItem;
    Teachers: TMenuItem;
    TimeTable: TMenuItem;
    procedure AboutMenuClick(Sender: TObject);
    procedure DirectoryMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OpenMenuItemClick(Sender: TObject);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}
{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  MainForm.Constraints.MinHeight := MainForm.Height;
  MainForm.Constraints.MinWidth := MainForm.Width;
  MainForm.Caption := APP_NAME + CURRENT_VERSION;
  FIsConnected := True;
  FStatusBar.SimpleText := DbConnection.CurrentConnection;

  FNotifications := TSubscriber.Create(True);
end;

procedure TMainForm.CreateDirectoryForm(Table: TDBTable);
var
  Directory: TDirectory;
  NotificationClass: TNClass;
  i: integer;
begin
  SetLength(NotificationClass, 0);
  for i := 0 to Table.Count - 1 do begin
    SetLength(NotificationClass, Length(NotificationClass) + 1);
    NotificationClass[High(NotificationClass)] := Table.Fields[i].ParentTable.Index;
  end;
  Application.CreateForm(TDirectory, Directory);
  Directory.InitConnection(DbConnection);
  Directory.Load(NotificationClass, Table);
  FNotifications.Subscribe(Directory.ThisSubscriber);
  Directory.Show;
end;

procedure TMainForm.OpenMenuItemClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
begin
  OpenDialog := TOpenDialog.Create(Self);
  OpenDialog.Filter := 'Firebird Database|*.fdb|All Files|*.*';
  if OpenDialog.Execute then begin
    FIsConnected := True;
    DbConnection.Connect(OpenDialog.FileName, 'sysdba', 'masterkey');
    FStatusBar.SimpleText := DbConnection.CurrentConnection;
  end;
  OpenDialog.Free;
end;

procedure TMainForm.DirectoryMenuItemClick(Sender: TObject);
begin
  if FIsConnected = True then begin
    CreateDirectoryForm(DBData.Tables[TMenuItem(Sender).Tag]);
  end
  else begin
    ShowMessage('Необходимо подключение к базе данных.');
  end;
end;

procedure TMainForm.AboutMenuClick(Sender: TObject);
var
  About: TAboutForm;
begin
  Application.CreateForm(TAboutForm, About);
  About.Show;
end;

end.
