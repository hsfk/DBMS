unit UDb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, IBConnection, Dialogs;

type

  TDbConnection = class
  private
    FConnected: boolean;
    FConnection: TIBConnection;
    FTransaction: TSQLTransaction;
    function GetCurrentConnection: string;
  public
    constructor Create;
    procedure Connect(DbName, UserName, Password: string);
  published
    property Connected: boolean read FConnected;
    property CurrentConnection: string read GetCurrentConnection;
    property Connection: TIBConnection read FConnection;
    property Transaction: TSQLTransaction read FTransaction;
  end;

var
  DbConnection: TDbConnection;

implementation

constructor TDbConnection.Create;
begin
  FConnected := False;
  FConnection := TIBConnection.Create(FConnection);
  FTransaction := TSQLTransaction.Create(FTransaction);
  FConnection.Transaction := FTransaction;
  FTransaction.DataBase := FConnection;
end;

procedure TDbConnection.Connect(DbName, UserName, Password: string);
begin
  FConnection.DatabaseName := DBName;
  FConnection.UserName := UserName;
  FConnection.Password := Password;
  FConnection.Connected := False;
  FConnected := True;
  try
    Connection.Connected := True;
  except
    FConnected := False;
    ShowMessage('Не удалось подключится к базе данных');
    FConnection.DatabaseName := '';
    FConnection.UserName := '';
    FConnection.Password := '';
  end;
end;

function TDbConnection.GetCurrentConnection: string;
begin
  with FConnection do begin
    if DatabaseName = '' then
      Exit('[NO DATABASE SELECTED]');
    Exit('USER: ' + UserName + ' | ' + DatabaseName);
  end;
end;

initialization

  DbConnection := TDbConnection.Create;
  DbConnection.Connect('DataBase.fdb', 'sysdba', 'masterkey');

end.
