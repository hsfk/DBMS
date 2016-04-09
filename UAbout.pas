unit UAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, UVector;

const
  CURRENT_VERSION = 'v0.1.6 ';
  APP_NAME = 'DBViewer ';
  ABOUT = APP_NAME + CURRENT_VERSION + chr(10) + 'Автор: Протасов Сергей' +
    chr(10) + '2016г.';

type

  TAboutForm = class(TForm)
  published
    FAboutLabel: TLabel;
    procedure FormCreate(Sender: TObject);
  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.lfm}

{ TAboutForm }

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  Self.Caption := APP_NAME + CURRENT_VERSION + ' - ' + 'О программе';
  FAboutLabel.Caption := ABOUT;
end;

end.
