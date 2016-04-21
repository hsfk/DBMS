unit UAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

const
  CURRENT_VERSION = 'v0.2.0 ';
  APP_NAME = 'DBViewer ';
  APP_CAPTION = APP_NAME + CURRENT_VERSION;
  ABOUT = APP_CAPTION + #13#10 + 'Автор: Протасов Сергей' + #13#10 + '2016г.';

type

  TAboutForm = class(TForm)
  published
    FAboutLabel: TLabel;
    procedure FormCreate(Sender: TObject);
  end;

implementation

{$R *.lfm}

{ TAboutForm }

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  Self.Caption := APP_CAPTION + ' - ' + 'О программе';
  FAboutLabel.Caption := ABOUT;
end;

end.
