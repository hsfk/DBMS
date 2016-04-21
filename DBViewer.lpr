program DBViewer;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UMain;

{$R *.res}

{#DEFINE AppName 'DBViewer'}
{#DEFINE AppV 'v0.2.0'}

{#IFNDEF AppName}
  {#DEFINE AppName 'AppName'}
{#ENDIF}
{#IFNDEF AppV}
  {#DEFINE AppV 'v0.0.0'}
{#ENDIF}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
