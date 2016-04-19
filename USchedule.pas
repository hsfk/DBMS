unit USchedule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls, ComCtrls, CheckLst, UDBForm;

type

  { TSchedule }

  TSchedule = class(TDBForm)
  private
  public
  published
    FCheckListBox: TCheckListBox;
    FDrawGrid: TDrawGrid;
    FVisibleRecsGBox: TGroupBox;
    FFiltersGBox: TGroupBox;
    FStatusBar: TStatusBar;
    FApplyFilterBtn: TButton;
    FHCBox: TComboBox;
    FVLabel: TLabel;
    FVCBox: TComboBox;
    FHLabel: TLabel;
    procedure FApplyFilterBtnClick(Sender: TObject);
  end;

implementation

{$R *.lfm}

{ TSchedule }

procedure TSchedule.FApplyFilterBtnClick(Sender: TObject);
begin

end;

end.

