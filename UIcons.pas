unit UIcons;

{$mode objfpc}{$H+}

interface

uses
  Graphics, sysutils, Classes;

var
  delete_20x20: TIcon;
  expand_20x20: TIcon;
  drag_20x20: TIcon;
  edit_20x20: TIcon;
  alert_20x20: TIcon;

implementation

function GetIcon(Path: string): TIcon;
begin
  Result := nil;
  if FileExists(Path) then begin
    Result := TIcon.Create;
    Result.LoadFromFile(Path);
  end;
end;

initialization

  delete_20x20 := GetIcon('icons/delete_20x20.ico');
  expand_20x20 := GetIcon('icons/expand_20x20.ico');
  drag_20x20 := GetIcon('icons/drag_20x20.ico');
  edit_20x20 := GetIcon('icons/edit_20x20.ico');
  alert_20x20 := GetIcon('icons/alert_20x20.ico');

end.

