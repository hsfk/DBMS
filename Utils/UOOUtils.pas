unit UOOUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, comobj, variants, Graphics;

type

  TOOCalc = class
  private
    FServer: variant;
    FDesktop: variant;
    FDoc: variant;
    FSheet: variant;
    procedure SetColWidth(i: word; Width: word);
    procedure SetRowHeight(i: word; Height: word);
    procedure SetCellText(i, j: word; AText: string);
    procedure SetCellColor(i, j: word; AColor: TColor);
    procedure SetTableName(AName: string);
    function GetCell(i, j: word): variant;
    function CreateProperty(AName: string; AVal: variant): variant;
    function ToURL(FileName: string): string;
  public
    procedure Close;
    procedure Open(Visible: boolean);
    procedure SaveAs(FileName: string);

    property TableName: string write SetTableName;
    property Cells[i, j: word]: variant read GetCell;
    property CellColor[i, j: word]: TColor write SetCellColor;
    property CellText[i, j: word]: string write SetCellText;
    property ColWidths[i: word]: word write SetColWidth;
    property RowHeights[i: word]: word write SetRowHeight;
  end;

implementation

function TOOCalc.ToURL(FileName: string): string;
var
  i: integer;
begin
  for i := 1 to Length(FileName) do
    if FileName[i] = '\' then
      FileName[i] := '/';
  Exit(FileName);
end;

procedure TOOCalc.Close;
begin
  FDoc.Close(True);
end;

procedure TOOCalc.SaveAs(FileName: string);
var
  Params: variant;
begin
  Params := VarArrayCreate([0, 1], varVariant);
  Params[0] := CreateProperty('FilterName', WideString('MS Excel 97'));
  Params[1] := CreateProperty('Overwrite', True);
  FDoc.StoreToURL(WideString('file:///' + ToURL(FileName)), Params);
end;

procedure TOOCalc.SetTableName(AName: string);
begin
  FSheet.Name := WideString(UTF8Decode(AName));
end;

procedure TOOCalc.SetColWidth(i: word; Width: word);
var
  Col: variant;
begin
  Col := FSheet.GetColumns.GetByIndex(i);
  Col.Width := Width;
end;

procedure TOOCalc.SetRowHeight(i: word; Height: word);
var
  Row: variant;
begin
  Row := FSheet.GetRows.GetByIndex(i);
  Row.Height := Height;
end;

procedure TOOCalc.SetCellText(i, j: word; AText: string);
var
  Cell: variant;
begin
  Cell := FSheet.GetCellByPosition(i, j);
  Cell.SetString(WideString(UTF8Decode(AText)));
end;

procedure TOOCalc.SetCellColor(i, j: word; AColor: TColor);
var
  Cell: variant;
begin
  Cell := FSheet.GetCellByPosition(i, j);
  Cell.CellBackColor := AColor;
end;

procedure TOOCalc.Open(Visible: boolean);
var
  i: word;
  Params: variant;
begin
  FServer := CreateOleObject('com.sun.star.ServiceManager');
  FDesktop := FServer.CreateInstance('com.sun.star.frame.Desktop');

  Params := VarArrayCreate([0, 0], varVariant);
  Params[0] := CreateProperty('Hidden', not Visible);
  FDoc := FDesktop.LoadComponentFromURL('private:factory/scalc', '_blank', 1, Params);

  for i := FDoc.GetSheets.Count - 1 downto 1 do
    FDoc.GetSheets.RemoveByName(WideString(FDoc.GetSheets.GetByIndex(i).GetName));
  FSheet := FDoc.GetSheets.GetByIndex(0);
end;

function TOOCalc.GetCell(i, j: word): variant;
begin
  Exit(FSheet.GetCellByPosition(i, j));
end;

function TOOCalc.CreateProperty(AName: string; AVal: variant): variant;
begin
  Result := FServer.Bridge_GetStruct('com.sun.star.beans.PropertyValue');
  Result.Name := WideString(AName);
  Result.Value := AVal;
end;

end.
