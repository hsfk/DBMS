unit UExport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, USchedule, UHTMLExportUtils, UOOUtils, UDBObjects;

type

  TScheduleExport = class
  private
    class procedure HTMLExportTable(Schedule: TSchedule; var HTMLExport: THTMLExport);
    class procedure HTMLExportFilters(Schedule: TSchedule; var HTMLExport: THTMLExport);
    class procedure OOPrepare(Schedule: TSchedule; var OOXLSExport: TOOCalc);
    class procedure OOBuildTable(Schedule: TSchedule; var OOXLSExport: TOOCalc);
    class procedure OOBuildFilters(Schedule: TSchedule; var OOXLSExport: TOOCalc);
  public
    class procedure SaveAsXLS(Schedule: TSchedule; FileName: string);
    class procedure SaveAsHTML(Schedule: TSchedule; FileName: string);
  end;

implementation

class procedure TScheduleExport.SaveAsXLS(Schedule: TSchedule; FileName: string);
var
  XLSTable: TOOCalc;
begin
  if Schedule.HTitleData.Empty then
    Exit;
  try
    XLSTable := TOOCalc.Create;
    XLSTable.Open(False);
    OOPrepare(Schedule, XLSTable);
    OOBuildTable(Schedule, XLSTable);
    OOBuildFilters(Schedule, XLSTable);
    XLSTable.SaveAs(FileName);
    XLSTable.Close;
  finally
    XLSTable.Free;
  end;
end;

class procedure TScheduleExport.SaveAsHTML(Schedule: TSchedule; FileName: string);
var
  HTML: THTMLExport;
begin
  HTML := THTMLExport.Create(FileName);
  with HTML do begin
    OpTag('HTML');
    OpTag('HEAD');
    TextOut('<META CHARSET = "UTF-8">');
    TextOut('<TITLE> Расписание </TITLE>');
    ClTag('HEAD');
    OpTag('BODY');
    HTMLExportTable(Schedule, HTML);
    HTMLExportFilters(Schedule, HTML);
    ClTag('BODY');
    ClTag('HTML');
  end;
  HTML.Free;
end;

class procedure TScheduleExport.HTMLExportTable(Schedule: TSchedule;
  var HTMLExport: THTMLExport);
var
  i: integer;
  j: integer;
  k: integer;
  l: integer;
begin
  with HTMLExport do
    with Schedule do begin
      OpTag('TABLE', 'BORDER = 1');
      TextOut('<CAPTION>Расписание</CAPTION');
      OpTag('TR');
      TextOut('<TD> </TD>');
      for i := 0 to HTitleData.Size - 1 do
        TextOut('<TD BGCOLOR = "LightGray">' + HTitleData[i].Data + '</TD>');
      ClTag('TR');
      for j := 0 to Cells.Height - 1 do begin
        OpTag('TR');
        TextOut('<TD BGCOLOR = "LightGray">' + VTitleData[j].Data + '</TD>');
        for i := 0 to Cells.Width - 1 do begin
          OpTag('TD', 'NOWRAP VALIGN = "TOP"');
          if Cells[i, j] <> nil then begin
            for k := 0 to Cells[i, j].Elements.Size - 1 do begin
              for l := 0 to Cells[i, j].Elements[k].Data.Size - 1 do
                if FVisFields.Checked[l] then
                  TextOut(Cells[i, j].Elements[k].TextOut(l) + '<BR>');
              TextOut('<BR>');
            end;
          end
          else
            TextOut('<BR>');
          ClTag('TD');
        end;
        ClTag('TR');
      end;
      ClTag('TABLE');
    end;
end;

class procedure TScheduleExport.HTMLExportFilters(Schedule: TSchedule;
  var HTMLExport: THTMLExport);
var
  i: integer;
begin
  with HTMLExport do
    with Schedule do begin
      if not Filters.Empty and Filters.Correct then begin
        OpTag('TABLE', 'BORDER = 1');
        TextOut('<CAPTION>Фильтры</CAPTION>');
        for i := 0 to Filters.Size - 1 do
          with Filters[i].Filter do
            TextOut('<TR><TD>' + Name + '</TD><TD>' + ConditionalOperator +
              '</TD><TD>' + Param + '</TD></TR>');
      end;
    end;
end;

class procedure TScheduleExport.OOPrepare(Schedule: TSchedule; var OOXLSExport: TOOCalc);
var
  i: word;
begin
  with OOXLSExport do
    with Schedule do begin
      TableName := 'Расписание';
      ColWidths[0] := FDrawGrid.ColWidths[0] * 29;
      for i := 0 to FDrawGrid.ColCount - 1 do begin
        if i > 0 then
          ColWidths[i] := MaxColWidth(i - 1) * 29;
        CellColor[i, 0] := $F0F0F0;
      end;
      RowHeights[0] := FDrawGrid.RowHeights[0] * 29;
      for i := 0 to FDrawGrid.RowCount - 1 do begin
        if i > 0 then
          RowHeights[i] := MaxRowHeight(i - 1) * 29;
        CellColor[0, i] := $F0F0F0;
      end;

      for i := 0 to HTitleData.Size - 1 do
        CellText[i + 1, 0] := HTitleData[i].Data;
      for i := 0 to VTitleData.Size - 1 do begin
        CellText[0, i + 1] := VTitleData[i].Data;
        OOXLSExport.Cells[0, i + 1].VertJustify := 1;
      end;
    end;
end;

class procedure TScheduleExport.OOBuildTable(Schedule: TSchedule;
  var OOXLSExport: TOOCalc);
var
  i: word;
  j: word;
  k: word;
  l: word;
  AText: string;
begin
  with OOXLSExport do
    with Schedule do begin
      for j := 0 to Cells.Height - 1 do
        for i := 0 to Cells.Width - 1 do begin
          if Cells[i, j] <> nil then begin
            AText := '';
            for k := 0 to Cells[i, j].Elements.Size - 1 do begin
              for l := 0 to Cells[i, j].Elements[k].Data.Size - 1 do
                if FVisFields.Checked[l] then
                  AText += Cells[i, j].Elements[k].TextOut(l) + #13#10;
              AText += #13#10;
            end;
          end
          else
            AText := ' ';
          CellText[i + 1, j + 1] := AText;
          OOXLSExport.Cells[i + 1, j + 1].VertJustify := 1;
        end;
    end;
end;

class procedure TScheduleExport.OOBuildFilters(Schedule: TSchedule;
  var OOXLSExport: TOOCalc);
var
  i: word;
  Row: word;
begin
  with OOXLSExport do
    with Schedule do begin
      if not Filters.Empty and Filters.Correct then begin
        Row := Cells.Height + 2;
        CellText[0, Row] := 'Фильтры';
        for i := 0 to Filters.Size - 1 do begin
          Row += 1;
          with Filters[i].Filter do begin
            CellText[0, Row] := Name;
            CellText[1, Row] := ConditionalOperator;
            CellText[2, Row] := Param;
          end;
        end;
      end;
    end;
end;

end.
