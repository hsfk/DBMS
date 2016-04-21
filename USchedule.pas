unit USchedule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls, ComCtrls, CheckLst, PairSplitter, UDBForm, UVector, UMatrix,
  UDBObjects, UAbout;

const
  CELL_W = 250;
  CELL_H = 100;
  TITLE_W = 100;
  TITLE_H = 30;
  LEFT_MARGIN = 5;

type

  TData = specialize TVector<string>;

  TCell = class
  private
    type
    TStringMatrix = specialize TMatrix<string>;
  private
    FTable: TDBTable;
    FBackGroundCol: TColor;
    FSelectCol: TColor;
    FTextCol: TColor;
    FFieldData: TStringMatrix;
  public
    constructor Create(Table: TDBTable);
    procedure Draw(Rect: TRect; Canvas: TCanvas; VisibleFields: TCheckListBox);
    procedure AddData(Data: TStringMatrix);
  published
    property FieldData: TStringMatrix read FFieldData write FFieldData;
    property BackGroundColor: TColor read FBackGroundCol write FBackGroundCol;
    property SelectionColor: TColor read FSelectCol write FSelectCol;
    property TextColor: TColor read FTextCol write FTextCol;
  end;

  { TSchedule }

  TSchedule = class(TDBForm)
  private
    type
    TCellMatrix = specialize TMatrix<TCell>;
    TCells = specialize TVector<TCell>;
  private
    FHTitleData: TData;
    FVTitleData: TData;
    FCells: TCellMatrix;
    procedure LoadCBoxData;
    procedure LoadCheckListBoxData;
    procedure LoadStringListData(Items: TStrings);
    procedure BuildMatrix(CellTouple: TCells);
    procedure SetCellsSize(CellW, CellH: integer);
    procedure SetTableSize;
    procedure FreePrevData;
    procedure LoadSchedule;
    function GetTitleData(FieldIndex: integer): TData;
    function GetCellDataTouple: TCells;
  public
    procedure Load(ANClass: TNClass; ATable: TDBTable; Params: TParams = nil); override;
  published
    FApplyFilterBtn: TButton;
    FCheckListBox: TCheckListBox;
    FDrawGrid: TDrawGrid;
    FFiltersGBox: TGroupBox;
    FHCBox: TComboBox;
    FHLabel: TLabel;
    FVCBox: TComboBox;
    FVisibleRecsGBox: TGroupBox;
    FVLabel: TLabel;
    FPairSplitter: TPairSplitter;
    FPairSplitterTop: TPairSplitterSide;
    FPairSplitterBot: TPairSplitterSide;
    FStatusBar: TStatusBar;
    procedure FDrawGridDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject); override;
    procedure FCheckListBoxClickCheck(Sender: TObject);
    procedure FApplyFilterBtnClick(Sender: TObject);
    procedure FDrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
  end;

implementation

{$R *.lfm}

constructor TCell.Create(Table: TDBTable);
begin
  FTable := TDBTable.Create;
  FTable.Assign(Table);
  FBackGroundCol := clWhite;
  FSelectCol := clRed;
  FTextCol := clBlack;
  FFieldData := TStringMatrix.Create;
  FFieldData.Resize(1, FTable.Count);
  FFieldData.Fill('');
end;

procedure TCell.Draw(Rect: TRect; Canvas: TCanvas; VisibleFields: TCheckListBox);
var
  i: integer;
  j: integer;
  TextH: integer;
  DataH: integer;
  SpaceC: integer = 0;
  InvFieldC: integer = 0;
begin
  TextH := Canvas.TextHeight('Нрб');
  Canvas.Brush.Color := FBackGroundCol;
  Canvas.FillRect(Rect);
  Canvas.Brush.Style := bsClear;
  DataH := FFieldData.Height - 1;
   for i := 0 to FFieldData.Width - 1 do begin
     for j := 0 to DataH do begin
       if VisibleFields.Checked[j mod FTable.Count] = False then begin
         InvFieldC += 1;
         continue;
       end;
       Canvas.TextRect(Rect,
         Rect.TopLeft.x + LEFT_MARGIN,
         Rect.TopLeft.y + (i * DataH + (j - InvFieldC) + i) * TextH + TextH * SpaceC,
         FTable.Fields[j mod FTable.Count].Name + ': ' + FFieldData[i, j]);
     end;
     SpaceC += 1;
   end;
end;

procedure TCell.AddData(Data: TStringMatrix);
var
  i: integer;
  j: integer;
  OldWidth: integer;
begin
  OldWidth := FFieldData.Width;
  FFieldData.AddColumns(Data.Width);
  for i := 0 to Data.Width - 1 do
    for j := 0 to Data.Height - 1 do
      FFieldData[i + OldWidth, j] := Data[i, j];
end;

procedure TSchedule.FormCreate(Sender: TObject);
begin
  inherited FormCreate(Sender);
  Constraints.MinHeight := 20;
  Constraints.MinWidth := 350;
  FCells := TCellMatrix.Create;
  FVTitleData := TData.Create;
  FHTitleData := TData.Create;
end;

procedure TSchedule.FApplyFilterBtnClick(Sender: TObject);
begin
  LoadSchedule;
  FDrawGrid.Invalidate;
end;

procedure TSchedule.Load(ANClass: TNClass; ATable: TDBTable; Params: TParams = nil);
begin
  inherited Load(ANClass, ATable, Params);
  Caption := APP_CAPTION + ' - ' + Table.Name;
  FStatusBar.SimpleText := Connection.CurrentConnection;
  LoadCBoxData;
  LoadSchedule;
end;

procedure TSchedule.FDrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
begin
  if (aCol = 0) and (aRow > 0) then begin
    FDrawGrid.Canvas.Pen.Color := clBlack;
    FDrawGrid.Canvas.TextRect(aRect,
      aRect.TopLeft.X + LEFT_MARGIN,
      aRect.TopLeft.Y,
      FVTitleData[aRow - 1]);
  end
  else if (aRow = 0) and (aCol > 0) then begin
    FDrawGrid.Canvas.Pen.Color := clBlack;
    FDrawGrid.Canvas.TextRect(aRect,
      aRect.TopLeft.x + LEFT_MARGIN,
      aRect.TopLeft.Y,
      FHTitleData[aCol - 1]);
  end
  else if (aCol > 0) and (aRow > 0) then
    if FCells[aCol - 1, aRow - 1] <> nil then
      FCells[aCol - 1, aRow - 1].Draw(aRect, FDrawGrid.Canvas, FCheckListBox);
end;

function TSchedule.GetTitleData(FieldIndex: integer): TData;
var
  Field: TDBField;
begin
  Result := TData.Create;
  Field := Table.Fields[FieldIndex];
  PerformQuery(Field.ParentTable.Query.Select(nil));
  while not FormQuery.EOF do begin
    Result.PushBack(FormQuery.Fields[Field.Index].AsString);
    FormQuery.Next;
  end;
end;

procedure TSchedule.FCheckListBoxClickCheck(Sender: TObject);
begin
  FDrawGrid.Invalidate;
end;

procedure TSchedule.FDrawGridDblClick(Sender: TObject);
begin

end;

procedure TSchedule.LoadCBoxData;
begin
  LoadStringListData(FHCBox.Items);
  LoadStringListData(FVCBox.Items);
  FHCBox.ItemIndex := 6;
  FVCBox.ItemIndex := 8;
end;

procedure TSchedule.LoadCheckListBoxData;
var
  i: integer;
begin
  LoadStringListData(FCheckListBox.Items);
  for i := 0 to FCheckListBox.Items.Count - 1 do
    FCheckListBox.Checked[i] := True;
  FCheckListBox.Checked[0] := False;
  FCheckListBox.Checked[FHCBox.ItemIndex] := False;
  FCheckListBox.Checked[FVCBox.ItemIndex] := False;
end;

procedure TSchedule.LoadStringListData(Items: TStrings);
var
  i: integer;
begin
  Items.Clear;
  for i := 0 to Table.Count - 1 do
    Items.Add(Table.Fields[i].Name);
end;

procedure TSchedule.BuildMatrix(CellTouple: TCells);
var
  i: integer;
  ColIndex: integer;
  RowIndex: integer;
begin
  FCells.Resize(FHTitleData.Size, FVTitleData.Size);
  FCells.Fill(nil);
  for i := 0 to CellTouple.Size - 1 do begin
    ColIndex := FHTitleData.FindInd(CellTouple[i].FieldData[0, FHCBox.ItemIndex]);
    RowIndex := FVTitleData.FindInd(CellTouple[i].FieldData[0, FVCBox.ItemIndex]);
    if FCells[ColIndex, RowIndex] = nil then
      FCells[ColIndex, RowIndex] := CellTouple[i]
    else
      FCells[ColIndex, RowIndex].AddData(CellTouple[i].FFieldData);
  end;
end;

procedure TSchedule.SetCellsSize(CellW, CellH: integer);
var
  i: integer;
begin
  FDrawGrid.ColWidths[0] := TITLE_W;
  FDrawGrid.RowHeights[0] := TITLE_H;
  for i := 1 to FDrawGrid.ColCount - 1 do
    FDrawGrid.ColWidths[i] := CellW;
  for i := 1 to FDrawGrid.RowCount - 1 do
    FDrawGrid.RowHeights[i] := CellH;
end;

procedure TSchedule.SetTableSize;
begin
  FDrawGrid.ColCount := FCells.Width + 1;
  FDrawGrid.RowCount := FCells.Height + 1;
end;

procedure TSchedule.FreePrevData;
var
  i: integer;
  j: integer;
begin
  FVTitleData.Free;
  FHTitleData.Free;
  for i := 0 to FCells.Width - 1 do
    for j := 0 to FCells.Height - 1 do
      FCells[i, j].Free;
end;

procedure TSchedule.LoadSchedule;
begin
  FreePrevData;
  LoadCheckListBoxData;
  FVTitleData := GetTitleData(FVCBox.ItemIndex);
  FHTitleData := GetTitleData(FHCBox.ItemIndex);
  BuildMatrix(GetCellDataTouple);
  SetTableSize;
  SetCellsSize(CELL_W, CELL_H);
end;

function TSchedule.GetCellDataTouple: TCells;
var
  i: integer;
  Cell: TCell;
begin
  PerformQuery(Table.Query.Select(nil));
  Result := TCells.Create;
  while not FormQuery.EOF do begin
    Cell := TCell.Create(Table);
    for i := 0 to Table.Count - 1 do
      Cell.FieldData[0, i] := FormQuery.Fields[i].AsString;
    FormQuery.Next;
    Result.PushBack(Cell);
  end;
end;

end.
