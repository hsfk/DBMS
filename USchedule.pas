unit USchedule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls, ComCtrls, CheckLst, PairSplitter, Menus, UDBForm, UVector, UMatrix,
  UDBObjects, UAbout, UDirectory, UFilterForm, DB, UCard, UPointUtils;

const
  DIR_SEED = 123456;
  CARD_SEED = 333;
  ID_FIELD_INDX = 0;
  CELL_W = 250;
  CELL_H = 120;
  TITLE_W = 100;
  TITLE_H = 30;
  LEFT_MARGIN = 5;
  BTN_SIZE = 14;

type

  TSchedule = class;
  TCell = class;

  TCellBtn = class
  private
    FCell: TCell;
    FBtn: TRect;
    FAnchor: TPoint;//Top Left
    FOffset: TPoint;
    FColor: TColor;
    FLineWidth: integer;
    FLineColor: TColor;
    FLineAnchor: TPoint;
    FElementID: integer;
    function InBtn(Mouse: TPoint): boolean;
    function ShiftedBtn: TRect;
  public
    constructor Create(Cell: TCell; ElementID: integer);
    procedure Draw(Canvas: TCanvas);
    function Moved: boolean;
  end;

  TCell = class
  private
    type
    TCellBtns = specialize TVector<TCellBtn>;
  private
    FRow: integer;
    FCol: integer;
    FMaxH: integer;
    FMaxW: integer;
    FTable: TDBTable;
    FBackGroundCol: TColor;
    FTextCol: TColor;
    FFieldData: TStringM;
    FExpandable: boolean;
    FCellBtns: TCellBtns;
    procedure DrawTriangle(AnchorPoint: TPoint; Canvas: TCanvas);
    procedure DrawBtn(Canvas: TCanvas; Rect: TRect; YOffset, Index: integer);
    function GetID(ElementID: integer): integer;
  public
    constructor Create(Table: TDBTable);
    procedure Copy(Cell: TCell; ElementID: integer);
    procedure Draw(Rect: TRect; Canvas: TCanvas; VisibleFields: TCheckListBox);
    procedure AddData(Data: TStringM);
    procedure AddCol(Data: TStringM; Col: integer);
    function GetButton(Mouse: TPoint): TCellBtn;
  published
    property Table: TDBTable read FTable;
    property MaxTextH: integer read FMaxH;
    property MaxTextW: integer read FMaxW;
    property Row: integer read FRow write FRow;
    property Col: integer read FCol write FCol;
    property FieldData: TStringM read FFieldData write FFieldData;
    property BackGroundColor: TColor read FBackGroundCol write FBackGroundCol;
    property TextColor: TColor read FTextCol write FTextCol;
  end;

  { TSchedule }

  TSchedule = class(TFilterForm)
  private
    type
    TCellMatrix = specialize TMatrix<TCell>;
    TCells = specialize TVector<TCell>;
  private
    FDelEmptyLines: boolean;
    FMouseCoords: TPoint;
    FSelectedCell: TCell;
    FBuffer: TCell;
    FSelectedCol: integer;
    FSelectedRow: integer;
    FHTitleData: TDBDataTuple;
    FVTitleData: TDBDataTuple;
    FCells: TCellMatrix;
    FDragBtn: TCellBtn;
    FIsDragging: boolean;
    procedure LoadCBoxData;
    procedure LoadCheckListBoxData;
    procedure LoadStringListData(Items: TStrings);
    procedure BuildMatrix(CellTuple: TCells);
    procedure SetCellsSize(CellW, CellH: integer);
    procedure SetCellsCoords;
    procedure SetTableSize;
    procedure FreePrevData;
    procedure LoadSchedule;
    procedure CreateDir;
    procedure DeleteEmptyLines;
    procedure NotificationRecieve(Sender: TObject);
    procedure ExpandSelectedCell;
    procedure Delete(ID: integer);
    procedure UpDate(Cell: TCell; RecIndex: integer; Data: string);
    procedure CreateEditCard(ID: integer);
    procedure CreateInsertCard;
    procedure PreSelectCardItems(Card: TCard);
    procedure Paste(Col, Row: integer);
    procedure Cut(Cell: TCell; ElementID: integer = -1); // -1 = Cuts all elements
    procedure SetCellMenuState(AEnabled: boolean);
    procedure DragBtnMouseDown;
    procedure DragBtnMouseMove;
    procedure DragBtnMouseUp(X, Y: integer);
    procedure AddDirFilter(Dir: TDirectory; Field, COp, Param: string;
      AEnabled: boolean);
    function CreateCard(RowIndex: integer; CardType: TDBFormType): TCard;
    function SelectedCellHash(Seed: integer): integer;
    function GetTitleData(FieldIndex: integer): TDBDataTuple;
    function GetCellDataTuple: TCells;
    function GetMaxTitleWidth: integer;
    function EmptyRow(Index: integer): boolean;
    function EmptyCol(Index: integer): boolean;
  public
    procedure Load(ANClass: TNClass; ATable: TDBTable; Params: TParams = nil); override;
  published
    FCheckListBox: TCheckListBox;
    FDrawGrid: TDrawGrid;
    FFiltersGBox: TGroupBox;
    FVisibleRecsGBox: TGroupBox;
    FPairSplitter: TPairSplitter;
    FPairSplitterTop: TPairSplitterSide;
    FPairSplitterBot: TPairSplitterSide;
    FStatusBar: TStatusBar;
    FAddFilterBtn: TButton;
    FApplyFilterBtn: TButton;
    FDelAllFiltersBtn: TButton;
    FDrawEmptyLines: TCheckBox;
    FFiltersSBox: TScrollBox;
    FHCBox: TComboBox;
    FVLabel: TLabel;
    FScheduleGBox: TGroupBox;
    FVCBox: TComboBox;
    FHLabel: TLabel;
    FCellMenu: TPopupMenu;
    FOpenDirMenu: TMenuItem;
    FInsertMenu: TMenuItem;
    FCutAllMenu: TMenuItem;
    FPasteMenu: TMenuItem;
    FCellBtnActions: TPopupMenu;
    FEditBtnMenu: TMenuItem;
    FCutBtnMenu: TMenuItem;
    FDelBtnMenu: TMenuItem;
    procedure FCutBtnMenuClick(Sender: TObject);
    procedure FDelBtnMenuClick(Sender: TObject);
    procedure FEditBtnMenuClick(Sender: TObject);
    procedure FCutAllMenuClick(Sender: TObject);
    procedure FPasteMenuClick(Sender: TObject);
    procedure FInsertMenuClick(Sender: TObject);
    procedure FOpenDirMenuClick(Sender: TObject);
    procedure FDrawEmptyLinesChange(Sender: TObject);
    procedure FAddFilterBtnClick(Sender: TObject);
    procedure FDelAllFiltersBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject); override;
    procedure FCheckListBoxClickCheck(Sender: TObject);
    procedure FApplyFilterBtnClick(Sender: TObject);
    procedure FDrawGridClick(Sender: TObject);
    procedure FDrawGridDblClick(Sender: TObject);
    procedure FDrawGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure FDrawGridSelectCell(Sender: TObject; aCol, aRow: integer;
      var CanSelect: boolean);
    procedure FDrawGridDrawCell(Sender: TObject; aCol, aRow: integer;
      aRect: TRect; aState: TGridDrawState);
    procedure FDrawGridMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
  end;

implementation

{$R *.lfm}

function FindDBDataInd(DataTuple: TDBDataTuple; Data: string): integer;
var
  i: integer;
begin
  for i := 0 to High(DataTuple) do
    if Data = DataTuple[i].Data then
      Exit(i);
  Exit(-1);
end;

procedure DeleteDBDataInd(DataTuple: TDBDataTuple; Index: integer);
begin
  DataTuple[Index] := DataTuple[High(DataTuple)];
  SetLength(DataTuple, Length(DataTuple) - 1);
end;

function Max(A, B: integer): integer;
begin
  if A > B then
    Exit(A);
  Exit(B);
end;

function TCellBtn.InBtn(Mouse: TPoint): boolean;
begin
  Exit(PointInRect(Mouse, ShiftedBtn));
end;

function TCellBtn.ShiftedBtn: TRect;
begin
  Result := FBtn;
  Result.TopLeft += FOffset + FAnchor;
  Result.BottomRight += FOffset + FAnchor;
end;

constructor TCellBtn.Create(Cell: TCell; ElementID: integer);
begin
  FCell := Cell;
  FColor := clGray;
  FLineColor := clBlack;
  FLineWidth := 2;
  FElementID := ElementID;
  FOffset := NULLP;
  FAnchor := NULLP;
  FLineAnchor := NULLP;
  FBtn.TopLeft := NULLP;
  FBtn.BottomRight.x := FBtn.TopLeft.x + BTN_SIZE;
  FBtn.BottomRight.y := FBtn.TopLeft.y + BTN_SIZE;
end;

procedure TCellBtn.Draw(Canvas: TCanvas);
var
  PrevBColor: TColor;
  PrevPColor: TColor;
  PrevPWidth: integer;
  Btn: TRect;
begin
  PrevBColor := Canvas.Brush.Color;
  PrevPColor := Canvas.Pen.Color;
  PrevPWidth := Canvas.Pen.Width;
  Canvas.Brush.Color := FColor;
  Btn := ShiftedBtn;
  Canvas.Pen.Color := FLineColor;
  Canvas.Pen.Width := FLineWidth;
  if FLineAnchor <> NULLP then
    Canvas.Line(FLineAnchor, Btn.TopLeft + ToPoint(4, 4));
  Canvas.Pen.Width := PrevPWidth;
  Canvas.Pen.Color := PrevPColor;
  Canvas.Rectangle(Btn);
  Canvas.Brush.Color := PrevBColor;
end;

function TCellBtn.Moved: boolean;
begin
  Exit(not (ShiftedBtn.TopLeft = FAnchor));
end;

constructor TCell.Create(Table: TDBTable);
begin
  FRow := -1;
  FCol := -1;
  FMaxH := 0;
  FMaxW := 0;
  FExpandable := False;
  FTable := Table;
  FBackGroundCol := clWhite;
  FTextCol := clBlack;
  FFieldData := TStringM.Create;
  FCellBtns := TCellBtns.Create;
end;

procedure TCell.Copy(Cell: TCell; ElementID: integer);
begin
  FRow := Cell.FRow;
  FCol := Cell.FCol;
  FMaxH := Cell.FMaxH;
  FMaxW := Cell.FMaxW;
  FTable := Cell.FTable;
  FTextCol := Cell.FTextCol;
  FBackGroundCol := Cell.FBackGroundCol;

  if ElementID = -1 then
    AddData(Cell.FFieldData)
  else
    AddCol(Cell.FFieldData, ElementID);
end;

procedure TCell.Draw(Rect: TRect; Canvas: TCanvas; VisibleFields: TCheckListBox);
var
  i: integer;
  j: integer;
  TextH: integer;
  FieldsAmount: integer;
  SpaceC: integer = 0;
  InvFieldC: integer = 0;
  Offset: TPoint;
  RowIndex: integer = 0;
  Text: string;
  PrevI: integer = -1;
begin
  TextH := Canvas.TextHeight('Нрб');
  FieldsAmount := FFieldData.Height - 1;
  FMaxH := 0;
  FMaxW := 0;
  Canvas.Pen.Color := clBlack;
  Canvas.Brush.Color := FBackGroundCol;
  Canvas.FillRect(Rect);
  Canvas.Brush.Style := bsClear;

  for i := 0 to FFieldData.Width - 1 do begin
    for j := 0 to FieldsAmount do begin
      if VisibleFields.Checked[j mod FTable.Count] = False then begin
        InvFieldC += 1;
        continue;
      end;
      RowIndex := i * FieldsAmount + i + j + SpaceC - InvFieldC;
      Offset.X := Rect.TopLeft.x + LEFT_MARGIN;
      Offset.Y := Rect.Top + TextH * RowIndex;
      Text := FTable.Fields[j mod FTable.Count].Name + ': ' + FFieldData[i, j];
      Canvas.TextRect(Rect, Offset.X, Offset.Y, Text);
      if i <> PrevI then
        DrawBtn(Canvas, Rect, Offset.y, i);

      FMaxW := Max(FMaxW, LEFT_MARGIN + Canvas.TextWidth(Text));
      FMaxH := Max(FMaxH, Offset.Y - Rect.Top + 10);
      FExpandable := (FMaxW > (Rect.Right - Rect.Left)) or
        (FMaxH > (Rect.Bottom - Rect.Top));
      PrevI := i;
    end;
    SpaceC += 1;
  end;
  if FExpandable then
    DrawTriangle(Rect.BottomRight, Canvas);
end;

procedure TCell.AddData(Data: TStringM);
var
  i: integer;
begin
  for i := 0 to Data.Width - 1 do
    AddCol(Data, i);
end;

procedure TCell.AddCol(Data: TStringM; Col: integer);
var
  i: integer;
begin
  if FFieldData.Width = 0 then
    FFieldData.Resize(1, Data.Height)
  else
    FFieldData.AddColumns(1);
  FCellBtns.PushBack(TCellBtn.Create(Self, FCellBtns.Size));
  for i := 0 to Data.Height - 1 do
    FFieldData[FFieldData.Width - 1, i] := Data[Col, i];
end;

function TCell.GetButton(Mouse: TPoint): TCellBtn;
var
  i: integer;
begin
  for i := 0 to FCellBtns.Size - 1 do
    if FCellBtns[i].InBtn(Mouse) then
      Exit(FCellBtns[i]);
  Exit(nil);
end;

procedure TCell.DrawTriangle(AnchorPoint: TPoint; Canvas: TCanvas);
var
  FTriangle: array [0..2] of TPoint;
begin
  FTriangle[0] := AnchorPoint;
  FTriangle[1] := AnchorPoint;
  FTriangle[2] := AnchorPoint;
  FTriangle[1].X -= 14;
  FTriangle[2].Y -= 14;
  Canvas.Pen.Color := clBlack;
  Canvas.Brush.Color := clDkGray;
  Canvas.Polygon(FTriangle);
end;

procedure TCell.DrawBtn(Canvas: TCanvas; Rect: TRect; YOffset, Index: integer);
begin
  FCellBtns[Index].FAnchor := ToPoint(Rect.Right - BTN_SIZE, YOffset);
  FCellBtns[Index].Draw(Canvas);
end;

function TCell.GetID(ElementID: integer): integer;
begin
  Exit(StrToInt(FFieldData[ElementID, ID_FIELD_INDX]));
end;

procedure TSchedule.FormCreate(Sender: TObject);
begin
  inherited FormCreate(Sender);
  FSelectedCell := nil;
  FBuffer := nil;
  FDelEmptyLines := True;
  FDrawEmptyLines.Checked := not FDelEmptyLines;
  Constraints.MinHeight := 20;
  Constraints.MinWidth := 350;
  FCells := TCellMatrix.Create;
end;

procedure TSchedule.FApplyFilterBtnClick(Sender: TObject);
begin
  LoadSchedule;
  FDrawGrid.Invalidate;
end;

procedure TSchedule.Load(ANClass: TNClass; ATable: TDBTable; Params: TParams = nil);
begin
  inherited Load(ANClass, ATable, Params);
  ThisSubscriber.OnNotificationRecieve := @NotificationRecieve;
  Caption := APP_CAPTION + ' - ' + Table.Name;
  FStatusBar.SimpleText := Connection.CurrentConnection;
  LoadCBoxData;
  LoadSchedule;
end;

procedure TSchedule.FDrawGridDrawCell(Sender: TObject; aCol, aRow: integer;
  aRect: TRect; aState: TGridDrawState);
begin
  with FDrawGrid.Canvas do
    if (aCol = 0) and (aRow > 0) then begin
      Pen.Color := clBlack;
      TextRect(aRect,
        aRect.TopLeft.X + LEFT_MARGIN,
        aRect.TopLeft.Y,
        FVTitleData[aRow - 1].Data);
    end
    else if (aRow = 0) and (aCol > 0) then begin
      Pen.Color := clBlack;
      TextRect(aRect,
        aRect.TopLeft.X + LEFT_MARGIN,
        aRect.TopLeft.Y,
        FHTitleData[aCol - 1].Data);
    end
    else if (aCol > 0) and (aRow > 0) then
      if FCells[aCol - 1, aRow - 1] <> nil then
        FCells[aCol - 1, aRow - 1].Draw(aRect, FDrawGrid.Canvas, FCheckListBox);
end;

function TSchedule.GetTitleData(FieldIndex: integer): TDBDataTuple;
var
  Field: TDBField;
  Item: TDBData;
begin
  SetLength(Result, 0);
  Field := Table.Fields[FieldIndex];
  PerformQuery(Field.ParentTable.Query.Select(nil));
  while not FormQuery.EOF do begin
    Item.Data := FormQuery.Fields[Field.Index].AsString;
    Item.ID := FormQuery.Fields[ID_FIELD_INDX].AsInteger;
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := Item;
    FormQuery.Next;
  end;
end;

procedure TSchedule.FCheckListBoxClickCheck(Sender: TObject);
begin
  FDrawGrid.Invalidate;
end;

procedure TSchedule.FDrawGridDblClick(Sender: TObject);
begin
  ExpandSelectedCell;
end;

procedure TSchedule.FDrawGridSelectCell(Sender: TObject; aCol, aRow: integer;
  var CanSelect: boolean);
begin
  if (aCol > 0) and (aRow > 0) then begin
    FSelectedCol := aCol - 1;
    FSelectedRow := aRow - 1;
    FSelectedCell := FCells[aCol - 1, aRow - 1];
    if FSelectedCell = nil then
      SetCellMenuState(False)
    else
      SetCellMenuState(True);
  end;
end;

procedure TSchedule.FAddFilterBtnClick(Sender: TObject);
begin
  AddFilterPanel(TFilterPanel.Create(Table, FFiltersSBox, 5 + FilterCount * 25, 5));
end;

procedure TSchedule.FDelAllFiltersBtnClick(Sender: TObject);
begin
  DeleteFilters;
end;

procedure TSchedule.FDrawEmptyLinesChange(Sender: TObject);
begin
  FDelEmptyLines := not FDelEmptyLines;
  FDrawEmptyLines.Checked := not FDelEmptyLines;
  LoadSchedule;
end;

procedure TSchedule.FOpenDirMenuClick(Sender: TObject);
begin
  CreateDir;
end;

procedure TSchedule.DragBtnMouseDown;
var
  CellP: TPoint;
begin
  FIsDragging := False;
  CellP := FDrawGrid.MouseToCell(FMouseCoords);
  if (CellP.x > 0) and (CellP.y > 0) then
    FSelectedCell := FCells[CellP.x - 1, CellP.y - 1];

  if FSelectedCell = nil then
    Exit;

  FDragBtn := nil;
  FDragBtn := FSelectedCell.GetButton(FMouseCoords);
  if FDragBtn <> nil then
    FIsDragging := True;
end;

procedure TSchedule.DragBtnMouseMove;
var
  CellRect: TRect;
begin
  if FIsDragging = False then
    Exit;
  if FDragBtn = nil then
    Exit;

  Screen.Cursor := crDefault;
  with FDragBtn do begin
    FOffset := FMouseCoords - FAnchor - ToPoint(BTN_SIZE div 2, BTN_SIZE div 2);
    FDrawGrid.Repaint;
    CellRect := FDrawGrid.CellRect(FCell.Col + 1, FCell.Row + 1);
    FLineAnchor := ToPoint(CellRect.Right, CellRect.Top + Abs(FAnchor.y - CellRect.Top));
    Draw(FDrawGrid.Canvas);
  end;
end;

procedure TSchedule.DragBtnMouseUp(X, Y: integer);
var
  Moved: boolean;
begin
  if (FDragBtn = nil) or not FIsDragging then
    Exit;

  FIsDragging := False;
  Moved := FDragBtn.Moved;
  FDragBtn.FOffset := NULLP;
  FDragBtn.FLineAnchor := NULLP;
  if not Moved then
    FCellBtnActions.PopUp(X + Left, Y + Top);

  if FDragBtn.FCell = FSelectedCell then
    Exit;
  Cut(FDragBtn.FCell, FDragBtn.FElementID);
  FDragBtn := nil;
  Paste(FSelectedCol, FSelectedRow);
  LoadSchedule;
  FDrawGrid.Invalidate;
end;

procedure TSchedule.FDrawGridMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  Col: integer = 0;
  Row: integer = 0;
begin
  FDrawGrid.MouseToCell(X, Y, Col, Row);
  if Button = mbRight then begin
    if (Col - 1 = FSelectedCol) and (Row - 1 = FSelectedRow) then
      FCellMenu.PopUp(X + Left, Y + Top + 20);
  end
  else if Button = mbLeft then
    DragBtnMouseDown;
end;

procedure TSchedule.FDrawGridMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  FMouseCoords := ToPoint(X, Y);
  DragBtnMouseMove;
end;

procedure TSchedule.FInsertMenuClick(Sender: TObject);
begin
  CreateInsertCard;
end;

procedure TSchedule.FDrawGridClick(Sender: TObject);
begin
  DragBtnMouseUp(FMouseCoords.x, FMouseCoords.Y);
end;

procedure TSchedule.FCutAllMenuClick(Sender: TObject);
begin
  Cut(FSelectedCell, -1);
  FDrawGrid.Invalidate;
  SetCellMenuState(False);
end;

procedure TSchedule.FPasteMenuClick(Sender: TObject);
begin
  Paste(FSelectedCol, FSelectedRow);
  LoadSchedule;
  FDrawGrid.Invalidate;
  SetCellMenuState(True);
  FSelectedCell := FCells[FSelectedCol, FSelectedRow];
end;

procedure TSchedule.FCutBtnMenuClick(Sender: TObject);
begin
  if FDragBtn = nil then
    Exit;
  Cut(FSelectedCell, FDragBtn.FElementID);
  if FSelectedCell = nil then
    SetCellMenuState(False);
  FDrawGrid.Invalidate;
end;

procedure TSchedule.FDelBtnMenuClick(Sender: TObject);
begin
  if FDragBtn = nil then
    Exit;
  Delete(FDragBtn.FCell.GetID(FDragBtn.FElementID));
end;

procedure TSchedule.FEditBtnMenuClick(Sender: TObject);
begin
  if FDragBtn = nil then
    Exit;
  CreateEditCard(FDragBtn.FCell.GetID(FDragBtn.FElementID));
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

procedure TSchedule.BuildMatrix(CellTuple: TCells);
var
  i: integer;
  ColIndex: integer;
  RowIndex: integer;
begin
  FCells.Resize(Length(FHTitleData), Length(FVTitleData));
  FCells.Fill(nil);
  for i := 0 to CellTuple.Size - 1 do begin
    ColIndex := FindDBDataInd(FHTitleData, CellTuple[i].FieldData[0, FHCBox.ItemIndex]);
    RowIndex := FindDBDataInd(FVTitleData, CellTuple[i].FieldData[0, FVCBox.ItemIndex]);
    if FCells[ColIndex, RowIndex] = nil then
      FCells[ColIndex, RowIndex] := CellTuple[i]
    else
      FCells[ColIndex, RowIndex].AddData(CellTuple[i].FFieldData);
  end;
end;

procedure TSchedule.SetCellsCoords;
var
  i: integer;
  j: integer;
begin
  for i := 0 to FCells.Width - 1 do
    for j := 0 to FCells.Height - 1 do
      if FCells[i, j] <> nil then begin
        FCells[i, j].Col := i;
        FCells[i, j].Row := j;
      end;
end;

procedure TSchedule.SetCellsSize(CellW, CellH: integer);
var
  i: integer;
begin
  FDrawGrid.ColWidths[0] := GetMaxTitleWidth;
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
  SetLength(FVTitleData, 0);
  SetLength(FHTitleData, 0);
  FBuffer.Free;
  FBuffer := nil;
  for i := 0 to FCells.Width - 1 do
    for j := 0 to FCells.Height - 1 do
      FCells[i, j].Free;
end;

procedure TSchedule.LoadSchedule;
begin
  FreePrevData;
  LoadCheckListBoxData;
  FHTitleData := GetTitleData(FHCBox.ItemIndex);
  FVTitleData := GetTitleData(FVCBox.ItemIndex);
  BuildMatrix(GetCellDataTuple);
  if FDelEmptyLines then
    DeleteEmptyLines;
  SetCellsCoords;
  SetTableSize;
  SetCellsSize(CELL_W, CELL_H);
end;

function TSchedule.EmptyRow(Index: integer): boolean;
var
  i: integer;
begin
  for i := 0 to FCells.Width - 1 do
    if FCells[i, Index] <> nil then
      Exit(False);
  Exit(True);
end;

function TSchedule.EmptyCol(Index: integer): boolean;
var
  i: integer;
begin
  for i := 0 to FCells.Height - 1 do
    if FCells[Index, i] <> nil then
      Exit(False);
  Exit(True);
end;

procedure TSchedule.DeleteEmptyLines;
var
  i: integer = 0;
begin
  while i < FCells.Height do begin
    if EmptyRow(i) then begin
      FCells.DeleteRow(i);
      DeleteDBDataInd(FVTitleData, i);
      i -= 1;
    end;
    i += 1;
  end;

  i := 0;
  while i < FCells.Width do begin
    if EmptyCol(i) then begin
      FCells.DeleteCol(i);
      DeleteDBDataInd(FHTitleData, i);
      i -= 1;
    end;
    i += 1;
  end;
end;

procedure TSchedule.NotificationRecieve(Sender: TObject);
begin
  LoadSchedule;
  FDrawGrid.Invalidate;
end;

procedure TSchedule.CreateDir;
var
  Dir: TDirectory;
  i: integer;
begin
  if FSelectedCell = nil then
    Exit;
  Dir := TDirectory(CreateChildForm(ThisSubscriber.NClass, Table,
    TDirectory, nil, SelectedCellHash(DIR_SEED)));
  if Dir.FilterCount = 0 then begin
    AddDirFilter(Dir,
      FHCBox.Items[FHCBox.ItemIndex], ' = ',
      FHTitleData[FSelectedCol].Data, True);
    AddDirFilter(Dir,
      FVCBox.Items[FVCBox.ItemIndex], ' = ',
      FVTitleData[FSelectedRow].Data, True);

    for i := 0 to FilterCount - 1 do
      if FFilterPanels[i].Correct then
        AddDirFilter(Dir, FFilterPanels[i].Filter.Name,
          FFilterPanels[i].Filter.ConditionalOperator,
          FFilterPanels[i].Filter.Param, True);

    Dir.ApplyFilters;
  end;
end;

procedure TSchedule.ExpandSelectedCell;
var
  Row: integer = 0;
  Col: integer = 0;
begin
  if FSelectedCell = nil then
    Exit;
  with FDrawGrid do begin
    Row := FSelectedCell.Row + 1;
    Col := FSelectedCell.Col + 1;
    if FSelectedCell.FExpandable then begin
      if FSelectedCell.MaxTextH + 15 > RowHeights[Row] then
        RowHeights[Row] := FSelectedCell.MaxTextH + 15;
      if FSelectedCell.MaxTextW > ColWidths[Col] then
        ColWidths[Col] := FSelectedCell.MaxTextW;
    end
    else begin
      RowHeights[Row] := CELL_H;
      ColWidths[Col] := CELL_W;
    end;
  end;
end;

procedure TSchedule.SetCellMenuState(AEnabled: boolean);
begin
  FOpenDirMenu.Enabled := AEnabled;
  FCutAllMenu.Enabled := AEnabled;
end;

procedure TSchedule.Delete(ID: integer);
begin
  if FSelectedCell = nil then
    Exit;
  ExecQuery(Table.Query.Delete(ID));
  ThisSubscriber.CreateNotification(nil, ThisSubscriber.NClass);
end;

procedure TSchedule.UpDate(Cell: TCell; RecIndex: integer; Data: string);
var
  i: integer;
  NewData: TParam;
begin
  for i := 0 to Cell.FFieldData.Width - 1 do begin
    NewData := TParam.Create(nil, ptInput);
    NewData.Value := Data;
    ExecQuery(Table.Fields[RecIndex].Query.Update(Cell.GetID(i), NewData));
  end;
end;

procedure TSchedule.CreateEditCard(ID: integer);
begin
  if FSelectedCell = nil then
    Exit;
  CreateCard(ID, TEditCard);
end;

procedure TSchedule.Paste(Col, Row: integer);
var
  i: integer;
begin
  if FBuffer = nil then
    Exit;

  for i := 0 to FBuffer.FieldData.Width - 1 do begin
    if FBuffer.Col <> Col then begin
      FBuffer.FieldData[i, FHCBox.ItemIndex] := IntToStr(FHTitleData[Col].ID);
    end;
    if FBuffer.Row <> Row then begin
      FBuffer.FieldData[i, FVCBox.ItemIndex] := IntToStr(FVTitleData[Row].ID);
    end;
    UpDate(FBuffer, FHCBox.ItemIndex, IntToStr(FHTitleData[Col].ID));//Insert(FBuffer);
    UpDate(FBuffer, FVCBox.ItemIndex, IntToStr(FVTitleData[Row].ID));
  end;

  ThisSubscriber.CreateNotification(nil, ThisSubscriber.NClass);
  FBuffer.Free;
  FBuffer := nil;
end;

procedure TSchedule.Cut(Cell: TCell; ElementID: integer = -1);
begin
  if Cell = nil then
    Exit;
  FBuffer.Free;
  FBuffer := TCell.Create(Table);
  FBuffer.Copy(Cell, ElementID);
  if ElementID = -1 then begin
    FCells[Cell.Col, Cell.Row] := nil;
    Cell.Free;
  end
  else begin
    Cell.FFieldData.DeleteCol(ElementID);
    if Cell.FFieldData.Width = 0 then begin
      if FSelectedCell = FCells[Cell.Col, Cell.Row] then
        FSelectedCell := nil;
      FCells[Cell.Col, Cell.Row] := nil;
      Cell.Free;
    end;
  end;
end;

procedure TSchedule.PreSelectCardItems(Card: TCard);
begin
  if FVCBox.ItemIndex <> ID_FIELD_INDX then
    Card.Select(FVCBox.ItemIndex - 1, FVTitleData[FSelectedRow].Data);
  if FHCBox.ItemIndex <> ID_FIELD_INDX then
    Card.Select(FHCBox.ItemIndex - 1, FHTitleData[FSelectedCol].Data);
end;

procedure TSchedule.CreateInsertCard;
begin
  //Since we have an insert card row index isn't needed
  PreSelectCardItems(CreateCard(0, TInsertCard));
end;

function TSchedule.CreateCard(RowIndex: integer; CardType: TDBFormType): TCard;
var
  Params: TParams;
begin
  Params := TParams.Create;
  Params.CreateParam(ftInteger, 'Target', ptUnknown);
  Params.ParamByName('Target').AsInteger := RowIndex;
  Exit(TCard(CreateChildForm(ThisSubscriber.NClass, Table, CardType,
    Params, SelectedCellHash(CARD_SEED))));
end;

procedure TSchedule.AddDirFilter(Dir: TDirectory; Field, COp, Param: string;
  AEnabled: boolean);
var
  FilterPanel: TFilterPanel;
begin
  FilterPanel := TFilterPanel.Create(Table);
  Dir.AddFilterPanel(FilterPanel);
  FilterPanel.SetFilterData(Field, COp, Param);
  FilterPanel.Enabled := AEnabled;
end;

function TSchedule.SelectedCellHash(Seed: integer): integer;
begin
  Result := 1001 + (Seed + 3) * (Seed - 7);
  Result += Result shl (Seed mod 16) xor Result;
  Result += word(@(FSelectedCell.FieldData));
  Result += Result and Result xor Result shl 10;
end;

function TSchedule.GetCellDataTuple: TCells;
var
  i: integer;
  Cell: TCell;
  Data: TStringM;
begin
  ApplyFilters;
  Result := TCells.Create;
  while not FormQuery.EOF do begin
    Cell := TCell.Create(Table);
    Data := TStringM.Create;
    Data.Resize(1, Table.Count);
    for i := 0 to Table.Count - 1 do
      Data[0, i] := FormQuery.Fields[i].AsString;
    Cell.AddData(Data);
    Data.Free;
    FormQuery.Next;
    Result.PushBack(Cell);
  end;
end;

function TSchedule.GetMaxTitleWidth: integer;
var
  i: integer;
begin
  Result := TITLE_W;
  for i := 0 to High(FVTitleData) do
    Result := Max(Result, FDrawGrid.Canvas.TextWidth(FVTitleData[i].Data));
end;

end.
