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
  BTN_SIZE = 14;
  LEFT_MARGIN = 5;

type

  TSchedule = class;
  TData = specialize TVector<string>;
  TStringMatrix = specialize TMatrix<string>;

  TCellBtn = class
  private
    FBtn: TRect;
    FOffset: TPoint;
    FColor: TColor;
    FRowID: integer;
    FSideSize: integer;
    FInited: boolean;
    function InBtn(Mouse: TPoint): boolean;
    function ShiftedBtn: TRect;
    procedure SetOffset(Point: TPoint);
  public
    constructor Create(SideSize: integer; RowID: integer = 0);
    procedure Draw(Canvas: TCanvas);
    procedure Click(const Schedule: TSchedule); virtual; abstract;
    property Offset: TPoint write SetOffset;
    property OffsetInited: boolean read FInited write FInited;
  end;

  TDelBtn = class(TCellBtn)
  public
    constructor Create(SideSize: integer; RowID: integer = 0);
    procedure Click(const Schedule: TSchedule); override;
  end;

  TEditBtn = class(TCellBtn)
  public
    constructor Create(SideSize: integer; RowID: integer = 0);
    procedure Click(const Schedule: TSchedule); override;
  end;

  TAddBtn = class(TCellBtn)
  public
    constructor Create(SideSize: integer; RowID: integer = 0);
    procedure Click(const Schedule: TSchedule); override;
  end;

  TShowBtn = class(TCellBtn)
  public
    constructor Create(SideSize: integer; RowID: integer = 0);
    procedure Click(const Schedule: TSchedule); override;
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
    FBtns: TCellBtns;
    FFieldData: TStringMatrix;
    FExpandable: boolean;
    FTriangle: array [0..2] of TPoint;
    procedure DrawTriangle(AnchorPoint: TPoint; Canvas: TCanvas);
    procedure DrawBtns(Canvas: TCanvas; Rect: TRect; YOffset, Index: integer);
    function Max(A, B: integer): integer;
  public
    constructor Create(Table: TDBTable);
    procedure Click(Mouse: TPoint; const Schedule: TSchedule);
    procedure Draw(Rect: TRect; Canvas: TCanvas; VisibleFields: TCheckListBox);
    procedure CopyData(Data: TStringMatrix);
  published
    property MaxTextH: integer read FMaxH;
    property MaxTextW: integer read FMaxW;
    property Row: integer read FRow write FRow;
    property Col: integer read FCol write FCol;
    property FieldData: TStringMatrix read FFieldData write FFieldData;
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
    FHTitleData: TData;
    FVTitleData: TData;
    FCells: TCellMatrix;
    FInTriangle: boolean;
    procedure LoadCBoxData;
    procedure LoadCheckListBoxData;
    procedure LoadStringListData(Items: TStrings);
    procedure BuildMatrix(CellTuple: TCells);
    procedure SetCellsCoords;
    procedure SetCellsSize(CellW, CellH: integer);
    procedure SetTableSize;
    procedure FreePrevData;
    procedure LoadSchedule;
    procedure DeleteEmptyLines;
    procedure NotificationRecieve(Sender: TObject);
    procedure CreateDir;
    procedure ExpandSelectedCell;
    procedure DeleteElement(ID: integer);
    procedure CreateEditCard(ID: integer);
    procedure CreateInsertCard;
    procedure CreateCard(RowIndex: integer; CardType: TDBFormType);
    procedure AddDirFilter(Dir: TDirectory; Field, COp, Param: string;
      AEnabled: boolean);
    function SelectedCellHash(Seed: integer): integer;
    function GetTitleData(FieldIndex: integer): TData;
    function GetCellDataTuple: TCells;
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
    FOpenDirItem: TMenuItem;
    FInsertItem: TMenuItem;
    procedure FDrawGridClick(Sender: TObject);
    procedure FInsertItemClick(Sender: TObject);
    procedure FOpenDirItemClick(Sender: TObject);
    procedure FDrawEmptyLinesChange(Sender: TObject);
    procedure FAddFilterBtnClick(Sender: TObject);
    procedure FDelAllFiltersBtnClick(Sender: TObject);
    procedure FDrawGridDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject); override;
    procedure FCheckListBoxClickCheck(Sender: TObject);
    procedure FApplyFilterBtnClick(Sender: TObject);
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

function TCellBtn.InBtn(Mouse: TPoint): boolean;
begin
  Exit(PointInRect(Mouse, ShiftedBtn));
end;

function TCellBtn.ShiftedBtn: TRect;
begin
  Result := FBtn;
  Result.TopLeft += FOffset;
  Result.BottomRight += FOffset;
end;

procedure TCellBtn.SetOffset(Point: TPoint);
begin
  FInited := True;
  FOffset := Point;
end;

constructor TCellBtn.Create(SideSize: integer; RowID: integer = 0);
begin
  FInited := False;
  FRowID := RowID;
  FSideSize := SideSize;
  FOffset := NULLP;
  FBtn.TopLeft := NULLP;
  FBtn.BottomRight.x := SideSize;
  FBtn.BottomRight.y := SideSize;
  FColor := clBlack;
end;

procedure TCellBtn.Draw(Canvas: TCanvas);
var
  PrevBColor: TColor;
begin
  PrevBColor := Canvas.Brush.Color;
  Canvas.Brush.Color := FColor;
  Canvas.Rectangle(ShiftedBtn);
  Canvas.Brush.Color := PrevBColor;
end;

constructor TDelBtn.Create(SideSize: integer; RowID: integer = 0);
begin
  inherited Create(SideSize, RowID);
  FColor := clRed;
end;

procedure TDelBtn.Click(const Schedule: TSchedule);
begin
  Schedule.DeleteElement(FRowID);
end;

constructor TEditBtn.Create(SideSize: integer; RowID: integer = 0);
begin
  inherited Create(SideSize, RowID);
  FColor := clBlue;
end;

procedure TEditBtn.Click(const Schedule: TSchedule);
begin
  Schedule.CreateEditCard(FRowID);
end;

constructor TAddBtn.Create(SideSize: integer; RowID: integer = 0);
begin
  inherited Create(SideSize, RowID);
  FColor := clGreen;
end;

procedure TAddBtn.Click(const Schedule: TSchedule);
begin
  Schedule.CreateInsertCard;
end;

constructor TShowBtn.Create(SideSize: integer; RowID: integer = 0);
begin
  inherited Create(SideSize, RowID);
  FColor := clYellow;
end;

procedure TShowBtn.Click(const Schedule: TSchedule);
begin
  Schedule.CreateDir;
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
  FFieldData := TStringMatrix.Create;
  FBtns := TCellBtns.Create;
  FBtns.Resize(2);
  FBtns[0] := TAddBtn.Create(BTN_SIZE);
  FBtns[1] := TShowBtn.Create(BTN_SIZE);
end;

procedure TCell.Click(Mouse: TPoint; const Schedule: TSchedule);
var
  i: integer;
begin
  for i := 0 to FBtns.Size - 1 do
    if FBtns[i].InBtn(Mouse) then begin
      FBtns[i].Click(Schedule);
      Exit;
    end;
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

      FMaxW := Max(FMaxW, LEFT_MARGIN + Canvas.TextWidth(Text));
      FMaxH := Max(FMaxH, Offset.Y - Rect.Top + 10);
      FExpandable := (FMaxW > (Rect.Right - Rect.Left)) or
        (FMaxH > (Rect.Bottom - Rect.Top));
    end;
    DrawBtns(Canvas, Rect, Offset.y, i);
    SpaceC += 1;
  end;
  DrawBtns(Canvas, Rect, Rect.Top, -1);
  if FExpandable then
    DrawTriangle(Rect.BottomRight, Canvas);
end;

procedure TCell.CopyData(Data: TStringMatrix);
var
  i: integer;
  j: integer;
  OldWidth: integer;
  RowIndex: integer;
begin
  OldWidth := FFieldData.Width;
  if OldWidth = 0 then
    FFieldData.Resize(Data.Width, Data.Height)
  else
    FFieldData.AddColumns(Data.Width);

  for i := 0 to Data.Width - 1 do begin
    RowIndex := StrToInt(Data[i, ID_FIELD_INDX]);
    FBtns.PushBack(TDelBtn.Create(BTN_SIZE, RowIndex));
    FBtns.PushBack(TEditBtn.Create(BTN_SIZE, RowIndex));
    for j := 0 to Data.Height - 1 do
      FFieldData[i + OldWidth, j] := Data[i, j];
  end;
end;

procedure TCell.DrawTriangle(AnchorPoint: TPoint; Canvas: TCanvas);
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

procedure TCell.DrawBtns(Canvas: TCanvas; Rect: TRect; YOffset, Index: integer);
var
  Offset: TPoint;
begin
  Index += 1;
  Offset := ToPoint(Rect.Right - BTN_SIZE, YOffset);
  FBtns[2 * Index].Offset := Offset;
  FBtns[2 * Index].Draw(Canvas);
  Offset.x -= BTN_SIZE;
  FBtns[2 * Index + 1].Offset := Offset;
  FBtns[2 * Index + 1].Draw(Canvas);
end;

function TCell.Max(A, B: integer): integer;
begin
  if A > B then
    Exit(A);
  Exit(B);
end;

procedure TSchedule.FormCreate(Sender: TObject);
begin
  inherited FormCreate(Sender);
  FDelEmptyLines := True;
  FDrawEmptyLines.Checked := not FDelEmptyLines;
  Constraints.MinHeight := 20;
  Constraints.MinWidth := 350;
  FInTriangle := False;
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
  ThisSubscriber.OnNotificationRecieve := @NotificationRecieve;
  Caption := APP_CAPTION + ' - ' + Table.Name;
  FStatusBar.SimpleText := Connection.CurrentConnection;
  LoadCBoxData;
  LoadSchedule;
end;

procedure TSchedule.FDrawGridDrawCell(Sender: TObject; aCol, aRow: integer;
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
      aRect.TopLeft.X + LEFT_MARGIN,
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
  ExpandSelectedCell;
end;

procedure TSchedule.FDrawGridSelectCell(Sender: TObject; aCol, aRow: integer;
  var CanSelect: boolean);
begin
  if (aCol > 0) and (aRow > 0) then
    FSelectedCell := FCells[aCol - 1, aRow - 1];
end;

procedure TSchedule.FDrawGridMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  FMouseCoords.X := X;
  FMouseCoords.Y := Y;
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

procedure TSchedule.FOpenDirItemClick(Sender: TObject);
begin
  CreateDir;
end;

procedure TSchedule.FDrawGridMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  Col: integer = 0;
  Row: integer = 0;
begin
  if FSelectedCell = nil then
    Exit;
  FDrawGrid.MouseToCell(X, Y, Col, Row);
  if Button = mbRight then begin
    if ((Col - 1 = FSelectedCell.Col) and (Row - 1 = FSelectedCell.Row)) then
      FCellMenu.PopUp(X + Left, Y + Top + 20);
  end;
  //else if Button = mbLeft then;
  //  FSelectedCell.Click(FMouseCoords, Self);
end;

procedure TSchedule.FInsertItemClick(Sender: TObject);
begin
  CreateInsertCard;
end;

procedure TSchedule.FDrawGridClick(Sender: TObject);
begin
  if FSelectedCell = nil then
    Exit;
  FSelectedCell.Click(FMouseCoords, Self);
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
  FCells.Resize(FHTitleData.Size, FVTitleData.Size);
  FCells.Fill(nil);
  for i := 0 to CellTuple.Size - 1 do begin
    ColIndex := FHTitleData.FindInd(CellTuple[i].FieldData[0, FHCBox.ItemIndex]);
    RowIndex := FVTitleData.FindInd(CellTuple[i].FieldData[0, FVCBox.ItemIndex]);
    if FCells[ColIndex, RowIndex] = nil then
      FCells[ColIndex, RowIndex] := CellTuple[i]
    else
      FCells[ColIndex, RowIndex].CopyData(CellTuple[i].FFieldData);
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
      FVTitleData.DeleteInd(i);
      i -= 1;
    end;
    i += 1;
  end;

  i := 0;
  while i < FCells.Width do begin
    if EmptyCol(i) then begin
      FCells.DeleteCol(i);
      FHTitleData.DeleteInd(i);
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
      FHTitleData[FSelectedCell.Col], True);
    AddDirFilter(Dir,
      FVCBox.Items[FVCBox.ItemIndex], ' = ',
      FVTitleData[FSelectedCell.Row], True);

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
      RowHeights[Row] := FSelectedCell.MaxTextH + 10;
      ColWidths[Col] := FSelectedCell.MaxTextW;
    end
    else begin
      RowHeights[Row] := CELL_H;
      ColWidths[Col] := CELL_W;
    end;
  end;
end;

procedure TSchedule.DeleteElement(ID: integer);
begin
  if FSelectedCell = nil then
    Exit;
  ExecQuery(Table.Query.Delete(ID));
  ThisSubscriber.CreateNotification(nil, ThisSubscriber.NClass);
end;

procedure TSchedule.CreateEditCard(ID: integer);
begin
  if FSelectedCell = nil then
    Exit;
  CreateCard(ID, TEditCard);
end;

procedure TSchedule.CreateInsertCard;
var
  RowIndex: integer;
begin
  if FSelectedCell = nil then
    Exit;
  //Since we have an insert card we don't need
  //row index, so we take it from the first field
  RowIndex := StrToInt(FSelectedCell.FieldData[0, ID_FIELD_INDX]);
  CreateCard(RowIndex, TInsertCard);
end;

procedure TSchedule.CreateCard(RowIndex: integer; CardType: TDBFormType);
var
  Params: TParams;
begin
  Params := TParams.Create;
  Params.CreateParam(ftInteger, 'Target', ptUnknown);
  Params.ParamByName('Target').AsInteger := RowIndex;
  CreateChildForm(ThisSubscriber.NClass, Table, CardType, Params,
    SelectedCellHash(CARD_SEED));
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
  Data: TStringMatrix;
begin
  ApplyFilters;
  Result := TCells.Create;
  while not FormQuery.EOF do begin
    Cell := TCell.Create(Table);
    Data := TStringMatrix.Create;
    Data.Resize(1, Table.Count);
    for i := 0 to Table.Count - 1 do
      Data[0, i] := FormQuery.Fields[i].AsString;
    Cell.CopyData(Data);
    Data.Free;
    FormQuery.Next;
    Result.PushBack(Cell);
  end;
end;

end.
