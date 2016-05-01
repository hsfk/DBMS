unit USchedule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls, ComCtrls, CheckLst, PairSplitter, Menus, UDBForm, UVector, UMatrix,
  UDBObjects, UAbout, UDirectory, DB, UCard, UPointUtils, UFilters,
  UCanvasUtils, UIcons, URectUtils;

const
  DIR_SEED = 123456;
  CARD_SEED = 333;
  ID_FIELD_INDX = 0;
  CELL_W = 250;
  CELL_H = 120;
  TITLE_W = 100;
  TITLE_H = 30;
  LEFT_MARGIN = 5;
  BTN_SIZE = 20;
  BTN_TAB_OFFSET = BTN_SIZE + 4;

type

  TSchedule = class;
  TCell = class;

  TCellBtn = class
  private
    FSchedule: TSchedule;
    FCell: TCell;
    FBtn: TRect;
    FIcon: TIcon;
    FAnchor: TPoint;//Top Left
    FElementID: integer;
    FColor: TColor; // if icon not exists then rect with FColor will appear
    function ShiftedBtn: TRect; virtual;
  public
    constructor Create(Cell: TCell; ElementID: integer;
      const Schedule: TSchedule); virtual;
    procedure Draw(Canvas: TCanvas); virtual;
    procedure Click; virtual;
    procedure MouseDown; virtual;
    procedure MouseUp; virtual;
    procedure MouseMove; virtual;
    function UnderMouse(Mouse: TPoint): boolean;
  end;

  TEditBtn = class(TCellBtn)
  public
    constructor Create(Cell: TCell; ElementID: integer; const Schedule: TSchedule);
      override;
    procedure MouseUp; override;
  end;

  TDelBtn = class(TCellBtn)
  public
    constructor Create(Cell: TCell; ElementID: integer; const Schedule: TSchedule);
      override;
    procedure MouseUp; override;
  end;

  TDragBtn = class(TCellBtn)
  private
    FDragging: boolean;
    FOffset: TPoint;
    FLineWidth: integer;
    FLineColor: TColor;
    FLineAnchor: TPoint;
    function ShiftedBtn: TRect; override;
  public
    constructor Create(Cell: TCell; ElementID: integer; const Schedule: TSchedule);
      override;
    procedure Draw(Canvas: TCanvas); override;
    procedure MouseDown; override;
    procedure MouseUp; override;
    procedure MouseMove; override;
  end;

  TCell = class
  private
    type
    TCellBtns = specialize TObjVector<TCellBtn>;
  private
    FRow: integer;
    FCol: integer;
    FMaxH: integer;
    FMaxW: integer;
    FTable: TDBTable;
    FRect: TRect;
    FSchedule: TSchedule;
    FFieldData: TStringM;
    FExpandable: boolean;
    FSelected: boolean;
    FCellBtns: TCellBtns;
    procedure DrawBtns(Canvas: TCanvas; ElementID, X, Y, YConstraint: integer);
    function GetID(ElementID: integer): integer;
    function TextOut(i, j: integer): string;
  public
    constructor Create(Table: TDBTable; Schedule: TSchedule);
    destructor Destroy; override;
    procedure Copy(Cell: TCell; ElementID: integer);
    procedure Draw(Rect: TRect; Canvas: TCanvas; VisFields: TCheckListBox);
    procedure AddData(Data: TStringM);
    procedure AddData(Data: TStringM; Col: integer);
    function MouseToBtn(Mouse: TPoint): TCellBtn;
    function MaxTextHeight(VisFields: TCheckListBox; TextH: integer): integer;
    function MaxTextWidth(Canvas: TCanvas): integer;
  published
    property Table: TDBTable read FTable;
    property MaxTextH: integer read FMaxH;
    property MaxTextW: integer read FMaxW;
    property Row: integer read FRow write FRow;
    property Col: integer read FCol write FCol;
    property FieldData: TStringM read FFieldData write FFieldData;
  end;

  { TSchedule }

  TSchedule = class(TDBForm)
  private
    type
    TCellMatrix = specialize TObjMatrix<TCell>;
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
    FFilters: TFilterPanels;
    FCells: TCellMatrix;
    FCurBtn: TCellBtn;
    procedure LoadCBoxData;
    procedure LoadCheckListBoxData;
    procedure LoadStringListData(Items: TStrings);
    procedure BuildMatrix(CellTuple: TCells);
    procedure FreePrevData;
    procedure MakeSchedule;
    procedure DeleteEmptyLines;
    procedure NotificationRecieve(Sender: TObject);
    procedure ExpandSelectedCell;
    procedure Delete(ID: integer);
    procedure UpDate(Cell: TCell; RecIndex: integer; Data: string);
    procedure CreateDir;
    procedure CreateEditCard(ID: integer);
    procedure CreateInsertCard;
    procedure PreSelectCardItems(Card: TCard);
    procedure Paste(Col, Row: integer);
    procedure Cut(Cell: TCell; ElementID: integer = -1); // -1 = Cuts all elements
    procedure SetCellMenuState(AEnabled: boolean);
    procedure SetCellsCoords;
    procedure SetTableSize;
    procedure SetCellsSize(CellW, CellH: integer);
    procedure SelectCell(Mouse: TPoint);
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
    procedure FormCreate(Sender: TObject); override;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction); override;
    procedure FCutAllMenuClick(Sender: TObject);
    procedure FPasteMenuClick(Sender: TObject);
    procedure FInsertMenuClick(Sender: TObject);
    procedure FOpenDirMenuClick(Sender: TObject);
    procedure FDrawEmptyLinesChange(Sender: TObject);
    procedure FAddFilterBtnClick(Sender: TObject);
    procedure FDelAllFiltersBtnClick(Sender: TObject);
    procedure FCheckListBoxClickCheck(Sender: TObject);
    procedure FApplyFilterBtnClick(Sender: TObject);
    procedure FDrawGridDblClick(Sender: TObject);
    procedure FDrawGridDrawCell(Sender: TObject; aCol, aRow: integer;
      aRect: TRect; aState: TGridDrawState);
    procedure FDrawGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure FDrawGridMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure FDrawGridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
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

constructor TEditBtn.Create(Cell: TCell; ElementID: integer; const Schedule: TSchedule);
begin
  inherited Create(Cell, ElementID, Schedule);
  FIcon := edit_20x20;
  FColor := clSkyBlue;
end;

procedure TEditBtn.MouseUp;
begin
  FSchedule.CreateEditCard(FCell.GetID(FElementID));
end;

constructor TDelBtn.Create(Cell: TCell; ElementID: integer; const Schedule: TSchedule);
begin
  inherited Create(Cell, ElementID, Schedule);
  FIcon := delete_20x20;
  FColor := clRed;
end;

procedure TDelBtn.MouseUp;
begin
  FSchedule.Delete(FCell.GetID(FElementID));
end;

function TDragBtn.ShiftedBtn: TRect;
begin
  Exit(Shift(FBtn, FOffset + FAnchor));
end;

constructor TDragBtn.Create(Cell: TCell; ElementID: integer; const Schedule: TSchedule);
begin
  inherited Create(Cell, ElementID, Schedule);
  FDragging := False;
  FIcon := drag_20x20;
  FColor := clDkGray;
  FLineColor := clBlack;
  FLineWidth := 1;
  FOffset := NULLP;
  FLineAnchor := NULLP;
end;

procedure TDragBtn.Draw(Canvas: TCanvas);
var
  Btn: TRect;
begin
  Btn := ShiftedBtn;
  SaveState(Canvas);
  LoadState(Canvas, ToState(FLineColor, clWhite, FLineWidth, psSolid, bsSolid));
  if FLineAnchor <> NULLP then
    Canvas.Line(FLineAnchor, Btn.TopLeft + ToPoint(4, 4));
  inherited Draw(Canvas);
  RestoreState(Canvas);
end;

procedure TDragBtn.MouseDown;
begin
  FDragging := True;
end;

procedure TDragBtn.MouseUp;
begin
  with FSchedule do begin
    FOffset := NULLP;
    FLineAnchor := NULLP;
    if FDragging and (FCell <> FSelectedCell) then begin
      Cut(FCell, FElementID);
      Paste(FSelectedCol, FSelectedRow);
      MakeSchedule;
      FDrawGrid.Invalidate;
    end;
    FDragging := False;
  end;
end;

procedure TDragBtn.MouseMove;
var
  CellRect: TRect;
begin
  if not FDragging then
    Exit;
  with FSchedule do begin
    FOffset := FMouseCoords - FAnchor - ToPoint(BTN_SIZE div 2, BTN_SIZE div 2);
    FOffset := FMouseCoords - FAnchor - ToPoint(BTN_SIZE div 2, BTN_SIZE div 2);
    CellRect := FDrawGrid.CellRect(FCell.Col + 1, FCell.Row + 1);
    FLineAnchor := ToPoint(CellRect.Right - BTN_SIZE div 2 + 4,
      CellRect.Top + 4 + Abs(FAnchor.y - CellRect.Top) + BTN_SIZE div 2);
    FDrawGrid.Repaint;
    Draw(FDrawGrid.Canvas);
  end;
end;

function TCellBtn.ShiftedBtn: TRect;
begin
  Exit(Shift(FBtn, FAnchor));
end;

constructor TCellBtn.Create(Cell: TCell; ElementID: integer; const Schedule: TSchedule);
begin
  FSchedule := Schedule;
  FCell := Cell;
  FAnchor := NULLP;
  FElementID := ElementID;
  FColor := clBlack;
  FBtn.TopLeft := NULLP;
  FBtn.Right := BTN_SIZE;
  FBtn.Bottom := BTN_SIZE;
  FIcon := nil;
end;

procedure TCellBtn.Draw(Canvas: TCanvas);
begin
  if FIcon = nil then begin
    Canvas.Brush.Color := FColor;
    Canvas.Rectangle(ShiftedBtn);
  end
  else
    Canvas.Draw(ShiftedBtn.Left, ShiftedBtn.Top, FIcon);
end;

procedure TCellBtn.Click;
begin

end;

procedure TCellBtn.MouseUp;
begin

end;

procedure TCellBtn.MouseMove;
begin

end;

procedure TCellBtn.MouseDown;
begin

end;

function TCellBtn.UnderMouse(Mouse: TPoint): boolean;
begin
  Exit(PointInRect(Mouse, ShiftedBtn));
end;

constructor TCell.Create(Table: TDBTable; Schedule: TSchedule);
begin
  FRow := -1;
  FCol := -1;
  FMaxH := 0;
  FMaxW := 0;
  FSelected := False;
  FExpandable := False;
  FTable := Table;
  FFieldData := TStringM.Create;
  FSchedule := Schedule;
  FCellBtns := TCellBtns.Create;
end;

destructor TCell.Destroy;
begin
  FFieldData.Free;
  FCellBtns.Free;
  inherited Destroy;
end;

procedure TCell.Copy(Cell: TCell; ElementID: integer);
begin
  FRow := Cell.FRow;
  FCol := Cell.FCol;
  FMaxH := Cell.FMaxH;
  FMaxW := Cell.FMaxW;
  FTable := Cell.FTable;

  if ElementID = -1 then
    AddData(Cell.FFieldData)
  else
    AddData(Cell.FFieldData, ElementID);
end;

procedure TCell.Draw(Rect: TRect; Canvas: TCanvas; VisFields: TCheckListBox);
var
  i: integer;
  j: integer;
  TextH: integer;
  FieldsAmount: integer;
  InvFieldC: integer = 0;
  RowIndex: integer = 0;
  PrevI: integer = -1;
  Offset: TPoint;
begin
  Fill(clWhite, Squeeze(Rect, 1), Canvas);
  FRect := Rect;
  TextH := Canvas.TextHeight('Нрб');
  FieldsAmount := FFieldData.Height - 1;
  try
    FExpandable := False;
    Rect.Right := Rect.Right - BTN_TAB_OFFSET;
    Rect.Left := Rect.Left + LEFT_MARGIN;
    for i := 0 to FFieldData.Width - 1 do begin
      for j := 0 to FieldsAmount do begin
        if not VisFields.Checked[j mod FTable.Count] then begin
          InvFieldC += 1;
          continue;
        end;
        RowIndex := i * FieldsAmount + i + i + j - InvFieldC;
        Offset := ToPoint(Rect.Left, Rect.Top + TextH * RowIndex);
        if Offset.Y > Rect.Bottom then begin
          FExpandable := True;
          Exit;
        end;
        Canvas.TextRect(Rect, Offset.X, Offset.Y, TextOut(i, j));
        FExpandable := not Fit(TextOut(i, j), Rect, Canvas) or FExpandable;
        if (i <> PrevI) and FSelected and (FieldsAmount - InvFieldC div
          (i + 1) >= 2) then
          DrawBtns(Canvas, i, Rect.Right + 4, Offset.Y, Rect.Bottom);
        PrevI := i;
      end;
    end;
  finally
    if FSelected and FExpandable then
      Canvas.Draw(Rect.Right + 3, Rect.Bottom - 21, expand_20x20);
  end;
end;

procedure TCell.AddData(Data: TStringM);
var
  i: integer;
begin
  for i := 0 to Data.Width - 1 do
    AddData(Data, i);
end;

procedure TCell.AddData(Data: TStringM; Col: integer);
var
  i: integer;
begin
  if FFieldData.Width = 0 then
    FFieldData.Resize(1, Data.Height)
  else
    FFieldData.AddColumns(1);
  FCellBtns.PushBack(TDragBtn.Create(Self, FFieldData.Width - 1, FSchedule));
  FCellBtns.PushBack(TEditBtn.Create(Self, FFieldData.Width - 1, FSchedule));
  FCellBtns.PushBack(TDelBtn.Create(Self, FFieldData.Width - 1, FSchedule));
  for i := 0 to Data.Height - 1 do
    FFieldData[FFieldData.Width - 1, i] := Data[Col, i];
end;

function TCell.MouseToBtn(Mouse: TPoint): TCellBtn;
var
  i: integer;
begin
  for i := 0 to FCellBtns.Size - 1 do
    if FCellBtns[i].UnderMouse(Mouse) then
      Exit(FCellBtns[i]);
  Exit(nil);
end;

function TCell.MaxTextHeight(VisFields: TCheckListBox; TextH: integer): integer;
var
  i: integer;
  InvFieldC: integer = 0;
begin
  for i := 0 to VisFields.Count - 1 do
    if not VisFields.Checked[i] then
      InvFieldC += 1;
  Result := FFieldData.Width * FFieldData.Height + FFieldData.Width -
    InvFieldC * FFieldData.Width;
  Exit(Result * TextH);
end;

function TCell.MaxTextWidth(Canvas: TCanvas): integer;
var
  i: integer;
  j: integer;
begin
  Result := 0;
  for i := 0 to FFieldData.Width - 1 do
    for j := 0 to FFieldData.Height - 1 do
      Result := Max(Result, Canvas.TextWidth(TextOut(i, j)));
  Result += LEFT_MARGIN + BTN_TAB_OFFSET + 4;
end;

function TCell.GetID(ElementID: integer): integer;
begin
  Exit(StrToInt(FFieldData[ElementID, ID_FIELD_INDX]));
end;

function TCell.TextOut(i, j: integer): string;
begin
  Exit(FTable.Fields[j mod FTable.Count].Name + ': ' + FFieldData[i, j]);
end;

procedure TCell.DrawBtns(Canvas: TCanvas; ElementID, X, Y, YConstraint: integer);
var
  i: integer;
  Index: integer;
begin
  for i := 0 to 2 do begin
    if Y >= YConstraint then
      Exit;
    Index := ElementID * 3 + i;
    FCellBtns[Index].FAnchor := ToPoint(X, Y);
    FCellBtns[Index].Draw(Canvas);
    Y += BTN_SIZE;
  end;
end;

procedure TSchedule.FormCreate(Sender: TObject);
begin
  inherited FormCreate(Sender);
  FDrawGrid.FocusRectVisible := False;
  FSelectedCell := nil;
  FBuffer := nil;
  FCurBtn := nil;
  FDelEmptyLines := True;
  FDrawEmptyLines.Checked := not FDelEmptyLines;
  Constraints.MinHeight := 20;
  Constraints.MinWidth := 350;
  FCells := TCellMatrix.Create;
end;

procedure TSchedule.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FFilters.Free;
  FCells.Free;
  FCells := nil;
  inherited FormClose(Sender, CloseAction);
end;

procedure TSchedule.FApplyFilterBtnClick(Sender: TObject);
begin
  MakeSchedule;
  FDrawGrid.Invalidate;
end;

procedure TSchedule.Load(ANClass: TNClass; ATable: TDBTable; Params: TParams = nil);
begin
  inherited Load(ANClass, ATable, Params);
  FFilters := TFilterPanels.Create(ATable, FFiltersSBox, 5, 5);
  ThisSubscriber.OnNotificationRecieve := @NotificationRecieve;
  Caption := APP_CAPTION + ' - Расписание(Б.)';
  FStatusBar.SimpleText := Connection.CurrentConnection;
  LoadCBoxData;
  MakeSchedule;
end;

procedure TSchedule.FDrawGridDrawCell(Sender: TObject; aCol, aRow: integer;
  aRect: TRect; aState: TGridDrawState);
begin
  if FCells = nil then
    Exit;
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

procedure TSchedule.FAddFilterBtnClick(Sender: TObject);
begin
  FFilters.AddFilterPanel;
end;

procedure TSchedule.FDelAllFiltersBtnClick(Sender: TObject);
begin
  FFilters.DeleteAll;
end;

procedure TSchedule.FDrawEmptyLinesChange(Sender: TObject);
begin
  FDelEmptyLines := not FDelEmptyLines;
  FDrawEmptyLines.Checked := not FDelEmptyLines;
  MakeSchedule;
end;

procedure TSchedule.FOpenDirMenuClick(Sender: TObject);
begin
  CreateDir;
end;

procedure TSchedule.FDrawGridMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if FSelectedCell = nil then
    Exit;
  FCurBtn := FSelectedCell.MouseToBtn(FMouseCoords);
  if FCurBtn <> nil then
    FCurBtn.MouseDown;
end;

procedure TSchedule.FDrawGridMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
var
  PrevCell: TCell;
begin
  FMouseCoords := ToPoint(X, Y);
  PrevCell := FSelectedCell;
  SelectCell(FMouseCoords);
  if (FSelectedCell <> nil) and (FSelectedCell <> PrevCell) then begin
    FSelectedCell.Draw(FSelectedCell.FRect, FDrawGrid.Canvas, FCheckListBox);
    if PrevCell <> nil then
      PrevCell.Draw(PrevCell.FRect, FDrawGrid.Canvas, FCheckListBox);
  end;
  if FCurBtn <> nil then
    FCurBtn.MouseMove;
end;

procedure TSchedule.FDrawGridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if Button = mbLeft then begin
    if FCurBtn <> nil then begin
      FCurBtn.MouseUp;
      FCurBtn := nil;
    end;
  end
  else if Button = mbRight then
    FCellMenu.PopUp(X + Left, Y + Top);
end;

procedure TSchedule.FInsertMenuClick(Sender: TObject);
begin
  CreateInsertCard;
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
  MakeSchedule;
  FDrawGrid.Invalidate;
  SetCellMenuState(True);
  FSelectedCell := FCells[FSelectedCol, FSelectedRow];
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
    else begin
      FCells[ColIndex, RowIndex].AddData(CellTuple[i].FFieldData);
      CellTuple[i].Free;
      CellTuple[i] := nil;
    end;
  end;
  CellTuple.Free;
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
begin
  FCells.FreeItems;
  FVTitleData := nil;
  FHTitleData := nil;
  FSelectedCell := nil;
  FBuffer := nil;
end;

procedure TSchedule.MakeSchedule;
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

procedure TSchedule.SelectCell(Mouse: TPoint);
var
  Coord: TPoint;
begin
  Coord := FDrawGrid.MouseToCell(Mouse);
  Coord.x -= 1;
  Coord.y -= 1;
  if Coord.x < 0 then
    Coord.x := 0;
  if Coord.y < 0 then
    Coord.y := 0;

  if FSelectedCell <> nil then
    FSelectedCell.FSelected := False;
  FSelectedCell := FCells[Coord.x, Coord.y];
  FSelectedCol := Coord.x;
  FSelectedRow := Coord.y;

  if FSelectedCell = nil then
    SetCellMenuState(False)
  else begin
    SetCellMenuState(True);
    FSelectedCell.FSelected := True;
  end;
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
    end
    else
      i += 1;
  end;

  i := 0;
  while i < FCells.Width do begin
    if EmptyCol(i) then begin
      FCells.DeleteCol(i);
      DeleteDBDataInd(FHTitleData, i);
    end
    else
      i += 1;
  end;
end;

procedure TSchedule.NotificationRecieve(Sender: TObject);
begin
  MakeSchedule;
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
  if Dir.FilterCount <> 0 then
    Exit;

  Dir.AddFilterPanel(FHCBox.Items[FHCBox.ItemIndex], ' = ',
    FHTitleData[FSelectedCol].Data, True);
  Dir.AddFilterPanel(FVCBox.Items[FVCBox.ItemIndex], ' = ',
    FVTitleData[FSelectedRow].Data, True);
  for i := 0 to FFilters.Size - 1 do
    if FFilters[i].Correct then
      with FFilters[i].Filter do
        Dir.AddFilterPanel(Name, ConditionalOperator, Param, True);
  Dir.ApplyFilters;
end;

procedure TSchedule.ExpandSelectedCell;
var
  MaxW: integer;
  MaxH: integer;
begin
  if FSelectedCell = nil then
    Exit;
  with FDrawGrid do begin
    if FSelectedCell.FExpandable then begin
      MaxW := FSelectedCell.MaxTextWidth(FDrawGrid.Canvas);
      MaxH := FSelectedCell.MaxTextHeight(FCheckListBox,
        FDrawGrid.Canvas.TextHeight('Нрб'));

      if MaxH > RowHeights[FSelectedRow + 1] then
        RowHeights[FSelectedRow + 1] := MaxH;
      if MaxW > ColWidths[FSelectedCol + 1] then
        ColWidths[FSelectedCol + 1] := MaxW;
    end
    else begin
      RowHeights[FSelectedRow + 1] := CELL_H;
      ColWidths[FSelectedCol + 1] := CELL_W;
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
    UpDate(FBuffer, FHCBox.ItemIndex, IntToStr(FHTitleData[Col].ID));
    UpDate(FBuffer, FVCBox.ItemIndex, IntToStr(FVTitleData[Row].ID));
  end;

  FBuffer.Free;
  FBuffer := nil;
  ThisSubscriber.CreateNotification(nil, ThisSubscriber.NClass);
end;

procedure TSchedule.Cut(Cell: TCell; ElementID: integer = -1);
begin
  if Cell = nil then
    Exit;
  FBuffer.Free;
  FBuffer := TCell.Create(Table, Self);
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
  PerformQuery(FFilters.Apply);
  Result := TCells.Create;
  Data := TStringM.Create;
  Data.Resize(1, Table.Count);
  while not FormQuery.EOF do begin
    Cell := TCell.Create(Table, Self);
    for i := 0 to Table.Count - 1 do
      Data[0, i] := FormQuery.Fields[i].AsString;
    Cell.AddData(Data);
    FormQuery.Next;
    Result.PushBack(Cell);
  end;
  Data.Free;
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
