unit USchedule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls, ComCtrls, CheckLst, PairSplitter, Menus, UDBForm, UVector, UMatrix,
  UDBObjects, UAbout, UDirectory, DB, UCard, UPointUtils, UFilters,
  UCanvasUtils, UIcons, URectUtils, UConflicts, UConflictForm, UConflictTreeViewForm;

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
  MIN_ELEMENTS_TO_DRAW_BTNS = 4;

type

  TSchedule = class;
  TCell = class;
  TCellMatrix = specialize TObjMatrix<TCell>;
  TCells = specialize TVector<TCell>;

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

  TAlertBtn = class(TCellBtn)
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
    procedure MouseUp; override;
    procedure MouseDown; override;
    procedure MouseMove; override;
  end;

  TCellBtns = specialize TObjVector<TCellBtn>;

  TCellElement = class
  private
    FID: integer;
    FData: TStringV;
    FBtns: TCellBtns;
    FCell: TCell;
    FRect: TRect;
    FMaxTextW: integer;
    FConflicted: boolean;
    procedure SetData(AData: TStringV);
    procedure SetID(AID: integer);
    procedure SetCell(ACell: TCell);
    function TextOut(Index: integer): string;
    function GetHeight: integer;
  public
    constructor Create(Cell: TCell);
    destructor Destroy; override;
    procedure Draw(var Offset: TPoint; Rect: TRect; Canvas: TCanvas);
    procedure DrawBtns(TopRight: TPoint; Canvas: TCanvas);
    procedure AddAlerBtn;
    function MouseToBtn(Mouse: TPoint): TCellBtn;
  published
    property Data: TStringV read FData write SetData;
    property ID: integer read FID write SetID;
    property Cell: TCell read FCell write SetCell;
    property MaxTextH: integer read GetHeight;
    property MaxTextW: integer read FMaxTextW;
  end;

  TElements = specialize TObjVector<TCellElement>;

  TCell = class
  private
    FRow: integer;
    FCol: integer;
    FTable: TDBTable;
    FRect: TRect;
    FSchedule: TSchedule;
    FExpandable: boolean;
    FSelected: boolean;
    FElements: TElements;
    FFreeElements: boolean;
    function GetID(ElementID: integer): integer;
  public
    constructor Create(Table: TDBTable; Schedule: TSchedule);
    destructor Destroy; override;
    procedure AddElement(Data: TStringV);
    procedure AddElement(Element: TCellElement);
    procedure Draw(Rect: TRect; Canvas: TCanvas);
    function Copy(ElementID: integer): TCell;
    function MaxTextHeight: integer;
    function MaxTextWidth: integer;
    function MouseToBtn(Mouse: TPoint): TCellBtn;
  published
    property FreeElementsOnRelease: boolean write FFreeElements;
    property Table: TDBTable read FTable;
    property Row: integer read FRow write FRow;
    property Col: integer read FCol write FCol;
    property Elements: TElements read FElements;
  end;

  { TSchedule }

  TSchedule = class(TDBForm)
  private
    FConflicts: TConflictPanels;
    FDelEmptyLines: boolean;
    FDrawBtns: boolean;
    FMouseCoords: TPoint;
    FSelectedCell: TCell;
    FBuffer: TCell;
    FSelectedCol: integer;
    FSelectedRow: integer;
    FHTitleData: TDBDataTuple;
    FVTitleData: TDBDataTuple;
    FFilters: TFilterPanels;
    FCells: TCellMatrix;
    FHeights: TIntegerV;
    FCurBtn: TCellBtn;
    FConflictCard: TConflictForm;
    FConflictView: TConflictTreeViewForm;
    FConflictedCells: TResultTuple;
    PrevHCBoxInd: integer;
    PrevVCBoxInd: integer;
    PrevRowCount: integer;
    FVisFieldCounter: integer;
    FTextH: integer;
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
    procedure CreateConflictDir;
    procedure CreateEditCard(ID: integer);
    procedure CreateInsertCard;
    procedure CreateInsertConflictCard(Conflict: TConflictPanel = nil);
    procedure PreSelectCardItems(Card: TCard);
    procedure Paste(Col, Row: integer);
    procedure Cut(Cell: TCell; ElementID: integer = -1); // -1 = Cuts all elements
    procedure SetCellMenuState(AEnabled: boolean);
    procedure SetCellsCoords;
    procedure SetTableSize;
    procedure SetCellsSize(CellW, CellH: integer);
    procedure SelectCell(Mouse: TPoint);
    procedure SetBtnsDrawState;
    procedure CheckConflicts;
    procedure LoadHeights;
    function FindElement(RecID: integer): TCellElement;
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
    FVisFields: TCheckListBox;
    FDrawGrid: TDrawGrid;
    FVisibleRecsGBox: TGroupBox;
    FPairSplitter: TPairSplitter;
    FPairSplitterTop: TPairSplitterSide;
    FPairSplitterBot: TPairSplitterSide;
    FStatusBar: TStatusBar;
    FApplyFilterBtn: TButton;
    FDrawEmptyLines: TCheckBox;
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
    FAddFilterBtn: TButton;
    FConfSheet: TTabSheet;
    FDelAllFiltersBtn: TButton;
    FFiltersSBox: TScrollBox;
    FFiltersSheet: TTabSheet;
    FPControl: TPageControl;
    FAddConflictBtn: TButton;
    FDelAllConflictsBtn: TButton;
    FConflictsSBox: TScrollBox;
    FDirShowConflictBtn: TButton;
    FTreeShowConflictBtn: TButton;
    procedure FDirShowConflictBtnClick(Sender: TObject);
    procedure FTreeShowConflictBtnClick(Sender: TObject);
    procedure FAddConflictBtnClick(Sender: TObject);
    procedure FDelAllConflictsBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject); override;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction); override;
    procedure FCutAllMenuClick(Sender: TObject);
    procedure FPasteMenuClick(Sender: TObject);
    procedure FInsertMenuClick(Sender: TObject);
    procedure FOpenDirMenuClick(Sender: TObject);
    procedure FDrawEmptyLinesChange(Sender: TObject);
    procedure FAddFilterBtnClick(Sender: TObject);
    procedure FDelAllFiltersBtnClick(Sender: TObject);
    procedure FVisFieldsClickCheck(Sender: TObject);
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

constructor TAlertBtn.Create(Cell: TCell; ElementID: integer; const Schedule: TSchedule);
begin
  inherited Create(Cell, ElementID, Schedule);
  FIcon := alert_20x20;
  FColor := clYellow;
end;

procedure TAlertBtn.MouseUp;
begin

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
    end;
    FDrawGrid.Invalidate;
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


procedure TCellElement.SetData(AData: TStringV);
begin
  FBtns.PushBack(TDragBtn.Create(FCell, FID, FCell.FSchedule));
  FBtns.PushBack(TEditBtn.Create(FCell, FID, FCell.FSchedule));
  FBtns.PushBack(TDelBtn.Create(FCell, FID, FCell.FSchedule));
  if FData <> nil then
    FData.Free;
  FData := AData;
end;

procedure TCellElement.SetID(AID: integer);
var
  i: integer;
begin
  FID := AID;
  for i := 0 to FBtns.Size - 1 do
    FBtns[i].FElementID := AID;
end;

procedure TCellElement.SetCell(ACell: TCell);
var
  i: integer;
begin
  FCell := ACell;
  for i := 0 to FBtns.Size - 1 do
    FBtns[i].FCell := ACell;
end;

function TCellElement.TextOut(Index: integer): string;
begin
  Exit(FCell.FTable.Fields[Index].Name + ': ' + FData[Index]);
end;

function TCellElement.GetHeight: integer;
begin
  Exit(Frect.Bottom - FRect.Top);
end;

constructor TCellElement.Create(Cell: TCell);
begin
  FID := -1;
  FCell := Cell;
  FData := TStringV.Create;
  FBtns := TCellBtns.Create;
  FConflicted := False;
end;

destructor TCellElement.Destroy;
begin
  FData.Free;
  FBtns.Free;
end;

procedure TCellElement.DrawBtns(TopRight: TPoint; Canvas: TCanvas);
var
  i: integer;
  Anchor: TPoint;
begin
  Anchor := ToPoint(TopRight.x - BTN_SIZE, TopRight.y);
  for i := 0 to FBtns.Size - 1 do begin
    FBtns[i].FAnchor := Anchor;
    FBtns[i].Draw(Canvas);
    Anchor.y += BTN_SIZE;
  end;
end;

procedure TCellElement.AddAlerBtn;
begin
  if FBtns.Size = 3 then
    FBtns.PushBack(TAlertBtn.Create(FCell, 0, FCell.FSchedule));
end;

procedure TCellElement.Draw(var Offset: TPoint; Rect: TRect; Canvas: TCanvas);
var
  i: integer;
  TextH: integer;
begin
  FMaxTextW := CELL_W;
  TextH := FCell.FSchedule.FTextH;
  FRect := Rect;
  FRect.Top := Offset.y;
  FRect.Bottom := Offset.y + TextH * (FCell.FSchedule.FVisFieldCounter + 1);

  if FConflicted then
    Fill(RGBToColor(255, 225, 225), Squeeze(FRect, 1), Canvas);
  for i := 0 to FData.Size - 1 do begin
    if not FCell.FSchedule.FVisFields.Checked[i] then
      continue;

    Canvas.TextRect(Rect, Offset.x, Offset.y, TextOut(i));
    FMaxTextW := Max(FMaxTextW, Canvas.TextWidth(TextOut(i)));
    Offset.y += TextH;
  end;
  Offset.y += TextH;
end;

function TCellElement.MouseToBtn(Mouse: TPoint): TCellBtn;
var
  i: integer;
begin
  for i := 0 to FBtns.Size - 1 do
    if FBtns[i].UnderMouse(Mouse) then
      Exit(FBtns[i]);
  Exit(nil);
end;

constructor TCell.Create(Table: TDBTable; Schedule: TSchedule);
begin
  FRow := -1;
  FCol := -1;
  FSelected := False;
  FExpandable := False;
  FTable := Table;
  FSchedule := Schedule;
  FElements := TElements.Create;
  FFreeElements := False;
end;

destructor TCell.Destroy;
begin
  if FFreeElements then
    FElements.Free;
  inherited Destroy;
end;

procedure TCell.AddElement(Data: TStringV);
var
  Element: TCellElement;
begin
  Element := TCellElement.Create(Self);
  Element.Data := Data;
  Element.ID := FElements.Size;
  FElements.PushBack(Element);
end;

procedure TCell.AddElement(Element: TCellElement);
begin
  Element.ID := FElements.Size;
  Element.Cell := Self;
  FElements.PushBack(Element);
end;

procedure TCell.Draw(Rect: TRect; Canvas: TCanvas);
var
  i: integer;
  Offset: TPoint;
begin
  FExpandable := False;
  Fill(clWhite, Squeeze(Rect, 1), Canvas);

  FRect := Rect;
  Offset := ToPoint(Rect.Left, Rect.Top);
  for i := 0 to Elements.Size - 1 do begin
    Elements[i].Draw(Offset, Rect, Canvas);
    FExpandable := FExpandable or (Elements[i].FMaxTextW > Width(Rect));
    if FSelected and FSchedule.FDrawBtns then
      with Elements[i] do
        DrawBtns(ToPoint(Frect.Right, FRect.Top), Canvas);
  end;
  FExpandable := FExpandable or (Offset.y > Rect.Bottom);

  if FSelected and FExpandable then
    Canvas.Draw(Rect.Right - BTN_SIZE, Rect.Bottom - BTN_SIZE, expand_20x20);
end;

function TCell.Copy(ElementID: integer): TCell;
var
  i: integer;
begin
  Result := TCell.Create(FTable, FSchedule);
  Result.FRow := FRow;
  Result.FCol := FCol;
  if ElementID = -1 then
    for i := 0 to Elements.Size - 1 do
      Result.AddElement(FElements[i])
  else
    Result.AddElement(FElements[ElementID]);
end;

function TCell.MouseToBtn(Mouse: TPoint): TCellBtn;
var
  i: integer;
begin
  if not FSchedule.FDrawBtns then
    Exit(nil);
  for i := 0 to FElements.Size - 1 do begin
    Result := FElements[i].MouseToBtn(Mouse);
    if Result <> nil then
      Exit(Result);
  end;
end;

function TCell.MaxTextHeight: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to FElements.Size - 1 do
    Result += FElements[i].MaxTextH;
end;

function TCell.MaxTextWidth: integer;
var
  i: integer;
begin
  Result := CELL_W;
  for i := 0 to FElements.Size - 1 do
    Result := Max(Result, FElements[i].FMaxTextW);
  Result += LEFT_MARGIN + BTN_SIZE;
end;

function TCell.GetID(ElementID: integer): integer;
begin
  Exit(StrToInt(FElements[ElementID].Data[ID_FIELD_INDX]));
end;

procedure TSchedule.FormCreate(Sender: TObject);
begin
  inherited FormCreate(Sender);
  FDrawGrid.FocusRectVisible := False;
  FSelectedCell := nil;
  FBuffer := nil;
  FCurBtn := nil;
  FConflicts := nil;
  FDelEmptyLines := True;
  FDrawBtns := True;
  FDrawEmptyLines.Checked := not FDelEmptyLines;
  Constraints.MinHeight := 20;
  Constraints.MinWidth := 350;
  PrevHCBoxInd := -1;
  PrevVCBoxInd := -1;
  PrevRowCount := -1;
  FTextH := 25;
  FCells := TCellMatrix.Create;
  FHeights := TIntegerV.Create;
  FConflicts := TConflictPanels.Create(FConflictsSBox, 5, 5);
  FConflicts.OnEditClick := @CreateInsertConflictCard;
  Application.CreateForm(TConflictForm, FConflictCard);
  Application.CreateForm(TConflictTreeViewForm, FConflictView);
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
  LoadCheckListBoxData;
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
  FHeights[aRow] := FDrawGrid.RowHeights[aRow];
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
        FCells[aCol - 1, aRow - 1].Draw(aRect, FDrawGrid.Canvas);
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

procedure TSchedule.FVisFieldsClickCheck(Sender: TObject);
begin
  SetBtnsDrawState;
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
  if (FSelectedCell <> nil) and (FSelectedCell <> PrevCell) then
    FDrawGrid.Repaint;
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

procedure TSchedule.FAddConflictBtnClick(Sender: TObject);
begin
  CreateInsertConflictCard(nil);
end;

procedure TSchedule.FDelAllConflictsBtnClick(Sender: TObject);
begin
  FConflicts.DeleteAll;
end;

procedure TSchedule.FDirShowConflictBtnClick(Sender: TObject);
begin
  CreateConflictDir;
end;

procedure TSchedule.FTreeShowConflictBtnClick(Sender: TObject);
begin
  FConflictView.Load(Table, FConflicts);
  FConflictView.Show;
end;

procedure TSchedule.LoadCBoxData;
begin
  LoadStringListData(FHCBox.Items);
  LoadStringListData(FVCBox.Items);
  FHCBox.ItemIndex := 6;
  FVCBox.ItemIndex := 10;
end;

procedure TSchedule.LoadCheckListBoxData;
var
  i: integer;
begin
  LoadStringListData(FVisFields.Items);
  for i := 0 to FVisFields.Items.Count - 1 do
    FVisFields.Checked[i] := True;
  FVisFields.Checked[ID_FIELD_INDX] := False;
  FVisFields.Checked[Table.FieldsByName['Размер группы'].Index] := False;
  FVisFields.Checked[Table.FieldsByName['Размер аудитории'].Index] := False;
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
  j: integer;
  ColIndex: integer;
  RowIndex: integer;
begin
  FCells.Resize(Length(FHTitleData), Length(FVTitleData));
  FCells.Fill(nil);
  for i := 0 to CellTuple.Size - 1 do begin
    ColIndex := FindDBDataInd(FHTitleData,
      CellTuple[i].Elements[0].Data[FHCBox.ItemIndex]);
    RowIndex := FindDBDataInd(FVTitleData,
      CellTuple[i].Elements[0].Data[FVCBox.ItemIndex]);
    if FCells[ColIndex, RowIndex] = nil then
      FCells[ColIndex, RowIndex] := CellTuple[i]
    else
      with FCells[ColIndex, RowIndex] do begin
        for j := 0 to CellTuple[i].Elements.Size - 1 do
          AddElement(CellTuple[i].Elements[j]);
        CellTuple[i].Free;
        CellTuple[i] := nil;
      end;
  end;
  CellTuple.Free;
end;

procedure TSchedule.LoadHeights;
var
  i: integer;
begin
  FHeights[0] := TITLE_H;
  if FHeights.Size > FDrawGrid.RowCount then
    Exit;
  for i := 0 to FHeights.Size - 1 do begin
    if (i > 0) and (FHeights[i] < CELL_H) then
      FHeights[i] := CELL_H;
    FDrawGrid.RowHeights[i] := FHeights[i];
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
  for i := 0 to FCells.Width - 1 do
    for j := 0 to FCells.Height - 1 do
      if FCells[i, j] <> nil then
        FCells[i, j].FFreeElements := True;
  FCells.FreeItems;
  FVTitleData := nil;
  FHTitleData := nil;
  FSelectedCell := nil;
  FBuffer := nil;
end;

procedure TSchedule.MakeSchedule;
var
  DataTuple: TCells;
begin
  FreePrevData;
  FTextH := FDrawGrid.Canvas.TextHeight('Нрб');
  if PrevHCBoxInd <> -1 then
    FVisFields.Checked[PrevHCBoxInd] := True;
  if PrevVCBoxInd <> -1 then
    FVisFields.Checked[PrevVCBoxInd] := True;

  FHTitleData := GetTitleData(FHCBox.ItemIndex);
  FVTitleData := GetTitleData(FVCBox.ItemIndex);
  DataTuple := GetCellDataTuple;
  BuildMatrix(DataTuple);
  CheckConflicts;
  FVisFields.Checked[FHCBox.ItemIndex] := False;
  FVisFields.Checked[FVCBox.ItemIndex] := False;
  if FDelEmptyLines then
    DeleteEmptyLines;
  SetCellsCoords;
  SetTableSize;
  FHeights.Resize(FDrawGrid.RowCount);
  if (PrevHCBoxInd = FHCBox.ItemIndex) and (PrevVCBoxInd = FVCBox.ItemIndex) and
    (FHeights.Size = PrevRowCount) then
    LoadHeights
  else
    SetCellsSize(CELL_W, CELL_H);
  PrevHCBoxInd := FHCBox.ItemIndex;
  PrevVCBoxInd := FVCBox.ItemIndex;
  PrevRowCount := FDrawGrid.RowCount;
  SetBtnsDrawState;
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
  if (Coord.x < FCells.Width) and (Coord.y < FCells.Height) then
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

procedure TSchedule.CheckConflicts;
var
  i: integer;
  j: integer;
  k: integer;
  Data: TDataTuple;
  Element: TCellElement;
begin
  if FCells = nil then
    Exit;
  Data := TDataTuple.Create;
  for i := 0 to FCells.Width - 1 do
    for j := 0 to FCells.Height - 1 do begin
      if FCells[i, j] <> nil then
        for k := 0 to FCells[i, j].Elements.Size - 1 do
          Data.PushBack(FCells[i, j].Elements[k].Data);
    end;

  FConflicts.AnalyzeData(Data);
  FConflictedCells := nil;
  FConflictedCells := FConflicts.GetResult;
  if Length(FConflictedCells) > 0 then;
  for i := 0 to High(FConflictedCells) do begin
    Element := FindElement(FConflictedCells[i].RecID);
    if Element <> nil then begin
      Element.AddAlerBtn;
      Element.FConflicted := True;
    end;
  end;
end;

procedure TSchedule.SetBtnsDrawState;
var
  i: integer;
begin
  FVisFieldCounter := 0;
  for i := 0 to FVisFields.Count - 1 do
    if FVisFields.Checked[i] then
      FVisFieldCounter += 1;
  FDrawBtns := FVisFieldCounter >= MIN_ELEMENTS_TO_DRAW_BTNS;
end;

function TSchedule.FindElement(RecID: integer): TCellElement;
var
  i: integer;
  j: integer;
  k: integer;
begin
  for i := 0 to FCells.Width - 1 do
    for j := 0 to FCells.Height - 1 do
      if FCells[i, j] <> nil then
        for k := 0 to FCells[i, j].Elements.Size - 1 do
          if RecID = FCells[i, j].GetID(k) then
            Exit(FCells[i, j].Elements[k]);
  Exit(nil);
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
  if Dir.Filters.Size <> 0 then
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

procedure TSchedule.CreateConflictDir;
var
  Dir: TDirectory;
  FilterPanel: TFilterPanel;
  i: integer;
begin
  if FConflictedCells = nil then
    Exit;
  if Length(FConflictedCells) = 0 then
    Exit;
  Dir := TDirectory(CreateChildForm(ThisSubscriber.NClass, Table,
    TDirectory, nil, 333212333));
  if Dir.Filters.Size <> 0 then
    Dir.Filters.DeleteAll;
  for i := 0 to High(FConflictedCells) do begin
    FilterPanel := Dir.AddFilterPanel('ID', ' = ',
      IntToStr(FConflictedCells[i].RecID), True);
    FilterPanel.Filter.Connection := 'OR';
  end;
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
      MaxW := FSelectedCell.MaxTextWidth;
      MaxH := FSelectedCell.MaxTextHeight;

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
  for i := 0 to Cell.Elements.Size - 1 do begin
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

  for i := 0 to FBuffer.Elements.Size - 1 do begin
    if FBuffer.Col <> Col then
      FBuffer.Elements[i].Data[FHCBox.ItemIndex] := IntToStr(FHTitleData[Col].ID);
    if FBuffer.Row <> Row then
      FBuffer.Elements[i].Data[FVCBox.ItemIndex] := IntToStr(FVTitleData[Row].ID);
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
  FBuffer := Cell.Copy(ElementID);
  if ElementID = -1 then begin
    FCells[Cell.Col, Cell.Row] := nil;
    Cell.Free;
  end
  else begin
    Cell.Elements.DeleteIndS(ElementID);
    if Cell.FElements.Size = 0 then begin
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

procedure TSchedule.CreateInsertConflictCard(Conflict: TConflictPanel = nil);
begin
  FConflictCard.Load(Table, FConflicts, Conflict);
  FConflictCard.Show;
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
  Result += word(@(FSelectedCell.FElements));
  Result += Result and Result xor Result shl 10;
end;


function TSchedule.GetCellDataTuple: TCells;
var
  i: integer;
  Data: TStringV;
  Cell: TCell;
begin
  PerformQuery(FFilters.Apply);
  Result := TCells.Create;
  FormQuery.First;
  while not FormQuery.EOF do begin
    Cell := TCell.Create(Table, Self);
    Data := TStringV.Create;
    for i := 0 to Table.Count - 1 do
      Data.PushBack(FormQuery.Fields[i].AsString);
    Cell.AddElement(Data);
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
