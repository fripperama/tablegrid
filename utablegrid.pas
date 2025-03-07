{

  TTableGrid - This class enhances the TStringGrid component, setting it up
               to behave as a Data Table, offering features like
               column swaping, sorting, odd and even colors, exclusive
               header font, indicator arrow, and much more.
               This is a class, not a component. You need to have
               a TStringGrid component to pass as a parameter when creating
               the TTableGrid instance.
  Delphi and Lazarus version.

  version 1.0 - 2013
  version 1.1 - 2017

  Copyright (C) <2013> <Gabriel Vidal> utablegrid.pas

  E-Mail: gab.alv.vidal@gmail.com

  Website: http://comicedit.wordpress.com

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit uTableGrid;

{$ifdef fpc}
  {$MODE Delphi}
{$endif}

interface

uses
  Grids, Classes, Graphics, ExtCtrls, DBGrids, StdCtrls, Controls, Types,
  DB;

const
  COLOR_ODD_DEFAULT = clWhite;
  COLOR_EVEN_DEFAULT = $00FFEFD5;
  COLOR_GRID_DEFAULT = clInactiveBorder;
  COLOR_FIXED_DEFAULT = clBtnFace;

  GRID_VALUE_TRUE  = '           ';
  GRID_VALUE_FALSE = '                 ';

type
  TGridEditType = (teString, teCheckBox, teNumeric, teNone);

  TGridTextAlign = ( ptLeft, ptRight );

  TGridModifyingType = (mtInsertLine, mtRemoveLine, mtClearGrid, mtInsertObject);

  RecGridEditType = record
    GridEditType: TGridEditType;
    ColumnIndex: Integer;
  end;

  TUpdateColumnEvent = procedure (Sender: TObject; Line, Column: Integer) of object;

  RecUpdateType = record
    UpdateType: TGridModifyingType; // Specify the type of update to be done
    InsertParameter: TStringList;   // The line content to be inserted
    RemoveParameter: Integer;       // The line to be removed
    bInsertIndex: Boolean;          // The line number where to insert the new line
    anObject: TObject;              // The object to be attached to the row
  end;

  RecImageColumn = record
    ColumnIndex: Integer;
    ImageIndex: Integer;
    Width: Integer;
    Height: Integer;
  end;

  { TTableGrid }

  TTableGrid = class
  private
    BitmapArrow: TBitmap;
    BitMapCheckBox: TBitmap;
    BitMapIndicator: TBitMap;
    FImageArrow: TImage;
    FPreviousDefaultDrawing: Boolean;
    FAttachedGrid: TStringGrid;
    FIndicator: Boolean;
    FEditing: Boolean;
    FUpdating: Boolean;
    PreviousOptions: TGridOptions;
    SmartLabel: TStaticText;
    SelectedColumn: Integer;
    LineIndex: Integer;
    Sorted: Boolean;
    ClickedOnTitle: Boolean;
    IsNotEmpty: Boolean;
    arrImageColumn: array of RecImageColumn;
    ImageList: TList;
    SortedColumnsList: TStringList;
    arrEditingColumn: array of RecGridEditType;
    SwapSortedColumnsList: TStringList; // used during column swaping
    arrUpdateType: array of RecUpdateType;

    // used for the mouse draw cell
    PreviousPenColor: TColor;
    PreviousBrushColor: TColor;
    ColumnIndex, ImageIndex: Integer;
    iCont: Integer;
    RectAux: TRect;
    DotCommaPos: Integer;
    Text: String;
    XPrev, YPrev: Integer;
    ColAux, RowAux: Integer;
    AuxRowCount: Integer;
    FReadOnly: Boolean;
    FAllowSorting: Boolean;
    FAllowColumnReordering: Boolean;
    procedure SetColCount(const Value: Integer);
    procedure SetRowCount(const Value: Integer);
    function GetColCount: Integer;
    function GetRowCount: Integer;
    procedure SetIndicator(const Value: Boolean);
    procedure QuickSortRec(First, Last: Integer);
    function CompareLines(Line1, Line2: Integer): Integer;
    procedure ReverseLines;
    function GetSortedColumn: Integer;
    procedure BuildSwapSortedColumnList;
    procedure BuildArrowImage;
    procedure BuildCheckBoxImage;
    procedure BuildIndicatorImage;
    function TextPos(Column: Integer): TGridTextAlign;
    function GridEditType(Column: Integer): TGridEditType;
    procedure SetReadOnly(const Value: Boolean);
    procedure SortInnerGrid(Column: Integer; Ascending: Boolean = True; DoNotAllowReversing: Boolean = false);
    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
    function GetSelectedLine: Integer;
    procedure SetSelectedLine(const Value: Integer);
    function GetAllowColumnResizing: Boolean;
    procedure SetAllowColumnResizing(const Value: Boolean);
    function GetSelectedCol: Integer;
    procedure SetSelectedCol(const Value: Integer);
  public
    OddColor: TColor;
    EvenColor: TColor;
    TitleFont: TFont;
    PreviousMouseDown: TMouseEvent;
    PreviousMouseUp: TMouseEvent;
    {$ifdef fpc}
    PreviousDrawCell: TOnDrawCell;
    PreviousSelectCell: TOnSelectCellEvent;
    {$else}
    PreviousDrawCell: TDrawCellEvent;
    PreviousSelectCell: TSelectCellEvent;
    {$endif}
    PreviousDoubleClick: TNotifyEvent;
    PreviousKeyPress: TKeyPressEvent;
    PreviousMouseMove: TMouseMoveEvent;
    OnUpdateColumn: TUpdateColumnEvent;

    procedure GridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure GridDrawCell(Sender: TObject; ACol, ARow: Longint; Rect: TRect; State: TGridDrawState);
    procedure GridSelectCell(Sender: TObject; ACol, ARow: Longint; var CanSelect: Boolean);
    procedure GridDoubleClick(Sender: TObject);
    procedure GridKeypress(Sender: TObject; var Key: Char);

    constructor Create(StringGrid: TStringGrid);
    destructor Destroy; override;
    procedure RemoveLine(LineIndex: Integer); overload;
    procedure RemoveLine; overload;
    procedure InsertLine(Line: TStringList); overload;
    procedure InsertLine(Line: array of String); overload;
    procedure InsertLine(Line: TStringList; LineIndex: Integer); overload;
    procedure UpdateLine(Line: TStringList); overload;
    procedure UpdateLine(Line: array of String); overload;
    procedure UpdateLine(Line: TStringList; LineIndex: Integer); overload;
    procedure DefineHeader(Header: array of String); overload;
    procedure DefineHeader(Header: TStringList); overload;
    procedure Clear;
    procedure InsertObject(LineIndex: Integer; anObject: TObject); overload;
    procedure InsertObject(Line: TStringList; anObject: TObject); overload;
    function  ReadObject(LineIndex: Integer): TObject; overload;
    function  ReadObject: TObject; overload;
    function  ReadColumn(Col: Integer): String;
    function  ReadCell(Col: Integer; Line: Integer): String;
    procedure UpdateCell(Col, Line: Integer; Value: String);
    function  Find(Value: String; Col: Integer): Boolean;
    function  IsEmpty: Boolean;
    procedure RegisterImageColumn(ColumnIndex, Width, Height: Integer);
    function  RegisterImage(Bitmap: TBitmap): Integer;
    procedure RegisterEditField(ColumnIndex: Integer; GridEditType: TGridEditType);
    procedure BeginEdit;
    procedure EndEdit;
    procedure BeginUpdate;
    procedure CommitUpdate;
    procedure RollbackUpdate;
    procedure SwapColumns(Col1, Col2: Integer);
    procedure ClearUpdateList;
    procedure SetColWidth(ColumnIndex, Width: Integer);
    function ReadColumnSize(ColumnIndex: Integer): Integer;
    function CheckForErrors: Boolean;
    procedure SortGrid(Column: Integer; Ascending: Boolean = True);
    procedure FitColumnToGridWidth(ColumnIndex: Integer);
    procedure ReadDataSet ( DataSet: TDataset );
    procedure SaveToFile ( FileName: String );
    function GridToLineIndex(Value: Integer): Integer;
    function LineIndexToGrid(Value: Integer): Integer;
    function ReadHeader(Value: Integer ): String;
    procedure SetColumnSize( i, j: Integer );
    function ColumnIndexToGrid(Value: Integer): Integer;
    function GridToColumnIndex(Value: Integer): Integer;
    function ReadFieldEditType( Value: Integer ): TGridEditType;
  published
    // the TStringGrid component that the Table Grid is using to host his transformations.
    // use this property to directly manipulate it.
    property AttachedGrid: TStringGrid read FAttachedGrid write FAttachedGrid;
    // the number of columns. does not include the indicator column.
    property ColCount: Integer read GetColCount write SetColCount;
    // the number of rows in the grid. does not include the header.
    property RowCount:  Integer read GetRowCount  write SetRowCount;
    // indicates if the grid will have an indicator arrow to show the selected line.
    property Indicator: Boolean read FIndicator write SetIndicator;
    // the last column used in the sorting process. the list of columns sorted is stored in
    // the SortedColumnsList.
    property SortedColumn: Integer read GetSortedColumn;
    // when Readonly is set to true, the grid can not be edited.
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
    // when Enabled is false, the grid cannot be focused.
    property Enabled: Boolean read GetEnabled write SetEnabled;
    // the line currently selected in the grid. the header does not count as a line.
    property SelectedLine: Integer read GetSelectedLine write SetSelectedLine;
    // the column currently selected in the grid. the indicator does not count as a column.
    property SelectedCol: Integer read GetSelectedCol write SetSelectedCol;
    // if true, the user will be able to sort the grid by clicking on the column
    // however, programatically calling SortGrid will also allow sorting, even if this
    // property is set to false
    property AllowSorting: Boolean read FAllowSorting write FAllowSorting;
    // sets up if the user will be able to rearrange the columns of the grid
    property AllowColumnReordering: Boolean read FAllowColumnReordering write FAllowColumnReordering;
    // sets up if the user will be able to resize the columns of the grid
    property AllowColumnResizing: Boolean read GetAllowColumnResizing write SetAllowColumnResizing;
  end;

  function strGridBoolean( Value: Boolean ): String;

implementation

uses
  SysUtils, Forms, Math;

(**************************************************************************************
* NumbersOnly
* Keeps only numeric and decimal separators from a String
* In parameters:
* Value: String - the full string with all digits
* Result: String - the string formated retained only numeric and decimal separators
***************************************************************************************)
function NumbersOnly(Value: String): String;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(Value) do
  begin
    if Value[i] in ['0'..'9', DecimalSeparator] then
      Result := Result + Value[i];
  end;
end;

(**************************************************************************************
* ValidValue
* This function checks if the in-parameter contains only numeric and decimal separator
* digits.
* In parameters:
* Value: String - the full string with all digits to check
* Result: Boolean - True: the value has only numeric and decimal separator / False: it
*         contains alpha-numeric digits.
***************************************************************************************)
function ValidValue ( Value: String ): Boolean;
var
  i: Integer;
begin
  Result := True;

  if Trim(Value) <> '' then
  begin
    for i := 1 to Length(Value) do
    begin
      if not (Value[i] in ['0'..'9', '.', ',']) then
      begin
        Result := False;
        break;
      end;
    end;
  end;
end;

(**************************************************************************************
* The TTableGrid constructor
* In parameters:
* StringGrid: TStringGrid - this is the component that the TTableGrid class will morph
*                           and expand the functionalities of the native String Grid.
***************************************************************************************)
constructor TTableGrid.Create(StringGrid: TStringGrid);
begin
  inherited Create;
  FUpdating := False;
  FReadOnly := False;
  FAttachedGrid := StringGrid;
  FPreviousDefaultDrawing := FAttachedGrid.DefaultDrawing;
  FAttachedGrid.DefaultDrawing := False;
  FAllowSorting := True;
  FAllowColumnReordering := True;
  OddColor := COLOR_ODD_DEFAULT;
  EvenColor := COLOR_EVEN_DEFAULT;
  FAttachedGrid.Color := COLOR_GRID_DEFAULT;
  FAttachedGrid.FixedColor := COLOR_FIXED_DEFAULT;
  TitleFont := TFont.Create;
  TitleFont.Assign(FAttachedGrid.Font);
  TitleFont.Style := TitleFont.Style + [fsBold];

  FAttachedGrid.DefaultRowHeight := 19;

  PreviousMouseDown := FAttachedGrid.OnMouseDown;
  PreviousMouseUp := FAttachedGrid.OnMouseUp;
  PreviousMouseMove := FAttachedGrid.OnMouseMove;
  PreviousDrawCell := FAttachedGrid.OnDrawCell;
  PreviousSelectCell := FAttachedGrid.OnSelectCell;
  PreviousDoubleClick := FAttachedGrid.OnDblClick;
  PreviousKeyPress := FAttachedGrid.OnKeyPress;

  FAttachedGrid.OnMouseDown := GridMouseDown;
  FAttachedGrid.OnMouseUp := GridMouseUp;
  FAttachedGrid.OnMouseMove := GridMouseMove;
  FAttachedGrid.OnDrawCell := GridDrawCell;
  FAttachedGrid.OnSelectCell := GridSelectCell;
  FAttachedGrid.OnDblClick := GridDoubleClick;
  FAttachedGrid.OnKeyPress := GridKeypress;

  SmartLabel := TStaticText.Create(FAttachedGrid);
  SmartLabel.Visible := False;
  SmartLabel.Parent := FAttachedGrid;
  SmartLabel.BorderStyle := sbsSunken;
  SmartLabel.AutoSize := False;

  BuildIndicatorImage;
  BuildCheckBoxImage;

  ClickedOnTitle := False;
  IsNotEmpty := False;

  SetLength(arrImageColumn, 0);
  SetLength(arrEditingColumn, 0);
  ImageList := TList.Create;
  SortedColumnsList := TStringList.Create;
  FEditing := False;
  LineIndex := FAttachedGrid.Row;

  SwapSortedColumnsList := TStringList.Create;
  BuildSwapSortedColumnList;
  BuildArrowImage;
  Indicator := True;

  FUpdating := True;
  PreviousOptions := FAttachedGrid.Options;
  FAttachedGrid.Options := FAttachedGrid.Options + [goRowSelect] - [goRangeSelect] + [goColSizing];
  FUpdating := False;
  OnUpdateColumn := nil;
end;

(**************************************************************************************
* RemoveLine
* This procedure will remove the specified line from the grid.
* In parameters:
* LineIndex: Integer - the index to be removed from the grid.
***************************************************************************************)
procedure TTableGrid.RemoveLine(LineIndex: Integer);
var
  i: Integer;
begin
  if not FUpdating then
  begin
    if (LineIndex > RowCount) or (LineIndex < 1) then
      exit;
    // clears the only data line available
    for i := 0 to FAttachedGrid.ColCount - 1 do
      FAttachedGrid.Cells[i, LineIndex] := '';
    // remove the line
    for i := LineIndex to RowCount - 1  do
    begin
      FAttachedGrid.Objects[0, i] := FAttachedGrid.Objects[0, i + 1];
      FAttachedGrid.Rows[i] := FAttachedGrid.Rows[i + 1];
    end;
    if RowCount > 1 then
      RowCount := RowCount - 1;
    if IsEmpty then
      IsNotEmpty := False;
  end
  else
  begin
    SetLength(arrUpdateType, Length(arrUpdateType) + 1);
    arrUpdateType[High(arrUpdateType)].UpdateType := mtRemoveLine;
    arrUpdateType[High(arrUpdateType)].RemoveParameter := LineIndex;
    if AuxRowCount > 0 then
      dec(AuxRowCount);
  end;
end;

(**************************************************************************************
* ReadObject
* This function returns the attached object with the specified line.
* In parameters:
* LineIndex: Integer - the line where the desired object is.
* Result: TObject - The attached object.
***************************************************************************************)
function TTableGrid.ReadObject(LineIndex: Integer): TObject;
begin
  result := FAttachedGrid.Objects[0, LineIndexToGrid(LineIndex)];
end;

(**************************************************************************************
* RemoveLine
* This procedure will remove the selected line from the grid.
***************************************************************************************)
procedure TTableGrid.RemoveLine;
begin
  RemoveLine(GridToLineIndex(FAttachedGrid.Row));
end;

(**************************************************************************************
* InsertObject
* This procedure will attach an object to the specified line index, which can be
* later recovered or read.
* In values:
* LineIndex: Integer - The line where you want to attach the object
* anObject: TObject - an object of any class type to be attached to the specified line
***************************************************************************************)
procedure TTableGrid.InsertObject(LineIndex: Integer; anObject: TObject);
begin
  if not FUpdating then
    FAttachedGrid.Objects[0, LineIndexToGrid(LineIndex)] := anObject
  else
  begin
    SetLength(arrUpdateType, Length(arrUpdateType) + 1);
    arrUpdateType[High(arrUpdateType)].UpdateType := mtInsertObject;
    arrUpdateType[High(arrUpdateType)].RemoveParameter := LineIndex;
    arrUpdateType[High(arrUpdateType)].anObject := anObject;
  end;
end;

(**************************************************************************************
* InsertObject
* This procedure will add a new line to the grid with an object attached to it.
* In values:
* Line: TStringList - Each line of the string list represents a column of the grid.
* anObject: TObject - an object of any class type to be attached to the new line
***************************************************************************************)
procedure TTableGrid.InsertObject(Line: TStringList; anObject: TObject);
begin
  InsertLine( Line );
  InsertObject( RowCount, anObject );
end;

(**************************************************************************************
* InsertLine
* This procedure will insert a new line to the grid. This new line will be inserted
* after the last line of the grid.
* In values:
* Line: TStringList - Each line of the string list represents a column of the grid.
***************************************************************************************)
procedure TTableGrid.InsertLine(Line: TStringList);
var
  i: Integer;
begin
  if not FUpdating then
  begin
    RowCount := RowCount + 1;

    if Line.Count < ColCount then
    begin
      i := 0;
      while (i < ColCount) do
      begin
        if Line.Count < ColCount then
          Line.Add( '' );
        inc(i);
      end;
    end;

    i := 1;
    while (i <= ColCount) and
          (i <= Line.Count) do
    begin
      FAttachedGrid.Cells[ColumnIndexToGrid(i), RowCount] :=
         Line.Strings[i-1];
      inc(i);
    end;
  end
  else
  begin
    SetLength(arrUpdateType, Length(arrUpdateType) + 1);
    arrUpdateType[High(arrUpdateType)].UpdateType := mtInsertLine;
    arrUpdateType[High(arrUpdateType)].InsertParameter := TStringList.Create;
    arrUpdateType[High(arrUpdateType)].InsertParameter.AddStrings(Line);
    arrUpdateType[High(arrUpdateType)].bInsertIndex := False;
    inc(AuxRowCount);
  end;
end;

(**************************************************************************************
* Clear
* This procedure will clear all data in the grid, remaining only the header columns.
***************************************************************************************)
procedure TTableGrid.Clear;
var
  i: Integer;
begin
  if not FUpdating then
  begin
    FAttachedGrid.RowCount := FAttachedGrid.FixedRows + 1;
    if FAttachedGrid.RowCount > 1 then
    begin
      for i := 0 to FAttachedGrid.ColCount - 1 do
        FAttachedGrid.Cells[i, 1] := '';
    end;
    IsNotEmpty := False;
  end
  else
  begin
    SetLength(arrUpdateType, Length(arrUpdateType) + 1);
    arrUpdateType[High(arrUpdateType)].UpdateType := mtClearGrid;
    AuxRowCount := 0;
  end;
end;

(**************************************************************************************
* DefineHeader
* Sets the header of the grid. This will affect the col count of the grid. Any extra
* column previously set will be erased.
* In values:
* Header: array of string - each item in the array represents one title of the columns.
***************************************************************************************)
procedure TTableGrid.DefineHeader(Header: array of String);
var
  i: Integer;
  AuxHeader: TStringList;
begin
  AuxHeader := TStringList.Create;
  try
    for i := Low(Header) to High(Header) do
      AuxHeader.Add(Header[i]);
    DefineHeader( AuxHeader );
  finally
    AuxHeader.Free;
  end;
end;

(**************************************************************************************
* DefineHeader
* Same as above, but In Value is TStringList.
* In values:
* Header: TStringList - each item in the array represents one title of the columns.
***************************************************************************************)
procedure TTableGrid.DefineHeader(Header: TStringList);
begin
  if FIndicator then
    Header.Insert(0, ' ');
  FAttachedGrid.ColCount := Header.Count;
  FAttachedGrid.Rows[0] := Header;
  BuildSwapSortedColumnList;
end;

(**************************************************************************************
* ReadObject
* This function returns the attached object in the selected line.
* Result: TObject - the attached object.
***************************************************************************************)
function TTableGrid.ReadObject: TObject;
begin
  result := ReadObject(GridToLineIndex(FAttachedGrid.Row));
end;

(**************************************************************************************
* ReadColumn
* Reads the value at the specified column in the selected line.
* In parameters:
* Col: Integer - the column where the value will be returned.
* Result: String - the value at the column specified at the selected line.
***************************************************************************************)
function TTableGrid.ReadColumn(Col: Integer): String;
begin
  result := FAttachedGrid.Cells[ColumnIndexToGrid(Col), FAttachedGrid.Row];
end;

(**************************************************************************************
* InsertLine
* This procedure will insert a new line to the grid. This new line will be inserted
* at the line index informed.
* In values:
* Line: TStringList - Each line of the string list represents a column of the grid.
* LineIndex: Integer - The line index where the new line will be inserted.
***************************************************************************************)
procedure TTableGrid.InsertLine(Line: TStringList; LineIndex: Integer);
var
  i: Integer;
begin
  if not FUpdating then
  begin
    if RowCount >= 1 then
    begin
      RowCount := RowCount + 1;
      for i := RowCount - 1 downto LineIndex do
      begin
        FAttachedGrid.Objects[0, i+ 1] := FAttachedGrid.Objects[0, i];
        FAttachedGrid.Rows[i+ 1] := FAttachedGrid.Rows[i];
      end;
    end
    else
    begin
      if not IsEmpty then
        RowCount := RowCount + 1;
    end;

    i := 1;
    while (i <= ColCount) and
          (i <= Line.Count) do
    begin
      FAttachedGrid.Cells[ColumnIndexToGrid(i), LineIndexToGrid(LineIndex)] :=
                                                           Line.Strings[i-1];
      inc(i);
    end;
  end
  else
  begin
    SetLength(arrUpdateType, Length(arrUpdateType) + 1);
    arrUpdateType[High(arrUpdateType)].UpdateType := mtInsertLine;
    arrUpdateType[High(arrUpdateType)].InsertParameter := TStringList.Create;
    arrUpdateType[High(arrUpdateType)].InsertParameter.AddStrings(Line);
    arrUpdateType[High(arrUpdateType)].bInsertIndex := True;
    arrUpdateType[High(arrUpdateType)].RemoveParameter := LineIndex;
    inc(AuxRowCount);
  end;
end;


(**************************************************************************************
* UpdateLine
* This procedure will update the choosen line in the grid. All new information will
* overwrite previously written data.
* In values:
* Line: TStringList - Each string in the list represents a column of the grid.
* LineIndex: Integer - the line to be updated.
***************************************************************************************)
procedure TTableGrid.UpdateLine(Line: TStringList; LineIndex: Integer);
var
  i: Integer;
begin
  i := 1;
  while (i <= ColCount) and
        (i <= Line.Count) do
  begin
    FAttachedGrid.Cells[ColumnIndexToGrid(i), LineIndexToGrid(LineIndex)] :=
                                                         Line.Strings[i-1];
    inc(i);
  end;
end;

function TTableGrid.Find(Value: String; Col: Integer): Boolean;
var
  i: Integer;
  Text: String;
  j: Integer;
  FlagComp: Boolean;
begin
  Result := False;
  if Col = 0 then
    Col := 1;
  for i := 1 to RowCount do
  begin
    Text := FAttachedGrid.Cells[ColumnIndexToGrid(Col), LineIndexToGrid(i)];
    if Length(Text) < Length(Value) then
      continue;
    FlagComp := True;
    for j := 1 to Length(Value) do
    begin
      if UpperCase(Value[j]) <> UpperCase(Text[j]) then
      begin
        FlagComp := False;
        break;
      end;
    end;
    if (not FlagComp) and (Value = '#') then // seleção de número
    begin
      if Text[1] in ['0'..'9'] then
        FlagComp := True;
    end;
    if FlagComp then
    begin
      SelectedLine := i;
      FAttachedGrid.Col := ColumnIndexToGrid(Col);
      Result := True;
      break;
    end;
  end;
end;

(**************************************************************************************
* InsertLine
* This procedure will insert a new line to the grid. This new line will be inserted
* after the last line of the grid.
* In values:
* Line: array of String - Each element of the array represents a column of the grid.
***************************************************************************************)
procedure TTableGrid.InsertLine(Line: array of String);
var
  List: TStringList;
  i: Integer;
begin
  List := TStringList.Create;
  try
    for i := Low(Line) to High(Line) do
      List.Add(Line[i]);
    InsertLine(List);
  finally
    List.Free;
  end;
end;

(**************************************************************************************
* The TTableGrid destructor
* After destroying the table grid, the string grid will come back to its original
* state before creating the table grid. However, the rows and columns remain.
***************************************************************************************)
destructor TTableGrid.Destroy;
begin
  Indicator := False;
  FAttachedGrid.OnMouseUp := PreviousMouseUp;
  FAttachedGrid.OnMouseDown := PreviousMouseDown;
  FAttachedGrid.OnDrawCell := PreviousDrawCell;
  FAttachedGrid.OnDblClick := PreviousDoubleClick;
  FAttachedGrid.OnKeyPress := PreviousKeyPress;
  FAttachedGrid.OnSelectCell := PreviousSelectCell;
  FAttachedGrid.OnMouseMove := PreviousMouseMove;
  SmartLabel.Free;
  BitMapIndicator.Free;
  BitMapCheckBox.Free;
  ImageList.Free;
  SortedColumnsList.Free;
  TitleFont.Free;
  FAttachedGrid.Options := PreviousOptions;
  FAttachedGrid.DefaultDrawing := FPreviousDefaultDrawing;
  SwapSortedColumnsList.Free;
  FImageArrow.Free;
  inherited;
end;

(**************************************************************************************
* GridDrawCell
* This is the OnGridDrawCell event procedure the Table Grid will use to behave like it
* should. If the component has its own GridDrawCell event, it will be run before the
* Table Grid runs its transformations.
***************************************************************************************)
procedure TTableGrid.GridDrawCell(Sender: TObject; ACol, ARow: Longint;
  Rect: TRect; State: TGridDrawState);
begin
  if not FUpdating then
  begin
    PreviousBrushColor := FAttachedGrid.Canvas.Brush.Color;
    PreviousPenColor := FAttachedGrid.Canvas.Pen.Color;

    // set the grid font
    if ARow < FAttachedGrid.FixedRows then
      FAttachedGrid.Canvas.Font.Assign(TitleFont)
    else
      FAttachedGrid.Canvas.Font.Assign(FAttachedGrid.Font);

    if assigned(PreviousDrawCell) then
      PreviousDrawCell(Sender, ACol, ARow, Rect, State);

    // fill the cell grid with its color
    if gdFixed in State then
    begin
      FAttachedGrid.Canvas.Brush.Color := FAttachedGrid.FixedColor
    end
    else
    begin
      if gdFocused in State then
      begin
        if not (goRowSelect in FAttachedGrid.Options) then
          FAttachedGrid.Canvas.Brush.Color := FAttachedGrid.Color
        else
        begin
          FAttachedGrid.Canvas.Font.Color := clHighlightText;
          FAttachedGrid.Canvas.Brush.Color := clHighlight;
        end;
      end
      else
      begin
        if (gdSelected in State) or (ARow = LineIndex) then
        begin
          FAttachedGrid.Canvas.Font.Color := clHighlightText;
          FAttachedGrid.Canvas.Brush.Color := clHighlight
        end
        else
        begin
          if (ARow - FAttachedGrid.FixedRows + 1) mod 2 = 1 then
            FAttachedGrid.Canvas.Brush.Color := OddColor
          else
            FAttachedGrid.Canvas.Brush.Color := EvenColor;
        end;
      end;
    end;

    FAttachedGrid.Canvas.FillRect(Rect);

    RectAux := Rect;
    RectAux.TopLeft := Point(Rect.Left + 2, Rect.Top + 2);
    RectAux.BottomRight := Point(Rect.Right - 2, Rect.Bottom - 2);
    Text := FAttachedGrid.Cells[ACol, ARow];

    if gdFixed in State then
    begin
      // 3D
      FAttachedGrid.Canvas.Pen.Color := clBlack;
      FAttachedGrid.Canvas.PenPos := Point(Rect.Left, Rect.Bottom);
      FAttachedGrid.Canvas.LineTo(Rect.Right, Rect.Bottom);
      FAttachedGrid.Canvas.LineTo(Rect.Right, Rect.Top);
      {$ifndef fpc}
      if FAttachedGrid.Ctl3D then
      begin
        FAttachedGrid.Canvas.Pen.Color := clWhite;
        FAttachedGrid.Canvas.PenPos := Point(Rect.Left, Rect.Bottom - 1);
        FAttachedGrid.Canvas.LineTo(Rect.Left, Rect.Top);
        FAttachedGrid.Canvas.LineTo(Rect.Right - 1, Rect.Top);
        FAttachedGrid.Canvas.Pen.Color := clGray;
        FAttachedGrid.Canvas.PenPos := Point(Rect.Left + 1, Rect.Bottom - 1);
        FAttachedGrid.Canvas.LineTo(Rect.Right - 1, Rect.Bottom - 1);
        FAttachedGrid.Canvas.LineTo(Rect.Right - 1, Rect.Top - 1);
      end;
      {$endif}
    end;

    if not ((FIndicator) and (ACol = 0)) then
      ColumnIndex := GridToColumnIndex(ACol);

    // drawing the images
    if Length(arrImageColumn) > 0 then
    begin
      if not ((FIndicator) and (ACol = 0)) then
      begin
        iCont := Low(arrImageColumn);
        while iCont <= High(arrImageColumn) do
        begin
          if arrImageColumn[iCont].ColumnIndex = ColumnIndex then
          begin
            // we should paint an image here
            DotCommaPos := Pos(';', AttachedGrid.Cells[ACol, ARow]);
            if (DotCommaPos <> 0) and (TryStrToInt(Copy(AttachedGrid.Cells[ACol, ARow], 1, DotCommaPos - 1), ImageIndex)) then
            begin
              Text := Copy(Text, DotCommaPos + 1, Length(Text) - DotCommaPos);
              if ImageIndex < ImageList.Count then
              begin
                RectAux.Right := RectAux.Left + arrImageColumn[iCont].Width;
                RectAux.Bottom := RectAux.Top + arrImageColumn[iCont].Height;
                if RectAux.Right > Rect.Right then
                  RectAux.Right := Rect.Right;
                if RectAux.Bottom > Rect.Bottom then
                  RectAux.Bottom := Rect.Bottom;
                AttachedGrid.Canvas.StretchDraw(RectAux, TBitmap(ImageList.Items[ImageIndex]));
                RectAux.Left := RectAux.Right;
                RectAux.Right := Rect.Right;
              end;
            end;
          end;
          iCont := iCont + 1;
        end;
      end;
    end;

    // drawing the checkbox
    if (FAttachedGrid.Cells[ACol, ARow] = GRID_VALUE_TRUE) or
          (FAttachedGrid.Cells[ACol, ARow] = GRID_VALUE_FALSE) then
    begin
      PreviousPenColor := FAttachedGrid.Canvas.Pen.Color;

      FAttachedGrid.Canvas.Pen.Color := clBlack;
      FAttachedGrid.Canvas.FillRect(RectAux);
      FAttachedGrid.Canvas.Brush.Color := clWhite;

      Rect.Left := Rect.Left + ((Rect.Right - Rect.Left) div 2) - (12 div 2);
      Rect.Right := Rect.Left + 12;
      Rect.Top := Rect.Top + ((Rect.Bottom - Rect.Top) div 2) - (12 div 2);
      Rect.Bottom := Rect.Top + 12;

      FAttachedGrid.Canvas.Rectangle(Rect);
      Rect.Top := Rect.Top + 1;
      Rect.Left := Rect.Left + 1;
      Rect.Bottom := Rect.Bottom - 1;
      Rect.Right := Rect.Right - 1;
      FAttachedGrid.Canvas.Brush.Color := clWhite;

      if FAttachedGrid.Cells[ACol, ARow] = GRID_VALUE_TRUE then
      begin
        FAttachedGrid.Canvas.Draw(Rect.Left + 1, Rect.Top + 1, BitMapCheckBox);
      end;
    end
    else
    begin
      if TextPos(ColumnIndex) = ptLeft then
        FAttachedGrid.Canvas.TextRect(RectAux, RectAux.Left, RectAux.Top, Text)
      else
        FAttachedGrid.Canvas.TextRect(RectAux, RectAux.Right - FAttachedGrid.Canvas.TextWidth(Text) - 3, RectAux.Top, Text)
    end;

    if (Indicator) and (not IsEmpty) then
    begin
      if (ACol = 0) and (ARow = LineIndex) then
      begin
        FAttachedGrid.Canvas.Draw(Rect.Left, Rect.Top + 3, BitMapIndicator);
      end;
    end;

    FAttachedGrid.Canvas.Brush.Color := PreviousBrushColor;
    FAttachedGrid.Canvas.Pen.Color := PreviousPenColor;
  end;
end;

(**************************************************************************************
* GridMouseDown
* This is the OnGridMouseDown event procedure the Table Grid will use to behave like it
* should. If the component has its own GridMouseDown event, it will be run after the
* Table Grid runs its transformations.
***************************************************************************************)
procedure TTableGrid.GridMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  auxRect: TRect;
  Col, Row: Integer;
  bUpdate: Boolean;
begin
  Sorted := False;

  if (FAttachedGrid.FixedRows >= 1) and (Y <= FAttachedGrid.RowHeights[0]) and
     (FAttachedGrid.Cursor <> crHSplit) then
  begin
    FAttachedGrid.MouseToCell(X, Y, Col, Row);
    if (Col >= FAttachedGrid.FixedCols) then
    begin
      Sorted := True;
      if ( FAllowColumnReordering ) or (FAllowSorting) then
      begin
        auxRect := FAttachedGrid.CellRect(Col, 0);
        if assigned(TitleFont) then
          SmartLabel.Font.Assign(TitleFont)
        else
          SmartLabel.Font.Assign(FAttachedGrid.Font);
        SmartLabel.Font.Style := SmartLabel.Font.Style + [fsBold];
        SmartLabel.Color := FAttachedGrid.FixedColor;
        SmartLabel.Caption := FAttachedGrid.Cells[Col, 0];
        SmartLabel.Left := auxRect.Left;
        SmartLabel.Width := FAttachedGrid.ColWidths[Col] + 1;
        SmartLabel.Top := 0;
        SmartLabel.Height := FAttachedGrid.RowHeights[0] + 1;
        SmartLabel.BringToFront;
        SmartLabel.Visible := True;
        SelectedColumn := Col;
        ClickedOnTitle := True;
        XPrev := X;
        YPrev := Y;
      end;
    end;
  end
  else
  begin
    FAttachedGrid.MouseToCell(X, Y, Col, Row);
    if (FEditing) and (not (FReadOnly)) and
         (Length(arrEditingColumn) > 0) and
         (RowCount > 0 )  then
    begin
      iCont := Low(arrEditingColumn);
      while iCont <= High(arrEditingColumn) do
      begin
        bUpdate := True;
        if ColumnIndexToGrid(arrEditingColumn[iCont].ColumnIndex) = Col then
        begin
          if FAttachedGrid.Cells[Col, Row] = GRID_VALUE_TRUE then
            FAttachedGrid.Cells[Col, Row] := GRID_VALUE_FALSE
          else if FAttachedGrid.Cells[Col, Row] = GRID_VALUE_FALSE then
            FAttachedGrid.Cells[Col, Row] := GRID_VALUE_TRUE
          else if (arrEditingColumn[iCont].GridEditType = teCheckBox) and (Trim(FAttachedGrid.Cells[Col, Row]) = '') then
            FAttachedGrid.Cells[Col, Row] := GRID_VALUE_TRUE
          else
            bUpdate := False;

          if (bUpdate) and (assigned(OnUpdateColumn)) then
            OnUpdateColumn( self, LineIndexToGrid(Row), ColumnIndexToGrid(Col) );
          break;
        end;
        inc(iCont);
      end;
    end;
  end;

  if assigned(PreviousMouseDown) then
    PreviousMouseDown(Sender, Button, Shift, X, Y);
end;

(**************************************************************************************
* GridMouseUp
* This is the OnGridMouseUp event procedure the Table Grid will use to behave like it
* should. If the component has its own GridMouseUp event, it will be run before the
* Table Grid runs its transformations.
***************************************************************************************)
procedure TTableGrid.GridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Col, Row: Integer;
begin
  if assigned(PreviousMouseUp) then
    PreviousMouseUp(Sender, Button, Shift, X, Y);

  if Sorted then
  begin
    FAttachedGrid.MouseToCell(SmartLabel.Left, 0, Col, Row);
    if (Col <> SelectedColumn) and ((not Indicator) or ((Indicator) and (ColAux <> 0))) then
    begin
      if (Col >= 0) and (SelectedColumn >= 0) then
        if FAllowColumnReordering then
          SwapColumns(SelectedColumn, Col);
    end
    else
    begin
      if (SmartLabel.Left >= FAttachedGrid.CellRect(Col, Row).Left - 3) and
         (SmartLabel.Left <= FAttachedGrid.CellRect(Col, Row).Left + 3) then
        if FAllowSorting then
          SortInnerGrid(GridToColumnIndex(SelectedColumn));
    end;
    Sorted := False;
    SmartLabel.Visible := False;
    if FImageArrow.Visible then
      FImageArrow.Visible := False;
  end;
end;

(**************************************************************************************
* GridMouseMove
* This is the OnGridMouseMove event procedure the Table Grid will use to behave like it
* should. If the component has its own GridMouseMove event, it will be run after the
* Table Grid runs its transformations.
***************************************************************************************)
procedure TTableGrid.GridMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if (Sorted) and (FAllowColumnReordering) then
  begin
    SmartLabel.Left := SmartLabel.Left + (X - XPrev);
    //SmartLabel.Top := SmartLabel.Top + (Y - YPrev);
    XPrev := X;
    YPrev := Y;

    FAttachedGrid.MouseToCell(SmartLabel.Left, 0, ColAux, RowAux);
    if (ColAux >= 0) and (ColAux <> SelectedColumn) and ((not Indicator) or ((Indicator) and (ColAux <> 0))) then
    begin
      FImageArrow.Visible := True;
      FImageArrow.Top := FAttachedGrid.Top - FImageArrow.Height - 2;

      FImageArrow.Left := (FAttachedGrid.CellRect(ColAux, RowAux).Left) - (FImageArrow.Width div 2) + FAttachedGrid.Left;
    end
    else
      FImageArrow.Visible := False;
  end;
  if assigned(PreviousMouseMove) then
    PreviousMouseMove(Sender, Shift, X, Y);
end;


(**************************************************************************************
* GridSelectCell
* This is the OnGridSelectCell event procedure the Table Grid will use to behave like it
* should. If the component has its own GridSelectCell event, it will be run before the
* Table Grid runs its transformations.
***************************************************************************************)
procedure TTableGrid.GridSelectCell(Sender: TObject; ACol, ARow: Longint;
  var CanSelect: Boolean);
var
  Rect: TRect;
  xLine: Integer;
  i: Integer;
  bFound: Boolean;
begin
  if FUpdating then
    exit;

  if assigned(PreviousSelectCell) then
    PreviousSelectCell(Sender, ACol, ARow, CanSelect);

  if (ARow <> LineIndex) and (LineIndex >= 0) then
  begin
    LineIndex := ARow;
    AttachedGrid.Refresh;
  end;

  if not FUpdating then
  begin
    ClickedOnTitle := False;

    // process the editing cells
    if FEditing then
    begin
      ColumnIndex := GridToColumnIndex(ACol);
      bFound := False;
      for i := Low(arrEditingColumn) to High(arrEditingColumn) do
      begin
        if arrEditingColumn[i].ColumnIndex = ColumnIndex then
        begin
          bFound := True;
          case arrEditingColumn[i].GridEditType of
            teCheckBox:
            begin
              FAttachedGrid.Options := FAttachedGrid.Options - [goEditing];
            end;
            teString, teNumeric:
            begin
              if not FReadOnly then
                FAttachedGrid.Options := FAttachedGrid.Options + [goEditing];
            end;
          end;
          break;
        end;
      end;

      if not bFound then
      begin
        FAttachedGrid.Options := FAttachedGrid.Options - [goEditing];
      end;
    end;
  end;
  LineIndex := ARow;
end;

(**************************************************************************************
* SortInnerGrid
* This procedure will sort the grid.
* Used internally. Use SortGrid instead.
* In values:
* Column: Integer - The column index where the grid will use for sorting comparison.
* Ascending: Boolean - A value of true will sort ascending, a value of false will
*                      sort descending.
* DoNotAllowReversing: Boolean - If set to true, the sort will not allow reversing
*       the direction of the sorting. If set to false, the sorting direction will
*       keep changing from ascending to descending to ascending and so on if
*       the procedure is called multiple times.
***************************************************************************************)
procedure TTableGrid.SortInnerGrid(Column: Integer; Ascending: Boolean; DoNotAllowReversing: Boolean);
var
  i: Integer;
  bFind, bReverse: Boolean;
begin
  // convert TTableGrid column to StringGrid column
  bFind := False;
  bReverse := False;
  for i := 0 to SortedColumnsList.Count - 1 do
  begin
    if Column = StrToInt(SortedColumnsList.Strings[i]) then
    begin
      bFind := True;
      if i <> 0 then
      begin
        SortedColumnsList.Delete(i);
        SortedColumnsList.Insert(0, IntToStr(Column));
      end
      else
      begin
        bReverse := True;
      end;
      break;
    end;
  end;
  if not bFind then
    SortedColumnsList.Insert(0, IntToStr(Column));

  Application.ProcessMessages;
  if ( bReverse ) and ( not DoNotAllowReversing )then
    ReverseLines
  else
    QuickSortRec(LineIndexToGrid(1), FAttachedGrid.RowCount - 1);
end;

(**************************************************************************************
* QuickSortRec
* Recurring algorithm to sort a list. Used internally only, use SortGrid instead.
***************************************************************************************)
procedure TTableGrid.QuickSortRec(First, Last: Integer);
var
  i, j: Integer;
  Pivot: Integer;
  AuxStr: TStringList;
  obj1, obj2: TObject;
begin
  AuxStr := TStringList.Create;
  repeat
    I := First;
    J := Last;
    Pivot := (First + Last) shr 1;
    repeat
      while CompareLines(I, Pivot) < 0 do Inc(I);
      while CompareLines(J, Pivot) > 0 do Dec(J);
      if I <= J then
      begin
        obj1 := FAttachedGrid.Objects[0, I];
        obj2 := FAttachedGrid.Objects[0, J];

        AuxStr.Clear;
        AuxStr.AddStrings(FAttachedGrid.Rows[I]);
        FAttachedGrid.Rows[I].Text := FAttachedGrid.Rows[J].Text;
        FAttachedGrid.Rows[J].Text := AuxStr.Text;

        FAttachedGrid.Objects[0, I] := obj2;
        FAttachedGrid.Objects[0, J] := obj1;

        if Pivot = I then
          Pivot := J
        else if Pivot = J then
          Pivot := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if First < J then QuickSortRec(First, J);
    First := I;
  until I >= Last;
  AuxStr.Free;
end;

(**************************************************************************************
* GridDoubleClick
* This is the OnGridDoubleClick event procedure. For now, it only calls the original
* DoubleClick for the StringGrid component.
***************************************************************************************)
procedure TTableGrid.GridDoubleClick(Sender: TObject);
begin
  if (not ClickedOnTitle) and (assigned(PreviousDoubleClick)) then
    PreviousDoubleClick(Sender);
end;

(**************************************************************************************
* IsEmpty
* This function returns if the grid has no data.
*
***************************************************************************************)
function TTableGrid.IsEmpty: Boolean;
var
  i: Integer;
begin
  if FAttachedGrid.RowCount > FAttachedGrid.FixedRows + 1 then
    result := False
  else
  begin
    result := True;
    if not FUpdating then
    begin
      for i := 0 to FAttachedGrid.ColCount - 1 do
      begin
        result := True;
        if Trim(FAttachedGrid.Cells[i, FAttachedGrid.FixedRows]) <> '' then
        begin
          result := False;
          break;
        end;
      end;
    end
    else
    begin
      if Length(arrUpdateType) > 0 then
        result := False
      else
        result := True;
    end;
  end;
end;

(**************************************************************************************
* BuildCheckBoxImage
* Used internally. Builds up the checkbox bitmap by code.
***************************************************************************************)
procedure TTableGrid.BuildCheckBoxImage;
begin
  BitMapCheckBox := TBitmap.Create;
  BitMapCheckBox.Width := 9;
  BitMapCheckBox.Height := 9;
  BitMapCheckBox.Canvas.Brush.Color:= clWhite;
  BitMapCheckBox.Canvas.FillRect(Rect( 0,0,BitMapCheckBox.Width, BitMapCheckBox.Height ));

  with BitMapCheckBox do
  begin
    Canvas.Pixels[1, 2] := clBlack;
    Canvas.Pixels[1, 3] := clBlack;
    Canvas.Pixels[1, 4] := clBlack;
    Canvas.Pixels[2, 3] := clBlack;
    Canvas.Pixels[2, 4] := clBlack;
    Canvas.Pixels[2, 5] := clBlack;
    Canvas.Pixels[3, 4] := clBlack;
    Canvas.Pixels[3, 5] := clBlack;
    Canvas.Pixels[3, 6] := clBlack;
    Canvas.Pixels[4, 3] := clBlack;
    Canvas.Pixels[4, 4] := clBlack;
    Canvas.Pixels[4, 5] := clBlack;
    Canvas.Pixels[5, 2] := clBlack;
    Canvas.Pixels[5, 3] := clBlack;
    Canvas.Pixels[5, 4] := clBlack;
    Canvas.Pixels[6, 1] := clBlack;
    Canvas.Pixels[6, 2] := clBlack;
    Canvas.Pixels[6, 3] := clBlack;
    Canvas.Pixels[7, 0] := clBlack;
    Canvas.Pixels[7, 1] := clBlack;
    Canvas.Pixels[7, 2] := clBlack;
  end;
end;

(**************************************************************************************
* BuildIndicatorImage
* Used internally. Builds up the indicator bitmap by code.
***************************************************************************************)
procedure TTableGrid.BuildIndicatorImage;
var
  dx,dy, x, y: Integer;
  Opt: TDataSetState;
  ACanvas: TCanvas;
  R: TRect;
  MultiSel: Boolean;
  procedure CenterY;
  begin
    y := R.Top + (R.Bottom-R.Top) div 2;
  end;
  procedure CenterX;
  begin
    X := R.Left + (R.Right-R.Left) div 2;
  end;
  procedure DrawEdit(clr: Tcolor);
  begin
    ACanvas.Pen.Color := clr;
    CenterY;
    CenterX;
    ACanvas.MoveTo(X-2, Y-Dy);
    ACanvas.LineTo(X+3, Y-Dy);
    ACanvas.MoveTo(X, Y-Dy);
    ACanvas.LineTo(X, Y+Dy);
    ACanvas.MoveTo(X-2, Y+Dy);
    ACanvas.LineTo(X+3, Y+Dy);
  end;
begin
  BitMapIndicator := TBitmap.Create;
  BitMapIndicator.Width:= 12;
  BitMapIndicator.Height:= 16;
  ACanvas := BitMapIndicator.Canvas;
  R := Rect(0,0,BitMapIndicator.Width -1,BitMapIndicator.Height -1);
  ACanvas.Brush.Color:= clWhite;
  ACanvas.FillRect(R);
  MultiSel:= goRowSelect in AttachedGrid.Options;

  dx := 6;
  dy := 6;
  ACanvas.Brush.Color:=clBlack;
  ACanvas.Pen.Color:=clBlack;
  CenterY;
  x:= R.Left+3;
  ACanvas.Polygon([point(x,y-dy),point(x+dx,y),point(x, y+dy),point(x,y-dy)]);
  BitMapIndicator.TransparentColor:= clWhite;
  BitMapIndicator.TransparentMode:=tmFixed;
  BitMapIndicator.Transparent := True;
end;

(**************************************************************************************
* UpdateLine
* This procedure will update the selected line in the grid. All new information will
* overwrite previously written data.
* In values:
* Line: TStringList - Each line of the string list represents a column of the grid.
***************************************************************************************)
procedure TTableGrid.UpdateLine(Line: TStringList);
begin
  UpdateLine(Line, GridToLineIndex(FAttachedGrid.Row));
end;

(**************************************************************************************
* UpdateLine
* This procedure will update the selected line in the grid. All new information will
* overwrite previously written data.
* In values:
* Line: array of String - Each element of the array represents a column of the grid.
***************************************************************************************)
procedure TTableGrid.UpdateLine(Line: array of String);
var
  List: TStringList;
  i: Integer;
begin
  List := TStringList.Create;
  try
    for i := Low(Line) to High(Line) do
      List.Add(Line[i]);
    UpdateLine(List);
  finally
    List.Free;
  end;
end;

(**************************************************************************************
* RegisterImageColumn
* This will register a column, indicating that a image will be loaded in its cells.
* To load the image, insert anywhere you wish the index of the image following a
* comma.
* Example: '0; Ok' // Where is '0;' it will be replaced by the registered image
*                                   at index 0.
* In values:
* ColumnIndex: Integer - The column where you wish to use an image.
* Width: Integer - Default width of the image.
* Height: Integer - Default height of the image.
***************************************************************************************)
procedure TTableGrid.RegisterImageColumn(ColumnIndex, Width,
  Height: Integer);
var
  aRecImageColumn: RecImageColumn;
begin
  aRecImageColumn.ColumnIndex := ColumnIndex;
  aRecImageColumn.Width := Width;
  aRecImageColumn.Height := Height;
  SetLength(arrImageColumn, Length(arrImageColumn) + 1);
  arrImageColumn[High(arrImageColumn)] := aRecImageColumn;
end;

(**************************************************************************************
* RegisterImage
* This will register an image in the ImageList. You can use its index to load the image
* in the cell grid of a registered image column <see RegisterImageColumn>.
* In values:
* Bitmap: TBitmap - The image that can be loaded in a cell grid.
* Result: Integer - The bitmap index in the ImageList. Use this index to load this
* image in the grid.
***************************************************************************************)
function TTableGrid.RegisterImage(Bitmap: TBitmap): Integer;
begin
  result := ImageList.Add(Bitmap);
end;

(**************************************************************************************
* CompareLines
* This is used during sorting the grid. Used internally only, use SortGrid instead.
***************************************************************************************)
function TTableGrid.CompareLines(Line1, Line2: Integer): Integer;
var
  i, Column, aux1, aux2: Integer;
  aux3, aux4: Extended;
begin
  result := 0;
  for i := 0 to SortedColumnsList.Count - 1 do
  begin
    Column := ColumnIndexToGrid(StrToInt(SortedColumnsList.Strings[i]));
    if GridEditType( StrToInt(SortedColumnsList.Strings[i]) ) = teNumeric then
    begin
      if not TryStrToFloat(NumbersOnly(FAttachedGrid.Cells[Column, Line1]), aux3) then
        aux3 := 0;
      if not TryStrToFloat(NumbersOnly(FAttachedGrid.Cells[Column, Line2]), aux4) then
        aux4 := 0;
      result := round( aux3 - aux4 );
    end
    else
    begin
      if TryStrToInt(FAttachedGrid.Cells[Column, Line1], aux1) and
           TryStrToInt(FAttachedGrid.Cells[Column, Line2], aux2) then
        result := aux1 - aux2
      else
      begin
        if FAttachedGrid.Cells[Column, Line1] < FAttachedGrid.Cells[Column, Line2] then
          result := -1
        else if FAttachedGrid.Cells[Column, Line1] > FAttachedGrid.Cells[Column, Line2] then
          result := 1;
      end;
    end;
    if result <> 0 then
      break;
  end;
end;

(**************************************************************************************
* ReverseLines
* This is used during sorting the grid. Used internally only, use SortGrid instead.
***************************************************************************************)
procedure TTableGrid.ReverseLines;
var
  i, j, RowCountToBeSorted: Integer;
  AuxStr: TStringList;
  obj1, obj2: TObject;
begin
  AuxStr := TStringList.Create;
  RowCountToBeSorted := Floor((FAttachedGrid.RowCount - FAttachedGrid.FixedRows) / 2);
  i := 0;
  while i < RowCountToBeSorted do
  begin
    j := FAttachedGrid.RowCount - i - 1;
    obj1 := FAttachedGrid.Objects[0, I + FAttachedGrid.FixedRows];
    obj2 := FAttachedGrid.Objects[0, J];

    AuxStr.Clear;
    AuxStr.AddStrings(FAttachedGrid.Rows[I + FAttachedGrid.FixedRows]);
    FAttachedGrid.Rows[i + FAttachedGrid.FixedRows].Text := FAttachedGrid.Rows[J].Text;
    FAttachedGrid.Rows[J].Text := AuxStr.Text;

    FAttachedGrid.Objects[0, I + FAttachedGrid.FixedRows] := obj2;
    FAttachedGrid.Objects[0, J] := obj1;

    inc(i);
  end;
  AuxStr.Free;
end;

(**************************************************************************************
* BeginEdit
* When called, the grid will allow the user to insert information in the grid, only
* on the columns registered in the RegisterEditField procedure.
* When you're ready to finish editing and stop allowing editing on the grid, call the
* EndEdit procedure.
***************************************************************************************)
procedure TTableGrid.BeginEdit;
begin
  if not FEditing then
  begin
    PreviousOptions := FAttachedGrid.Options;
    FAttachedGrid.Options := FAttachedGrid.Options - [goRowSelect];
  end;

  FEditing := True;
end;

(**************************************************************************************
* RegisterEditField
* This registers a column of the grid to allow user input. The user will be allowed
* to edit the grid after the BeginEdit call. This procedure can be called multiple
* times, thus you can have more than one columns that can be edited.
* The different ways on how the user can edit is specified by the GridEditType
* parameter.
* In values:
* ColumnIndex: Integer - The column that can be edited by the user.
* GridEditType: TGridEditType - Specify how the edit will be done. Possible values are:
*               - teString: Text editing.
*               - teCheckBox: Check box editing, allows true or false results.
*                             When value is true, the result is GRID_VALUE_TRUE.
*                             When value is false, the result is GRID_VALUE_FALSE.
*               - teNumeric: Text editing, but allows only numeric values.
*               - teNone: No specific type specified, will use the component default
*                         editing.
***************************************************************************************)
procedure TTableGrid.RegisterEditField(ColumnIndex: Integer;
  GridEditType: TGridEditType);
var
  aGridEditType: RecGridEditType;
begin
  aGridEditType.ColumnIndex := ColumnIndex;
  aGridEditType.GridEditType := GridEditType;
  SetLength(arrEditingColumn, Length(arrEditingColumn) + 1);
  arrEditingColumn[High(arrEditingColumn)] := aGridEditType;
end;

(**************************************************************************************
* EndEdit
* After this call, the grid will not allow the user to directly edit the grid.
***************************************************************************************)
procedure TTableGrid.EndEdit;
begin
  if FEditing then
  begin
    FEditing := False; // the command below activate the SelectCell
    FAttachedGrid.Options := PreviousOptions;
  end
  else
    FEditing := False;
end;

(**************************************************************************************
* GridToColumnIndex
* Used internally. Converts the column String Grid index to table grid index.
***************************************************************************************)
function TTableGrid.GridToColumnIndex(Value: Integer): Integer;
begin
  if FIndicator then
    result := StrToInt(SwapSortedColumnsList.Strings[Value - 1])
  else
    Result := StrToInt(SwapSortedColumnsList.Strings[Value]);
end;

function TTableGrid.ReadFieldEditType(Value: Integer): TGridEditType;
begin
  Result := GridEditType( Value );  // provisório
end;

(**************************************************************************************
* ColumnIndexToGrid
* Used internally. Converts the column table grid index to String Grid index.
***************************************************************************************)
function TTableGrid.ColumnIndexToGrid(Value: Integer): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to SwapSortedColumnsList.Count - 1 do
  begin
    if StrToInt(SwapSortedColumnsList.Strings[i]) = Value then
    begin
      if FIndicator then
        result := i + 1
      else
        Result := i;
    end;
  end;
end;

(**************************************************************************************
* BeginUpdate
* This will begin the update process of the grid. During the updates, you will be able
* to call all methods of inserting, updating and deleting, however, the grid will not
* visually change untill you commit or rollback the update.
* This way of working is specially useful if you're inserting a good amount of lines
* in the grid. By calling this method, the data will enter all at once, to help avoid
* the flickering effect.
***************************************************************************************)
procedure TTableGrid.BeginUpdate;
begin
  ClearUpdateList;
  if FUpdating then
    RollbackUpdate;
  AuxRowCount := RowCount;
  FUpdating := True;
end;

(**************************************************************************************
* CommitUpdate
* This procedure will commit all updates made after the BeginUpdate call and the user
* will be able to see the data on the Table Grid.
***************************************************************************************)
procedure TTableGrid.CommitUpdate;
var
  i, j: Integer;
  aRec: RecUpdateType;
  LineCount, BeginingUpdate, BeginingIndex: Integer;
  RemovedIndexes: array of Integer;
  bRemoved: Boolean;
begin
  if FUpdating then
  begin
    FUpdating := False;

    // number of lines calculation
    LineCount := RowCount;
    BeginingUpdate := RowCount + 1;
    BeginingIndex := Low(arrUpdateType);
    SetLength(RemovedIndexes, 0);
    for i := Low(arrUpdateType) to High(arrUpdateType) do
    begin
      aRec := arrUpdateType[i];
      case aRec.UpdateType of
        mtInsertLine: inc(LineCount);
        mtRemoveLine:
        begin
          dec(LineCount);
          SetLength(RemovedIndexes, Length(RemovedIndexes) + 1);
          RemovedIndexes[High(RemovedIndexes)] := aRec.RemoveParameter;
        end;
        mtClearGrid:
        begin
          LineCount := 0;
          BeginingUpdate := 1;
          BeginingIndex := i;
          SetLength(RemovedIndexes, 0);
        end;
      end;
    end;

    // update
    if LineCount = 0 then
      Clear
    else
    begin
      RowCount := LineCount;
      for i := BeginingIndex to High(arrUpdateType) do
      begin
        aRec := arrUpdateType[i];
        case aRec.UpdateType of
          mtInsertLine, mtInsertObject:
          begin
            bRemoved := False;
            for j := Low(RemovedIndexes) to High(RemovedIndexes) do
            begin
              if ((aRec.UpdateType = mtInsertLine) and (aRec.bInsertIndex)) or
                  (aRec.UpdateType = mtInsertObject) then
              begin
                if aRec.RemoveParameter = RemovedIndexes[j] then
                  bRemoved := True;
              end
              else
                if BeginingUpdate = RemovedIndexes[j] then
                  bRemoved := True;

              if bRemoved then
              begin
                RemovedIndexes[j] := -1;
                break;
              end;
            end;
            if not bRemoved then
            begin
              if aRec.UpdateType = mtInsertLine then
              begin
                if (aRec.bInsertIndex) then
                  UpdateLine(aRec.InsertParameter, aRec.RemoveParameter)
                else
                  UpdateLine(aRec.InsertParameter, BeginingUpdate);
                inc(BeginingUpdate);
              end
              else
                InsertObject(aRec.RemoveParameter, aRec.anObject);
            end;
          end;
          mtRemoveLine:
          begin
          end;
          mtClearGrid:
          begin
            BeginingUpdate := 1;
          end;
        end;
      end;
    end;
    // Clear list
    ClearUpdateList;
  end;
end;

(**************************************************************************************
* RollbackUpdate
* This procedure will rollback all updates, thus no change will happen in the
* table grid.
***************************************************************************************)
procedure TTableGrid.RollbackUpdate;
begin
  FUpdating := False;
  ClearUpdateList;
end;

(**************************************************************************************
* ClearUpdateList
* All the updates made will be cleared up.
***************************************************************************************)
procedure TTableGrid.ClearUpdateList;
var
  i: Integer;
begin
  for i := Low(arrUpdateType) to High(arrUpdateType) do
  begin
    if arrUpdateType[i].UpdateType = mtInsertLine then
      arrUpdateType[i].InsertParameter.Free;
  end;
  SetLength(arrUpdateType, 0);
end;

(**************************************************************************************
* GridToLineIndex
* Used internally. Converts the row String grid index to the Table Grid index.
* Use selectedline instead.
***************************************************************************************)
function TTableGrid.GridToLineIndex(Value: Integer): Integer;
begin
  result := Value - FAttachedGrid.FixedRows + 1;
end;

(**************************************************************************************
* LineIndexToGrid
* Used internally. Converts the Table Grid index to the row String grid index.
***************************************************************************************)
function TTableGrid.LineIndexToGrid(Value: Integer): Integer;
begin
  result := Value + FAttachedGrid.FixedRows - 1;
end;

function TTableGrid.ReadHeader(Value: Integer): String;
begin
  Result := FAttachedGrid.Cells[ColumnIndexToGrid( Value ), 0];
end;

procedure TTableGrid.SetColumnSize(i, j: Integer);
begin
  // do nothing
  // provisório, esperando a versão oficial.
end;

(**************************************************************************************
* GridKeyPress
* This is the OnGridKeyPress event procedure the Table Grid will use to behave like it
* should. If the component has its own GridKeyPress event, it will be run before the
* Table Grid runs its transformations.
***************************************************************************************)
procedure TTableGrid.GridKeypress(Sender: TObject; var Key: Char);
begin
  if assigned(PreviousKeyPress) then
    PreviousKeyPress(Sender, Key);

  if (FEditing) and (not (FReadOnly)) and ((Key = #13) or (Key = ' ')) and
       (Length(arrEditingColumn) > 0) then
  begin
    iCont := Low(arrEditingColumn);
    while iCont <= High(arrEditingColumn) do
    begin
      if (ColumnIndexToGrid(arrEditingColumn[iCont].ColumnIndex) = FAttachedGrid.Col) and
         (arrEditingColumn[iCont].GridEditType = teCheckBox) then
      begin
        if FAttachedGrid.Cells[FAttachedGrid.Col, FAttachedGrid.Row] = GRID_VALUE_TRUE then
          FAttachedGrid.Cells[FAttachedGrid.Col, FAttachedGrid.Row] :=  GRID_VALUE_FALSE
        else if FAttachedGrid.Cells[FAttachedGrid.Col, FAttachedGrid.Row] = GRID_VALUE_FALSE then
          FAttachedGrid.Cells[FAttachedGrid.Col, FAttachedGrid.Row] := GRID_VALUE_TRUE
        else if Trim(FAttachedGrid.Cells[FAttachedGrid.Col, FAttachedGrid.Row]) = '' then
          FAttachedGrid.Cells[FAttachedGrid.Col, FAttachedGrid.Row] := GRID_VALUE_TRUE;
        if (assigned(OnUpdateColumn)) then
          OnUpdateColumn( self, LineIndexToGrid(FAttachedGrid.Row), ColumnIndexToGrid(FAttachedGrid.Col) );
        break;
      end;
      inc(iCont);
    end;
  end;
end;

(**************************************************************************************
* BuildSwapSortedColumnList
* Used internally.
***************************************************************************************)
procedure TTableGrid.BuildSwapSortedColumnList;
var
  i, Begining: Integer;
begin
  if Indicator then
    Begining := 1
  else
    Begining := 0;
  SwapSortedColumnsList.Clear;
  for i := Begining to FAttachedGrid.ColCount - 1 do
  begin
    if Indicator then
      SwapSortedColumnsList.Add(IntToStr(i))
    else
      SwapSortedColumnsList.Add(IntToStr(i + 1));
  end;
end;

(**************************************************************************************
* SwapColumns
* Use this method to change columns positions.
* In values:
* Col1: The column where the col2 should be placed before.
* Col2: The column to place before the source column.
***************************************************************************************)
procedure TTableGrid.SwapColumns(Col1, Col2: Integer);
var
  Pos: Integer;
  i: Integer;
  Aux: String;
  iAux: Integer;
  slAux: TStringList;
begin
  if Indicator then
    Pos := 1
  else
    Pos := 0;
  slAux := TStringList.Create;

  if Col1 < Col2 then
  begin
    Aux := SwapSortedColumnsList.Strings[Col1 - Pos];
    iAux := FAttachedGrid.ColWidths[Col1];
    slAux.AddStrings(FAttachedGrid.Cols[Col1]);
    for i := Col1 to Col2 - 1 do
    begin
      SwapSortedColumnsList.Strings[i - Pos] := SwapSortedColumnsList.Strings[i - Pos + 1];
      FAttachedGrid.ColWidths[i] := FAttachedGrid.ColWidths[i + 1];
      FAttachedGrid.Cols[i] := FAttachedGrid.Cols[i + 1];
    end;
    SwapSortedColumnsList.Strings[Col2 - Pos] := Aux;
    FAttachedGrid.ColWidths[Col2] := iAux;
    FAttachedGrid.Cols[Col2] := slAux;
  end
  else
  begin
    Aux := SwapSortedColumnsList.Strings[Col1 - Pos];
    iAux := FAttachedGrid.ColWidths[Col1];
    slAux.AddStrings(FAttachedGrid.Cols[Col1]);
    for i := Col1 downto Col2 + 1 do
    begin
      SwapSortedColumnsList.Strings[i - Pos] := SwapSortedColumnsList.Strings[i - Pos - 1];
      FAttachedGrid.ColWidths[i] := FAttachedGrid.ColWidths[i - 1];
      FAttachedGrid.Cols[i] := FAttachedGrid.Cols[i - 1];
    end;
    SwapSortedColumnsList.Strings[Col2 - Pos] := Aux;
    FAttachedGrid.ColWidths[Col2] := iAux;
    FAttachedGrid.Cols[Col2] := slAux;
  end;

  slAux.Free;
end;

(**************************************************************************************
* BuildArrowImage
* Used internally. Builds up the arrow bitmap by code.
***************************************************************************************)
procedure TTableGrid.BuildArrowImage;
var
  Rect: TRect;
  Polygon: array of TPoint;
begin
  BitmapArrow := TBitmap.Create;
  BitmapArrow.Width := 9;
  BitmapArrow.Height:= 12;
  Rect.TopLeft := Point(0,0);
  Rect.BottomRight := Point(BitmapArrow.Width, BitmapArrow.Height);
  BitmapArrow.Canvas.Pen.Color := clBlack;
  BitmapArrow.Canvas.Brush.Color := clBlue;
  BitmapArrow.Canvas.FillRect(Rect);
  SetLength(Polygon, 7);
  Polygon[0] := Point((BitmapArrow.Width - 1) div 4, 0);
  Polygon[1] := Point(Polygon[0].X, BitmapArrow.Height - 1 - ((BitmapArrow.Height - 1) div 2));
  Polygon[2] := Point(0, Polygon[1].Y);
  Polygon[3] := Point(BitmapArrow.Width div 2, BitmapArrow.Height - 1);
  Polygon[4] := Point(BitmapArrow.Width - 1, Polygon[2].Y);
  Polygon[5] := Point(BitmapArrow.Width - 1 - ((BitmapArrow.Width - 1) div 4), Polygon[4].Y);
  Polygon[6] := Point(Polygon[5].X, 0);

  BitmapArrow.Canvas.Brush.Color := clYellow;
  BitmapArrow.Canvas.Polygon(Polygon);
  BitmapArrow.TransparentColor := clBlue;
  BitmapArrow.Transparent := True;

  FImageArrow := TImage.Create(FAttachedGrid.Parent);
  FImageArrow.AutoSize := True;
  FImageArrow.Parent := FAttachedGrid.Parent;
  FImageArrow.Picture.Bitmap := BitmapArrow;
  BitmapArrow.Free;
  FImageArrow.Visible := False;
  FImageArrow.Transparent := True;
end;

(**************************************************************************************
* SetColWidth
* This procedure sets up the width of the defined column.
* In values:
* ColumnIndex: The index of the column.
* Width: The new width of the column.
***************************************************************************************)
procedure TTableGrid.SetColWidth(ColumnIndex, Width: Integer);
begin
  FAttachedGrid.ColWidths[ColumnIndexToGrid(ColumnIndex)] := Width;
end;

(**************************************************************************************
* ReadColumnSize
* This procedure reads the width of the defined column.
* In values:
* ColumnIndex: The index of the column.
* Result: The current width of the column.
***************************************************************************************)
function TTableGrid.ReadColumnSize(ColumnIndex: Integer): Integer;
begin
  result := FAttachedGrid.ColWidths[ColumnIndexToGrid(ColumnIndex)];
end;

(**************************************************************************************
* UpdateCell
* Updates the value in the desired cell.
* In values:
* Col: The index of the column.
* Line: The index of the line.
* Value: The new text of the cell.
***************************************************************************************)
procedure TTableGrid.UpdateCell(Col, Line: Integer; Value: String);
begin
  if (Line > 0) and (Col > 0) then
    FAttachedGrid.Cells[ColumnIndexToGrid(Col), LineIndexToGrid(Line)] := Value;
end;

(**************************************************************************************
* ReadCell
* Reads the value in the desired cell.
* In values:
* Col: The index of the column.
* Line: The index of the line.
* Result: The current text of the cell.
***************************************************************************************)
function TTableGrid.ReadCell(Col: Integer; Line: Integer): String;
begin
  result := FAttachedGrid.Cells[ColumnIndexToGrid(Col), LineIndexToGrid(Line)];
end;

(**************************************************************************************
* TextPos
* Reads the text formating of the column. Currently, number columns is aligned to
* the right and text columns is aligned to the left.
* In values:
* Column: The index of the column.
* Result: The alignment of the text in the column.
*         ptLeft: Aligned to the left / ptRight: Aligned to the right
***************************************************************************************)
function TTableGrid.TextPos(Column: Integer): TGridTextAlign;
begin
  if GridEditType(Column) = teNumeric then
    Result := ptRight
  else
    Result := ptLeft;
end;

(**************************************************************************************
* GridEditType
* Used internally. Returns the edit type of the column.
* In values:
* Column: The index of the column.
* Result: The edit type of the column.
***************************************************************************************)
function TTableGrid.GridEditType(Column: Integer): TGridEditType;
var
  i: Integer;
begin
  Result := teNone;
  for i := Low(arrEditingColumn) to High(arrEditingColumn) do
  begin
    if arrEditingColumn[i].ColumnIndex = Column then
    begin
      Result := arrEditingColumn[i].GridEditType;
      break;
    end;
  end;
end;


(**************************************************************************************
* CheckForErros
* This checks if there is any value that is invalid. Currently, only checks
* if columns of numeric type contain values that are not numeric.
* Result: True - error found, and  the error cell is selected / False - no errors found
***************************************************************************************)
function TTableGrid.CheckForErrors: Boolean;
var
  i, j: Integer;
begin
  Result := False;
  for i := Low(arrEditingColumn) to High(arrEditingColumn) do
  begin
    if arrEditingColumn[i].GridEditType = teNumeric then
    begin
      for j := 1 to RowCount do
      begin
        if not ( ValidValue ( ReadCell(arrEditingColumn[i].ColumnIndex,j) ) ) then
        begin
          AttachedGrid.Col := GridToColumnIndex(arrEditingColumn[i].ColumnIndex);
          AttachedGrid.Row := GridToLineIndex(j);
          Result := True;
          break;
        end;
      end;
    end;
    if Result then
      break;
  end;
end;

(**************************************************************************************
* SortGrid
* This procedure will sort the grid.
* In values:
* Column: Integer - The column index where the grid will use for sorting comparison.
* Ascending: Boolean - A value of true will sort ascending, a value of false will
*                      sort descending.
***************************************************************************************)
procedure TTableGrid.SortGrid(Column: Integer; Ascending: Boolean);
begin
  SortInnerGrid ( Column, Ascending, True );
end;

(**************************************************************************************
* FitColumnToGridWidth
* This method will resize the specified column to fill the available space in the grid.
* In values:
* ColumnIndex: Integer - The column index to be resized.
***************************************************************************************)
procedure TTableGrid.FitColumnToGridWidth(ColumnIndex: Integer);
var
  i: Integer;
  TotalSum: Integer;
begin
  TotalSum := 0;
  for i := 0 to AttachedGrid.ColCount - 1 do
  begin
    if i <> ColumnIndexToGrid(ColumnIndex) then
    begin
      TotalSum := TotalSum + FAttachedGrid.ColWidths[i];
    end;
  end;
  if TotalSum < AttachedGrid.ClientWidth then
  begin
    SetColWidth(ColumnIndex, AttachedGrid.ClientWidth - TotalSum - (AttachedGrid.GridLineWidth * AttachedGrid.ColCount));
  end;
end;

(**************************************************************************************
* ReadDataset
* This procedure will make the table grid reflect the contents of the dataset.
* The header will contain all visible columns and the rows of the dataset will contain
* the rows of the grid. Further updates in the Database will not be automatically
* reflect on the grid, you will need to manually update it or call this method again.
* In values:
* Dataset: TDataset - Generic dataset with all the data already set.
***************************************************************************************)
procedure TTableGrid.ReadDataSet(DataSet: TDataset);
var
  Row: TStringList;
  i: Integer;
  Header: array of String;
begin
  Clear;

  Row := TStringList.Create;
  SetLength ( Header, 0 );
  try
    for i := 0 to DataSet.Fields.Count - 1 do
    begin
      if DataSet.Fields.Fields[i].Visible then
      begin
        SetLength ( Header, Length(Header) + 1 );
        Header [ High(Header) ] := DataSet.Fields.Fields[i].DisplayName;
      end;
    end;
    DefineHeader( Header );

    DataSet.First;
    while not DataSet.Eof do
    begin
      Row.Clear;
      for i := 0 to DataSet.Fields.Count - 1 do
      begin
        if DataSet.Fields.Fields[i].Visible then
        begin
          if ( DataSet.Fields.Fields[i] is TBooleanField ) and ( UpperCase ( DataSet.Fields.Fields[i].DisplayText ) <> 'FALSE' ) and ( UpperCase ( DataSet.Fields.Fields[i].DisplayText ) <> 'TRUE' ) then
          begin
            if DataSet.Fields.Fields[i].IsNull then
              Row.Add( '' )
            else if TBooleanField ( DataSet.Fields.Fields[i] ).Value then
              Row.Add( GRID_VALUE_TRUE )
            else
              Row.Add( GRID_VALUE_FALSE )
          end
          else
            Row.Add( DataSet.Fields.Fields[i].DisplayText );
        end;
      end;
      InsertLine( Row );

      DataSet.Next;
    end;
  finally
    Row.Free;
  end;
end;

(**************************************************************************************
* SaveToFile
* This will write the contents of the grid to a text file. Each row of the grid will
* be a different file. Ex.: filename_1.txt, filename_2.txt and so on.
* In values:
* Filename: String - full path to the default filename to be saved in disk.
***************************************************************************************)
procedure TTableGrid.SaveToFile(FileName: String);
var
  i, j: Integer;
  arrStringList: array of TStringList;

  procedure AddStringList;
  begin
    SetLength ( arrStringList, Length(arrStringList) + 1 );
    arrStringList[ High ( arrStringList ) ] := TStringList.Create;
  end;

  procedure DestroyStringLists;
  var
    iLocal: Integer;
  begin
    for iLocal := Low ( arrStringList ) to High ( arrStringList ) do
      arrStringList[iLocal].Free;
  end;

begin
  try
    for i := 0 to ColCount - 1 do
    begin
      AddStringList;
      arrStringList[ i ].Add( AttachedGrid.Cells[ ColumnIndexToGrid( i + 1 ), 0 ] );
    end;

    for i := 1 to RowCount do
    begin
      for j := Low ( arrStringList ) to High ( arrStringList ) do
      begin
        if ( ReadCell( j + 1, i ) = GRID_VALUE_TRUE ) or ( ReadCell( j + 1, i ) = GRID_VALUE_FALSE ) then
        begin
          if ( ReadCell( j + 1, i ) = GRID_VALUE_TRUE ) then
            arrStringList[j].Add( 'X' )
          else
            arrStringList[j].Add( ' ' )
        end
        else
          arrStringList[j].Add( ReadCell( j + 1, i ) );
      end;
    end;

    for i := Low ( arrStringList ) to High ( arrStringList ) do
    begin
      arrStringList[i].SaveToFile( FileName + '_' + IntToStr(i + 1) + '.txt' );
    end;
  finally
    DestroyStringLists;
  end;
end;

// Get and Set sections for the properties

function TTableGrid.GetSortedColumn: Integer;
begin
  if SortedColumnsList.Count = 0 then
    result := 0
  else
    result := StrToInt(SortedColumnsList.Strings[0]);
end;


function TTableGrid.GetEnabled: Boolean;
begin
  Result := FAttachedGrid.Enabled;
end;

procedure TTableGrid.SetEnabled(const Value: Boolean);
begin
  FAttachedGrid.Enabled := Value;
end;

procedure TTableGrid.SetReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;
  FAttachedGrid.Options := FAttachedGrid.Options - [goEditing];
end;

procedure TTableGrid.SetColCount(const Value: Integer);
begin
  if FIndicator then
    FAttachedGrid.ColCount := value + 1
  else
    FAttachedGrid.ColCount := value;
  BuildSwapSortedColumnList;
end;

procedure TTableGrid.SetRowCount(const Value: Integer);
begin
  if Value = 0 then
  begin
    FAttachedGrid.RowCount := FAttachedGrid.FixedRows + 1;
    IsNotEmpty := False;
  end
  else
  begin
    IsNotEmpty := True;
    FAttachedGrid.RowCount := value + FAttachedGrid.FixedRows;
  end;
  SortedColumnsList.Clear;
end;

function TTableGrid.GetColCount: Integer;
begin
  if FIndicator then
    result := FAttachedGrid.ColCount - 1
  else
    result := FAttachedGrid.ColCount;
end;

function TTableGrid.GetRowCount: Integer;
begin
  if not FUpdating then
  begin
    if (not IsNotEmpty) and (IsEmpty) then
      result := 0
    else
      result := FAttachedGrid.RowCount - FAttachedGrid.FixedRows;
  end
  else
    result := AuxRowCount;
end;

procedure TTableGrid.SetIndicator(const Value: Boolean);
var
  i: Integer;
begin
  if (Value <> FIndicator) then
  begin
    CommitUpdate;
    BeginUpdate;
    FIndicator := Value;
    if Value then
    begin
      FAttachedGrid.ColCount := FAttachedGrid.ColCount + 1;
      FAttachedGrid.FixedCols := FAttachedGrid.FixedCols + 1;
      for i := FAttachedGrid.ColCount - 1 downto 1 do
      begin
        FAttachedGrid.ColWidths[i] := FAttachedGrid.ColWidths[i-1];
        FAttachedGrid.Cols[i].Clear;
        FAttachedGrid.Cols[i].AddStrings(FAttachedGrid.Cols[i-1]);
      end;
      FAttachedGrid.Cols[0].Clear;
      FAttachedGrid.ColWidths[0] := 12;
    end
    else
    begin
      FAttachedGrid.FixedCols := FAttachedGrid.FixedCols - 1;
      for i := 0 to FAttachedGrid.ColCount - 2 do
      begin
        FAttachedGrid.ColWidths[i] := FAttachedGrid.ColWidths[i+1];
        FAttachedGrid.Cols[i].Clear;
        FAttachedGrid.Cols[i].AddStrings(FAttachedGrid.Cols[i+1]);
      end;
      FAttachedGrid.ColCount := FAttachedGrid.ColCount - 1;
    end;
    CommitUpdate;
  end;
end;


function TTableGrid.GetSelectedLine: Integer;
begin
  if not IsEmpty then
    Result := GridToLineIndex( FAttachedGrid.Row )
  else
    Result := 0;
end;

procedure TTableGrid.SetSelectedLine(const Value: Integer);
begin
  if Value <= RowCount then
    FAttachedGrid.Row := LineIndexToGrid(Value);
end;

function TTableGrid.GetAllowColumnResizing: Boolean;
begin
  Result := goColSizing in FAttachedGrid.Options;
end;

procedure TTableGrid.SetAllowColumnResizing(const Value: Boolean);
begin
  if Value then
    FAttachedGrid.Options := FAttachedGrid.Options + [goColSizing]
  else
    FAttachedGrid.Options := FAttachedGrid.Options - [goColSizing];
end;

function TTableGrid.GetSelectedCol: Integer;
begin
  if not IsEmpty then
    Result := GridToColumnIndex( FAttachedGrid.Col )
  else
    Result := 0;
end;

procedure TTableGrid.SetSelectedCol(const Value: Integer);
begin
  if Value <= ColCount then
    FAttachedGrid.Col := ColumnIndexToGrid(Value);
end;

function strGridBoolean( Value: Boolean ): String;
begin
  if Value then
    Result := GRID_VALUE_TRUE
  else
    Result := GRID_VALUE_FALSE;
end;

end.
