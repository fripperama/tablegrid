unit ufrmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  ComCtrls, StdCtrls, uTableGrid, IBConnection, sqldb, db;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    Button1: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Button14: TButton;
    Button15: TButton;
    Button16: TButton;
    Button17: TButton;
    Button18: TButton;
    Button19: TButton;
    Button2: TButton;
    Button20: TButton;
    Button21: TButton;
    Button22: TButton;
    Button23: TButton;
    Button24: TButton;
    Button25: TButton;
    Button26: TButton;
    Button27: TButton;
    Button28: TButton;
    Button29: TButton;
    Button3: TButton;
    Button30: TButton;
    Button31: TButton;
    Button32: TButton;
    Button33: TButton;
    Button34: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    chAllowReordering: TCheckBox;
    chAllowResizing: TCheckBox;
    chAllowSorting: TCheckBox;
    ColorDialog1: TColorDialog;
    edName: TEdit;
    edSearchText: TEdit;
    FontDialog1: TFontDialog;
    IBConnection1: TIBConnection;
    ImStatus: TImageList;
    PageControl1: TPageControl;
    aStringGrid: TStringGrid;
    SaveDialog1: TSaveDialog;
    SQLQuery1: TSQLQuery;
    SQLQuery1Address1: TStringField;
    SQLQuery1Email1: TStringField;
    SQLQuery1Id1: TLongintField;
    SQLQuery1Name1: TStringField;
    SQLQuery1Phone1: TStringField;
    SQLQuery1Recomended1: TStringField;
    SQLQuery1Status1: TLongintField;
    SQLTransaction1: TSQLTransaction;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure Button16Click(Sender: TObject);
    procedure Button17Click(Sender: TObject);
    procedure Button18Click(Sender: TObject);
    procedure Button19Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button20Click(Sender: TObject);
    procedure Button21Click(Sender: TObject);
    procedure Button22Click(Sender: TObject);
    procedure Button23Click(Sender: TObject);
    procedure Button24Click(Sender: TObject);
    procedure Button25Click(Sender: TObject);
    procedure Button26Click(Sender: TObject);
    procedure Button27Click(Sender: TObject);
    procedure Button28Click(Sender: TObject);
    procedure Button29Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button30Click(Sender: TObject);
    procedure Button31Click(Sender: TObject);
    procedure Button32Click(Sender: TObject);
    procedure Button33Click(Sender: TObject);
    procedure Button34Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure chAllowReorderingChange(Sender: TObject);
    procedure chAllowResizingChange(Sender: TObject);
    procedure chAllowSortingChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SQLQuery1Recomended1GetText(Sender: TField; var aText: string;
      DisplayText: Boolean);
    procedure SQLQuery1Status1GetText(Sender: TField; var aText: string;
      DisplayText: Boolean);
  private
    { private declarations }
    TableGrid: TTableGrid;
    Count: Integer;
    StOk: Integer;
    StFail: Integer;
    ImApproved: TBitmap;
    ImFail: TBitmap;
    function StatusToString(Status: Integer): String;
  public
    { public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

const
  COL_ID = 1;
  COL_NAME = 2;
  COL_SECTOR = 3;
  COL_PHONE = 4;
  COL_EMAIL = 5;
  COL_CE = 6;
  COL_STATUS = 7;

{ TfrmMain }

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  TableGrid := TTableGrid.Create(aStringGrid);
  StOk := TableGrid.RegisterImage(ImApproved);
  StFail := TableGrid.RegisterImage(ImFail);
end;

procedure TfrmMain.Button20Click(Sender: TObject);
begin
  TableGrid.RegisterEditField( COL_NAME, teString );
end;

procedure TfrmMain.Button21Click(Sender: TObject);
begin
  TableGrid.BeginEdit;
end;

procedure TfrmMain.Button22Click(Sender: TObject);
begin
  TableGrid.RegisterEditField( COL_SECTOR, teString );
end;

procedure TfrmMain.Button23Click(Sender: TObject);
begin
  TableGrid.EndEdit;
end;

procedure TfrmMain.Button24Click(Sender: TObject);
begin
  TableGrid.RegisterEditField( COL_PHONE, teString );
end;

procedure TfrmMain.Button25Click(Sender: TObject);
begin
  TableGrid.InsertLine( [] );
end;

procedure TfrmMain.Button26Click(Sender: TObject);
begin
  // warning, this will only work if you registered the column Id for editing.
  if TableGrid.CheckForErrors then
  begin
    ShowMessage( 'There was an error in the inputing data. Please, check the selected cell in the Grid and ensure it contains only numeric values.' );
  end;
end;

procedure TfrmMain.Button27Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    TableGrid.SaveToFile( SaveDialog1.FileName );
  end;
end;

procedure TfrmMain.Button28Click(Sender: TObject);
begin
  TableGrid.RegisterEditField( COL_EMAIL, teString );
end;

procedure TfrmMain.Button29Click(Sender: TObject);
begin
  // this will search the text in the last sorted column
  TableGrid.Find( edSearchText.Text, TableGrid.SortedColumn );
end;

procedure TfrmMain.Button15Click(Sender: TObject);
begin
  TableGrid.FitColumnToGridWidth( COL_NAME );
end;

procedure TfrmMain.Button16Click(Sender: TObject);
begin
  TableGrid.SwapColumns( 2, 3 );
end;

procedure TfrmMain.Button10Click(Sender: TObject);
begin
  TableGrid.ClearUpdateList;
end;

procedure TfrmMain.Button11Click(Sender: TObject);
begin
  TableGrid.CommitUpdate;
end;

procedure TfrmMain.Button12Click(Sender: TObject);
begin
  TableGrid.RollbackUpdate;
end;

procedure TfrmMain.Button13Click(Sender: TObject);
begin
  TableGrid.UpdateCell( COL_NAME, TableGrid.SelectedLine, edName.Text );
end;

procedure TfrmMain.Button14Click(Sender: TObject);
begin
  TableGrid.RemoveLine;
end;

procedure TfrmMain.Button17Click(Sender: TObject);
var
  i: Integer;
  FormatedCode: String;
begin
  for i := 1 to 1000 do
  begin
    inc(Count);
    FormatedCode := StringOfChar( '0', 5 - Length(IntToStr(Count)) ) + IntToStr(Count);
    TableGrid.InsertLine(
                          [ FormatedCode, 'Line ' + FormatedCode, 'Undefined', ' ', ' ', ' ' ]
                             );
  end;
end;

procedure TfrmMain.Button18Click(Sender: TObject);
begin
  TableGrid.RegisterEditField( COL_ID, teNumeric );
end;

procedure TfrmMain.Button19Click(Sender: TObject);
begin
  TableGrid.RegisterEditField( COL_CE, teCheckBox );
end;

procedure TfrmMain.Button2Click(Sender: TObject);
begin
  TableGrid.Free;
end;

procedure TfrmMain.Button30Click(Sender: TObject);
begin
  if FontDialog1.Execute then
  begin
    TableGrid.TitleFont.Assign(FontDialog1.Font);
    TableGrid.AttachedGrid.RowHeights[0] := (FontDialog1.Font.Height * (-1)) + 6;
    TableGrid.AttachedGrid.Refresh;
  end;
end;

procedure TfrmMain.Button31Click(Sender: TObject);
begin
  IBConnection1.HostName := 'localhost';
  IBConnection1.DatabaseName := 'D:\projeto\delphi\Compartilhado\Classes\Grid Tabela\clientlist.fdb';
  IBConnection1.UserName:= 'SYSDBA';
  IBConnection1.Password:= 'masterkey';
  IBConnection1.Connected := True;
  try
    SQLQuery1.Open;
    try
      TableGrid.ReadDataSet( SQLQuery1 );
      TableGrid.RegisterImageColumn( COL_CE, 16, 16 );
      TableGrid.AttachedGrid.Refresh;
    finally
      SQLQuery1.Close;
    end;
  finally
    IBConnection1.Connected := False;
  end;
end;

procedure TfrmMain.Button32Click(Sender: TObject);
begin
  TableGrid.RegisterImageColumn( COL_STATUS, 16, 16 );
  TableGrid.AttachedGrid.Refresh;
end;

procedure TfrmMain.Button33Click(Sender: TObject);
begin
  if ColorDialog1.Execute then
  begin
    TableGrid.OddColor := ColorDialog1.Color;
    TableGrid.AttachedGrid.Refresh;
  end;
end;

procedure TfrmMain.Button34Click(Sender: TObject);
begin
  if ColorDialog1.Execute then
  begin
    TableGrid.EvenColor := ColorDialog1.Color;
    TableGrid.AttachedGrid.Refresh;
  end;
end;

procedure TfrmMain.Button3Click(Sender: TObject);
begin
  TableGrid.DefineHeader( ['Id Number', 'Full Name', 'Sector', 'Phone', 'E-mail', 'C.E.', 'Status'] );
end;

procedure TfrmMain.Button4Click(Sender: TObject);
begin
  TableGrid.InsertLine( ['035678-2', 'Vidal, Gabriel', 'Researching', '352-4859-88','vidal.gabriel@coldfall.com', GRID_VALUE_TRUE, StatusToString(StOk) ] );
  TableGrid.InsertLine( ['549633-5', 'Serafim, Joselito', 'Purchase', '544-LODERIVER-#','s.9@let.it.be', GRID_VALUE_FALSE, StatusToString(StOk) ] );
  TableGrid.InsertLine( ['A#467.B', 'Pallet, Gurdal', 'Financial Flurry', '4298-1566','palletling@dd.com', GRID_VALUE_FALSE, StatusToString(StOk) ] );
  TableGrid.InsertLine( ['0144.666-52', 'Mordenkain, Luminus', 'Cattering', '246-6969','betlentan@madison.edu', GRID_VALUE_TRUE, StatusToString(StOk) ] );
  TableGrid.InsertLine( ['6523', 'Borgognon, Fritadello', 'Purchase', '2545-0067','carcaroth@mail.me', GRID_VALUE_FALSE, StatusToString(StOk) ] );
  TableGrid.InsertLine( ['6524', 'Warner, Roland', 'Purchase', '2136-5546','rw@usep.edu.im', GRID_VALUE_TRUE, StatusToString(StFail) ] );
end;

procedure TfrmMain.Button5Click(Sender: TObject);
begin
  TableGrid.Clear;
  Count := 0;
end;

procedure TfrmMain.Button6Click(Sender: TObject);
begin
  TableGrid.SortGrid( COL_NAME );
end;

procedure TfrmMain.Button7Click(Sender: TObject);
begin
  TableGrid.SortGrid( COL_SECTOR );
end;

procedure TfrmMain.Button8Click(Sender: TObject);
begin
  TableGrid.SortGrid( COL_NAME );
  TableGrid.SortGrid( COL_SECTOR );
end;

procedure TfrmMain.Button9Click(Sender: TObject);
begin
  TableGrid.BeginUpdate;
  ShowMessage( 'Update begins. Now, try mass populating the grid before commiting.' );
end;

procedure TfrmMain.chAllowReorderingChange(Sender: TObject);
begin
  TableGrid.AllowColumnReordering := chAllowReordering.Checked;
end;

procedure TfrmMain.chAllowResizingChange(Sender: TObject);
begin
  TableGrid.AllowColumnResizing := chAllowResizing.Checked;
end;

procedure TfrmMain.chAllowSortingChange(Sender: TObject);
begin
  TableGrid.AllowSorting := chAllowSorting.Checked;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Count := 0;
  ImApproved := TBitmap.Create;
  ImFail := TBitmap.Create;
  ImStatus.GetBitmap( 0, ImApproved );
  ImStatus.GetBitmap( 1, ImFail );
  ImApproved.Transparent := True;
  ImFail.Transparent := False;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  ImApproved.Free;
  ImFail.Free;
end;

procedure TfrmMain.SQLQuery1Recomended1GetText(Sender: TField;
  var aText: string; DisplayText: Boolean);
begin
  DisplayText := True;
  if Sender.AsString = 'Y' then
    aText := GRID_VALUE_TRUE
  else
    aText := GRID_VALUE_FALSE;
end;

procedure TfrmMain.SQLQuery1Status1GetText(Sender: TField; var aText: string;
  DisplayText: Boolean);
var
  StatusCode: Integer;
begin
  if TryStrToInt( Sender.AsString, StatusCode ) then
  begin
    if StatusCode = 1 then
      aText := StatusToString( StOk )
    else
      aText := StatusToString( StFail );
    DisplayText := True;
  end
  else
    DisplayText := False;
end;

function TfrmMain.StatusToString(Status: Integer): String;
begin
  if Status = StOk then
    Result := IntToStr(StOk) + '; Approved'
  else if Status = StFail then
    Result := IntToStr(StFail) + '; Failed';
end;

end.

