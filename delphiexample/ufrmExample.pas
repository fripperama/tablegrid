unit ufrmExample;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, uTableGrid, ComCtrls, Grids, DBXpress, DB, SqlExpr,
  FMTBcd, ExtCtrls, ImgList;

type
  TForm1 = class(TForm)
    aStringGrid: TStringGrid;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    edName: TEdit;
    Button13: TButton;
    Button14: TButton;
    Button15: TButton;
    Button16: TButton;
    Button17: TButton;
    chAllowSorting: TCheckBox;
    chAllowReordering: TCheckBox;
    TabSheet2: TTabSheet;
    chAllowResizing: TCheckBox;
    Button18: TButton;
    Button20: TButton;
    Button22: TButton;
    Button24: TButton;
    Button28: TButton;
    Button19: TButton;
    Button21: TButton;
    Button23: TButton;
    Button25: TButton;
    Button26: TButton;
    TabSheet3: TTabSheet;
    Button27: TButton;
    edSearchText: TEdit;
    Button29: TButton;
    Button30: TButton;
    FontDialog1: TFontDialog;
    SaveDialog1: TSaveDialog;
    Button31: TButton;
    SQLConnection1: TSQLConnection;
    SQLQuery1: TSQLQuery;
    SQLQuery1ID: TSmallintField;
    SQLQuery1NAME: TStringField;
    SQLQuery1ADDRESS: TStringField;
    SQLQuery1PHONE: TStringField;
    SQLQuery1EMAIL: TStringField;
    SQLQuery1STATUS: TSmallintField;
    SQLQuery1RECOMENDED: TStringField;
    Button33: TButton;
    Button34: TButton;
    ColorDialog1: TColorDialog;
    Button32: TButton;
    ImStatus: TImageList;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure Button16Click(Sender: TObject);
    procedure Button17Click(Sender: TObject);
    procedure chAllowSortingClick(Sender: TObject);
    procedure chAllowReorderingClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure chAllowResizingClick(Sender: TObject);
    procedure Button18Click(Sender: TObject);
    procedure Button20Click(Sender: TObject);
    procedure Button22Click(Sender: TObject);
    procedure Button24Click(Sender: TObject);
    procedure Button28Click(Sender: TObject);
    procedure Button19Click(Sender: TObject);
    procedure Button21Click(Sender: TObject);
    procedure Button23Click(Sender: TObject);
    procedure Button25Click(Sender: TObject);
    procedure Button26Click(Sender: TObject);
    procedure Button27Click(Sender: TObject);
    procedure Button30Click(Sender: TObject);
    procedure Button29Click(Sender: TObject);
    procedure Button31Click(Sender: TObject);
    procedure SQLQuery1RECOMENDEDGetText(Sender: TField; var Text: String;
      DisplayText: Boolean);
    procedure Button33Click(Sender: TObject);
    procedure Button34Click(Sender: TObject);
    procedure Button32Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SQLQuery1STATUSGetText(Sender: TField; var Text: String;
      DisplayText: Boolean);
  private
    { Private declarations }
    TableGrid: TTableGrid;
    Count: Integer;
    StOk: Integer;
    StFail: Integer;
    ImApproved: TBitmap;
    ImFail: TBitmap;
    function StatusToString(Status: Integer): String;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

const
  COL_ID = 1;
  COL_NAME = 2;
  COL_SECTOR = 3;
  COL_PHONE = 4;
  COL_EMAIL = 5;
  COL_CE = 6;
  COL_STATUS = 7;

procedure TForm1.Button1Click(Sender: TObject);
begin
  TableGrid := TTableGrid.Create(aStringGrid);
  StOk := TableGrid.RegisterImage(ImApproved);
  StFail := TableGrid.RegisterImage(ImFail);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  TableGrid.Free;
  TableGrid := nil;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  TableGrid.DefineHeader( ['Id Number', 'Full Name', 'Sector', 'Phone', 'E-mail', 'C.E.', 'Status'] );
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  TableGrid.InsertLine( ['035678-2', 'Vidal, Gabriel', 'Researching', '352-4859-88','vidal.gabriel@coldfall.com', GRID_VALUE_TRUE, StatusToString(StOk) ] );
  TableGrid.InsertLine( ['549633-5', 'Serafim, Joselito', 'Purchase', '544-LODERIVER-#','s.9@let.it.be', GRID_VALUE_FALSE, StatusToString(StOk) ] );
  TableGrid.InsertLine( ['A#467.B', 'Pallet, Gurdal', 'Financial Flurry', '4298-1566','palletling@dd.com', GRID_VALUE_FALSE, StatusToString(StOk) ] );
  TableGrid.InsertLine( ['0144.666-52', 'Mordenkain, Luminus', 'Cattering', '246-6969','betlentan@madison.edu', GRID_VALUE_TRUE, StatusToString(StOk) ] );
  TableGrid.InsertLine( ['6523', 'Borgognon, Fritadello', 'Purchase', '2545-0067','carcaroth@mail.me', GRID_VALUE_FALSE, StatusToString(StOk) ] );
  TableGrid.InsertLine( ['6524', 'Warner, Roland', 'Purchase', '2136-5546','rw@usep.edu.im', GRID_VALUE_TRUE, StatusToString(StFail) ] );
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  TableGrid.Clear;
  Count := 0;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  TableGrid.SortGrid( COL_NAME );
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  TableGrid.SortGrid( COL_SECTOR );
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
  TableGrid.SortGrid( COL_NAME );
  TableGrid.SortGrid( COL_SECTOR );
end;

procedure TForm1.Button9Click(Sender: TObject);
begin
  TableGrid.BeginUpdate;
  ShowMessage( 'Update begins. Now, try mass populating the grid before commiting.' );
end;

procedure TForm1.Button10Click(Sender: TObject);
begin
  TableGrid.ClearUpdateList;
end;

procedure TForm1.Button11Click(Sender: TObject);
begin
  TableGrid.CommitUpdate;
end;

procedure TForm1.Button12Click(Sender: TObject);
begin
  TableGrid.RollbackUpdate;
end;

procedure TForm1.Button13Click(Sender: TObject);
begin
  TableGrid.UpdateCell( COL_NAME, TableGrid.SelectedLine, edName.Text );
end;

procedure TForm1.Button14Click(Sender: TObject);
begin
  TableGrid.RemoveLine;
end;

procedure TForm1.Button15Click(Sender: TObject);
begin
  TableGrid.FitColumnToGridWidth( COL_NAME );
end;

procedure TForm1.Button16Click(Sender: TObject);
begin
  TableGrid.SwapColumns( 2, 3 );
end;

procedure TForm1.Button17Click(Sender: TObject);
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

procedure TForm1.chAllowSortingClick(Sender: TObject);
begin
  TableGrid.AllowSorting := chAllowSorting.Checked;
end;

procedure TForm1.chAllowReorderingClick(Sender: TObject);
begin
  TableGrid.AllowColumnReordering := chAllowReordering.Checked;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Count := 0;
  ImApproved := TBitmap.Create;
  ImFail := TBitmap.Create;
  ImStatus.GetBitmap( 0, ImApproved );
  ImStatus.GetBitmap( 1, ImFail );
  ImApproved.Transparent := True;
  ImFail.Transparent := False;
end;

procedure TForm1.chAllowResizingClick(Sender: TObject);
begin
  TableGrid.AllowColumnResizing := chAllowResizing.Checked;
end;

procedure TForm1.Button18Click(Sender: TObject);
begin
  TableGrid.RegisterEditField( COL_ID, teNumeric );
end;

procedure TForm1.Button20Click(Sender: TObject);
begin
  TableGrid.RegisterEditField( COL_NAME, teString );
end;

procedure TForm1.Button22Click(Sender: TObject);
begin
  TableGrid.RegisterEditField( COL_SECTOR, teString );
end;

procedure TForm1.Button24Click(Sender: TObject);
begin
  TableGrid.RegisterEditField( COL_PHONE, teString );
end;

procedure TForm1.Button28Click(Sender: TObject);
begin
  TableGrid.RegisterEditField( COL_EMAIL, teString );
end;

procedure TForm1.Button19Click(Sender: TObject);
begin
  TableGrid.RegisterEditField( COL_CE, teCheckBox );
end;

procedure TForm1.Button21Click(Sender: TObject);
begin
  TableGrid.BeginEdit;
end;

procedure TForm1.Button23Click(Sender: TObject);
begin
  TableGrid.EndEdit;
end;

procedure TForm1.Button25Click(Sender: TObject);
begin
  TableGrid.InsertLine( [] );
end;

procedure TForm1.Button26Click(Sender: TObject);
begin
  // warning, this will only work if you registered the column Id for editing.
  if TableGrid.CheckForErrors then
  begin
    ShowMessage( 'There was an error in the inputing data. Please, check the selected cell in the Grid and ensure it contains only numeric values.' );
  end;
end;

procedure TForm1.Button27Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    TableGrid.SaveToFile( SaveDialog1.FileName );
  end;
end;

procedure TForm1.Button30Click(Sender: TObject);
begin
  if FontDialog1.Execute then
  begin
    TableGrid.TitleFont.Assign(FontDialog1.Font);
    TableGrid.AttachedGrid.RowHeights[0] := (FontDialog1.Font.Height * (-1)) + 6;
    TableGrid.AttachedGrid.Refresh;
  end;
end;

procedure TForm1.Button29Click(Sender: TObject);
begin
  // this will search the text in the last sorted column
  TableGrid.Find( edSearchText.Text, TableGrid.SortedColumn );
end;

procedure TForm1.Button31Click(Sender: TObject);
begin
  SQLConnection1.Params.Values['Database'] := ExtractFilePath(Application.ExeName) + '..\clientlist.fdb';
  SQLConnection1.Connected := True;
  try
    SQLQuery1.Open;
    try
      TableGrid.ReadDataSet( SQLQuery1 );
      TableGrid.RegisterImageColumn( COL_CE, 16, 16 );
    finally
      SQLQuery1.Close;
    end;
  finally
    SQLConnection1.Connected := False;
  end;
end;

procedure TForm1.SQLQuery1RECOMENDEDGetText(Sender: TField;
  var Text: String; DisplayText: Boolean);
begin
  DisplayText := True;
  if Sender.AsString = 'Y' then
    Text := GRID_VALUE_TRUE
  else
    Text := GRID_VALUE_FALSE;
end;

procedure TForm1.Button33Click(Sender: TObject);
begin
  if ColorDialog1.Execute then
  begin
    TableGrid.OddColor := ColorDialog1.Color;
    TableGrid.AttachedGrid.Refresh;
  end;
end;

procedure TForm1.Button34Click(Sender: TObject);
begin
  if ColorDialog1.Execute then
  begin
    TableGrid.EvenColor := ColorDialog1.Color;
    TableGrid.AttachedGrid.Refresh;
  end;
end;

procedure TForm1.Button32Click(Sender: TObject);
begin
  TableGrid.RegisterImageColumn( COL_STATUS, 16, 16 );
  TableGrid.AttachedGrid.Refresh;
end;

function TForm1.StatusToString(Status: Integer): String;
begin
  if Status = StOk then
    Result := IntToStr(StOk) + '; Approved'
  else if Status = StFail then
    Result := IntToStr(StFail) + '; Failed';
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  ImApproved.Free;
  ImFail.Free;
end;

procedure TForm1.SQLQuery1STATUSGetText(Sender: TField; var Text: String;
  DisplayText: Boolean);
var
  StatusCode: Integer;
begin
  if TryStrToInt( Sender.AsString, StatusCode ) then
  begin
    if StatusCode = 1 then
      Text := StatusToString( StOk )
    else
      Text := StatusToString( StFail );
    DisplayText := True;
  end
  else
    DisplayText := False;
end;

end.
