unit uGridTabela;

interface

uses
  Grids, Classes, Graphics, ExtCtrls, DBGrids, StdCtrls, Controls, Types,
  DB;

const
  COR_1_PADRAO = clWhite;
  COR_2_PADRAO = $00FFEFD5;
  COR_GRID_PADRAO = clInactiveBorder;
  COR_FIXED_PADRAO = clBtnFace;

  GRID_VALOR_VERDADEIRO = '           ';
  GRID_VALOR_FALSO = '                 ';

type
  TTipoEdicao = (teString, teCheckBox, teNumerico, teNenhum);

  TPosicaoTexto = ( ptEsquerda, ptDireita );

  TTipoAtualizacao = (taInsercaoLinha, taRemocaoLinha, taLimpezaGrid, taInsercaoObjeto);

  TRegTipoEdicao = record
    TipoEdicao: TTipoEdicao;
    IndiceColuna: Integer;
  end;

  evColunaAtualizada = procedure (Sender: TObject; Linha, Coluna: Integer) of object;

  TRegTipoAtualizacao = record
    TipoAtualizacao: TTipoAtualizacao;
    ParametroInsercao: TStringList;
    ParametroRemocao: Integer;
    bIndiceInsercao: Boolean;
    Objeto: TObject;
  end;

  TRegColunaImagem = record
    IndiceColuna: Integer;
    IndiceImagem: Integer;
    Largura: Integer;
    Altura: Integer;
  end;

  TGridTabela = class
  private
    BitmapSetaIndicativa: TBitmap;
    FImagemSetaIndicativa: TImage;
    FDefaultDrawingAnterior: Boolean;
    FOptionsInicial: TGridOptions;
    FGridAssociado: TStringGrid;
    FIndicador: Boolean;
    FEmEdicao: Boolean;
    FAtualizando: Boolean;
    OpcoesAnteriores: TGridOptions;
    LabelMallandro: TStaticText;
    ColunaSelecionada: Integer;
    LinhaSelecionada: Integer;
    Ordena: Boolean;
    ClicouNoTitulo: Boolean;
    GridNaoVazia: Boolean;
    BitMapCheckBox: TBitmap;
    BitMapIndicator: TBitMap;
    ListaColunaImagens: array of TRegColunaImagem;
    ListaImagens: TList;
    ListaColunasOrdenadas: TStringList;
    ListaColunaEdicao: array of TRegTipoEdicao;
    ListaOrdemColunas: TStringList; // para implementação da troca de colunas
    ListaAtualizacoes: array of TRegTipoAtualizacao;

    // para o mouse draw cell
    PenColorAnterior: TColor;
    BrushColorAnterior: TColor;
    IndiceColuna, IndiceImagem: Integer;
    iCont: Integer;
    RectAux: TRect;
    PosPontoVirgula: Integer;
    CorAPreencher: TColor;
    Texto: String;
    XAnt, YAnt: Integer;
    ColAux, RowAux: Integer;
    AuxQtdLinhas: Integer;
    FLeituraApenas: Boolean;

    procedure AtribuiQtdColunas(const Value: Integer);
    procedure AtribuiQtdLinhas(const Value: Integer);
    function LeQtdColunas: Integer;
    function LeQtdLinhas: Integer;
    procedure AtribuiIndicador(const Value: Boolean);
    procedure QuickSortRec(Inicio, Fim: Integer);
    procedure CriaBitmapCheckBox;
    function ComparaLinhasGrid(Linha1, Linha2: Integer): Integer;
    procedure InverteLinhas;
    function GridParaIndiceColuna(Valor: Integer): Integer;
    function IndiceColunaParaGrid(Valor: Integer): Integer;
    function GridParaIndiceLinha(Valor: Integer): Integer;
    function IndiceLinhaParaGrid(Valor: Integer): Integer;
    function LeColunaOrdenada: Integer;
    procedure MontaOrdemColunas;
    procedure ConstroiImagemSeta;
    function PosicaoTexto(Coluna: Integer): TPosicaoTexto;
    function TipoEdicao(Coluna: Integer): TTipoEdicao;
    procedure AtribuiLeituraApenas(const Value: Boolean);
    procedure OrdenaGridInterno(Coluna: Integer; Crescente: Boolean = True; NaoDeixeInverter: Boolean = False);
  public
    Cor1: TColor;
    Cor2: TColor;
    FonteTitulo: TFont;
    MouseDownAnterior: TMouseEvent;
    MouseUpAnterior: TMouseEvent;
    DrawCellAnterior: TDrawCellEvent;
    SelectCellAnterior: TSelectCellEvent;
    DuploCliqueAnterior: TNotifyEvent;
    KeyPressAnterior: TKeyPressEvent;
    MouseMoveAnterior: TMouseMoveEvent;
    aoAtualizarColuna: evColunaAtualizada;

    procedure GridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure GridDrawCell(Sender: TObject; ACol, ARow: Longint; Rect: TRect; State: TGridDrawState);
    procedure GridSelectCell(Sender: TObject; ACol, ARow: Longint; var CanSelect: Boolean);
    procedure GridDuploClique(Sender: TObject);
    procedure GridKeypress(Sender: TObject; var Key: Char);

    constructor Create(StringGrid: TStringGrid);
    destructor Destroy; override;
    procedure ExcluiLinha(IndiceLinha: Integer); overload;
    procedure ExcluiLinha; overload;
    procedure InserirLinha(Linha: TStringList); overload;
    procedure InserirLinha(Linha: array of String); overload;
    procedure InserirLinha(Linha: TStringList; Indice: Integer); overload;
    procedure AtualizaLinha(Linha: TStringList); overload;
    procedure AtualizaLinha(Linha: array of String); overload;
    procedure AtualizaLinha(Linha: TStringList; Indice: Integer); overload;
    procedure PreencheCabecalho(Cabecalho: array of String);
    procedure Limpa;
    procedure SelecionaLinha(Linha: Integer);
    procedure InsereObjeto(Linha: Integer; Objeto: TObject);
    procedure Habilita;
    procedure Desabilita;
    function  DevolveObjeto(Linha: Integer): TObject; overload;
    function  DevolveObjeto: TObject; overload;
    function  Coluna(Col: Integer): String;
    function  LeCelula(Col: Integer; Linha: Integer): String;
    procedure AtualizaCelula(Col, Linha: Integer; Valor: String);
    function  Localiza(Valor: String; pColuna: Integer): Boolean;
    function  GridVazia: Boolean;
    procedure RegistraColunaImagem(IndiceColuna, Largura, Altura: Integer);
    function  RegistraImagem(Bitmap: TBitmap): Integer;
    procedure RegistraCampoEdicao(IndiceColuna: Integer; TipoEdicao: TTipoEdicao);
    procedure IniciaEdicao;
    procedure FinalizaEdicao;
    procedure IniciaAtualizacao;
    procedure FinalizaAtualizacao;
    procedure CancelaAtualizacao;
    procedure TrocaColuna(Coluna1, Coluna2: Integer);
    procedure LimpaListaAtualizacao;
    procedure AtribuiTamanhoColuna(IndiceColuna, Tamanho: Integer);
    function LeTamanhoColuna(IndiceColuna: Integer): Integer;
    function ChecaErros: Boolean;
    procedure OrdenaGrid(Coluna: Integer; Crescente: Boolean = True);
    procedure AjustaTamanhoColuna(IndiceColuna: Integer);
    procedure ExibeDataSet ( DataSet: TDataset );
    procedure GravaRegistrosEmArquivo ( NomeArquivoModelo: String );
  published
    property GridAssociado: TStringGrid read FGridAssociado write FGridAssociado;
    property QtdColunas: Integer read LeQtdColunas write AtribuiQtdColunas;
    property QtdLinhas:  Integer read LeQtdLinhas  write AtribuiQtdLinhas;
    property Indicador: Boolean read FIndicador write AtribuiIndicador;
    property ColunaOrdenada: Integer read LeColunaOrdenada;
    property LeituraApenas: Boolean read FLeituraApenas write AtribuiLeituraApenas;
  end;

implementation

uses
  SysUtils, Forms, Math;

function ApenasNumeros(Valor: String): String;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(Valor) do
  begin
    if Valor[i] in ['0'..'9', DecimalSeparator] then
      Result := Result + Valor[i];
  end;
end;

function ValorValido ( Valor: String ): Boolean;
var
  i: Integer;
begin
  Result := True;

  if Trim(Valor) <> '' then
  begin
    for i := 1 to Length(Valor) do
    begin
      if not (Valor[i] in ['0'..'9', '.', ',']) then
      begin
        Result := False;
        break;
      end;
    end;
  end;
end;


procedure TGridTabela.AtribuiQtdColunas(const Value: Integer);
begin
  if FIndicador then
    FGridAssociado.ColCount := value + 1
  else
    FGridAssociado.ColCount := value;
  MontaOrdemColunas;
end;

procedure TGridTabela.AtribuiQtdLinhas(const Value: Integer);
begin
  if Value = 0 then
  begin
    FGridAssociado.RowCount := FGridAssociado.FixedRows + 1;
    GridNaoVazia := False;
  end
  else
  begin
    GridNaoVazia := True;
    FGridAssociado.RowCount := value + FGridAssociado.FixedRows;
  end;
  ListaColunasOrdenadas.Clear;
end;

constructor TGridTabela.Create(StringGrid: TStringGrid);
begin
  inherited Create;
  FAtualizando := False;
  FLeituraApenas := False;
  FGridassociado := StringGrid;
  FDefaultDrawingAnterior := FGridAssociado.DefaultDrawing;
  FGridAssociado.DefaultDrawing := False;
//  Indicador := True;
  Cor1 := COR_1_PADRAO;
  Cor2 := COR_2_PADRAO;
  FGridAssociado.Color := COR_GRID_PADRAO;
  FGridAssociado.FixedColor := COR_FIXED_PADRAO;
  FonteTitulo := TFont.Create;
  FonteTitulo.Assign(FGridAssociado.Font);
  FonteTitulo.Style := FonteTitulo.Style + [fsBold];

  FGridAssociado.DefaultRowHeight := 19;

  MouseDownAnterior := FGridAssociado.OnMouseDown;
  MouseUpAnterior := FGridAssociado.OnMouseUp;
  MouseMoveAnterior := FGridAssociado.OnMouseMove;
  DrawCellAnterior := FGridAssociado.OnDrawCell;
  SelectCellAnterior := FGridAssociado.OnSelectCell;
  DuploCliqueAnterior := FGridAssociado.OnDblClick;
  KeyPressAnterior := FGridAssociado.OnKeyPress;

  FGridAssociado.OnMouseDown := GridMouseDown;
  FGridAssociado.OnMouseUp := GridMouseUp;
  FGridAssociado.OnMouseMove := GridMouseMove;
  FGridAssociado.OnDrawCell := GridDrawCell;
  FGridAssociado.OnSelectCell := GridSelectCell;
  FGridAssociado.OnDblClick := GridDuploClique;
  FGridAssociado.OnKeyPress := GridKeypress;

  LabelMallandro := TStaticText.Create(FGridAssociado);
  LabelMallandro.Visible := False;
  LabelMallandro.Parent := FGridAssociado;
  LabelMallandro.BorderStyle := sbsSunken;
  LabelMallandro.AutoSize := False;

  BitMapIndicator := TBitmap.Create;
  BitMapIndicator.LoadFromResourceName(HInstance, 'DBGARROW');
  BitMapIndicator.Transparent := True;

  CriaBitmapCheckBox;

  ClicouNoTitulo := False;
  GridNaoVazia := False;

  SetLength(ListaColunaImagens, 0);
  SetLength(ListaColunaEdicao, 0);
  ListaImagens := TList.Create;
  ListaColunasOrdenadas := TStringList.Create;
  FEmEdicao := False;
  LinhaSelecionada := FGridAssociado.Row;

  ListaOrdemColunas := TStringList.Create;
  MontaOrdemColunas;
  ConstroiImagemSeta;
  Indicador := True;

  FAtualizando := True;
  OpcoesAnteriores := FGridAssociado.Options;
  FGridAssociado.Options := FGridAssociado.Options + [goRowSelect] - [goRangeSelect] + [goColSizing];
  FAtualizando := False;
  aoAtualizarColuna := nil;
end;

procedure TGridTabela.ExcluiLinha(IndiceLinha: Integer);
var
  i: Integer;
  LinhaAux: Integer;
begin
  if not FAtualizando then
  begin
    if (IndiceLinha > QtdLinhas) or (IndiceLinha < 1) then
      exit;
    // embranquece a linha
    for i := 0 to FGridAssociado.ColCount - 1 do
      FGridAssociado.Cells[i, IndiceLinha] := '';
    // exclui a linha
    LinhaAux := IndiceLinhaParaGrid(IndiceLinha);
    for i := IndiceLinha to QtdLinhas - 1  do
    begin
      FGridAssociado.Objects[0, i] := FGridAssociado.Objects[0, i + 1];
      FGridAssociado.Rows[i] := FGridAssociado.Rows[i + 1];
    end;
    if QtdLinhas > 1 then
      QtdLinhas := QtdLinhas - 1;
    if GridVazia then
      GridNaoVazia := False;
  end
  else
  begin
    SetLength(ListaAtualizacoes, Length(ListaAtualizacoes) + 1);
    ListaAtualizacoes[High(ListaAtualizacoes)].TipoAtualizacao := taRemocaoLinha;
    ListaAtualizacoes[High(ListaAtualizacoes)].ParametroRemocao := IndiceLinha;
    if AuxQtdLinhas > 0 then
      dec(AuxQtdLinhas);
  end;
end;

function TGridTabela.DevolveObjeto(Linha: Integer): TObject;
begin
  result := FGridAssociado.Objects[0, IndiceLinhaParaGrid(Linha)];
end;

procedure TGridTabela.ExcluiLinha;
begin
  ExcluiLinha(GridParaIndiceLinha(FGridAssociado.Row));
end;

procedure TGridTabela.InsereObjeto(Linha: Integer; Objeto: TObject);
begin
  if not FAtualizando then
    FGridAssociado.Objects[0, IndiceLinhaParaGrid(Linha)] := Objeto
  else
  begin
    SetLength(ListaAtualizacoes, Length(ListaAtualizacoes) + 1);
    ListaAtualizacoes[High(ListaAtualizacoes)].TipoAtualizacao := taInsercaoObjeto;
    ListaAtualizacoes[High(ListaAtualizacoes)].ParametroRemocao := Linha;
    ListaAtualizacoes[High(ListaAtualizacoes)].Objeto := Objeto;
  end;
end;

procedure TGridTabela.InserirLinha(Linha: TStringList);
var
  i: Integer;
begin
  if not FAtualizando then
  begin
    QtdLinhas := QtdLinhas + 1;

    i := 1;
    while (i <= QtdColunas) and
          (i <= Linha.Count) do
    begin
      FGridAssociado.Cells[IndiceColunaParaGrid(i), QtdLinhas] :=
         Linha.Strings[i-1];
      inc(i);
    end;
  end
  else
  begin
    SetLength(ListaAtualizacoes, Length(ListaAtualizacoes) + 1);
    ListaAtualizacoes[High(ListaAtualizacoes)].TipoAtualizacao := taInsercaoLinha;
    ListaAtualizacoes[High(ListaAtualizacoes)].ParametroInsercao := TStringList.Create;
    ListaAtualizacoes[High(ListaAtualizacoes)].ParametroInsercao.AddStrings(Linha);
    ListaAtualizacoes[High(ListaAtualizacoes)].bIndiceInsercao := False;
    inc(AuxQtdLinhas);
  end;
end;

function TGridTabela.LeQtdColunas: Integer;
begin
  if FIndicador then
    result := FGridAssociado.ColCount - 1
  else
    result := FGridAssociado.ColCount;
end;

function TGridTabela.LeQtdLinhas: Integer;
begin
  if not FAtualizando then
  begin
    if (not GridNaoVazia) and (GridVazia) then
      result := 0
    else
      result := FGridAssociado.RowCount - FGridAssociado.FixedRows;
  end
  else
    result := AuxQtdLinhas;
end;

procedure TGridTabela.Limpa;
var
  i: Integer;
begin
  if not FAtualizando then
  begin
    FGridAssociado.RowCount := FGridAssociado.FixedRows + 1;
    if FGridAssociado.RowCount > 1 then
    begin
      for i := 0 to FGridAssociado.ColCount - 1 do
        FGridAssociado.Cells[i, 1] := '';
    end;
    GridNaoVazia := False;
  end
  else
  begin
    SetLength(ListaAtualizacoes, Length(ListaAtualizacoes) + 1);
    ListaAtualizacoes[High(ListaAtualizacoes)].TipoAtualizacao := taLimpezaGrid;
    AuxQtdLinhas := 0;;
  end;
end;

procedure TGridTabela.PreencheCabecalho(Cabecalho: array of String);
var
  i: Integer;
  AuxCabecalho: TStringList;
begin
  AuxCabecalho := TStringList.Create;
  for i := Low(Cabecalho) to High(Cabecalho) do
    AuxCabecalho.Add(Cabecalho[i]);

  // insere quantos espaços em branco forem necessários
  if FIndicador then
    AuxCabecalho.Insert(0, ' ');
  FGridAssociado.ColCount := AuxCabecalho.Count;
  FGridAssociado.Rows[0] := AuxCabecalho;
  AuxCabecalho.Free;

  MontaOrdemColunas;
end;

procedure TGridTabela.SelecionaLinha(Linha: Integer);
begin
  if Linha <= QtdLinhas then
    FGridAssociado.Row := IndiceLinhaParaGrid(Linha);
end;

function TGridTabela.DevolveObjeto: TObject;
begin
  result := DevolveObjeto(GridParaIndiceLinha(FGridAssociado.Row));
end;

function TGridTabela.Coluna(Col: Integer): String;
begin
  result := FGridAssociado.Cells[IndiceColunaParaGrid(Col), FGridAssociado.Row];
end;

procedure TGridTabela.Desabilita;
begin
  FGridAssociado.Enabled := False;
end;

procedure TGridTabela.Habilita;
begin
  FGridAssociado.Enabled := True;
end;

procedure TGridTabela.InserirLinha(Linha: TStringList; Indice: Integer);
var
  i: Integer;
  EmBranco: Boolean;
begin
  if not FAtualizando then
  begin
    if QtdLinhas >= 1 then
    begin
      QtdLinhas := QtdLinhas + 1;
      for i := QtdLinhas - 1 downto Indice do
      begin
        FGridAssociado.Objects[0, i+ 1] := FGridAssociado.Objects[0, i];
        FGridAssociado.Rows[i+ 1] := FGridAssociado.Rows[i];
      end;
    end
    else
    begin
      // verifica se o grid está em branco
      EmBranco := True;
      for i := 1 to QtdColunas do
      begin
        if FGridAssociado.Cells[IndiceColunaParaGrid(i), QtdLinhas] <> '' then
        begin
          EmBranco := False;
          break;
        end;
      end;
      if not EmBranco then
        QtdLinhas := QtdLinhas + 1;
    end;

    i := 1;
    while (i <= QtdColunas) and
          (i <= Linha.Count) do
    begin
      FGridAssociado.Cells[IndiceColunaParaGrid(i), IndiceLinhaParaGrid(Indice)] :=
                                                           Linha.Strings[i-1];
      inc(i);
    end;
  end
  else
  begin
    SetLength(ListaAtualizacoes, Length(ListaAtualizacoes) + 1);
    ListaAtualizacoes[High(ListaAtualizacoes)].TipoAtualizacao := taInsercaoLinha;
    ListaAtualizacoes[High(ListaAtualizacoes)].ParametroInsercao := TStringList.Create;
    ListaAtualizacoes[High(ListaAtualizacoes)].ParametroInsercao.AddStrings(Linha);
    ListaAtualizacoes[High(ListaAtualizacoes)].bIndiceInsercao := True;
    ListaAtualizacoes[High(ListaAtualizacoes)].ParametroRemocao := Indice;
    inc(AuxQtdLinhas);
  end;
end;


procedure TGridTabela.AtualizaLinha(Linha: TStringList; Indice: Integer);
var
  i: Integer;
begin
  i := 1;
  while (i <= QtdColunas) and
        (i <= Linha.Count) do
  begin
    FGridAssociado.Cells[IndiceColunaParaGrid(i), IndiceLinhaParaGrid(Indice)] :=
                                                         Linha.Strings[i-1];
    inc(i);
  end;
end;

procedure TGridTabela.AtribuiIndicador(const Value: Boolean);
var
  i: Integer;
begin
  if (Value <> FIndicador) then
  begin
    FinalizaAtualizacao;
    IniciaAtualizacao;
    FIndicador := Value;
    if Value then
    begin
      FGridAssociado.ColCount := FGridAssociado.ColCount + 1;
      FGridAssociado.FixedCols := FGridAssociado.FixedCols + 1;
      for i := FGridAssociado.ColCount - 1 downto 1 do
      begin
        FGridAssociado.ColWidths[i] := FGridAssociado.ColWidths[i-1];
        FGridAssociado.Cols[i].Clear;
        FGridAssociado.Cols[i].AddStrings(FGridAssociado.Cols[i-1]);
      end;
      FGridAssociado.Cols[0].Clear;
      FGridAssociado.ColWidths[0] := 12;
    end
    else
    begin
      FGridAssociado.FixedCols := FGridAssociado.FixedCols - 1;
      for i := 0 to FGridAssociado.ColCount - 2 do
      begin
        FGridAssociado.ColWidths[i] := FGridAssociado.ColWidths[i+1];
        FGridAssociado.Cols[i].Clear;
        FGridAssociado.Cols[i].AddStrings(FGridAssociado.Cols[i+1]);
      end;
      FGridAssociado.ColCount := FGridAssociado.ColCount - 1;
    end;
    FinalizaAtualizacao;
  end;
end;

function TGridTabela.Localiza(Valor: String; pColuna: Integer): Boolean;
var
  i: Integer;
  Texto: String;
  j: Integer;
  FlagComp: Boolean;
begin
  Result := False;
  if pColuna = 0 then
    pColuna := 1;
  for i := 1 to QtdLinhas do
  begin
    Texto := FGridAssociado.Cells[IndiceColunaParaGrid(pColuna), indiceLinhaParaGrid(i)];
    if Length(Texto) < Length(Valor) then
      continue;
    FlagComp := True;
    for j := 1 to Length(Valor) do
    begin
      if UpperCase(Valor[j]) <> UpperCase(Texto[j]) then
      begin
        FlagComp := False;
        break;
      end;
    end;
    if (not FlagComp) and (Valor = '#') then // seleção de número
    begin
      if Texto[1] in ['0'..'9'] then
        FlagComp := True;
    end;
    if FlagComp then
    begin
      SelecionaLinha(i);
      FGridAssociado.Col := IndiceColunaParaGrid(pColuna);
      Result := True;
      break;
    end;
  end;
end;

procedure TGridTabela.InserirLinha(Linha: array of String);
var
  Lista: TStringList;
  i: Integer;
begin
  Lista := TStringList.Create;
  for i := Low(Linha) to High(Linha) do
    Lista.Add(Linha[i]);
  InserirLinha(Lista);
  Lista.Free;
end;

destructor TGridTabela.Destroy;
begin
  FGridAssociado.OnMouseUp := MouseUpAnterior;
  FGridAssociado.OnMouseDown := MouseDownAnterior;
  FGridAssociado.OnDrawCell := DrawCellAnterior;
  FGridAssociado.OnDblClick := DuploCliqueAnterior;
  FGridAssociado.OnKeyPress := KeyPressAnterior;
  FGridAssociado.OnSelectCell := SelectCellAnterior;
  FGridAssociado.OnMouseMove := MouseMoveAnterior;
  LabelMallandro.Free;
  BitMapIndicator.Free;
  BitMapCheckBox.Free;
  ListaImagens.Free;
  ListaColunasOrdenadas.Free;
  FonteTitulo.Free;
  FGridAssociado.Options := OpcoesAnteriores;
  FGridAssociado.DefaultDrawing := FDefaultDrawingAnterior;
  ListaOrdemColunas.Free;
  FImagemSetaIndicativa.Free;
  inherited;
end;

procedure TGridTabela.GridDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
begin
  if not FAtualizando then
  begin
    BrushColorAnterior := FGridAssociado.Canvas.Brush.Color;
    PenColorAnterior := FGridAssociado.Canvas.Pen.Color;

    // ajusta o tipo da fonte da grid
    if ARow < FGridAssociado.FixedRows then
      FGridAssociado.Canvas.Font.Assign(FonteTitulo)
    else
      FGridAssociado.Canvas.Font.Assign(FGridAssociado.Font);

    if assigned(DrawCellAnterior) then
      DrawCellAnterior(Sender, ACol, ARow, Rect, State);

    // preenche a célula com a cor adequada
    if gdFixed in State then
    begin
      FGridAssociado.Canvas.Brush.Color := FGridAssociado.FixedColor
    end
    else
    begin
      if gdFocused in State then
      begin
        //FGridAssociado.Canvas.Font.Color := clHighlightText;
        if not (goRowSelect in fgridAssociado.Options) then
          FGridAssociado.Canvas.Brush.Color := FGridAssociado.Color
        else
        begin
          FGridAssociado.Canvas.Font.Color := clHighlightText;
          FGridAssociado.Canvas.Brush.Color := clHighlight;
        end;
      end
      else
      begin
        if (gdSelected in State) or (ARow = LinhaSelecionada) then
        begin
          FGridAssociado.Canvas.Font.Color := clHighlightText;
          FGridAssociado.Canvas.Brush.Color := clHighlight
        end
        else
        begin
          if (ARow - FGridAssociado.FixedRows + 1) mod 2 = 1 then
            FGridAssociado.Canvas.Brush.Color := Cor1
          else
            FGridAssociado.Canvas.Brush.Color := Cor2;
        end;
      end;
    end;

    FGridAssociado.Canvas.FillRect(Rect);

    RectAux := Rect;
    RectAux.TopLeft := Point(Rect.Left + 2, Rect.Top + 2);
    RectAux.BottomRight := Point(Rect.Right - 2, Rect.Bottom - 2);
    Texto := FGridAssociado.Cells[ACol, ARow];

    if gdFixed in State then
    begin
      // 3D
        FGridAssociado.Canvas.Pen.Color := clBlack;
        FGridAssociado.Canvas.PenPos := Point(Rect.Left, Rect.Bottom);
        FGridAssociado.Canvas.LineTo(Rect.Right, Rect.Bottom);
        FGridAssociado.Canvas.LineTo(Rect.Right, Rect.Top);
      if FGridAssociado.Ctl3D then
      begin
        FGridAssociado.Canvas.Pen.Color := clWhite;
        FGridAssociado.Canvas.PenPos := Point(Rect.Left, Rect.Bottom - 1);
        FGridAssociado.Canvas.LineTo(Rect.Left, Rect.Top);
        FGridAssociado.Canvas.LineTo(Rect.Right - 1, Rect.Top);
        FGridAssociado.Canvas.Pen.Color := clGray;
        FGridAssociado.Canvas.PenPos := Point(Rect.Left + 1, Rect.Bottom - 1);
        FGridAssociado.Canvas.LineTo(Rect.Right - 1, Rect.Bottom - 1);
        FGridAssociado.Canvas.LineTo(Rect.Right - 1, Rect.Top - 1);
      end;
    end;

    if not ((FIndicador) and (ACol = 0)) then
      IndiceColuna := GridParaIndiceColuna(ACol);

    // desenho das imagens
    if Length(ListaColunaImagens) > 0 then
    begin
      if not ((FIndicador) and (ACol = 0)) then
      begin
        iCont := Low(ListaColunaImagens);
        while iCont <= High(ListaColunaImagens) do
        begin
          if ListaColunaImagens[iCont].IndiceColuna = IndiceColuna then
          begin
            // devemos pintar uma imagem aqui...
            PosPontoVirgula := Pos(';', GridAssociado.Cells[ACol, ARow]);
            if (PosPontoVirgula <> 0) and (TryStrToInt(Copy(GridAssociado.Cells[ACol, ARow], 1, PosPontoVirgula - 1), IndiceImagem)) then
            begin
              Texto := Copy(Texto, PosPontoVirgula + 1, Length(Texto) - PosPontoVirgula);
              if IndiceImagem < ListaImagens.Count then
              begin
                RectAux.Right := RectAux.Left + ListaColunaImagens[iCont].Largura;
                RectAux.Bottom := RectAux.Top + ListaColunaImagens[iCont].Altura;
                if RectAux.Right > Rect.Right then
                  RectAux.Right := Rect.Right;
                if RectAux.Bottom > Rect.Bottom then
                  RectAux.Bottom := Rect.Bottom;
                GridAssociado.Canvas.StretchDraw(RectAux, TBitmap(ListaImagens.Items[IndiceImagem]));
                //GridAssociado.Canvas.StretchDraw(RectAux, BitMapIndicator);
                RectAux.Left := RectAux.Right;
                RectAux.Right := Rect.Right;
                RectAux.Top := Rect.Top;
                RectAux.Bottom := Rect.Bottom;
              end;
            end;
          end;
          iCont := iCont + 1;
        end;
      end;
    end; // fim desenho na célula

    // desenho da checkbox
    if (FGridAssociado.Cells[ACol, ARow] = GRID_VALOR_VERDADEIRO) or
          (FGridAssociado.Cells[ACol, ARow] = GRID_VALOR_FALSO) then
    begin
      PenColorAnterior := FGridAssociado.Canvas.Pen.Color;

      FGridAssociado.Canvas.Pen.Color := clBlack;
      FGridAssociado.Canvas.FillRect(RectAux);
      FGridAssociado.Canvas.Brush.Color := clWhite;

      Rect.Left := Rect.Left + ((Rect.Right - Rect.Left) div 2) - (12 div 2);
      Rect.Right := Rect.Left + 12;
      Rect.Top := Rect.Top + ((Rect.Bottom - Rect.Top) div 2) - (12 div 2);
      Rect.Bottom := Rect.Top + 12;

      FGridAssociado.Canvas.Rectangle(Rect);
      Rect.Top := Rect.Top + 1;
      Rect.Left := Rect.Left + 1;
      Rect.Bottom := Rect.Bottom - 1;
      Rect.Right := Rect.Right - 1;
      FGridAssociado.Canvas.Brush.Color := clWhite;
      //FGridAssociado.Canvas.FillRect(Rect);

      if FGridAssociado.Cells[ACol, ARow] = GRID_VALOR_VERDADEIRO then
      begin
        FGridAssociado.Canvas.Draw(Rect.Left + 1, Rect.Top + 1, BitMapCheckBox);
      end;
    end  // fim desenho da check-box
    else
    begin
      if PosicaoTexto(IndiceColuna) = ptEsquerda then
        FGridAssociado.Canvas.TextRect(RectAux, RectAux.Left, RectAux.Top, Texto)
      else
        FGridAssociado.Canvas.TextRect(RectAux, RectAux.Right - FGridAssociado.Canvas.TextWidth(Texto) - 3, RectAux.Top, Texto)
    end;

    if (Indicador) and (not GridVazia) then
    begin
      if (ACol = 0) and (ARow = LinhaSelecionada) then
      begin
        Rect.Top := Rect.Top + 3;
        Rect.Left := 2;
        Rect.Right := Rect.Left + 6;
        Rect.Bottom := Rect.Top + FGridAssociado.DefaultRowHeight - 3;
        FGridAssociado.Canvas.Draw(Rect.Left, Rect.Top, BitMapIndicator);
      end;
    end;

    FGridAssociado.Canvas.Brush.Color := BrushColorAnterior;
    FGridAssociado.Canvas.Pen.Color := PenColorAnterior;
  end;
end;

procedure TGridTabela.GridMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  auxRect: TRect;
  Col, Row: Integer;
begin
  Ordena := False;

  if (FGridAssociado.FixedRows >= 1) and (Y <= FGridAssociado.RowHeights[0]) and
     (not Screen.Cursor <> crHSplit) then
  begin
    FGridAssociado.MouseToCell(X, Y, Col, Row);
    if (Col >= FGridAssociado.FixedCols) then
    begin
      Ordena := True;
      auxRect := FGridAssociado.CellRect(Col, 0);
      LabelMallandro.Font.Assign(FGridAssociado.Font);
      LabelMallandro.Font.Style := LabelMallandro.Font.Style + [fsBold];
      LabelMallandro.Color := FGridAssociado.FixedColor;
      LabelMallandro.Caption := FGridAssociado.Cells[Col, 0];
      LabelMallandro.Left := auxRect.Left;
      LabelMallandro.Width := FGridAssociado.ColWidths[Col] + 1;
      LabelMallandro.Top := 0;
      LabelMallandro.Height := FGridAssociado.RowHeights[0] + 1;
      LabelMallandro.BringToFront;
      LabelMallandro.Visible := True;
      ColunaSelecionada := Col;
      ClicouNoTitulo := True;
      XAnt := X;
      YAnt := Y;
    end;
  end
  else
  begin
    FGridAssociado.MouseToCell(X, Y, Col, Row);
    if (FEmEdicao) and (not (FLeituraApenas)) and
         ((FGridAssociado.Cells[Col, Row] = GRID_VALOR_VERDADEIRO) or
         (FGridAssociado.Cells[Col, Row] = GRID_VALOR_FALSO)) and
         (Length(ListaColunaEdicao) > 0) and
         (QtdLinhas > 0 )  then
    begin
      iCont := Low(ListaColunaEdicao);
      while iCont <= High(ListaColunaEdicao) do
      begin
        if IndiceColunaParaGrid(ListaColunaEdicao[iCont].IndiceColuna) = Col then
        begin
          if FGridAssociado.Cells[FGridAssociado.Col, FGridAssociado.Row] = GRID_VALOR_VERDADEIRO then
            FGridAssociado.Cells[FGridAssociado.Col, FGridAssociado.Row] := GRID_VALOR_FALSO
          else if FGridAssociado.Cells[FGridAssociado.Col, FGridAssociado.Row] = GRID_VALOR_FALSO then
            FGridAssociado.Cells[FGridAssociado.Col, FGridAssociado.Row] := GRID_VALOR_VERDADEIRO;

          if assigned(aoAtualizarColuna) then
            aoAtualizarColuna( self, IndiceLinhaParaGrid(Row), IndiceColunaParaGrid(Col) );
          break;
        end;
        inc(iCont);
      end;
    end;
  end;

  if assigned(MouseDownAnterior) then
    MouseDownAnterior(Sender, Button, Shift, X, Y);
end;

procedure TGridTabela.GridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Col, Row: Integer;
begin
  if assigned(MouseUpAnterior) then
    MouseUpAnterior(Sender, Button, Shift, X, Y);

  if Ordena then
  begin
    FGridAssociado.MouseToCell(LabelMallandro.Left, 0, Col, Row);
    if (Col <> ColunaSelecionada) and ((not Indicador) or ((Indicador) and (ColAux <> 0))) then
    begin
      if (Col >= 0) and (ColunaSelecionada >= 0) then
        TrocaColuna(ColunaSelecionada, Col);
    end
    else
    begin
      if (LabelMallandro.Left >= FGridAssociado.CellRect(Col, Row).Left - 3) and
         (LabelMallandro.Left <= FGridAssociado.CellRect(Col, Row).Left + 3) then
        OrdenaGridInterno(GridParaIndiceColuna(ColunaSelecionada));
    end;
    Ordena := False;
    LabelMallandro.Visible := False;
    if FImagemSetaIndicativa.Visible then
      FImagemSetaIndicativa.Visible := False;
  end;
end;

procedure TGridTabela.GridSelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
var
  Rect: TRect;
  xlinha: Integer;
  i: Integer;
  bAchou: Boolean;
  RegAux: TRegTipoEdicao;
  LinhaAnterior: Integer;
begin
  if FAtualizando then
    exit;

  if assigned(SelectCellAnterior) then
    SelectCellAnterior(Sender, ACol, ARow, CanSelect);

  if (ARow <> LinhaSelecionada) and (LinhaSelecionada >= 0) then
  begin
    LinhaAnterior := LinhaSelecionada;
    LinhaSelecionada := ARow;
{    Rect := FGridAssociado.CellRect(0, LinhaAnterior);
    if not ((Rect.Top = 0) and (Rect.Bottom = 0) and
       (Rect.Left = 0) and (Rect.Right = 0)) then
    begin
      for i := 0 to FGridAssociado.ColCount - 1 do
      begin
        Rect := FGridAssociado.CellRect(i, LinhaAnterior);
        if i < FGridAssociado.FixedCols then
          GridDrawCell(self, i, LinhaAnterior, Rect, [gdFixed])
        else
          GridDrawCell(self, i, LinhaAnterior, Rect, []);
      end;
    end;}
    GridAssociado.Refresh;
  end;

{  for i := 0 to FGridAssociado.ColCount - 1 do
  begin
    if i = ACol then
      continue;
    if i < FGridAssociado.FixedCols then
      GridDrawCell(self, i, LinhaSelecionada, FGridAssociado.CellRect(i, LinhaSelecionada), [gdSelected, gdFixed])
    else
      GridDrawCell(self, i, ARow, FGridAssociado.CellRect(i, ARow), [gdSelected]);
  end;
  GridDrawCell(self, ACol, ARow, FGridAssociado.CellRect(ACol, ARow), [gdFocused, gdSelected]);
 }
  if not FAtualizando then
  begin
    if (Indicador) and (not GridVazia) then
    begin
      {if LinhaSelecionada >= 0 then
        FGridAssociado.Cells[0, LinhaSelecionada] := ' ';}

      Rect := FGridAssociado.CellRect(0, ARow);
      Rect.Top := Rect.Top + 3;
      Rect.Bottom := Rect.Top + FGridAssociado.DefaultRowHeight - 3;
      Rect.Left := 2;
      xLinha := Round((BitMapIndicator.Width * (Rect.Bottom - Rect.Top)) / BitMapIndicator.Height);
      Rect.Right := Rect.Left + xLinha;

      //FGridAssociado.Canvas.StretchDraw(Rect, BitMapIndicator);
      FGridAssociado.Canvas.Draw(Rect.Left, Rect.Top, BitMapIndicator);
    end;

    ClicouNoTitulo := False;

    // Trata a edição
    if FEmEdicao then
    begin
      IndiceColuna := GridParaIndiceColuna(ACol);
      bAchou := False;
      for i := Low(ListaColunaEdicao) to High(ListaColunaEdicao) do
      begin
        if ListaColunaEdicao[i].IndiceColuna = IndiceColuna then
        begin
          bAchou := True;
          case ListaColunaEdicao[i].TipoEdicao of
            teCheckBox:
            begin
              FGridAssociado.Options := FGridAssociado.Options - [goEditing];
            end;
            teString, teNumerico:
            begin
              if not FLeituraApenas then
                FGridAssociado.Options := FGridAssociado.Options + [goEditing];
            end;
          end;
          break;
        end;
      end;

      if not bAchou then
      begin
        FGridAssociado.Options := FGridAssociado.Options - [goEditing];
      end;
    end;
  end;
  LinhaSelecionada := ARow;
end;

procedure TGridTabela.OrdenaGridInterno(Coluna: Integer; Crescente: Boolean; NaoDeixeInverter: Boolean);
{var
  i, j: Integer;
  lista1, lista2: tstringlist;
  obj1, obj2: TObject;
  Nome1, Nome2: String;
  Troca: Boolean;}
var
  i: Integer;
  bAchou, bInverte: Boolean;
  ColunaConvertida: Integer;
begin
{  lista1 := TStringList.Create;
  lista2 := TStringList.Create;
  for i := FGridAssociado.FixedRows to FGridAssociado.RowCount - 2 do
  begin
    for j := i + 1 to FGridAssociado.RowCount - 1 do
    begin
      Nome1 := FGridAssociado.Cells[Coluna + FGridAssociado.FixedCols - 1, i];
      Nome2 := FGridAssociado.Cells[Coluna + FGridAssociado.FixedCols - 1, j];
      Troca := False;
      if (Crescente) and (Nome2 < Nome1) then
        Troca := True
      else if (not Crescente) and (Nome2 > Nome1) then
        Troca := True;
      if Troca then
      begin
        lista1.Text := FGridAssociado.Rows[i].Text;
        lista2.Text := FGridAssociado.Rows[j].Text;
        obj1 := FGridAssociado.Objects[0, i];
        obj2 := FGridAssociado.Objects[0, j];

        FGridAssociado.Rows[i].Text := lista2.Text;
        FGridAssociado.Rows[j].Text := lista1.Text;
        FGridAssociado.Objects[0, i] := obj2;
        FGridAssociado.Objects[0, j] := obj1;
      end;
    end;
  end;
  lista1.free;
  lista2.free;}

  // converte a coluna para o formato do StringGrid
  bAchou := False;
  bInverte := False;
  for i := 0 to ListaColunasOrdenadas.Count - 1 do
  begin
    if Coluna = StrToInt(ListaColunasOrdenadas.Strings[i]) then
    begin
      bAchou := True;
      if i <> 0 then
      begin
        ListaColunasOrdenadas.Delete(i);
        ListaColunasOrdenadas.Insert(0, IntToStr(Coluna));
      end
      else
      begin
        bInverte := True;
      end;
      break;
    end;
  end;
  if not bAchou then
    ListaColunasOrdenadas.Insert(0, IntToStr(Coluna));

  Application.ProcessMessages;
  if ( bInverte ) and ( not NaoDeixeInverter )then
    InverteLinhas
  else
    QuickSortRec(IndiceLinhaParaGrid(1), FGridAssociado.RowCount - 1);
end;

procedure TGridTabela.QuickSortRec(Inicio, Fim: Integer);
var
  i, j: Integer;
  Pivot: Integer;
  AuxStr: TStringList;
  obj1, obj2: TObject;
begin
  AuxStr := TStringList.Create;
  repeat
    I := Inicio;
    J := Fim;
    Pivot := (Inicio + Fim) shr 1;
    repeat
      while ComparaLinhasGrid(I, Pivot) < 0 do Inc(I);
      while ComparaLinhasGrid(J, Pivot) > 0 do Dec(J);
      if I <= J then
      begin
        obj1 := FGridAssociado.Objects[0, I];
        obj2 := FGridAssociado.Objects[0, J];

        AuxStr.Clear;
        AuxStr.AddStrings(FGridAssociado.Rows[I]);
        // as instruções abaixo removem as informações dos objetos associados
        FGridAssociado.Rows[I].Text := FGridAssociado.Rows[J].Text;
        FGridAssociado.Rows[J].Text := AuxStr.Text;

        FGridAssociado.Objects[0, I] := obj2;
        FGridAssociado.Objects[0, J] := obj1;

        if Pivot = I then
          Pivot := J
        else if Pivot = J then
          Pivot := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if Inicio < J then QuickSortRec(Inicio, J);
    Inicio := I;
  until I >= Fim;
  AuxStr.Free;
end;

procedure TGridTabela.GridDuploClique(Sender: TObject);
var
  i: Integer;
begin
  if (not ClicouNoTitulo) and (assigned(DuploCliqueAnterior)) then
    DuploCliqueAnterior(Sender);
{  if FEmEdicao then
  begin
    for i := Low(ListaColunaEdicao) to High(ListaColunaEdicao) do
    begin
      if ListaColunaEdicao[i].IndiceColuna =
    end;
  end;}
end;

function TGridTabela.GridVazia: Boolean;
var
  i: Integer;
begin
  if FGridAssociado.RowCount > FGridAssociado.FixedRows + 1 then
    result := False
  else
  begin
    result := True;
    if not FAtualizando then
    begin
      for i := 0 to FGridAssociado.ColCount - 1 do
      begin
        result := True;
        if Trim(FGridAssociado.Cells[i, FGridAssociado.FixedRows]) <> '' then
        begin
          result := False;
          break;
        end;
      end;
    end
    else
    begin
      if Length(ListaAtualizacoes) > 0 then
        result := False
      else
        result := True;
    end;
  end;
end;

procedure TGridTabela.CriaBitmapCheckBox;
begin
  BitMapCheckBox := TBitmap.Create;
  BitMapCheckBox.Width := 9;
  BitMapCheckBox.Height := 9;

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

procedure TGridTabela.AtualizaLinha(Linha: TStringList);
begin
  AtualizaLinha(Linha, gridParaIndiceLinha(FGridAssociado.Row));
end;

procedure TGridTabela.AtualizaLinha(Linha: array of String);
var
  Lista: TStringList;
  i: Integer;
begin
  Lista := TStringList.Create;
  for i := Low(Linha) to High(Linha) do
    Lista.Add(Linha[i]);
  AtualizaLinha(Lista);
  Lista.Free;
end;

procedure TGridTabela.RegistraColunaImagem(IndiceColuna, Largura,
  Altura: Integer);
var
  novoRegistroImagem: TRegColunaImagem;
begin
  novoRegistroImagem.IndiceColuna := IndiceColuna;
  novoRegistroImagem.Largura := Largura;
  novoRegistroImagem.Altura := Altura;
  SetLength(ListaColunaImagens, Length(ListaColunaImagens) + 1);
  ListaColunaImagens[High(ListaColunaImagens)] := novoRegistroImagem;
end;

function TGridTabela.RegistraImagem(Bitmap: TBitmap): Integer;
begin
  result := ListaImagens.Add(Bitmap);
end;

function TGridTabela.ComparaLinhasGrid(Linha1, Linha2: Integer): Integer;
var
  i, Coluna, aux1, aux2: Integer;
  aux3, aux4: Extended;
begin
  result := 0;
  for i := 0 to ListaColunasOrdenadas.Count - 1 do
  begin
    Coluna := IndiceColunaParaGrid(StrToInt(ListaColunasOrdenadas.Strings[i]));
    if TipoEdicao( StrToInt(ListaColunasOrdenadas.Strings[i]) ) = teNumerico then
    begin
      if not TryStrToFloat(ApenasNumeros(FGridAssociado.Cells[Coluna, Linha1]), aux3) then
        aux3 := 0;
      if not TryStrToFloat(ApenasNumeros(FGridAssociado.Cells[Coluna, Linha2]), aux4) then
        aux4 := 0;
      result := round( aux3 - aux4 );
    end
    else
    begin
      // para compatibilidades anteriores...
      if TryStrToInt(FGridAssociado.Cells[Coluna, Linha1], aux1) and
           TryStrToInt(FGridAssociado.Cells[Coluna, Linha2], aux2) then
        result := aux1 - aux2
      else
      begin
        if FGridAssociado.Cells[Coluna, Linha1] < FGridAssociado.Cells[Coluna, Linha2] then
          result := -1
        else if FGridAssociado.Cells[Coluna, Linha1] > FGridAssociado.Cells[Coluna, Linha2] then
          result := 1;
      end;
    end;
    if result <> 0 then
      break;
  end;
end;

procedure TGridTabela.InverteLinhas;
var
  i, j, QtdLinhasAOrdenar: Integer;
  AuxStr: TStringList;
  obj1, obj2: TObject;
begin
  AuxStr := TStringList.Create;
  QtdLinhasAOrdenar := Floor((FGridAssociado.RowCount - FGridAssociado.FixedRows) / 2);
  i := 0;
  while i < QtdLinhasAOrdenar do
  begin
    j := FGridAssociado.RowCount - i - 1;
    obj1 := FGridAssociado.Objects[0, I + FGridAssociado.FixedRows];
    obj2 := FGridAssociado.Objects[0, J];

    AuxStr.Clear;
    AuxStr.AddStrings(FGridAssociado.Rows[I + FGridAssociado.FixedRows]);
    // as instruções abaixo removem as informações dos objetos associados
    FGridAssociado.Rows[i + FGridAssociado.FixedRows].Text := FGridAssociado.Rows[J].Text;
    FGridAssociado.Rows[J].Text := AuxStr.Text;

    FGridAssociado.Objects[0, I + FGridAssociado.FixedRows] := obj2;
    FGridAssociado.Objects[0, J] := obj1;

    inc(i);
  end;
  AuxStr.Free;
end;

procedure TGridTabela.IniciaEdicao;
var
  CanSelect: Boolean;
begin
  if not FEmEdicao then
  begin
    OpcoesAnteriores := FGridAssociado.Options;
    FGridAssociado.Options := FGridAssociado.Options - [goRowSelect];
  end;

  FEmEdicao := True;
end;

procedure TGridTabela.RegistraCampoEdicao(IndiceColuna: Integer;
  TipoEdicao: TTipoEdicao);
var
  umTipoEdicao: TRegTipoEdicao;
begin
  umTipoEdicao.IndiceColuna := IndiceColuna;
  umTipoEdicao.TipoEdicao := TipoEdicao;
  SetLength(ListaColunaEdicao, Length(ListaColunaEdicao) + 1);
  ListaColunaEdicao[High(ListaColunaEdicao)] := umTipoEdicao;
end;

procedure TGridTabela.FinalizaEdicao;
begin
  if FEmEdicao then
  begin
    FEmEdicao := False; // o comando abaixo aciona um SelectCell
    FGridAssociado.Options := OpcoesAnteriores;
  end
  else
    FEmEdicao := False;
end;

function TGridTabela.GridParaIndiceColuna(Valor: Integer): Integer;
var
  i, IndiceRealColuna: Integer;
begin
  if FIndicador then
    result := StrToInt(ListaOrdemColunas.Strings[Valor - 1])
  else
    Result := StrToInt(ListaOrdemColunas.Strings[Valor]);
end;

function TGridTabela.IndiceColunaParaGrid(Valor: Integer): Integer;
var
  i: Integer;
begin
  for i := 0 to ListaOrdemColunas.Count - 1 do
  begin
    if StrToInt(ListaOrdemColunas.Strings[i]) = Valor then
    begin
      if FIndicador then
        result := i + 1
      else
        Result := i;
    end;
  end;
end;

procedure TGridTabela.FinalizaAtualizacao;
var
  i, j: Integer;
  umReg: TRegTipoAtualizacao;
  NumLinhas, InicioAtualizacao, IndiceInicio: Integer;
  IndicesExcluidos: array of Integer;
  bExcluido: Boolean;
begin
  if FAtualizando then
  begin
    FAtualizando := False;

    // cálculo do Número de Linhas
    NumLinhas := QtdLinhas;
    InicioAtualizacao := QtdLinhas + 1;
    IndiceInicio := Low(ListaAtualizacoes);
    SetLength(IndicesExcluidos, 0);
    for i := Low(ListaAtualizacoes) to High(ListaAtualizacoes) do
    begin
      umReg := ListaAtualizacoes[i];
      case umReg.TipoAtualizacao of
        taInsercaoLinha: inc(NumLinhas);
        taRemocaoLinha:
        begin
          dec(NumLinhas);
          SetLength(IndicesExcluidos, Length(IndicesExcluidos) + 1);
          IndicesExcluidos[High(IndicesExcluidos)] := umReg.ParametroRemocao;
        end;
        taLimpezaGrid:
        begin
          NumLinhas := 0;
          InicioAtualizacao := 1;
          IndiceInicio := i;
          SetLength(IndicesExcluidos, 0);
        end;
      end;
    end;

    // atualiza
    if NumLinhas = 0 then
      Limpa
    else
    begin
      QtdLinhas := NumLinhas;
      for i := IndiceInicio to High(ListaAtualizacoes) do
      begin
        umReg := ListaAtualizacoes[i];
        case umReg.TipoAtualizacao of
          taInsercaoLinha, taInsercaoObjeto:
          begin
            bExcluido := False;
            for j := Low(IndicesExcluidos) to High(IndicesExcluidos) do
            begin
              if ((umReg.TipoAtualizacao = taInsercaoLinha) and (umReg.bIndiceInsercao)) or
                  (umReg.TipoAtualizacao = taInsercaoObjeto) then
              begin
                if umReg.ParametroRemocao = IndicesExcluidos[j] then
                  bExcluido := True;
              end
              else
                if InicioAtualizacao = IndicesExcluidos[j] then
                  bExcluido := True;

              if bExcluido then
              begin
                IndicesExcluidos[j] := -1;
                break;
              end;
            end;
            if not bExcluido then
            begin
              if umReg.TipoAtualizacao = taInsercaoLinha then
              begin
                if (umReg.bIndiceInsercao) then
                  AtualizaLinha(umReg.ParametroInsercao, umReg.ParametroRemocao)
                else
                  AtualizaLinha(umReg.ParametroInsercao, InicioAtualizacao);
                inc(InicioAtualizacao);
              end
              else
                InsereObjeto(umReg.ParametroRemocao, umReg.Objeto);
            end;
          end;
          taRemocaoLinha:
          begin
          end;
          taLimpezaGrid:
          begin
            NumLinhas := 0;
            InicioAtualizacao := 1;
          end;
        end;
      end;
    end;
    // limpa lista
    LimpaListaAtualizacao;
  end;
end;

procedure TGridTabela.IniciaAtualizacao;
begin
  LimpaListaAtualizacao;
  if FAtualizando then
    CancelaAtualizacao;
  AuxQtdLinhas := QtdLinhas;
  FAtualizando := True;
end;

function TGridTabela.GridParaIndiceLinha(Valor: Integer): Integer;
begin
  result := Valor - FGridAssociado.FixedRows + 1;
end;

function TGridTabela.IndiceLinhaParaGrid(Valor: Integer): Integer;
begin
  result := Valor + FGridAssociado.FixedRows - 1;
end;

procedure TGridTabela.GridKeypress(Sender: TObject; var Key: Char);
begin
  if assigned(KeyPressAnterior) then
    KeyPressAnterior(Sender, Key);

  if (FEmEdicao) and (not (FLeituraApenas)) and ((Key = #13) or (Key = ' ')) and
       ((FGridAssociado.Cells[FGridAssociado.Col, FGridAssociado.Row] = GRID_VALOR_VERDADEIRO) or
       (FGridAssociado.Cells[FGridAssociado.Col, FGridAssociado.Row] = GRID_VALOR_FALSO)) and
       (Length(ListaColunaEdicao) > 0) then
  begin
    iCont := Low(ListaColunaEdicao);
    while iCont <= High(ListaColunaEdicao) do
    begin
      if IndiceColunaParaGrid(ListaColunaEdicao[iCont].IndiceColuna) = FGridAssociado.Col then
      begin
        if FGridAssociado.Cells[FGridAssociado.Col, FGridAssociado.Row] = GRID_VALOR_VERDADEIRO then
          FGridAssociado.Cells[FGridAssociado.Col, FGridAssociado.Row] := GRID_VALOR_FALSO
        else if FGridAssociado.Cells[FGridAssociado.Col, FGridAssociado.Row] = GRID_VALOR_FALSO then
          FGridAssociado.Cells[FGridAssociado.Col, FGridAssociado.Row] := GRID_VALOR_VERDADEIRO;
        break;
      end;
      inc(iCont);
    end;
  end;
end;

function TGridTabela.LeColunaOrdenada: Integer;
begin
  if ListaColunasOrdenadas.Count = 0 then
    result := 0
  else
    result := StrToInt(ListaColunasOrdenadas.Strings[0]);
end;

procedure TGridTabela.MontaOrdemColunas;
var
  i, Inicio: Integer;
begin
  if Indicador then
    Inicio := 1
  else
    Inicio := 0;
  ListaOrdemColunas.Clear;
  for i := Inicio to FGridAssociado.ColCount - 1 do
  begin
    if Indicador then
      ListaOrdemColunas.Add(IntToStr(i))
    else
      ListaOrdemColunas.Add(IntToStr(i + 1));
  end;
end;

procedure TGridTabela.TrocaColuna(Coluna1, Coluna2: Integer);
var
  Pos: Integer;
  i: Integer;
  Aux: String;
  iAux: Integer;
  slAux: TStringList;
begin
  if Indicador then
    Pos := 1
  else
    Pos := 0;
  slAux := TStringList.Create;

  if Coluna1 < Coluna2 then
  begin
    Aux := ListaOrdemColunas.Strings[Coluna1 - Pos];
    iAux := FGridAssociado.ColWidths[Coluna1];
    slAux.AddStrings(FGridAssociado.Cols[Coluna1]);
    for i := Coluna1 to Coluna2 - 1 do
    begin
      ListaOrdemColunas.Strings[i - Pos] := ListaOrdemColunas.Strings[i - Pos + 1];
      FGridAssociado.ColWidths[i] := FGridAssociado.ColWidths[i + 1];
      FGridAssociado.Cols[i] := FGridAssociado.Cols[i + 1];
    end;
    ListaOrdemColunas.Strings[Coluna2 - Pos] := Aux;
    FGridAssociado.ColWidths[Coluna2] := iAux;
    FGridAssociado.Cols[Coluna2] := slAux;
  end
  else
  begin
    Aux := ListaOrdemColunas.Strings[Coluna1 - Pos];
    iAux := FGridAssociado.ColWidths[Coluna1];
    slAux.AddStrings(FGridAssociado.Cols[Coluna1]);
    for i := Coluna1 downto Coluna2 + 1 do
    begin
      ListaOrdemColunas.Strings[i - Pos] := ListaOrdemColunas.Strings[i - Pos - 1];
      FGridAssociado.ColWidths[i] := FGridAssociado.ColWidths[i - 1];
      FGridAssociado.Cols[i] := FGridAssociado.Cols[i - 1];
    end;
    ListaOrdemColunas.Strings[Coluna2 - Pos] := Aux;
    FGridAssociado.ColWidths[Coluna2] := iAux;
    FGridAssociado.Cols[Coluna2] := slAux;
  end;

  slAux.Free;
end;

procedure TGridTabela.GridMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if Ordena then
  begin
    LabelMallandro.Left := LabelMallandro.Left + (X - XAnt);
    LabelMallandro.Top := LabelMallandro.Top + (Y - YAnt);
    XAnt := X;
    YAnt := Y;

    FGridAssociado.MouseToCell(LabelMallandro.Left, 0, ColAux, RowAux);
    if (ColAux >= 0) and (ColAux <> ColunaSelecionada) and ((not Indicador) or ((Indicador) and (ColAux <> 0))) then
    begin
      FImagemSetaIndicativa.Visible := True;
      FImagemSetaIndicativa.Top := FGridAssociado.Top - FImagemSetaIndicativa.Height - 2;

      FImagemSetaIndicativa.Left := (FGridAssociado.CellRect(ColAux, RowAux).Left) - (FImagemSetaIndicativa.Width div 2) + FGridAssociado.Left;
    end
    else
      FImagemSetaIndicativa.Visible := False;
  end;
end;

procedure TGridTabela.ConstroiImagemSeta;
var
  Rect: TRect;
  Poligono: array of TPoint;
begin
  BitmapSetaIndicativa := TBitmap.Create;
  BitmapSetaIndicativa.Width := 9;
  BitmapSetaIndicativa.Height:= 12;
  Rect.TopLeft := Point(0,0);
  Rect.BottomRight := Point(BitmapSetaIndicativa.Width, BitmapSetaIndicativa.Height);
  BitmapSetaIndicativa.Canvas.Pen.Color := clBlack;
  BitmapSetaIndicativa.Canvas.Brush.Color := clBlue;
  BitmapSetaIndicativa.Canvas.FillRect(Rect);
  SetLength(Poligono, 7);
  Poligono[0] := Point((BitmapSetaIndicativa.Width - 1) div 4, 0);
  Poligono[1] := Point(Poligono[0].X, BitmapSetaIndicativa.Height - 1 - ((BitmapSetaIndicativa.Height - 1) div 2));
  Poligono[2] := Point(0, poligono[1].Y);
  Poligono[3] := Point(BitmapSetaIndicativa.Width div 2, BitmapSetaIndicativa.Height - 1);
  Poligono[4] := Point(BitmapSetaIndicativa.Width - 1, Poligono[2].Y);
  Poligono[5] := Point(BitmapSetaIndicativa.Width - 1 - ((BitmapSetaIndicativa.Width - 1) div 4), Poligono[4].Y);
  Poligono[6] := Point(poligono[5].X, 0);

  BitmapSetaIndicativa.Canvas.Brush.Color := clYellow;
  BitmapSetaIndicativa.Canvas.Polygon(Poligono);
  BitmapSetaIndicativa.TransparentColor := clBlue;
  BitmapSetaIndicativa.Transparent := True;

  FImagemSetaIndicativa := TImage.Create(FGridAssociado.Parent);
  FImagemSetaIndicativa.AutoSize := True;
  FImagemSetaIndicativa.Parent := FGridAssociado.Parent;
  FImagemSetaIndicativa.Picture.Bitmap := BitmapSetaIndicativa;
  BitmapSetaIndicativa.Free;
  FImagemSetaIndicativa.Visible := False;
  FImagemSetaIndicativa.Transparent := True;
end;

procedure TGridTabela.CancelaAtualizacao;
begin
  FAtualizando := False;
  LimpaListaAtualizacao;
end;

procedure TGridTabela.LimpaListaAtualizacao;
var
  i: Integer;
begin
  for i := Low(ListaAtualizacoes) to High(ListaAtualizacoes) do
  begin
    if ListaAtualizacoes[i].TipoAtualizacao = taInsercaoLinha then
      ListaAtualizacoes[i].ParametroInsercao.Free;
  end;
  SetLength(ListaAtualizacoes, 0);
end;

procedure TGridTabela.AtribuiTamanhoColuna(IndiceColuna, Tamanho: Integer);
begin
  FGridAssociado.ColWidths[IndiceColunaParaGrid(IndiceColuna)] := Tamanho;
end;

function TGridTabela.LeTamanhoColuna(IndiceColuna: Integer): Integer;
begin
  result := FGridAssociado.ColWidths[IndiceColunaParaGrid(IndiceColuna)];
end;

procedure TGridTabela.AtualizaCelula(Col, Linha: Integer; Valor: String);
begin
  FGridAssociado.Cells[IndiceColunaParaGrid(Col), IndiceLinhaParaGrid(Linha)] := Valor;
end;

function TGridTabela.LeCelula(Col, Linha: Integer): String;
begin
  result := FGridAssociado.Cells[IndiceColunaParaGrid(Col), IndiceLinhaParaGrid(Linha)];
end;

function TGridTabela.PosicaoTexto(Coluna: Integer): TPosicaoTexto;
begin
  if TipoEdicao(Coluna) = teNumerico then
    Result := ptDireita
  else
    Result := ptEsquerda;
end;

function TGridTabela.TipoEdicao(Coluna: Integer): TTipoEdicao;
var
  i: Integer;
begin
  Result := teNenhum;
  for i := Low(ListaColunaEdicao) to High(ListaColunaEdicao) do
  begin
    if ListaColunaEdicao[i].IndiceColuna = Coluna then
    begin
      Result := ListaColunaEdicao[i].TipoEdicao;
      break;
    end;
  end;
end;

procedure TGridTabela.AtribuiLeituraApenas(const Value: Boolean);
begin
  FLeituraApenas := Value;
  FGridAssociado.Options := FGridAssociado.Options - [goEditing];
end;

function TGridTabela.ChecaErros: Boolean;
var
  i, j: Integer;
begin
  Result := False;
  for i := Low(ListaColunaEdicao) to High(ListaColunaEdicao) do
  begin
    if ListaColunaEdicao[i].TipoEdicao = teNumerico then
    begin
      for j := 1 to QtdLinhas do
      begin
        if not ( ValorValido ( LeCelula(ListaColunaEdicao[i].IndiceColuna,j) ) ) then
        begin
          GridAssociado.Col := GridParaIndiceColuna(ListaColunaEdicao[i].IndiceColuna);
          GridAssociado.Row := GridParaIndiceLinha(j);
          Result := True;
          break;
        end;
      end;
    end;
    if Result then
      break;
  end;
end;

procedure TGridTabela.OrdenaGrid(Coluna: Integer; Crescente: Boolean);
begin
  OrdenaGridInterno ( Coluna, Crescente, True );
end;

procedure TGridTabela.AjustaTamanhoColuna(IndiceColuna: Integer);
var
  i: Integer;
  TotalSoma: Integer;
begin
  TotalSoma := 0;
  for i := 0 to GridAssociado.ColCount - 1 do
  begin
    if i <> IndiceColunaParaGrid(IndiceColuna) then
    begin
      TotalSoma := TotalSoma + LeTamanhoColuna(GridParaIndiceColuna(i));
    end;
  end;
  if TotalSoma < GridAssociado.ClientWidth then
  begin
    AtribuiTamanhoColuna(IndiceColuna, GridAssociado.ClientWidth - TotalSoma - (GridAssociado.GridLineWidth * QtdColunas));
  end;
end;

procedure TGridTabela.ExibeDataSet(DataSet: TDataset);
var
  Tupla: TStringList;
  i: Integer;
  Cabecalho: array of String;
begin
  Limpa;

  Tupla := TStringList.Create;
  SetLength ( Cabecalho, 0 );
  try
    for i := 0 to DataSet.Fields.Count - 1 do
    begin
      if DataSet.Fields.Fields[i].Visible then
      begin
        SetLength ( Cabecalho, Length(Cabecalho) + 1 );
        Cabecalho [ High(Cabecalho) ] := DataSet.Fields.Fields[i].DisplayName;
      end;
    end;
    PreencheCabecalho( Cabecalho );

    DataSet.First;
    while not DataSet.Eof do
    begin
      Tupla.Clear;
      for i := 0 to DataSet.Fields.Count - 1 do
      begin
        if DataSet.Fields.Fields[i].Visible then
        begin
          if ( DataSet.Fields.Fields[i] is TBooleanField ) and ( UpperCase ( DataSet.Fields.Fields[i].DisplayText ) <> 'FALSE' ) and ( UpperCase ( DataSet.Fields.Fields[i].DisplayText ) <> 'TRUE' ) then
          begin
            if DataSet.Fields.Fields[i].IsNull then
              Tupla.Add( '' )
            else if TBooleanField ( DataSet.Fields.Fields[i] ).Value then
              Tupla.Add( GRID_VALOR_VERDADEIRO )
            else
              Tupla.Add( GRID_VALOR_FALSO )
          end
          else
            Tupla.Add( DataSet.Fields.Fields[i].DisplayText );
        end;
      end;
      InserirLinha( Tupla );

      DataSet.Next;
    end;
  finally
    Tupla.Free;
  end;
end;

procedure TGridTabela.GravaRegistrosEmArquivo(NomeArquivoModelo: String);
var
  i, j, valorColuna: Integer;
  VetoresStringList: array of TStringList;

  procedure AdicionaStringList;
  begin
    SetLength ( VetoresStringList, Length(VetoresStringList) + 1 );
    VetoresStringList[ High ( VetoresStringList ) ] := TStringList.Create;
  end;

  procedure DestroiStringsLists;
  var
    iLocal: Integer;
  begin
    for iLocal := Low ( VetoresStringList ) to High ( VetoresStringList ) do
      VetoresStringList[iLocal].Free;
  end;

begin
  try
    for i := 0 to QtdColunas - 1 do
    begin
      AdicionaStringList;
      VetoresStringList[ i ].Add( GridAssociado.Cells[ IndiceColunaParaGrid( i + 1 ), 0 ] );
    end;

    for i := 1 to QtdLinhas do
    begin
      for j := Low ( VetoresStringList ) to High ( VetoresStringList ) do
      begin
        if ( LeCelula( j + 1, i ) = GRID_VALOR_VERDADEIRO ) or ( LeCelula( j + 1, i ) = GRID_VALOR_FALSO ) then
        begin
          if ( LeCelula( j + 1, i ) = GRID_VALOR_VERDADEIRO ) then
            VetoresStringList[j].Add( 'Verdadeiro' )
          else
            VetoresStringList[j].Add( 'Falso' )
        end
        else
          VetoresStringList[j].Add( LeCelula( j + 1, i ) );
      end;
    end;

    for i := Low ( VetoresStringList ) to High ( VetoresStringList ) do
    begin
      VetoresStringList[i].SaveToFile( NomeArquivoModelo + '_' + IntToStr(i + 1) + '.txt' );
    end;
  finally
    DestroiStringsLists;
  end;
end;

end.