unit interfazBandejadeBorrador;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  estructuras;

type

  { TFormInterfazBandejadeBorrador }

  TFormInterfazBandejadeBorrador = class(TForm)
    btnCerrar: TButton;
    btnPreOrden: TButton;
    btnInOrden: TButton;
    btnRefrescar: TButton;
    btnPostOrden: TButton;
    Label1: TLabel;
    ListBox1: TListBox;
    procedure btnCerrarClick(Sender: TObject);
    procedure btnPreOrdenClick(Sender: TObject);
    procedure btnInOrdenClick(Sender: TObject);
    procedure btnPostOrdenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure btnRefrescarClick(Sender: TObject);
    procedure ListBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FUsuario: PUsuario; // referencia al usuario cuyos borradores mostramos
    ListaFiltrada: array of PBorradorAVL; // mapea items del ListBox a nodos del árbol (en orden mostrado)
    procedure LimpiarListBox;
    function CountNodes(raiz: PBorradorAVL): Integer;
    procedure FillPreOrder(raiz: PBorradorAVL; var idx: Integer);
    procedure FillInOrder(raiz: PBorradorAVL; var idx: Integer);
    procedure FillPostOrder(raiz: PBorradorAVL; var idx: Integer);
    procedure PoblarPreOrder;
    procedure PoblarInOrder;
    procedure PoblarPostOrder;
    function BuscarBorradorPorID(raiz: PBorradorAVL; id: Integer): PBorradorAVL;
    procedure AbrirBorradorPorID(id: Integer);
    procedure EnsureListBoxEvents;
  public
    procedure SetUsuario(usuario: PUsuario);
  end;

var
  FormInterfazBandejadeBorrador: TFormInterfazBandejadeBorrador;

implementation

uses
  interfazBorrador, // se necesita para abrir el formulario de borrador
  interfazUsuario;  // en implementation para evitar ciclos en la sección interface

{$R *.lfm}

{ -------------------- utilidades -------------------- }

procedure TFormInterfazBandejadeBorrador.FormCreate(Sender: TObject);
begin
  FUsuario := nil;
  SetLength(ListaFiltrada, 0);
  EnsureListBoxEvents;
  LimpiarListBox;
end;

procedure TFormInterfazBandejadeBorrador.EnsureListBoxEvents;
begin
  if Assigned(ListBox1) then
  begin
    ListBox1.OnDblClick := @ListBox1DblClick;
    ListBox1.OnMouseDown := @ListBox1MouseDown;
    ListBox1.OnClick := @ListBox1Click;
  end;
end;

procedure TFormInterfazBandejadeBorrador.LimpiarListBox;
begin
  if Assigned(ListBox1) then
    ListBox1.Clear;
  SetLength(ListaFiltrada, 0);
end;

function TFormInterfazBandejadeBorrador.CountNodes(raiz: PBorradorAVL): Integer;
begin
  if raiz = nil then
    Exit(0);
  Result := 1 + CountNodes(raiz^.izquierdo) + CountNodes(raiz^.derecho);
end;

procedure TFormInterfazBandejadeBorrador.FillPreOrder(raiz: PBorradorAVL; var idx: Integer);
var
  line: string;
  remitente, destinatario, asunto: string;
begin
  if raiz = nil then Exit;

  // guardar en mapeo
  ListaFiltrada[idx] := raiz;

  remitente := Trim(raiz^.remitente);
  destinatario := Trim(raiz^.destinatarioEmail);
  asunto := Trim(raiz^.asunto);
  line := Format('%d | %s | %s | %s | Ver', [raiz^.id, remitente, destinatario, asunto]);
  ListBox1.Items.Add(line);
  Inc(idx);

  FillPreOrder(raiz^.izquierdo, idx);
  FillPreOrder(raiz^.derecho, idx);
end;

procedure TFormInterfazBandejadeBorrador.FillInOrder(raiz: PBorradorAVL; var idx: Integer);
var
  line: string;
  remitente, destinatario, asunto: string;
begin
  if raiz = nil then Exit;

  FillInOrder(raiz^.izquierdo, idx);

  ListaFiltrada[idx] := raiz;
  remitente := Trim(raiz^.remitente);
  destinatario := Trim(raiz^.destinatarioEmail);
  asunto := Trim(raiz^.asunto);
  line := Format('%d | %s | %s | %s | Ver', [raiz^.id, remitente, destinatario, asunto]);
  ListBox1.Items.Add(line);
  Inc(idx);

  FillInOrder(raiz^.derecho, idx);
end;

procedure TFormInterfazBandejadeBorrador.FillPostOrder(raiz: PBorradorAVL; var idx: Integer);
var
  line: string;
  remitente, destinatario, asunto: string;
begin
  if raiz = nil then Exit;

  FillPostOrder(raiz^.izquierdo, idx);
  FillPostOrder(raiz^.derecho, idx);

  ListaFiltrada[idx] := raiz;
  remitente := Trim(raiz^.remitente);
  destinatario := Trim(raiz^.destinatarioEmail);
  asunto := Trim(raiz^.asunto);
  line := Format('%d | %s | %s | %s | Ver', [raiz^.id, remitente, destinatario, asunto]);
  ListBox1.Items.Add(line);
  Inc(idx);
end;

procedure TFormInterfazBandejadeBorrador.PoblarPreOrder;
var
  total, idx: Integer;
begin
  LimpiarListBox;
  if (FUsuario = nil) or (FUsuario^.listaBorradores = nil) then Exit;

  total := CountNodes(FUsuario^.listaBorradores);
  if total = 0 then Exit;

  SetLength(ListaFiltrada, total);
  idx := 0;
  FillPreOrder(FUsuario^.listaBorradores, idx);
end;

procedure TFormInterfazBandejadeBorrador.PoblarInOrder;
var
  total, idx: Integer;
begin
  LimpiarListBox;
  if (FUsuario = nil) or (FUsuario^.listaBorradores = nil) then Exit;

  total := CountNodes(FUsuario^.listaBorradores);
  if total = 0 then Exit;

  SetLength(ListaFiltrada, total);
  idx := 0;
  FillInOrder(FUsuario^.listaBorradores, idx);
end;

procedure TFormInterfazBandejadeBorrador.PoblarPostOrder;
var
  total, idx: Integer;
begin
  LimpiarListBox;
  if (FUsuario = nil) or (FUsuario^.listaBorradores = nil) then Exit;

  total := CountNodes(FUsuario^.listaBorradores);
  if total = 0 then Exit;

  SetLength(ListaFiltrada, total);
  idx := 0;
  FillPostOrder(FUsuario^.listaBorradores, idx);
end;

function TFormInterfazBandejadeBorrador.BuscarBorradorPorID(raiz: PBorradorAVL; id: Integer): PBorradorAVL;
begin
  Result := nil;
  if raiz = nil then Exit;

  if id = raiz^.id then
    Exit(raiz)
  else if id < raiz^.id then
    Result := BuscarBorradorPorID(raiz^.izquierdo, id)
  else
    Result := BuscarBorradorPorID(raiz^.derecho, id);
end;

procedure TFormInterfazBandejadeBorrador.AbrirBorradorPorID(id: Integer);
var
  nodo: PBorradorAVL;
begin
  if FUsuario = nil then Exit;
  nodo := BuscarBorradorPorID(FUsuario^.listaBorradores, id);
  if nodo = nil then
  begin
    ShowMessage('Borrador no encontrado (id: ' + IntToStr(id) + ').');
    Exit;
  end;

  if not Assigned(FormInterfazBorrador) then
    Application.CreateForm(TFormInterfazBorrador, FormInterfazBorrador);

  // pasar referencia del usuario y el puntero al borrador
  FormInterfazBorrador.SetUsuarioYBorrador(FUsuario, nodo);
  FormInterfazBorrador.Show;
  Self.Hide;
end;

{ -------------------- eventos UI -------------------- }

procedure TFormInterfazBandejadeBorrador.SetUsuario(usuario: PUsuario);
begin
  FUsuario := usuario;
  // por defecto mostrar PreOrder
  PoblarPreOrder;
end;

procedure TFormInterfazBandejadeBorrador.btnPreOrdenClick(Sender: TObject);
begin
  PoblarPreOrder;
end;

procedure TFormInterfazBandejadeBorrador.btnInOrdenClick(Sender: TObject);
begin
  PoblarInOrder;
end;

procedure TFormInterfazBandejadeBorrador.btnPostOrdenClick(Sender: TObject);
begin
  PoblarPostOrder;
end;

procedure TFormInterfazBandejadeBorrador.btnRefrescarClick(Sender: TObject);
begin
  // refrescar por defecto como PreOrder
  PoblarPreOrder;
end;

procedure TFormInterfazBandejadeBorrador.btnCerrarClick(Sender: TObject);
begin
  // volver a interfazUsuario
  if Assigned(FormUsuario) then
    FormUsuario.Show;
  Self.Hide;
end;

procedure TFormInterfazBandejadeBorrador.ListBox1Click(Sender: TObject);
begin
  // no hacemos nada especial en click simple (el MouseDown detecta clicks sobre "Ver")
end;

procedure TFormInterfazBandejadeBorrador.ListBox1DblClick(Sender: TObject);
var
  idx: Integer;
  nodo: PBorradorAVL;
  line: string;
  barPos: Integer;
  idStr: string;
  idVal: Integer;
begin
  idx := ListBox1.ItemIndex;
  if (idx < 0) then Exit;

  // si tenemos mapeo directo, usarlo
  if (Length(ListaFiltrada) > 0) and (idx <= High(ListaFiltrada)) and Assigned(ListaFiltrada[idx]) then
  begin
    nodo := ListaFiltrada[idx];
    if Assigned(nodo) then
    begin
      if not Assigned(FormInterfazBorrador) then
        Application.CreateForm(TFormInterfazBorrador, FormInterfazBorrador);
      FormInterfazBorrador.SetUsuarioYBorrador(FUsuario, nodo);
      FormInterfazBorrador.Show;
      Self.Hide;
      Exit;
    end;
  end;

  // fallback: extraer id de la cadena
  line := ListBox1.Items[idx];
  barPos := Pos(' |', line);
  if barPos = 0 then Exit;
  idStr := Trim(Copy(line, 1, barPos-1));
  if not TryStrToInt(idStr, idVal) then Exit;

  AbrirBorradorPorID(idVal);
end;

procedure TFormInterfazBandejadeBorrador.ListBox1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  idx: Integer;
  itemText, beforeVer, verText: string;
  posVer: Integer;
  textLeft, textWidthBefore, verStart, verEnd: Integer;
  nodo: PBorradorAVL;
  SUFIJO_VER: string;
begin
  idx := ListBox1.ItemAtPos(Point(X, Y), True);
  if idx = -1 then Exit;
  ListBox1.ItemIndex := idx;

  itemText := ListBox1.Items[idx];
  SUFIJO_VER := ' | Ver';

  // comprobar que la línea termine con el sufijo esperado
  posVer := Pos(SUFIJO_VER, itemText);
  if (posVer = 0) or (posVer + Length(SUFIJO_VER) - 1 <> Length(itemText)) then
    Exit;

  beforeVer := Copy(itemText, 1, posVer - 1);
  verText := Copy(itemText, posVer + 1, Length(SUFIJO_VER)-1); // 'Ver' parte (sin la barra inicial)

  // cálculo aproximado de la posición en pixeles del inicio del sufijo
  // textLeft = padding izquierdo aproximado del ListBox (ajustable si tu ListBox tiene margen distinto)
  textLeft := 2;
  textWidthBefore := ListBox1.Canvas.TextWidth(beforeVer);
  verStart := textLeft + textWidthBefore;
  verEnd := verStart + ListBox1.Canvas.TextWidth(verText);

  // si el X del click cae dentro del rectángulo del texto 'Ver', abrimos el borrador
  if (X >= verStart) and (X <= verEnd) then
  begin
    if (idx >= 0) and (idx <= High(ListaFiltrada)) then
    begin
      nodo := ListaFiltrada[idx];
      if Assigned(nodo) then
      begin
        if not Assigned(FormInterfazBorrador) then
          Application.CreateForm(TFormInterfazBorrador, FormInterfazBorrador);

        FormInterfazBorrador.SetUsuarioYBorrador(FUsuario, nodo);
        FormInterfazBorrador.Show;
        Self.Hide;
      end
      else
      begin
        // fallback: intentar extraer id de la cadena
        AbrirBorradorPorID(StrToIntDef(Trim(Copy(itemText, 1, Pos(' |', itemText)-1)), -1));
      end;
    end;
  end;
end;

end.

