unit interfazBandejadeFavoritos;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Types,
  estructuras;

type

  { TFormInterfazBandejadeFavoritos }

  TFormInterfazBandejadeFavoritos = class(TForm)
    btnContador: TButton;
    btnCerrar: TButton;
    LabelFavorito: TLabel;
    ListBox1: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure btnCerrarClick(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure btnContadorClick(Sender: TObject);
    procedure ListBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FUsuario: PUsuario;
    ListaMap: array of PCorreo; // mapea línea->PCorreo
    // B-tree helpers (operan sobre PFavoritoB)
    function CrearNodoFavorito(esHoja: Boolean): PFavoritoB;
    procedure SplitChild(parentNode: PFavoritoB; i: Integer);
    procedure InsertNonFull(nodo: PFavoritoB; key: Integer; correo: PCorreo);
    procedure InsertarFavorito(var raiz: PFavoritoB; key: Integer; correo: PCorreo);
    procedure RecorrerYMapear(raiz: PFavoritoB; var idx: Integer);
    // UI helpers
    procedure LimpiarList;
    procedure ConstruirArbolFavoritos;
    procedure PoblarListBoxDesdeArbol;
  public
    procedure SetUsuario(usuario: PUsuario);
  end;

var
  FormInterfazBandejadeFavoritos: TFormInterfazBandejadeFavoritos;

implementation

uses
  interfazFavorito, interfazUsuario;

{$R *.lfm}

const
  MAX_KEYS = 4; // orden 5 -> max 4 keys per node

{ ------------------ B-TREE: crear nodo ------------------ }
function TFormInterfazBandejadeFavoritos.CrearNodoFavorito(esHoja: Boolean): PFavoritoB;
var
  n: PFavoritoB;
  i: Integer;
begin
  New(n);
  for i := 1 to 4 do
  begin
    n^.keys[i] := 0;
    n^.corrs[i] := nil;
  end;
  for i := 0 to 4 do
    n^.hijos[i] := nil;
  n^.numKeys := 0;
  n^.esHoja := esHoja;
  Result := n;
end;

{ SplitChild: asume parentNode^.hijos[i] está lleno (numKeys = MAX_KEYS)
  Divide en dos nodos y promueve la clave mediana (en posición 3 si keys=1..4) }
procedure TFormInterfazBandejadeFavoritos.SplitChild(parentNode: PFavoritoB; i: Integer);
var
  y, z: PFavoritoB;
  j: Integer;
  medianIndex: Integer;
  kUp: Integer;
  cUp: PCorreo;
begin
  y := parentNode^.hijos[i];
  if y = nil then Exit;

  z := CrearNodoFavorito(y^.esHoja);

  // median index for MAX_KEYS=4 -> promote key at position 3 (1..4)
  medianIndex := 3;

  // move keys to z (keys right of median)
  if y^.numKeys >= medianIndex + 1 then
  begin
    z^.numKeys := y^.numKeys - (medianIndex);
    // copy keys medianIndex+1 .. y.numKeys into z keys 1..z.numKeys
    for j := 1 to z^.numKeys do
    begin
      z^.keys[j] := y^.keys[medianIndex + j];
      z^.corrs[j] := y^.corrs[medianIndex + j];
    end;
  end
  else
    z^.numKeys := 0;

  // move children if not leaf
  if not y^.esHoja then
  begin
    // children associated to right keys move to z
    for j := 0 to z^.numKeys do
      z^.hijos[j] := y^.hijos[medianIndex + j];
  end;

  // reduce y.numKeys
  y^.numKeys := medianIndex - 1; // if medianIndex=3 => y.numKeys=2

  // shift parent's children to make room for z
  for j := parentNode^.numKeys downto i do
    parentNode^.hijos[j+1] := parentNode^.hijos[j];

  parentNode^.hijos[i] := y;
  parentNode^.hijos[i+1] := z;

  // shift parent's keys to make room
  for j := parentNode^.numKeys downto i do
  begin
    parentNode^.keys[j+1] := parentNode^.keys[j];
    parentNode^.corrs[j+1] := parentNode^.corrs[j];
  end;

  // promote median key from y to parent at position i
  kUp := y^.keys[medianIndex];
  cUp := y^.corrs[medianIndex];
  parentNode^.keys[i] := kUp;
  parentNode^.corrs[i] := cUp;

  Inc(parentNode^.numKeys);
end;

{ InsertNonFull: insertar en nodo no lleno (claves < MAX_KEYS) }
procedure TFormInterfazBandejadeFavoritos.InsertNonFull(nodo: PFavoritoB; key: Integer; correo: PCorreo);
var
  i: Integer;
begin
  if nodo = nil then Exit;

  if nodo^.esHoja then
  begin
    i := nodo^.numKeys;
    while (i >= 1) and (key < nodo^.keys[i]) do
    begin
      nodo^.keys[i+1] := nodo^.keys[i];
      nodo^.corrs[i+1] := nodo^.corrs[i];
      Dec(i);
    end;
    nodo^.keys[i+1] := key;
    nodo^.corrs[i+1] := correo;
    Inc(nodo^.numKeys);
  end
  else
  begin
    i := nodo^.numKeys;
    while (i >= 1) and (key < nodo^.keys[i]) do
      Dec(i);
    // child index to descend (0..numKeys) -> transform to 1-based child index
    Inc(i);
    if (nodo^.hijos[i] <> nil) and (nodo^.hijos[i]^.numKeys = MAX_KEYS) then
    begin
      SplitChild(nodo, i);
      if (i <= nodo^.numKeys) and (key > nodo^.keys[i]) then
        Inc(i);
    end;
    InsertNonFull(nodo^.hijos[i], key, correo);
  end;
end;

{ InsertarFavorito: wrapper que gestiona la raíz (split si raíz está llena) }
procedure TFormInterfazBandejadeFavoritos.InsertarFavorito(var raiz: PFavoritoB; key: Integer; correo: PCorreo);
var
  s: PFavoritoB;
begin
  if raiz = nil then
  begin
    raiz := CrearNodoFavorito(True);
    raiz^.keys[1] := key;
    raiz^.corrs[1] := correo;
    raiz^.numKeys := 1;
    Exit;
  end;

  if raiz^.numKeys = MAX_KEYS then
  begin
    s := CrearNodoFavorito(False);
    s^.hijos[0] := raiz;
    s^.numKeys := 0;
    // split child 1 (old root)
    SplitChild(s, 1);
    if key > s^.keys[1] then
      InsertNonFull(s^.hijos[2], key, correo)
    else
      InsertNonFull(s^.hijos[1], key, correo);
    raiz := s;
  end
  else
    InsertNonFull(raiz, key, correo);
end;

{ Recorrer árbol B en in-order y poblar ListBox mapeando a ListaMap }
procedure TFormInterfazBandejadeFavoritos.RecorrerYMapear(raiz: PFavoritoB; var idx: Integer);
var
  i: Integer;
  remitente: string;
  line: string;
begin
  if raiz = nil then Exit;

  if raiz^.esHoja then
  begin
    for i := 1 to raiz^.numKeys do
    begin
      ListaMap[idx] := raiz^.corrs[i];
      if Assigned(ListaMap[idx]) and Assigned(ListaMap[idx]^.remitenteUsuario) then
        remitente := ListaMap[idx]^.remitenteUsuario^.email
      else
        remitente := '<desconocido>';
      line := Format('%d | %s | %s | Ver', [raiz^.keys[i], raiz^.corrs[i]^.asunto, remitente]);
      ListBox1.Items.Add(line);
      Inc(idx);
    end;
  end
  else
  begin
    for i := 1 to raiz^.numKeys do
    begin
      // recorrer hijo i-1
      RecorrerYMapear(raiz^.hijos[i-1], idx);
      // procesar clave i
      ListaMap[idx] := raiz^.corrs[i];
      if Assigned(ListaMap[idx]) and Assigned(ListaMap[idx]^.remitenteUsuario) then
        remitente := ListaMap[idx]^.remitenteUsuario^.email
      else
        remitente := '<desconocido>';
      line := Format('%d | %s | %s | Ver', [raiz^.keys[i], raiz^.corrs[i]^.asunto, remitente]);
      ListBox1.Items.Add(line);
      Inc(idx);
    end;
    // recorrer último hijo
    RecorrerYMapear(raiz^.hijos[raiz^.numKeys], idx);
  end;
end;

{ Limpiar listbox y mapa }
procedure TFormInterfazBandejadeFavoritos.LimpiarList;
begin
  ListBox1.Clear;
  SetLength(ListaMap, 0);
end;

{ Construir árbol B de favoritos para FUsuario recorriendo ListaCorreosGlobal }
procedure TFormInterfazBandejadeFavoritos.ConstruirArbolFavoritos;
var
  nodoCorreo: PNodoCorreo;
  root: PFavoritoB;
  destinatarioOK: Boolean;
  correoPtr: PCorreo;
begin
  root := nil;
  nodoCorreo := ListaCorreosGlobal.primero;
  while nodoCorreo <> nil do
  begin
    if Assigned(nodoCorreo^.datos) then
    begin
      correoPtr := nodoCorreo^.datos;
      if correoPtr^.favorito then
      begin
        destinatarioOK := False;
        if Assigned(FUsuario) and Assigned(correoPtr^.destinatarioUsuario) then
          destinatarioOK := (correoPtr^.destinatarioUsuario = FUsuario) or
                            SameText(Trim(correoPtr^.destinatarioUsuario^.email), Trim(FUsuario^.email));
        if destinatarioOK then
        begin
          InsertarFavorito(root, correoPtr^.id, correoPtr);
        end;
      end;
    end;
    nodoCorreo := nodoCorreo^.siguiente;
  end;

  // asignar la raíz al usuario
  FUsuario^.listaFavoritos := root;
end;

{ Poblar listbox desde árbol (construye mapa) }
procedure TFormInterfazBandejadeFavoritos.PoblarListBoxDesdeArbol;
var
  totalEstimate, idx: Integer;

  function CountKeys(raiz: PFavoritoB): Integer;
  var
    i, acc: Integer;
  begin
    if raiz = nil then Exit(0);
    acc := raiz^.numKeys;
    for i := 0 to raiz^.numKeys do
      acc := acc + CountKeys(raiz^.hijos[i]);
    Result := acc;
  end;

begin
  LimpiarList;
  if (FUsuario = nil) or (FUsuario^.listaFavoritos = nil) then Exit;

  totalEstimate := CountKeys(FUsuario^.listaFavoritos);
  if totalEstimate <= 0 then Exit;
  SetLength(ListaMap, totalEstimate);
  idx := 0;
  RecorrerYMapear(FUsuario^.listaFavoritos, idx);
  if idx < Length(ListaMap) then
    SetLength(ListaMap, idx);
end;

{ -------------------- UI / eventos -------------------- }

procedure TFormInterfazBandejadeFavoritos.FormCreate(Sender: TObject);
begin
  FUsuario := nil;
  SetLength(ListaMap, 0);
  // asegurar eventos (si ListBox1 viene del .lfm estará asignado)
  if Assigned(ListBox1) then
  begin
    ListBox1.OnDblClick := @ListBox1DblClick;
    ListBox1.OnMouseDown := @ListBox1MouseDown;
  end;
end;

procedure TFormInterfazBandejadeFavoritos.SetUsuario(usuario: PUsuario);
begin
  FUsuario := usuario;
  ConstruirArbolFavoritos;
  PoblarListBoxDesdeArbol;
end;

procedure TFormInterfazBandejadeFavoritos.btnCerrarClick(Sender: TObject);
begin
  if Assigned(FormUsuario) then
    FormUsuario.Show;
  Self.Hide;
end;

procedure TFormInterfazBandejadeFavoritos.ListBox1Click(Sender: TObject);
begin
  // no usado, pero se deja por compatibilidad
end;

procedure TFormInterfazBandejadeFavoritos.ListBox1DblClick(Sender: TObject);
var
  idx: Integer;
  correo: PCorreo;
begin
  idx := ListBox1.ItemIndex;
  if (idx < 0) or (idx > High(ListaMap)) then Exit;
  correo := ListaMap[idx];
  if correo = nil then Exit;

  if not Assigned(FormInterfazFavorito) then
    Application.CreateForm(TFormInterfazFavorito, FormInterfazFavorito);

  FormInterfazFavorito.SetCorreo(correo);
  FormInterfazFavorito.Show;
  Self.Hide;
end;

procedure TFormInterfazBandejadeFavoritos.btnContadorClick(Sender: TObject);
var
  cnt: Integer;
begin
  cnt := Length(ListaMap);
  btnContador.Caption := 'Favoritos: ' + IntToStr(cnt);
end;

procedure TFormInterfazBandejadeFavoritos.ListBox1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  idx: Integer;
  itemText, preText, verText: string;
  barPos: Integer;
  preWidth, verWidth, padding: Integer;
  correo: PCorreo;
begin
  idx := ListBox1.ItemAtPos(Point(X, Y), True);
  if idx = -1 then Exit;
  ListBox1.ItemIndex := idx;

  // seguridad: índice válido en el mapa
  if (idx < 0) or (idx > High(ListaMap)) then Exit;

  itemText := ListBox1.Items[idx];

  // buscamos la última '|' para verificar que lo que viene después sea 'Ver'
  barPos := LastDelimiter('|', itemText);
  if barPos = 0 then Exit;

  // extraemos parte previa y la palabra 'Ver'
  verText := Trim(Copy(itemText, barPos + 1, Length(itemText) - barPos));
  if not SameText(verText, 'Ver') then Exit; // no es el caso '...| Ver'

  // texto anterior (incluye el '|', si quieres medir con el espacio también)
  preText := Copy(itemText, 1, barPos); // esto contiene hasta el '|'
  // fijamos la fuente del canvas para que la medición sea correcta
  ListBox1.Canvas.Font := ListBox1.Font;
  preWidth := ListBox1.Canvas.TextWidth(preText);
  verWidth := ListBox1.Canvas.TextWidth(' ' + verText); // medimos también el espacio que separa
  padding := 4; // tolerancia para que el click no tenga que ser exacto

  // si el clic X cae dentro del ancho donde se dibuja ' Ver'
  if (X >= preWidth - padding) and (X <= preWidth + verWidth + padding) then
  begin
    correo := ListaMap[idx];
    if correo = nil then Exit;

    if not Assigned(FormInterfazFavorito) then
      Application.CreateForm(TFormInterfazFavorito, FormInterfazFavorito);

    FormInterfazFavorito.SetCorreo(correo);
    FormInterfazFavorito.Show;
    Self.Hide;
  end;
end;


end.

