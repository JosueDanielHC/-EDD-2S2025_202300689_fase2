unit interfazBandejadeEntrada;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Dialogs, Types, Graphics,
  estructuras; // sólo necesidades de tipos (estructuras)

type
  { TFormInterfazBandejadeEntrada }
  TFormInterfazBandejadeEntrada = class(TForm)
    btnBandejadeEntrada: TButton; // Refrescar lista
    btnContador: TButton;         // Mostrar cantidad No Leídos
    btnCerrar: TButton;           // Volver a InterfazUsuario
    ListBox1: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure btnBandejadeEntradaClick(Sender: TObject);
    procedure btnContadorClick(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure btnCerrarClick(Sender: TObject);
    procedure ListBox1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    { private declarations }
    ListaFiltrada: array of PNodoBandejaEntrada; // mapea items del ListBox a nodos de la bandeja (solo visibles, sin favoritos)
    procedure EnsureListBoxExists;
    procedure LimpiarMapa;
  public
    { public declarations }
    // Llenar ListBox mostrando la lista de bandeja del usuario (no reconstruye la lista, usa usuario^.listaBandejaEntrada)
    procedure CargarCorreosEnListBox(usuario: PUsuario);
  end;

var
  FormInterfazBandejadeEntrada: TFormInterfazBandejadeEntrada;

implementation

uses
  interfazUsuario, login, interfazvercorreo;

{$R *.lfm}

{ ==================== UTILIDADES ==================== }

procedure TFormInterfazBandejadeEntrada.EnsureListBoxExists;
begin
  if not Assigned(ListBox1) then
  begin
    ListBox1 := TListBox.Create(Self);
    ListBox1.Parent := Self;
    ListBox1.Left := 8;
    ListBox1.Top := 48;
    ListBox1.Width := Self.ClientWidth - 16;
    ListBox1.Height := Self.ClientHeight - ListBox1.Top - 8;
    ListBox1.AnchorSide[akLeft].Side := asrLeft;
    ListBox1.AnchorSide[akTop].Side := asrTop;
    ListBox1.AnchorSide[akRight].Side := asrRight;
    ListBox1.AnchorSide[akBottom].Side := asrBottom;
    ListBox1.OnDblClick := @ListBox1DblClick;
    ListBox1.OnMouseDown := @ListBox1MouseDown;
  end
  else
  begin
    ListBox1.OnDblClick := @ListBox1DblClick;
    ListBox1.OnMouseDown := @ListBox1MouseDown;
  end;
end;

procedure TFormInterfazBandejadeEntrada.LimpiarMapa;
begin
  SetLength(ListaFiltrada, 0);
end;

{ ==================== EVENTOS / LÓGICA ==================== }

procedure TFormInterfazBandejadeEntrada.FormCreate(Sender: TObject);
begin
  EnsureListBoxExists;
  LimpiarMapa;
end;

// Botón "Bandeja de Entrada" -> refrescar vista usando la lista que pertenece al usuario logueado
procedure TFormInterfazBandejadeEntrada.btnBandejadeEntradaClick(Sender: TObject);
begin
  if not Assigned(UsuarioLogueado) then
  begin
    ShowMessage('No hay usuario logueado.');
    Exit;
  end;
  CargarCorreosEnListBox(UsuarioLogueado);
end;

procedure TFormInterfazBandejadeEntrada.btnContadorClick(Sender: TObject);
var
  nodo: PNodoBandejaEntrada;
  totalNL: Integer;
begin
  totalNL := 0;
  if not Assigned(UsuarioLogueado) or (UsuarioLogueado^.listaBandejaEntrada = nil) then
  begin
    btnContador.Caption := 'No leídos: 0';
    Exit;
  end;

  nodo := UsuarioLogueado^.listaBandejaEntrada^.primero;
  while nodo <> nil do
  begin
    if Assigned(nodo^.correoRef) and (nodo^.correoRef^.estado = 'NL') and (not nodo^.correoRef^.favorito) then
      Inc(totalNL);
    nodo := nodo^.siguiente;
  end;

  btnContador.Caption := 'No leídos: ' + IntToStr(totalNL);
end;

procedure TFormInterfazBandejadeEntrada.CargarCorreosEnListBox(usuario: PUsuario);
var
  nodo: PNodoBandejaEntrada;
  idx, visibles: Integer;
  texto: string;
  remitenteEmail: string;
  SUFIJO_VER: string;
begin
  EnsureListBoxExists;
  SUFIJO_VER := ' - Ver';

  ListBox1.Items.BeginUpdate;
  try
    ListBox1.Clear;
    LimpiarMapa;

    if (usuario = nil) or (usuario^.listaBandejaEntrada = nil) then
      Exit;

    // contar nodos visibles (omitir favoritos)
    visibles := 0;
    nodo := usuario^.listaBandejaEntrada^.primero;
    while nodo <> nil do
    begin
      if Assigned(nodo^.correoRef) and (not nodo^.correoRef^.favorito) then
        Inc(visibles);
      nodo := nodo^.siguiente;
    end;

    if visibles = 0 then
    begin
      // no hay correos visibles
      btnContadorClick(nil);
      Exit;
    end;

    SetLength(ListaFiltrada, visibles);
    idx := 0;
    nodo := usuario^.listaBandejaEntrada^.primero;
    while nodo <> nil do
    begin
      // omitir si es favorito
      if (not Assigned(nodo^.correoRef)) or (nodo^.correoRef^.favorito) then
      begin
        nodo := nodo^.siguiente;
        Continue;
      end;

      ListaFiltrada[idx] := nodo;

      if Assigned(nodo^.correoRef) and Assigned(nodo^.correoRef^.remitenteUsuario) then
        remitenteEmail := nodo^.correoRef^.remitenteUsuario^.email
      else
        remitenteEmail := '<desconocido>';

      texto := Format('[%s] %s - De: %s%s',
                      [nodo^.correoRef^.estado,
                       nodo^.correoRef^.asunto,
                       remitenteEmail,
                       SUFIJO_VER]);

      ListBox1.Items.Add(texto);
      Inc(idx);
      nodo := nodo^.siguiente;
    end;

  finally
    ListBox1.Items.EndUpdate;
  end;

  // actualizar contador automáticamente
  btnContadorClick(nil);
end;

procedure TFormInterfazBandejadeEntrada.ListBox1DblClick(Sender: TObject);
var
  indice: Integer;
  nodo: PNodoBandejaEntrada;
begin
  EnsureListBoxExists;
  indice := ListBox1.ItemIndex;
  if (indice < 0) or (indice > High(ListaFiltrada)) then Exit;

  nodo := ListaFiltrada[indice];
  if Assigned(nodo) and Assigned(nodo^.correoRef) then
  begin
    // crear/FormVerCorreo si es necesario
    if not Assigned(FormVerCorreo) then
      Application.CreateForm(TFormVerCorreo, FormVerCorreo);

    // ahora sí mostrar correo
    FormVerCorreo.MostrarCorreo(nodo^.correoRef);
    FormVerCorreo.Show;
    Self.Hide;

    // marcar leído y refrescar
    nodo^.correoRef^.estado := 'L';
    CargarCorreosEnListBox(UsuarioLogueado);
  end;
end;

// Detectar click en la zona " - Ver" dentro del texto del ListBox
procedure TFormInterfazBandejadeEntrada.ListBox1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  idx: Integer;
  itemText, beforeVer: string;
  posVer, textLeft, textWidthBefore, verStart: Integer;
  nodo: PNodoBandejaEntrada;
  SUFIJO_VER: string;
begin
  EnsureListBoxExists;
  idx := ListBox1.ItemAtPos(Point(X, Y), True);
  if idx = -1 then Exit;
  ListBox1.ItemIndex := idx;

  itemText := ListBox1.Items[idx];
  SUFIJO_VER := ' - Ver';

  // comprobar sufijo
  posVer := Pos(SUFIJO_VER, itemText);
  if (posVer = 0) or (posVer + Length(SUFIJO_VER) - 1 <> Length(itemText)) then
    Exit;

  beforeVer := Copy(itemText, 1, posVer - 1);

  // cálculo aproximado de la posición en pixeles del inicio del sufijo
  textLeft := 2;
  textWidthBefore := ListBox1.Canvas.TextWidth(beforeVer);
  verStart := textLeft + textWidthBefore;

  if X >= verStart then
  begin
    if (idx >= 0) and (idx <= High(ListaFiltrada)) then
    begin
      nodo := ListaFiltrada[idx];
      if Assigned(nodo) and Assigned(nodo^.correoRef) then
      begin
        if not Assigned(FormVerCorreo) then
          Application.CreateForm(TFormVerCorreo, FormVerCorreo);

        FormVerCorreo.MostrarCorreo(nodo^.correoRef);
        FormVerCorreo.Show;
        Self.Hide;

        nodo^.correoRef^.estado := 'L';
        CargarCorreosEnListBox(UsuarioLogueado);
      end;
    end;
  end;
end;

procedure TFormInterfazBandejadeEntrada.btnCerrarClick(Sender: TObject);
begin
  // volver a la interfaz de usuario principal (FormUsuario)
  if Assigned(FormUsuario) then
    FormUsuario.Show;
  Self.Hide;
end;

end.

