unit interfazBorrador;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, estructuras;

type

  { TFormInterfazBorrador }

  TFormInterfazBorrador = class(TForm)
    btnCerrar: TButton;
    btnEnviar: TButton;
    EditDestinatario: TEdit;
    EditAsunto: TEdit;
    LabelMensaje: TLabel;
    LabelAsunto: TLabel;
    LabelBorrador: TLabel;
    LabelDestinatario: TLabel;
    Memo1: TMemo;
    procedure btnCerrarClick(Sender: TObject);
    procedure btnEnviarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FUsuario: PUsuario;           // referencia al usuario logueado
    FBorradorNodo: PBorradorAVL;  // nodo borrador actualmente cargado (puntero dentro del árbol)
    { AVL borradores helpers }
    function BuscarUsuarioGlobalPorEmail(const email: string): PUsuario;
    function AlturaNodo(n: PBorradorAVL): Integer;
    function MaxInt(a, b: Integer): Integer;
    procedure ActualizarAltura(var n: PBorradorAVL);
    function BalanceFactor(n: PBorradorAVL): Integer;
    function RotarDerecha(y: PBorradorAVL): PBorradorAVL;
    function RotarIzquierda(x: PBorradorAVL): PBorradorAVL;
    function BalancearNodo(n: PBorradorAVL): PBorradorAVL;
    function EncontrarMin(raiz: PBorradorAVL): PBorradorAVL;
    function EliminarBorradorPorID(raiz: PBorradorAVL; id: Integer): PBorradorAVL;
  public
    // Llamar antes de Show para pasar punteros: SetUsuarioYBorrador(usuario, nodoBorrador)
    procedure SetUsuarioYBorrador(usuario: PUsuario; borradorNodo: PBorradorAVL);
    procedure CargarCamposDesdeBorrador; // carga los TEdit/Memo con los datos del borrador
  end;

var
  FormInterfazBorrador: TFormInterfazBorrador;

implementation

uses
  interfazBandejadeBorrador; // para volver a esa pantalla

{$R *.lfm}

{ ==================== Inicialización ==================== }

procedure TFormInterfazBorrador.FormCreate(Sender: TObject);
begin
  FUsuario := nil;
  FBorradorNodo := nil;
end;

procedure TFormInterfazBorrador.SetUsuarioYBorrador(usuario: PUsuario; borradorNodo: PBorradorAVL);
begin
  FUsuario := usuario;
  FBorradorNodo := borradorNodo;
  CargarCamposDesdeBorrador;
end;

procedure TFormInterfazBorrador.CargarCamposDesdeBorrador;
begin
  if FBorradorNodo = nil then
  begin
    EditDestinatario.Text := '';
    EditAsunto.Text := '';
    Memo1.Lines.Clear;
    Exit;
  end;

  EditDestinatario.Text := FBorradorNodo^.destinatarioEmail;
  EditAsunto.Text := FBorradorNodo^.asunto;
  Memo1.Lines.Text := FBorradorNodo^.mensaje;
end;

{ ==================== Búsqueda de usuario global (helper) ==================== }

function TFormInterfazBorrador.BuscarUsuarioGlobalPorEmail(const email: string): PUsuario;
var
  nodo: PUsuarioNodo;
begin
  Result := nil;
  if Trim(email) = '' then Exit;
  nodo := ListaUsuariosGlobal.primero;
  while nodo <> nil do
  begin
    if Assigned(nodo^.datos) and SameText(Trim(nodo^.datos^.email), Trim(email)) then
    begin
      Result := nodo^.datos;
      Exit;
    end;
    nodo := nodo^.siguiente;
  end;
end;

{ ==================== Funciones AVL para borradores ==================== }

function TFormInterfazBorrador.AlturaNodo(n: PBorradorAVL): Integer;
begin
  if n = nil then Result := 0
  else Result := n^.altura;
end;

function TFormInterfazBorrador.MaxInt(a, b: Integer): Integer;
begin
  if a > b then Result := a else Result := b;
end;

procedure TFormInterfazBorrador.ActualizarAltura(var n: PBorradorAVL);
begin
  if n <> nil then
    n^.altura := 1 + MaxInt(AlturaNodo(n^.izquierdo), AlturaNodo(n^.derecho));
end;

function TFormInterfazBorrador.BalanceFactor(n: PBorradorAVL): Integer;
begin
  if n = nil then Exit(0);
  Result := AlturaNodo(n^.izquierdo) - AlturaNodo(n^.derecho);
end;

function TFormInterfazBorrador.RotarDerecha(y: PBorradorAVL): PBorradorAVL;
var
  x, T2: PBorradorAVL;
begin
  x := y^.izquierdo;
  T2 := x^.derecho;

  // rotación
  x^.derecho := y;
  y^.izquierdo := T2;

  // actualizar alturas
  ActualizarAltura(y);
  ActualizarAltura(x);

  Result := x;
end;

function TFormInterfazBorrador.RotarIzquierda(x: PBorradorAVL): PBorradorAVL;
var
  y, T2: PBorradorAVL;
begin
  y := x^.derecho;
  T2 := y^.izquierdo;

  // rotación
  y^.izquierdo := x;
  x^.derecho := T2;

  // actualizar alturas
  ActualizarAltura(x);
  ActualizarAltura(y);

  Result := y;
end;

function TFormInterfazBorrador.BalancearNodo(n: PBorradorAVL): PBorradorAVL;
var
  bf: Integer;
begin
  if n = nil then Exit(nil);

  ActualizarAltura(n);
  bf := BalanceFactor(n);

  // Left Left
  if (bf > 1) and (BalanceFactor(n^.izquierdo) >= 0) then
    Exit(RotarDerecha(n));

  // Left Right
  if (bf > 1) and (BalanceFactor(n^.izquierdo) < 0) then
  begin
    n^.izquierdo := RotarIzquierda(n^.izquierdo);
    Exit(RotarDerecha(n));
  end;

  // Right Right
  if (bf < -1) and (BalanceFactor(n^.derecho) <= 0) then
    Exit(RotarIzquierda(n));

  // Right Left
  if (bf < -1) and (BalanceFactor(n^.derecho) > 0) then
  begin
    n^.derecho := RotarDerecha(n^.derecho);
    Exit(RotarIzquierda(n));
  end;

  Result := n;
end;

function TFormInterfazBorrador.EncontrarMin(raiz: PBorradorAVL): PBorradorAVL;
begin
  if raiz = nil then Exit(nil);
  while raiz^.izquierdo <> nil do
    raiz := raiz^.izquierdo;
  Result := raiz;
end;

{ ----------------------------------------------------------------------------
  EliminarBorradorPorID:
  - elimina el nodo con clave id del árbol AVL (raiz),
  - devuelve la nueva raíz del subárbol,
  - libera memoria del nodo eliminado (Dispose).
-------------------------------------------------------------------------------}
function TFormInterfazBorrador.EliminarBorradorPorID(raiz: PBorradorAVL; id: Integer): PBorradorAVL;
var
  temp: PBorradorAVL;
  succ: PBorradorAVL;
begin
  // caso base
  if raiz = nil then Exit(nil);

  if id < raiz^.id then
    raiz^.izquierdo := EliminarBorradorPorID(raiz^.izquierdo, id)
  else if id > raiz^.id then
    raiz^.derecho := EliminarBorradorPorID(raiz^.derecho, id)
  else
  begin
    // Encontrado el nodo a eliminar
    // caso 1: solo un hijo o ninguno
    if (raiz^.izquierdo = nil) or (raiz^.derecho = nil) then
    begin
      if raiz^.izquierdo = nil then
        temp := raiz^.derecho
      else
        temp := raiz^.izquierdo;

      // Si no hay hijos, temp = nil => liberar raiz y devolver nil
      if temp = nil then
      begin
        // liberar memoria del nodo raiz
        Dispose(raiz);
        Exit(nil);
      end
      else
      begin
        // un solo hijo: reemplazar raiz por hijo
        // copiar contenido del hijo a raiz y liberar el hijo real
        // Para simplicidad haremos swap de contenido: (creamos copia y liberamos child)
        // Pero es más simple reasignar la referencia: devolvemos temp y liberamos raiz.
        Dispose(raiz);
        Exit(temp);
      end;
    end
    else
    begin
      // caso 2: dos hijos -> obtener sucesor (min en subárbol derecho)
      succ := EncontrarMin(raiz^.derecho);
      // copiar datos del sucesor en raiz
      raiz^.id := succ^.id;
      raiz^.remitente := succ^.remitente;
      raiz^.destinatarioEmail := succ^.destinatarioEmail;
      raiz^.asunto := succ^.asunto;
      raiz^.mensaje := succ^.mensaje;
      raiz^.fecha := succ^.fecha;
      // eliminar sucesor en subarbol derecho
      raiz^.derecho := EliminarBorradorPorID(raiz^.derecho, succ^.id);
    end;
  end;

  // Si el árbol tenía solo un nodo
  if raiz = nil then Exit(nil);

  // balancear y regresar
  Result := BalancearNodo(raiz);
end;

{ ==================== Eventos UI ==================== }

procedure TFormInterfazBorrador.btnCerrarClick(Sender: TObject);
begin
  // volver a bandeja de borradores
  if not Assigned(FormInterfazBandejadeBorrador) then
    Application.CreateForm(TFormInterfazBandejadeBorrador, FormInterfazBandejadeBorrador);

  // asegurar que la banda use la misma referencia de usuario
  if Assigned(FUsuario) then
    FormInterfazBandejadeBorrador.SetUsuario(FUsuario);

  FormInterfazBandejadeBorrador.Show;
  Self.Hide;
end;

procedure TFormInterfazBorrador.btnEnviarClick(Sender: TObject);
var
  destinatarioEmail: string;
  destinatarioUsuario: PUsuario;
  nuevoNodo: PNodoCorreo;
  fechaHoy: TDateTime;
  idBorrador: Integer;
begin
  if FUsuario = nil then
  begin
    ShowMessage('Referencia de usuario no establecida.');
    Exit;
  end;

  if FBorradorNodo = nil then
  begin
    ShowMessage('No se ha seleccionado un borrador válido.');
    Exit;
  end;

  destinatarioEmail := Trim(EditDestinatario.Text);
  if destinatarioEmail = '' then
  begin
    ShowMessage('Ingrese un destinatario.');
    Exit;
  end;

  // comprobar que destinatario existe en la lista global
  destinatarioUsuario := BuscarUsuarioGlobalPorEmail(destinatarioEmail);
  if destinatarioUsuario = nil then
  begin
    ShowMessage('Destinatario no encontrado en el sistema.');
    Exit;
  end;

  // Crear nuevo PNodoCorreo y su PCorreo
  New(nuevoNodo);
  New(nuevoNodo^.datos);

  // id autoincremental global
  Inc(ContadorIDCorreosGlobal);
  nuevoNodo^.datos^.id := ContadorIDCorreosGlobal;

  // asignaciones
  nuevoNodo^.datos^.remitenteUsuario := FUsuario;
  nuevoNodo^.datos^.destinatarioUsuario := destinatarioUsuario;
  nuevoNodo^.datos^.asunto := Trim(EditAsunto.Text);
  nuevoNodo^.datos^.mensaje := Memo1.Lines.Text;

  fechaHoy := Date;
  nuevoNodo^.datos^.fecha := fechaHoy;
  nuevoNodo^.datos^.hora := 0;
  nuevoNodo^.datos^.programado := 'No';
  nuevoNodo^.datos^.estado := 'NL';
  nuevoNodo^.datos^.favorito := False;
  nuevoNodo^.datos^.remitente := nil; // compatibilidad

  // enlazar al final de ListaCorreosGlobal
  nuevoNodo^.siguiente := nil;
  if ListaCorreosGlobal.primero = nil then
  begin
    ListaCorreosGlobal.primero := nuevoNodo;
    ListaCorreosGlobal.ultimo := nuevoNodo;
  end
  else
  begin
    ListaCorreosGlobal.ultimo^.siguiente := nuevoNodo;
    ListaCorreosGlobal.ultimo := nuevoNodo;
  end;

  // Guardar id del borrador a eliminar
  idBorrador := FBorradorNodo^.id;

  // Eliminar el borrador del AVL del usuario y liberar memoria
  if Assigned(FUsuario) then
  begin
    FUsuario^.listaBorradores := EliminarBorradorPorID(FUsuario^.listaBorradores, idBorrador);
    // ya no es válido FBorradorNodo
    FBorradorNodo := nil;
  end;

  ShowMessage('Correo enviado y borrador eliminado correctamente.');

  // Limpiar campos
  EditDestinatario.Clear;
  EditAsunto.Clear;
  Memo1.Clear;

  // Volver a la bandeja de borradores y forzar refresco
  if not Assigned(FormInterfazBandejadeBorrador) then
    Application.CreateForm(TFormInterfazBandejadeBorrador, FormInterfazBandejadeBorrador);

  if Assigned(FUsuario) then
    FormInterfazBandejadeBorrador.SetUsuario(FUsuario);

  FormInterfazBandejadeBorrador.Show;
  Self.Hide;
end;

end.

