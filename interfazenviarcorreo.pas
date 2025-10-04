unit interfazEnviarCorreo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, estructuras;

type

  { TFormInterfazEnviarCorreo }

  TFormInterfazEnviarCorreo = class(TForm)
    btnCerrar: TButton;
    btnEnviar: TButton;
    btnGuardarComoBorrador: TButton;
    EditDestinatario: TEdit;
    EditAsunto: TEdit;
    Memo1: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure btnCerrarClick(Sender: TObject);
    procedure btnEnviarClick(Sender: TObject);
    procedure btnGuardarComoBorradorClick(Sender: TObject);
  private
    FFormUsuarioRef: TObject; // referencia opcional al FormUsuario
    // Funciones AVL para borradores
    function AlturaNodo(n: PBorradorAVL): Integer;
    function MaxInt(a, b: Integer): Integer;
    function RotarDerecha(y: PBorradorAVL): PBorradorAVL;
    function RotarIzquierda(x: PBorradorAVL): PBorradorAVL;
    function GetBalance(n: PBorradorAVL): Integer;
    procedure InsertarBorradorAVL(var raiz: PBorradorAVL; nuevo: PBorradorAVL);
  public
    procedure SetUsuarioForm(usuarioForm: TObject);
  end;

var
  FormInterfazEnviarCorreo: TFormInterfazEnviarCorreo;

implementation

uses
  interfazUsuario;

{$R *.lfm}

{ ==================== Helpers AVL ==================== }

function TFormInterfazEnviarCorreo.AlturaNodo(n: PBorradorAVL): Integer;
begin
  if n = nil then
    Exit(0);
  Result := n^.altura;
end;

function TFormInterfazEnviarCorreo.MaxInt(a, b: Integer): Integer;
begin
  if a > b then Result := a else Result := b;
end;

function TFormInterfazEnviarCorreo.RotarDerecha(y: PBorradorAVL): PBorradorAVL;
var
  x: PBorradorAVL;
  T2: PBorradorAVL;
begin
  x := y^.izquierdo;
  T2 := x^.derecho;

  // rotación
  x^.derecho := y;
  y^.izquierdo := T2;

  // actualizar alturas
  y^.altura := MaxInt(AlturaNodo(y^.izquierdo), AlturaNodo(y^.derecho)) + 1;
  x^.altura := MaxInt(AlturaNodo(x^.izquierdo), AlturaNodo(x^.derecho)) + 1;

  Result := x;
end;

function TFormInterfazEnviarCorreo.RotarIzquierda(x: PBorradorAVL): PBorradorAVL;
var
  y: PBorradorAVL;
  T2: PBorradorAVL;
begin
  y := x^.derecho;
  T2 := y^.izquierdo;

  // rotación
  y^.izquierdo := x;
  x^.derecho := T2;

  // actualizar alturas
  x^.altura := MaxInt(AlturaNodo(x^.izquierdo), AlturaNodo(x^.derecho)) + 1;
  y^.altura := MaxInt(AlturaNodo(y^.izquierdo), AlturaNodo(y^.derecho)) + 1;

  Result := y;
end;

function TFormInterfazEnviarCorreo.GetBalance(n: PBorradorAVL): Integer;
begin
  if n = nil then
    Exit(0);
  Result := AlturaNodo(n^.izquierdo) - AlturaNodo(n^.derecho);
end;

{ InsertarBorradorAVL: inserta 'nuevo' en el árbol raiz aplicando balanceo AVL.
  Se asume que 'nuevo' ya está creado y sus campos (id, remitente, ...) están inicializados. }
procedure TFormInterfazEnviarCorreo.InsertarBorradorAVL(var raiz: PBorradorAVL; nuevo: PBorradorAVL);
begin
  if raiz = nil then
  begin
    raiz := nuevo;
    Exit;
  end;

  if nuevo^.id < raiz^.id then
    InsertarBorradorAVL(raiz^.izquierdo, nuevo)
  else if nuevo^.id > raiz^.id then
    InsertarBorradorAVL(raiz^.derecho, nuevo)
  else
    Exit; // ids únicos, no insertar duplicados

  // actualizar altura
  raiz^.altura := 1 + MaxInt(AlturaNodo(raiz^.izquierdo), AlturaNodo(raiz^.derecho));

  // balance
  if GetBalance(raiz) > 1 then
  begin
    if nuevo^.id < raiz^.izquierdo^.id then
      raiz := RotarDerecha(raiz) // Left Left
    else
    begin
      raiz^.izquierdo := RotarIzquierda(raiz^.izquierdo); // Left Right
      raiz := RotarDerecha(raiz);
    end;
    Exit;
  end;

  if GetBalance(raiz) < -1 then
  begin
    if nuevo^.id > raiz^.derecho^.id then
      raiz := RotarIzquierda(raiz) // Right Right
    else
    begin
      raiz^.derecho := RotarDerecha(raiz^.derecho); // Right Left
      raiz := RotarIzquierda(raiz);
    end;
    Exit;
  end;
end;

{ ==================== SetUsuarioForm ==================== }
procedure TFormInterfazEnviarCorreo.SetUsuarioForm(usuarioForm: TObject);
begin
  FFormUsuarioRef := usuarioForm;
end;

{ ==================== Botón Cerrar ==================== }
procedure TFormInterfazEnviarCorreo.btnCerrarClick(Sender: TObject);
var
  frmUsr: TFormUsuario;
begin
  frmUsr := nil;
  if Assigned(FFormUsuarioRef) then
    frmUsr := TFormUsuario(FFormUsuarioRef);

  if Assigned(frmUsr) then
    frmUsr.Show
  else if Assigned(FormUsuario) then
    FormUsuario.Show;

  Self.Hide;
end;

{ ==================== Botón Enviar (ya existente) ==================== }
procedure TFormInterfazEnviarCorreo.btnEnviarClick(Sender: TObject);
var
  destinatarioUsuario: PUsuario;
  nuevoNodo: PNodoCorreo;
  frmUsr: TFormUsuario;
  destinatarioEmail: string;
  fechaHoy: TDateTime;

  function BuscarUsuarioGlobalPorEmail(const email: string): PUsuario;
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

  function ContactoExisteRec(raiz: PContactoAVL; const em: string): Boolean;
  begin
    if raiz = nil then
      Exit(False);

    if SameText(em, raiz^.email) then
      Exit(True)
    else if em < raiz^.email then
      Exit(ContactoExisteRec(raiz^.izquierdo, em))
    else
      Exit(ContactoExisteRec(raiz^.derecho, em));
  end;

begin
  if not Assigned(UsuarioLogueado) then
  begin
    ShowMessage('No hay usuario logueado.');
    Exit;
  end;

  destinatarioEmail := Trim(EditDestinatario.Text);
  if destinatarioEmail = '' then
  begin
    ShowMessage('Ingrese un destinatario.');
    Exit;
  end;

  frmUsr := nil;
  if Assigned(FFormUsuarioRef) then
    frmUsr := TFormUsuario(FFormUsuarioRef);

  // Verificar contacto
  if Assigned(frmUsr) then
  begin
    if not frmUsr.ExisteContactoBST(UsuarioLogueado^.listaContactos, destinatarioEmail) then
    begin
      ShowMessage('No puede enviar correos a un usuario que no está en sus contactos.');
      Exit;
    end;
  end
  else
  begin
    if not ContactoExisteRec(UsuarioLogueado^.listaContactos, destinatarioEmail) then
    begin
      ShowMessage('No puede enviar correos a un usuario que no está en sus contactos.');
      Exit;
    end;
  end;

  // buscar destinatario
  if Assigned(frmUsr) then
    destinatarioUsuario := frmUsr.BuscarUsuarioPorEmail(destinatarioEmail)
  else
    destinatarioUsuario := BuscarUsuarioGlobalPorEmail(destinatarioEmail);

  if destinatarioUsuario = nil then
  begin
    ShowMessage('Destinatario no encontrado.');
    Exit;
  end;

  // crear PNodoCorreo y PCorreo
  New(nuevoNodo);
  New(nuevoNodo^.datos);

  Inc(ContadorIDCorreosGlobal);
  nuevoNodo^.datos^.id := ContadorIDCorreosGlobal;

  nuevoNodo^.datos^.remitenteUsuario := UsuarioLogueado;
  nuevoNodo^.datos^.destinatarioUsuario := destinatarioUsuario;
  nuevoNodo^.datos^.asunto := Trim(EditAsunto.Text);
  nuevoNodo^.datos^.mensaje := Memo1.Lines.Text;
  fechaHoy := Date;
  nuevoNodo^.datos^.fecha := fechaHoy;
  nuevoNodo^.datos^.hora := 0;
  nuevoNodo^.datos^.programado := 'No';
  nuevoNodo^.datos^.estado := 'NL';
  nuevoNodo^.datos^.favorito := False;
  nuevoNodo^.datos^.remitente := nil;

  // enlazar en lista global
  nuevoNodo^.siguiente := nil;
  nuevoNodo^.anterior := nil;
  if ListaCorreosGlobal.primero = nil then
  begin
    ListaCorreosGlobal.primero := nuevoNodo;
    ListaCorreosGlobal.ultimo := nuevoNodo;
  end
  else
  begin
    ListaCorreosGlobal.ultimo^.siguiente := nuevoNodo;
    nuevoNodo^.anterior := ListaCorreosGlobal.ultimo;
    ListaCorreosGlobal.ultimo := nuevoNodo;
  end;

  ShowMessage('Correo enviado correctamente.');

  // limpiar/volver al usuario
  EditDestinatario.Clear;
  EditAsunto.Clear;
  Memo1.Clear;

  if Assigned(frmUsr) then
  begin
    frmUsr.Show;
    Self.Hide;
  end
  else if Assigned(FormUsuario) then
  begin
    FormUsuario.Show;
    Self.Hide;
  end;
end;

{ ==================== Botón Guardar como borrador (AVL) ==================== }
procedure TFormInterfazEnviarCorreo.btnGuardarComoBorradorClick(Sender: TObject);
var
  destinatarioEmail: string;
  nuevoB: PBorradorAVL;
  fechaHoy: TDateTime;
  frmUsr: TFormUsuario;

  function ContactoExisteRec(raiz: PContactoAVL; const em: string): Boolean;
  begin
    if raiz = nil then Exit(False);
    if SameText(em, raiz^.email) then Exit(True)
    else if em < raiz^.email then Exit(ContactoExisteRec(raiz^.izquierdo, em))
    else Exit(ContactoExisteRec(raiz^.derecho, em));
  end;

  function BuscarUsuarioGlobalPorEmail(const email: string): PUsuario;
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

begin
  if not Assigned(UsuarioLogueado) then
  begin
    ShowMessage('No hay usuario logueado.');
    Exit;
  end;

  destinatarioEmail := Trim(EditDestinatario.Text);
  if destinatarioEmail = '' then
  begin
    ShowMessage('Ingrese un destinatario para guardar como borrador.');
    Exit;
  end;

  // validar que destinatario esté en contactos
  frmUsr := nil;
  if Assigned(FFormUsuarioRef) then
    frmUsr := TFormUsuario(FFormUsuarioRef);

  if Assigned(frmUsr) then
  begin
    if not frmUsr.ExisteContactoBST(UsuarioLogueado^.listaContactos, destinatarioEmail) then
    begin
      ShowMessage('No puede guardar borrador hacia un usuario que no está en sus contactos.');
      Exit;
    end;
  end
  else
  begin
    if not ContactoExisteRec(UsuarioLogueado^.listaContactos, destinatarioEmail) then
    begin
      ShowMessage('No puede guardar borrador hacia un usuario que no está en sus contactos.');
      Exit;
    end;
  end;

  // crear nuevo nodo borrador y asignar id autoincremental global
  New(nuevoB);
  nuevoB^.izquierdo := nil;
  nuevoB^.derecho := nil;
  Inc(ContadorIDBorradoresGlobal);
  nuevoB^.id := ContadorIDBorradoresGlobal;
  nuevoB^.remitente := UsuarioLogueado^.email;
  nuevoB^.destinatarioEmail := destinatarioEmail;
  nuevoB^.asunto := Trim(EditAsunto.Text);
  nuevoB^.mensaje := Memo1.Lines.Text;
  fechaHoy := Date;
  nuevoB^.fecha := fechaHoy;
  nuevoB^.altura := 1;

  // insertar en el árbol de borradores del usuario logueado
  InsertarBorradorAVL(UsuarioLogueado^.listaBorradores, nuevoB);

  ShowMessage('Borrador guardado correctamente (ID: ' + IntToStr(nuevoB^.id) + ').');

  // opcional: limpiar campos
  EditDestinatario.Clear;
  EditAsunto.Clear;
  Memo1.Clear;

  // regresar al formulario usuario
  if Assigned(frmUsr) then
  begin
    frmUsr.Show;
    Self.Hide;
  end
  else if Assigned(FormUsuario) then
  begin
    FormUsuario.Show;
    Self.Hide;
  end;
end;

end.

