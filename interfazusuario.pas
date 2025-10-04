unit interfazUsuario;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  estructuras; // <-- sólo tipos/estructuras en la section interface para evitar ciclos

type
  { TFormUsuario }
  TFormUsuario = class(TForm)
    btnBandejaEntrada: TButton;
    btnVerContactos: TButton;
    btnGenerarReportes: TButton;
    btnVerborradoresdeMensajes: TButton;
    btnCerrar: TButton;
    btnVerPapelera: TButton;
    btnProgramarCorreos: TButton;
    btnAgregarContactos: TButton;
    btnActualizarPerfil: TButton;
    btnPublicarmensajeencomunidad: TButton;
    btnEnviarCorreo: TButton;
    btnVerFavorito: TButton;
    btnVerCorreosProgramados: TButton;
    LabelBienvenido: TLabel;

    procedure btnActualizarPerfilClick(Sender: TObject);
    procedure btnAgregarContactosClick(Sender: TObject);
    procedure btnBandejaEntradaClick(Sender: TObject);
    procedure btnCerrarClick(Sender: TObject);
    procedure btnEnviarCorreoClick(Sender: TObject);
    procedure btnGenerarReportesClick(Sender: TObject);
    procedure btnProgramarCorreosClick(Sender: TObject);
    procedure btnPublicarmensajeencomunidadClick(Sender: TObject);
    procedure btnVerborradoresdeMensajesClick(Sender: TObject);
    procedure btnVerContactosClick(Sender: TObject);
    procedure btnVerCorreosProgramadosClick(Sender: TObject);
    procedure btnVerFavoritoClick(Sender: TObject);
    procedure btnVerPapeleraClick(Sender: TObject);
    procedure FormShow(Sender: TObject);

  private
    procedure ArrancarListaBandejaEntrada(usuario: PUsuario);
    procedure VaciarListaBandejaEntrada(usuario: PUsuario);
  public
    // API pública
    procedure InsertarContactoBST(var raiz: PContactoAVL; usuarioDest: PUsuario);
    function ExisteContactoBST(raiz: PContactoAVL; email: string): Boolean;
    procedure CrearListaContactos(usuario: PUsuario);
    function BuscarUsuarioPorEmail(const email: string): PUsuario;
  end;

var
  FormUsuario: TFormUsuario;

implementation

uses
  login,
  interfazBandejadeEntrada,
  interfazvercorreo,
  interfazPapelera,
  interfazProgramarCorreo,
  interfazCorreosProgramados,
  interfazAgregarContacto,
  interfazContactos,
  interfazEnviarCorreo,
  interfazActualizarPerfil,
  interfazBandejadeBorrador,
  interfazBorrador,
  interfazBandejadeFavoritos,
  interfazFavorito;

{$R *.lfm}

{ ==================== FUNCIONES PARA BST DE CONTACTOS ==================== }

procedure TFormUsuario.InsertarContactoBST(var raiz: PContactoAVL; usuarioDest: PUsuario);
begin
  if usuarioDest = nil then Exit;

  if raiz = nil then
  begin
    New(raiz);
    raiz^.usuarioRef := usuarioDest;
    raiz^.email := usuarioDest^.email;
    raiz^.izquierdo := nil;
    raiz^.derecho := nil;
    raiz^.altura := 1;
    Exit;
  end;

  if SameText(usuarioDest^.email, raiz^.email) then
    Exit
  else if usuarioDest^.email < raiz^.email then
    InsertarContactoBST(raiz^.izquierdo, usuarioDest)
  else
    InsertarContactoBST(raiz^.derecho, usuarioDest);
end;

function TFormUsuario.ExisteContactoBST(raiz: PContactoAVL; email: string): Boolean;
begin
  // búsqueda simple en BST por email
  if raiz = nil then
  begin
    Result := False;
    Exit;
  end;

  if SameText(email, raiz^.email) then
    Result := True
  else if email < raiz^.email then
    Result := ExisteContactoBST(raiz^.izquierdo, email)
  else
    Result := ExisteContactoBST(raiz^.derecho, email);
end;

{ ==================== BUSCAR USUARIO EN LISTA GLOBAL ==================== }
function TFormUsuario.BuscarUsuarioPorEmail(const email: string): PUsuario;
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

{ ==================== CREAR LISTA DE CONTACTOS (Construcción automática) ==================== }
procedure TFormUsuario.CrearListaContactos(usuario: PUsuario);
var
  actual: PNodoCorreo;
  destinatario: PUsuario;
begin
  if usuario = nil then Exit;

  // limpiar árbol previo (si existiera)
  usuario^.listaContactos := nil;

  actual := ListaCorreosGlobal.primero;
  while actual <> nil do
  begin
    if Assigned(actual^.datos) then
    begin
      // Si el remitente del correo coincide (por referencia o por email) con el usuario logueado
      if Assigned(actual^.datos^.remitenteUsuario) and
         ((actual^.datos^.remitenteUsuario = usuario) or
          SameText(actual^.datos^.remitenteUsuario^.email, usuario^.email)) then
      begin
        // obtener destinatario real desde lista de usuarios (por seguridad)
        if Assigned(actual^.datos^.destinatarioUsuario) then
          destinatario := BuscarUsuarioPorEmail(actual^.datos^.destinatarioUsuario^.email)
        else
          destinatario := nil;

        if destinatario <> nil then
          InsertarContactoBST(usuario^.listaContactos, destinatario);
      end;
    end;
    actual := actual^.siguiente;
  end;
end;

{ ==================== MANEJO BANDEJA DE ENTRADA ==================== }

procedure TFormUsuario.VaciarListaBandejaEntrada(usuario: PUsuario);
var
  nodo, tmp: PNodoBandejaEntrada;
begin
  if (usuario = nil) or (usuario^.listaBandejaEntrada = nil) then Exit;

  nodo := usuario^.listaBandejaEntrada^.primero;
  while nodo <> nil do
  begin
    tmp := nodo^.siguiente;
    Dispose(nodo);
    nodo := tmp;
  end;

  Dispose(usuario^.listaBandejaEntrada);
  usuario^.listaBandejaEntrada := nil;
end;

procedure TFormUsuario.ArrancarListaBandejaEntrada(usuario: PUsuario);
var
  actual: PNodoCorreo;
  nuevoNodo: PNodoBandejaEntrada;
  lista: PListaBandejaEntrada;
  destinatarioOK: Boolean;
begin
  if usuario = nil then Exit;

  if usuario^.listaBandejaEntrada <> nil then
    VaciarListaBandejaEntrada(usuario);

  New(lista);
  lista^.primero := nil;
  lista^.ultimo := nil;
  usuario^.listaBandejaEntrada := lista;

  actual := ListaCorreosGlobal.primero;
  while actual <> nil do
  begin
    destinatarioOK := False;

    if Assigned(actual^.datos) then
    begin
      if Assigned(actual^.datos^.destinatarioUsuario) then
      begin
        if actual^.datos^.destinatarioUsuario = usuario then
          destinatarioOK := True
        else if (Trim(actual^.datos^.destinatarioUsuario^.email) <> '') and
                (SameText(Trim(actual^.datos^.destinatarioUsuario^.email), Trim(usuario^.email))) then
          destinatarioOK := True;
      end;
    end;

    if destinatarioOK then
    begin
      New(nuevoNodo);
      nuevoNodo^.correoRef := actual^.datos;
      nuevoNodo^.siguiente := nil;
      nuevoNodo^.anterior := nil;

      if usuario^.listaBandejaEntrada^.primero = nil then
      begin
        usuario^.listaBandejaEntrada^.primero := nuevoNodo;
        usuario^.listaBandejaEntrada^.ultimo := nuevoNodo;
      end
      else
      begin
        nuevoNodo^.anterior := usuario^.listaBandejaEntrada^.ultimo;
        usuario^.listaBandejaEntrada^.ultimo^.siguiente := nuevoNodo;
        usuario^.listaBandejaEntrada^.ultimo := nuevoNodo;
      end;
    end;

    actual := actual^.siguiente;
  end;
end;

{ ==================== EVENTOS DEL FORMULARIO ==================== }

procedure TFormUsuario.FormShow(Sender: TObject);
begin
  // cada vez que aparece el formulario se reconstruyen contactos del usuario logueado
  if Assigned(UsuarioLogueado) then
    CrearListaContactos(UsuarioLogueado);
end;

procedure TFormUsuario.btnVerContactosClick(Sender: TObject);
begin
  if not Assigned(UsuarioLogueado) then
  begin
    ShowMessage('No hay usuario logueado.');
    Exit;
  end;

  if not Assigned(FormContactos) then
    Application.CreateForm(TFormContactos, FormContactos);

  FormContactos.CargarContactos(UsuarioLogueado);
  FormContactos.Show;
  Self.Hide;
end;

procedure TFormUsuario.btnCerrarClick(Sender: TObject);
begin
  UsuarioLogueado := nil;
  FormLogin.Show;
  Self.Hide;
end;

procedure TFormUsuario.btnBandejaEntradaClick(Sender: TObject);
begin
  if not Assigned(UsuarioLogueado) then
  begin
    ShowMessage('No hay usuario logueado.');
    Exit;
  end;

  ArrancarListaBandejaEntrada(UsuarioLogueado);

  if not Assigned(FormInterfazBandejadeEntrada) then
    Application.CreateForm(TFormInterfazBandejadeEntrada, FormInterfazBandejadeEntrada);

  FormInterfazBandejadeEntrada.CargarCorreosEnListBox(UsuarioLogueado);
  FormInterfazBandejadeEntrada.Show;
  Self.Hide;
end;

procedure TFormUsuario.btnEnviarCorreoClick(Sender: TObject);
begin
  // Creamos el formulario de enviar correo y le pasamos referencia a este FormUsuario
  if not Assigned(FormInterfazEnviarCorreo) then
    Application.CreateForm(TFormInterfazEnviarCorreo, FormInterfazEnviarCorreo);

  try
    FormInterfazEnviarCorreo.SetUsuarioForm(Self);
  except
    // defensivo: si no existe, seguimos
  end;

  FormInterfazEnviarCorreo.Show;
  Self.Hide;
end;

procedure TFormUsuario.btnProgramarCorreosClick(Sender: TObject);
begin
  if not Assigned(FormInterfazProgramarCorreo) then
    Application.CreateForm(TFormInterfazProgramarCorreo, FormInterfazProgramarCorreo);

  FormInterfazProgramarCorreo.SetUsuarioForm(Self);
  FormInterfazProgramarCorreo.Show;
  Self.Hide;
end;

procedure TFormUsuario.btnVerCorreosProgramadosClick(Sender: TObject);
begin
  if not Assigned(UsuarioLogueado) then
  begin
    ShowMessage('No hay usuario logueado.');
    Exit;
  end;

  if not Assigned(FormCorreosProgramados) then
    Application.CreateForm(TFormCorreosProgramados, FormCorreosProgramados);

  FormCorreosProgramados.RefrescarGrid;
  FormCorreosProgramados.Show;
  Self.Hide;
end;

procedure TFormUsuario.btnAgregarContactosClick(Sender: TObject);
begin
  if not Assigned(FormInterfazAgregarContacto) then
    Application.CreateForm(TFormInterfazAgregarContacto, FormInterfazAgregarContacto);

  FormInterfazAgregarContacto.Show;
  Self.Hide;
end;

procedure TFormUsuario.btnActualizarPerfilClick(Sender: TObject);
begin
  // Abrimos el formulario de actualizar perfil y le PASAMOS los datos del usuario logueado
  if not Assigned(FormInterfazActualizarPerfil) then
    Application.CreateForm(TFormInterfazActualizarPerfil, FormInterfazActualizarPerfil);

  // Asegurarnos de que hay un usuario logueado
  if not Assigned(UsuarioLogueado) then
  begin
    ShowMessage('No hay usuario logueado.');
    Exit;
  end;

  // Llamamos explicitamente al método para rellenar los edits con la referencia actual
  FormInterfazActualizarPerfil.CargarDatosUsuario(UsuarioLogueado);

  FormInterfazActualizarPerfil.Show;
  Self.Hide;
end;

procedure TFormUsuario.btnVerPapeleraClick(Sender: TObject);
begin
  if not Assigned(FormInterfazPapelera) then
    Application.CreateForm(TFormInterfazPapelera, FormInterfazPapelera);

  FormInterfazPapelera.Show;
  Self.Hide;
end;

procedure TFormUsuario.btnVerFavoritoClick(Sender: TObject);
begin
  if not Assigned(UsuarioLogueado) then
  begin
    ShowMessage('No hay usuario logueado.');
    Exit;
  end;

  if not Assigned(FormInterfazBandejadeFavoritos) then
    Application.CreateForm(TFormInterfazBandejadeFavoritos, FormInterfazBandejadeFavoritos);

  FormInterfazBandejadeFavoritos.SetUsuario(UsuarioLogueado);
  FormInterfazBandejadeFavoritos.Show;
  Self.Hide;
end;

procedure TFormUsuario.btnVerborradoresdeMensajesClick(Sender: TObject);
begin
  if not Assigned(UsuarioLogueado) then
  begin
    ShowMessage('No hay usuario logueado.');
    Exit;
  end;

  if not Assigned(FormInterfazBandejadeBorrador) then
    Application.CreateForm(TFormInterfazBandejadeBorrador, FormInterfazBandejadeBorrador);

  // pasar la referencia al usuario logueado para que la bandeja pueda recorrer su AVL
  FormInterfazBandejadeBorrador.SetUsuario(UsuarioLogueado);
  FormInterfazBandejadeBorrador.Show;
  Self.Hide;
end;

procedure TFormUsuario.btnPublicarmensajeencomunidadClick(Sender: TObject);
begin
  ShowMessage('Funcionalidad de publicar mensaje en comunidad pendiente de implementar');
end;

procedure TFormUsuario.btnGenerarReportesClick(Sender: TObject);
begin
  ShowMessage('Funcionalidad de generar reportes pendiente de implementar');
end;

end.

