unit interfazCrearUsuario;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormCrearUsuario }

  TFormCrearUsuario = class(TForm)
    btnCrear: TButton;
    btnCerrar: TButton;
    EditNombre: TEdit;
    EditUsuario: TEdit;
    EditPassword: TEdit;
    EditEmail: TEdit;
    EditTelefono: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    procedure btnCerrarClick(Sender: TObject);
    procedure btnCrearClick(Sender: TObject);
  private

  public

  end;

var
  FormCrearUsuario: TFormCrearUsuario;

implementation

uses
  estructuras, // contiene ListaUsuariosGlobal y tipos
  login;       // permite acceder a FormLogin

{$R *.lfm}

{ Helpers locales ------------------------------------------------------------ }

// Verifica suffix '@edd.com' (case-insensitive)
function EmailValido(const s: string): Boolean;
var
  suf: string;
  st: string;
begin
  st := Trim(s);
  Result := False;
  if st = '' then Exit;
  if Pos('@', st) <= 1 then Exit;
  if Length(st) < 9 then Exit; // mínimo x@edd.com
  suf := LowerCase(Copy(st, Length(st) - 7, 8)); // últimos 8 chars
  Result := (suf = '@edd.com');
end;

// Devuelve True si el string contiene solo dígitos (al menos 1)
function SoloDigitos(const s: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  if Trim(s) = '' then Exit;
  for i := 1 to Length(s) do
    if not (s[i] in ['0'..'9']) then Exit;
  Result := True;
end;

// Busca usuario por email en ListaUsuariosGlobal (devuelve PUsuario o nil)
function BuscarUsuarioPorEmail_Global(const email: string): PUsuario;
var
  nodo: PUsuarioNodo;
begin
  Result := nil;
  if Trim(email) = '' then Exit;
  nodo := ListaUsuariosGlobal.primero;
  while nodo <> nil do
  begin
    if (nodo^.datos <> nil) and (AnsiCompareText(nodo^.datos^.email, email) = 0) then
    begin
      Result := nodo^.datos;
      Exit;
    end;
    nodo := nodo^.siguiente;
  end;
end;

// Busca usuario por nombre de usuario (campo 'usuario') en ListaUsuariosGlobal (devuelve PUsuario o nil)
function BuscarUsuarioPorUsuario_Global(const usuario: string): PUsuario;
var
  nodo: PUsuarioNodo;
begin
  Result := nil;
  if Trim(usuario) = '' then Exit;
  nodo := ListaUsuariosGlobal.primero;
  while nodo <> nil do
  begin
    if (nodo^.datos <> nil) and (AnsiCompareText(nodo^.datos^.usuario, usuario) = 0) then
    begin
      Result := nodo^.datos;
      Exit;
    end;
    nodo := nodo^.siguiente;
  end;
end;

// Devuelve el mayor id actual en la lista (0 si la lista está vacía)
function ObtenerUltimoIDUsuario: Integer;
var
  nodo: PUsuarioNodo;
  maxID: Integer;
begin
  maxID := 0;
  nodo := ListaUsuariosGlobal.primero;
  while nodo <> nil do
  begin
    if (nodo^.datos <> nil) and (nodo^.datos^.id > maxID) then
      maxID := nodo^.datos^.id;
    nodo := nodo^.siguiente;
  end;
  Result := maxID;
end;

{ Event handlers ------------------------------------------------------------- }

procedure TFormCrearUsuario.btnCrearClick(Sender: TObject);
var
  nombre, usuario, password, email, telefonoStr: string;
  telefonoInt: Integer;
  newId: Integer;
  nuevoUser: PUsuario;
  nuevoNodo, ultimoNodo: PUsuarioNodo;
begin
  nombre := Trim(EditNombre.Text);
  usuario := Trim(EditUsuario.Text);
  password := Trim(EditPassword.Text);
  email := Trim(EditEmail.Text);
  telefonoStr := Trim(EditTelefono.Text);

  // Validar campos no vacíos
  if (nombre = '') or (usuario = '') or (password = '') or (email = '') or (telefonoStr = '') then
  begin
    ShowMessage('Error: debe completar todos los campos.');
    Exit;
  end;

  // Validar email
  if not EmailValido(email) then
  begin
    ShowMessage('Error: el email no es válido. Debe contener el sufijo @edd.com');
    Exit;
  end;

  // Validar teléfono (solo dígitos) y convertir
  if not SoloDigitos(telefonoStr) then
  begin
    ShowMessage('Error: el teléfono debe contener solo números.');
    Exit;
  end;
  telefonoInt := StrToIntDef(telefonoStr, 0);
  if telefonoInt <= 0 then
  begin
    ShowMessage('Error: teléfono inválido.');
    Exit;
  end;

  // Validar unicidad de email y usuario frente a lista global
  if BuscarUsuarioPorEmail_Global(email) <> nil then
  begin
    ShowMessage('Error: ya existe un usuario con ese email.');
    Exit;
  end;
  if BuscarUsuarioPorUsuario_Global(usuario) <> nil then
  begin
    ShowMessage('Error: ya existe ese nombre de usuario. Por favor elija otro.');
    Exit;
  end;

  // Calcular nuevo id (autoincremental)
  newId := ObtenerUltimoIDUsuario + 1;
  if newId <= 0 then newId := 1;

  // Crear estructura usuario
  New(nuevoUser);
  FillChar(nuevoUser^, SizeOf(TUsuario), 0);
  nuevoUser^.id := newId;
  nuevoUser^.nombre := nombre;
  nuevoUser^.usuario := usuario;
  nuevoUser^.password := password;
  nuevoUser^.email := email;
  nuevoUser^.telefono := telefonoInt;
  nuevoUser^.listaContactos := nil;
  nuevoUser^.listaPapelera := nil;
  nuevoUser^.listaBorradores := nil;
  nuevoUser^.listaBandejaEntrada := nil;
  nuevoUser^.listaCorreosEnviados := nil;
  nuevoUser^.listaFavoritos := nil;

  // Crear nodo y anexarlo al final
  New(nuevoNodo);
  FillChar(nuevoNodo^, SizeOf(TUsuarioNodo), 0);
  nuevoNodo^.datos := nuevoUser;
  nuevoNodo^.siguiente := nil;

  if ListaUsuariosGlobal.primero = nil then
    ListaUsuariosGlobal.primero := nuevoNodo
  else
  begin
    ultimoNodo := ListaUsuariosGlobal.primero;
    while ultimoNodo^.siguiente <> nil do
      ultimoNodo := ultimoNodo^.siguiente;
    ultimoNodo^.siguiente := nuevoNodo;
  end;

  ShowMessage(Format('Usuario creado con éxito. ID asignado: %d', [newId]));

  // Limpiar campos
  EditNombre.Text := '';
  EditUsuario.Text := '';
  EditPassword.Text := '';
  EditEmail.Text := '';
  EditTelefono.Text := '';
end;

procedure TFormCrearUsuario.btnCerrarClick(Sender: TObject);
begin
  FormLogin.Show;  // Regresa al login
  Self.Hide;       // Oculta este formulario
end;

end.


