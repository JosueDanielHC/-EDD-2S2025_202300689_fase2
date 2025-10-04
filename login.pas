unit login;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, estructuras;

type
  { TFormLogin }
  TFormLogin = class(TForm)
    btnIniciarSesion: TButton;
    btnCrearCuenta: TButton;
    EditEmail: TEdit;
    EditPassword: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure btnCrearCuentaClick(Sender: TObject);
    procedure btnIniciarSesionClick(Sender: TObject);
  private
    function ValidarCredenciales(email, password: string; out usuarioEncontrado: PUsuario): Boolean;
  public
  end;

var
  FormLogin: TFormLogin;

implementation

uses
  root, interfazUsuario, interfazCrearUsuario;

{$R *.lfm}

{ TFormLogin }

procedure TFormLogin.btnIniciarSesionClick(Sender: TObject);
var
  email, password: string;
  usuarioEncontrado: PUsuario;
begin
  email := Trim(EditEmail.Text);
  password := Trim(EditPassword.Text);

  if (email = '') or (password = '') then
  begin
    ShowMessage('Por favor ingrese email y contraseña');
    Exit;
  end;

  if (email = 'root@edd.com') and (password = 'root123') then
  begin
    ShowMessage('Bienvenido root');
    FormRoot.Show;
    Self.Hide;
    Exit;
  end;

  if ValidarCredenciales(email, password, usuarioEncontrado) then
  begin
    UsuarioLogueado := usuarioEncontrado;

    ShowMessage('Bienvenido ' + usuarioEncontrado^.nombre);
    FormUsuario.LabelBienvenido.Caption := 'Bienvenido: ' + usuarioEncontrado^.nombre;
    FormUsuario.CrearListaContactos(UsuarioLogueado); // crear contactos al loguearse
    FormUsuario.Show;
    Self.Hide;
  end
  else
  begin
    ShowMessage('Credenciales incorrectas');
    EditPassword.Text := '';
    EditEmail.SetFocus;
  end;
end;

procedure TFormLogin.btnCrearCuentaClick(Sender: TObject);
begin
  FormCrearUsuario.Show;
  Self.Hide;
end;

function TFormLogin.ValidarCredenciales(email, password: string; out usuarioEncontrado: PUsuario): Boolean;
var
  actual: PUsuarioNodo;
begin
  Result := False;
  usuarioEncontrado := nil;

  actual := ListaUsuariosGlobal.primero;
  while actual <> nil do
  begin
    if (actual^.datos <> nil) and
       (SameText(Trim(actual^.datos^.email), Trim(email))) and
       (actual^.datos^.password = password) then
    begin
      usuarioEncontrado := actual^.datos;
      Result := True;
      Exit;
    end;
    actual := actual^.siguiente;
  end;
end;

end.

