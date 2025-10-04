unit interfazAgregarContacto;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, estructuras;

type
  { TFormInterfazAgregarContacto }
  TFormInterfazAgregarContacto = class(TForm)
    btnAgregar: TButton;
    btnCerrar: TButton;
    EditCorreo: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure btnAgregarClick(Sender: TObject);
    procedure btnCerrarClick(Sender: TObject);
  private
    function BuscarUsuarioPorEmail(const email: string): PUsuario;
  public
  end;

var
  FormInterfazAgregarContacto: TFormInterfazAgregarContacto;

implementation

uses
  interfazUsuario;

{$R *.lfm}

{ ==================== BUSCAR USUARIO EN LISTA GLOBAL ==================== }
function TFormInterfazAgregarContacto.BuscarUsuarioPorEmail(const email: string): PUsuario;
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

{ ==================== BOTON AGREGAR CONTACTO ==================== }
procedure TFormInterfazAgregarContacto.btnAgregarClick(Sender: TObject);
var
  correo: string;
  usuarioEncontrado: PUsuario;
begin
  correo := Trim(EditCorreo.Text);

  if correo = '' then
  begin
    ShowMessage('Ingrese un correo.');
    Exit;
  end;

  if not Assigned(UsuarioLogueado) then
  begin
    ShowMessage('No hay usuario logueado.');
    Exit;
  end;

  if SameText(correo, UsuarioLogueado^.email) then
  begin
    ShowMessage('No puede agregarse a sí mismo como contacto.');
    Exit;
  end;

  // Verificar que el usuario exista
  usuarioEncontrado := FormUsuario.BuscarUsuarioPorEmail(correo);
  if usuarioEncontrado = nil then
  begin
    ShowMessage('No existe ese usuario en el sistema.');
    Exit;
  end;

  // Verificar que no esté ya en el BST
  if FormUsuario.ExisteContactoBST(UsuarioLogueado^.listaContactos, correo) then
  begin
    ShowMessage('Este contacto ya existe en su lista.');
    Exit;
  end;

  // Insertar contacto
  try
    FormUsuario.InsertarContactoBST(UsuarioLogueado^.listaContactos, usuarioEncontrado);
    ShowMessage('Contacto agregado correctamente.');
    EditCorreo.Clear;
  except
    on E: Exception do
      ShowMessage('Error al agregar contacto: ' + E.Message);
  end;
end;

procedure TFormInterfazAgregarContacto.btnCerrarClick(Sender: TObject);
begin
  Self.Hide;
  if Assigned(FormUsuario) then
    FormUsuario.Show;
end;

end.

