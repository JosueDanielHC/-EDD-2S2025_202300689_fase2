unit interfazActualizarPerfil;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, estructuras;

type
  { TFormInterfazActualizarPerfil }
  TFormInterfazActualizarPerfil = class(TForm)
    btnCerrar: TButton;
    btnActualizar: TButton;
    EditNombre: TEdit;
    EditUsuario: TEdit;
    EditCorreo: TEdit;
    EditTelefono: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    procedure btnCerrarClick(Sender: TObject);
    procedure btnActualizarClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    /// Rellena los TEdit con los valores del usuario dado y bloquea los no editables
    procedure CargarDatosUsuario(usuario: PUsuario);
  end;

var
  FormInterfazActualizarPerfil: TFormInterfazActualizarPerfil;

implementation

uses
  interfazUsuario; // en implementation para evitar circular en la sección interface

{$R *.lfm}

{ --------------------------------------------------
  CargarDatosUsuario:
  - Pone en los TEdit los valores del registro PUsuario pasado.
  - Muestra los campos que no se deben editar como ReadOnly.
---------------------------------------------------}
procedure TFormInterfazActualizarPerfil.CargarDatosUsuario(usuario: PUsuario);
begin
  if usuario = nil then
  begin
    // No hay usuario, limpiamos los campos
    EditNombre.Text := '';
    EditUsuario.Text := '';
    EditCorreo.Text := '';
    EditTelefono.Text := '';
    Exit;
  end;

  // Rellenar campos con la información del usuario
  EditNombre.Text := usuario^.nombre;
  EditUsuario.Text := usuario^.usuario;
  EditCorreo.Text := usuario^.email;
  EditTelefono.Text := IntToStr(usuario^.telefono);

  // Bloquear los campos que no deben cambiarse
  EditNombre.ReadOnly := True;
  EditCorreo.ReadOnly := True;
end;

{ --------------------------------------------------
  OnShow: cuando el formulario se muestra, no forzamos SetFocus aquí.
  El formulario se mostrará con los valores que haya pasado quien lo abrió.
---------------------------------------------------}
procedure TFormInterfazActualizarPerfil.FormShow(Sender: TObject);
begin
  // Si por algún motivo no se llamó a CargarDatosUsuario externamente,
  // intentamos cargar desde la variable global (defensivo).
  if (EditNombre.Text = '') and Assigned(UsuarioLogueado) then
    CargarDatosUsuario(UsuarioLogueado);
end;

{ --------------------------------------------------
  btnActualizarClick: valida y guarda los cambios permitidos
  (usuario y teléfono) en la estructura del UsuarioLogueado.
---------------------------------------------------}
procedure TFormInterfazActualizarPerfil.btnActualizarClick(Sender: TObject);
var
  nuevoTelefono: Integer;
begin
  if not Assigned(UsuarioLogueado) then
  begin
    ShowMessage('No hay usuario logueado.');
    Exit;
  end;

  // Actualizamos sólo los campos permitidos
  UsuarioLogueado^.usuario := Trim(EditUsuario.Text);

  // Validar teléfono (vacío -> 0)
  if Trim(EditTelefono.Text) = '' then
  begin
    UsuarioLogueado^.telefono := 0;
    ShowMessage('Perfil actualizado correctamente.');
  end
  else if TryStrToInt(Trim(EditTelefono.Text), nuevoTelefono) then
  begin
    UsuarioLogueado^.telefono := nuevoTelefono;
    ShowMessage('Perfil actualizado correctamente.');
  end
  else
    ShowMessage('Teléfono inválido. Ingrese un número.');

  // Volver al formulario usuario
  if Assigned(FormUsuario) then
  begin
    FormUsuario.LabelBienvenido.Caption := 'Bienvenido: ' + UsuarioLogueado^.nombre;
    FormUsuario.Show;
  end;
  Self.Hide;
end;

{ --------------------------------------------------
  btnCerrarClick: cierra el formulario y regresa
  a la interfazUsuario sin realizar cambios.
---------------------------------------------------}
procedure TFormInterfazActualizarPerfil.btnCerrarClick(Sender: TObject);
begin
  if Assigned(FormUsuario) then
    FormUsuario.Show;
  Self.Hide;
end;

end.

