unit interfazProgramarCorreo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, estructuras, DateUtils;

type
  { TFormInterfazProgramarCorreo }
  TFormInterfazProgramarCorreo = class(TForm)
    btnCerrar: TButton;
    btnProgramar: TButton;
    EditDestinatario: TEdit;
    EditAsunto: TEdit;
    EditFecha: TEdit;
    EditHora: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Memo1: TMemo;
    procedure btnCerrarClick(Sender: TObject);
    procedure btnProgramarClick(Sender: TObject);
  private
    FUsuarioForm: TForm;  // referencia al formulario de usuario
  public
    procedure SetUsuarioForm(aForm: TForm);
  end;

var
  FormInterfazProgramarCorreo: TFormInterfazProgramarCorreo;

implementation

{$R *.lfm}

{ forward declarations }
procedure InsertarContactoBST(var raiz: PContactoAVL; contacto: PUsuario); forward;
procedure CrearListaContactos(usuario: PUsuario); forward;
function BuscarContacto(raiz: PContactoAVL; email: string): PUsuario; forward;

procedure InsertarCorreoProgramadoPorPrioridad(var lista: TListaCorreoProgramado; nodo: PNodoCorreoProgramado); forward;
function FechaHoraDeNodo(datos: PCorreoProgramado): TDateTime; forward;

{ ===================== implementación ===================== }

procedure TFormInterfazProgramarCorreo.SetUsuarioForm(aForm: TForm);
begin
  FUsuarioForm := aForm;
end;

procedure InsertarContactoBST(var raiz: PContactoAVL; contacto: PUsuario);
begin
  if contacto = nil then Exit;

  if raiz = nil then
  begin
    New(raiz);
    raiz^.usuarioRef := contacto;
    raiz^.email := contacto^.email;
    raiz^.izquierdo := nil;
    raiz^.derecho := nil;
    raiz^.altura := 1;
    Exit;
  end;

  if SameText(contacto^.email, raiz^.email) then
    Exit  // ya existe
  else if contacto^.email < raiz^.email then
    InsertarContactoBST(raiz^.izquierdo, contacto)
  else
    InsertarContactoBST(raiz^.derecho, contacto);
end;

procedure CrearListaContactos(usuario: PUsuario);
var
  actual: PNodoCorreo;
begin
  if usuario = nil then Exit;

  usuario^.listaContactos := nil;  // reinicia el árbol

  actual := ListaCorreosGlobal.primero;
  while actual <> nil do
  begin
    if Assigned(actual^.datos) and Assigned(actual^.datos^.remitenteUsuario)
       and Assigned(actual^.datos^.destinatarioUsuario) then
    begin
      if actual^.datos^.remitenteUsuario = usuario then
        InsertarContactoBST(usuario^.listaContactos, actual^.datos^.destinatarioUsuario);
    end;
    actual := actual^.siguiente;
  end;
end;

function BuscarContacto(raiz: PContactoAVL; email: string): PUsuario;
begin
  if raiz = nil then
    Exit(nil);

  if SameText(email, raiz^.email) then
    Exit(raiz^.usuarioRef)
  else if email < raiz^.email then
    Exit(BuscarContacto(raiz^.izquierdo, email))
  else
    Exit(BuscarContacto(raiz^.derecho, email));
end;

function FechaHoraDeNodo(datos: PCorreoProgramado): TDateTime;
begin
  if datos = nil then
    Exit(0);
  Result := datos^.fecha + datos^.hora;
end;

procedure InsertarCorreoProgramadoPorPrioridad(var lista: TListaCorreoProgramado; nodo: PNodoCorreoProgramado);
var
  actual, anterior: PNodoCorreoProgramado;
  fhNuevo, fhActual: TDateTime;
begin
  if nodo = nil then Exit;

  nodo^.siguiente := nil;

  if lista.primero = nil then
  begin
    lista.primero := nodo;
    lista.ultimo := nodo;
    Exit;
  end;

  fhNuevo := FechaHoraDeNodo(nodo^.datos);

  // insertar al inicio si es más prioritaria
  fhActual := FechaHoraDeNodo(lista.primero^.datos);
  if fhNuevo <= fhActual then
  begin
    nodo^.siguiente := lista.primero;
    lista.primero := nodo;
    Exit;
  end;

  anterior := lista.primero;
  actual := lista.primero^.siguiente;

  while actual <> nil do
  begin
    fhActual := FechaHoraDeNodo(actual^.datos);
    if fhNuevo <= fhActual then
    begin
      anterior^.siguiente := nodo;
      nodo^.siguiente := actual;
      Exit;
    end;
    anterior := actual;
    actual := actual^.siguiente;
  end;

  // insertar al final
  anterior^.siguiente := nodo;
  nodo^.siguiente := nil;
  lista.ultimo := nodo;
end;

procedure TFormInterfazProgramarCorreo.btnCerrarClick(Sender: TObject);
begin
  if Assigned(FUsuarioForm) then
  begin
    FUsuarioForm.Show;
    Self.Hide;
  end
  else
    Self.Hide;
end;

procedure TFormInterfazProgramarCorreo.btnProgramarClick(Sender: TObject);
var
  destinatarioEmail, asunto, mensaje: string;
  fecha: TDateTime;
  hora: TTime;
  destinatario: PUsuario;
  nuevoNodo: PNodoCorreoProgramado;
begin
  destinatarioEmail := Trim(EditDestinatario.Text);
  asunto := Trim(EditAsunto.Text);
  mensaje := Trim(Memo1.Text);

  if (destinatarioEmail = '') or (asunto = '') or (mensaje = '') or
     (EditFecha.Text = '') or (EditHora.Text = '') then
  begin
    ShowMessage('Por favor complete todos los campos.');
    Exit;
  end;

  fecha := StrToDateDef(EditFecha.Text, 0);
  if fecha = 0 then
  begin
    ShowMessage('Formato de fecha incorrecto. Use dd/mm/yyyy');
    Exit;
  end;

  hora := StrToTimeDef(EditHora.Text, -1);
  if hora = -1 then
  begin
    ShowMessage('Formato de hora incorrecto. Use HH:MM (24h)');
    Exit;
  end;

  if not Assigned(UsuarioLogueado) then
  begin
    ShowMessage('No hay usuario logueado.');
    Exit;
  end;

  // Asegurar lista contactos actualizada
  CrearListaContactos(UsuarioLogueado);

  // Validar destinatario dentro de contactos
  destinatario := BuscarContacto(UsuarioLogueado^.listaContactos, destinatarioEmail);
  if destinatario = nil then
  begin
    ShowMessage('El destinatario no se encuentra en sus contactos.');
    Exit;
  end;

  // Crear nodo programado y asignar destinatarioEmail
  New(nuevoNodo);
  New(nuevoNodo^.datos);

  // Usamos el contador de programados local (200+) para ids de programados
  ListaCorreosProgramadosGlobal.contadorID := ListaCorreosProgramadosGlobal.contadorID + 1;
  nuevoNodo^.datos^.id := ListaCorreosProgramadosGlobal.contadorID;
  nuevoNodo^.datos^.remitenteID := UsuarioLogueado^.id;
  nuevoNodo^.datos^.remitente := UsuarioLogueado^.email;
  nuevoNodo^.datos^.destinatarioEmail := destinatarioEmail; // <-- guardado aquí
  nuevoNodo^.datos^.estado := 'Programado';
  nuevoNodo^.datos^.programado := 'SI';
  nuevoNodo^.datos^.asunto := asunto;
  nuevoNodo^.datos^.fecha := fecha;
  nuevoNodo^.datos^.hora := hora;
  nuevoNodo^.datos^.mensaje := mensaje;
  nuevoNodo^.datos^.favorito := False;
  nuevoNodo^.siguiente := nil;

  // Insertar por prioridad en la cola global de programados
  InsertarCorreoProgramadoPorPrioridad(ListaCorreosProgramadosGlobal, nuevoNodo);

  ShowMessage('Correo programado correctamente (insertado por prioridad). ID programado: ' + IntToStr(nuevoNodo^.datos^.id));

  EditDestinatario.Clear;
  EditAsunto.Clear;
  EditFecha.Clear;
  EditHora.Clear;
  Memo1.Clear;
end;

end.

