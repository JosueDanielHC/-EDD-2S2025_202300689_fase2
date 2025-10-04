unit interfazvercorreo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  estructuras; // para PCorreo y estructuras relacionadas

type

  { TFormVerCorreo }

  TFormVerCorreo = class(TForm)
    btnCerrar: TButton;
    btnEliminar: TButton;
    btnFavorito: TButton;
    LabelCorreo: TLabel;
    LabelMensaje: TLabel;
    LabelFecha: TLabel;
    LabelAsunto: TLabel;
    LabelRemitente: TLabel;
    Memo1: TMemo;
    procedure btnCerrarClick(Sender: TObject);
    procedure btnEliminarClick(Sender: TObject);
    procedure btnFavoritoClick(Sender: TObject);
  private
    FCorreo: PCorreo; // puntero al correo actualmente mostrado
  public
    // Muestra los datos del correo pasado (pone los textos en labels/memo)
    procedure MostrarCorreo(correo: PCorreo);
  end;

var
  FormVerCorreo: TFormVerCorreo;

implementation

{$R *.lfm}

uses
  DateUtils, interfazBandejadeEntrada, interfazUsuario, login;

{ ==================== AUXILIARES ==================== }

// Elimina de ListaCorreosGlobal el PNodoCorreo cuyo datos = correoRef.
// También libera la memoria del PCorreo y del nodo PNodoCorreo.
procedure EliminarCorreoDeListaGlobal(correoRef: PCorreo);
var
  nodo, tmp: PNodoCorreo;
begin
  if (correoRef = nil) or (ListaCorreosGlobal.primero = nil) then Exit;

  nodo := ListaCorreosGlobal.primero;
  while nodo <> nil do
  begin
    if nodo^.datos = correoRef then
    begin
      if nodo^.anterior = nil then
        ListaCorreosGlobal.primero := nodo^.siguiente
      else
        nodo^.anterior^.siguiente := nodo^.siguiente;

      if nodo^.siguiente = nil then
        ListaCorreosGlobal.ultimo := nodo^.anterior
      else
        nodo^.siguiente^.anterior := nodo^.anterior;

      Dispose(nodo^.datos);
      Dispose(nodo);
      Exit;
    end;
    nodo := nodo^.siguiente;
  end;
end;

// Elimina de la listaBandejaEntrada del usuario el PNodoBandejaEntrada que apunte a correoRef.
procedure EliminarNodoDeBandejaUsuario(usuario: PUsuario; correoRef: PCorreo);
var
  nodo: PNodoBandejaEntrada;
begin
  if (usuario = nil) or (usuario^.listaBandejaEntrada = nil) or (correoRef = nil) then Exit;

  nodo := usuario^.listaBandejaEntrada^.primero;
  while nodo <> nil do
  begin
    if nodo^.correoRef = correoRef then
    begin
      if nodo^.anterior = nil then
        usuario^.listaBandejaEntrada^.primero := nodo^.siguiente
      else
        nodo^.anterior^.siguiente := nodo^.siguiente;

      if nodo^.siguiente = nil then
        usuario^.listaBandejaEntrada^.ultimo := nodo^.anterior
      else
        nodo^.siguiente^.anterior := nodo^.anterior;

      Dispose(nodo);
      Exit;
    end;
    nodo := nodo^.siguiente;
  end;
end;

// Empujar a la pila listaPapelera del usuario una copia de los datos del correo
procedure PushPapeleraUsuario(usuario: PUsuario; correoRef: PCorreo);
var
  listaP: PListaPapelera;
  nodoP: PNodoPapelera;
  datosP: PPapelera;
begin
  if usuario = nil then Exit;

  if usuario^.listaPapelera = nil then
  begin
    New(listaP);
    listaP^.tope := nil;
    listaP^.contadorID := 100;
    usuario^.listaPapelera := listaP;
  end;

  listaP := usuario^.listaPapelera;

  New(datosP);
  datosP^.id := listaP^.contadorID;
  Inc(listaP^.contadorID);

  if Assigned(correoRef^.remitenteUsuario) then
    datosP^.remitente := correoRef^.remitenteUsuario^.email
  else
    datosP^.remitente := '<desconocido>';

  datosP^.estado := 'Eliminado';
  datosP^.programado := correoRef^.programado;
  datosP^.asunto := correoRef^.asunto;
  datosP^.fecha := correoRef^.fecha;
  datosP^.hora := correoRef^.hora;
  datosP^.mensaje := correoRef^.mensaje;

  New(nodoP);
  nodoP^.datos := datosP;
  nodoP^.siguiente := listaP^.tope;
  listaP^.tope := nodoP;
end;

{ ==================== EVENTOS del formulario ==================== }

procedure TFormVerCorreo.btnCerrarClick(Sender: TObject);
begin
  // Mostrar la interfaz de bandeja (si existe), y ocultar esta ventana
  if not Assigned(FormInterfazBandejadeEntrada) then
    Application.CreateForm(TFormInterfazBandejadeEntrada, FormInterfazBandejadeEntrada);

  if Assigned(UsuarioLogueado) then
    FormInterfazBandejadeEntrada.CargarCorreosEnListBox(UsuarioLogueado);

  FormInterfazBandejadeEntrada.Show;
  Self.Hide;
end;

procedure TFormVerCorreo.btnEliminarClick(Sender: TObject);
var
  correo: PCorreo;
begin
  if not Assigned(FCorreo) then
  begin
    ShowMessage('No hay correo seleccionado.');
    Exit;
  end;

  correo := FCorreo;

  if not Assigned(UsuarioLogueado) then
  begin
    ShowMessage('No hay usuario logueado. No se pudo eliminar el correo.');
    Exit;
  end;

  PushPapeleraUsuario(UsuarioLogueado, correo);
  EliminarNodoDeBandejaUsuario(UsuarioLogueado, correo);
  EliminarCorreoDeListaGlobal(correo);

  ShowMessage('Correo eliminado y movido a Papelera correctamente.');

  // Refrescar y mostrar siempre la bandeja
  if not Assigned(FormInterfazBandejadeEntrada) then
    Application.CreateForm(TFormInterfazBandejadeEntrada, FormInterfazBandejadeEntrada);

  if Assigned(UsuarioLogueado) then
    FormInterfazBandejadeEntrada.CargarCorreosEnListBox(UsuarioLogueado);

  FormInterfazBandejadeEntrada.Show;
  Self.Hide;
end;

procedure TFormVerCorreo.btnFavoritoClick(Sender: TObject);
begin
  if not Assigned(FCorreo) then
  begin
    ShowMessage('No hay correo seleccionado.');
    Exit;
  end;

  FCorreo^.favorito := True;

  ShowMessage('Correo marcado como favorito.');

  if Assigned(FormInterfazBandejadeEntrada) and Assigned(UsuarioLogueado) then
    FormInterfazBandejadeEntrada.CargarCorreosEnListBox(UsuarioLogueado);

  if Assigned(FormInterfazBandejadeEntrada) then
    FormInterfazBandejadeEntrada.Show;
  Self.Hide;
end;

procedure TFormVerCorreo.MostrarCorreo(correo: PCorreo);
var
  remitenteText: string;
begin
  FCorreo := correo;

  if not Assigned(correo) then
  begin
    ShowMessage('Correo inválido');
    Exit;
  end;

  if Assigned(correo^.remitenteUsuario) then
    remitenteText := correo^.remitenteUsuario^.email
  else
    remitenteText := '<desconocido>';

  LabelAsunto.Caption := 'Asunto: ' + correo^.asunto;
  LabelRemitente.Caption := 'Remitente: ' + remitenteText;
  LabelFecha.Caption := 'Fecha: ' + DateTimeToStr(correo^.fecha);
  Memo1.Lines.Text := correo^.mensaje;
end;

end.

