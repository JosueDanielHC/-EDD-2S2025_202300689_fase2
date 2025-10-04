unit interfazCorreosProgramados;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids, estructuras, DateUtils;

type

  { TFormCorreosProgramados }

  TFormCorreosProgramados = class(TForm)
    btnCerrar: TButton;
    btnEnviar: TButton;
    btnRefrescar: TButton;
    Label1: TLabel;
    StringGrid1: TStringGrid;
    procedure btnCerrarClick(Sender: TObject);
    procedure btnRefrescarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnEnviarClick(Sender: TObject);
  private
    // Busca usuario por email en ListaUsuariosGlobal
    function BuscarUsuarioPorEmail(email: string): PUsuario;
    // Elimina un nodo específico de la lista programada (mantiene lista consistente)
    procedure EliminarNodoProgramado(var lista: TListaCorreoProgramado; nodo: PNodoCorreoProgramado; anterior: PNodoCorreoProgramado);
  public
    procedure RefrescarGrid;
  end;

var
  FormCorreosProgramados: TFormCorreosProgramados;

implementation

uses
  interfazUsuario; // agregado para poder mostrar/regresar a FormUsuario sin errores

{$R *.lfm}

{ ===================== utilidades ===================== }

function FechaHoraToStr(fecha: TDateTime; hora: TTime): string;
begin
  Result := FormatDateTime('dd/mm/yyyy', fecha) + ' ' + FormatDateTime('hh:nn', hora);
end;

function TFormCorreosProgramados.BuscarUsuarioPorEmail(email: string): PUsuario;
var
  nodo: PUsuarioNodo;
begin
  Result := nil;
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

procedure TFormCorreosProgramados.EliminarNodoProgramado(var lista: TListaCorreoProgramado; nodo: PNodoCorreoProgramado; anterior: PNodoCorreoProgramado);
begin
  if nodo = nil then Exit;

  // si es el primero
  if anterior = nil then
  begin
    lista.primero := nodo^.siguiente;
    if lista.primero = nil then
      lista.ultimo := nil;
  end
  else
  begin
    anterior^.siguiente := nodo^.siguiente;
    if anterior^.siguiente = nil then
      lista.ultimo := anterior;
  end;

  // liberar memoria del dato y del nodo
  if Assigned(nodo^.datos) then
    Dispose(nodo^.datos);
  Dispose(nodo);
end;

{ ===================== eventos y métodos públicos ===================== }

procedure TFormCorreosProgramados.FormCreate(Sender: TObject);
begin
  // Recalcular el contador global para IDs de correos (por si hubo carga previa)
  RecalcularContadorIDCorreosGlobal;

  StringGrid1.ColCount := 3;
  StringGrid1.RowCount := 1;
  StringGrid1.FixedRows := 1;
  StringGrid1.Cells[0,0] := 'Asunto';
  StringGrid1.Cells[1,0] := 'Remitente';
  StringGrid1.Cells[2,0] := 'Fecha y Hora';
end;

procedure TFormCorreosProgramados.btnCerrarClick(Sender: TObject);
begin
  Self.Hide;
  if Assigned(FormUsuario) then
    FormUsuario.Show;
end;

// RefrescarGrid: recorre la lista programada y muestra solo los correos cuyo remitente coincide
// con UsuarioLogueado, ordenados por prioridad (la lista ya está ordenada por prioridad)
procedure TFormCorreosProgramados.RefrescarGrid;
var
  actual: PNodoCorreoProgramado;
  r: Integer;
begin
  StringGrid1.RowCount := 1; // dejar solo encabezado

  if not Assigned(UsuarioLogueado) then Exit;

  actual := ListaCorreosProgramadosGlobal.primero;
  while actual <> nil do
  begin
    if Assigned(actual^.datos) and SameText(Trim(actual^.datos^.remitente), Trim(UsuarioLogueado^.email)) then
    begin
      r := StringGrid1.RowCount;
      StringGrid1.RowCount := r + 1;
      StringGrid1.Cells[0, r] := actual^.datos^.asunto;
      StringGrid1.Cells[1, r] := actual^.datos^.remitente;
      StringGrid1.Cells[2, r] := FechaHoraToStr(actual^.datos^.fecha, actual^.datos^.hora);
    end;
    actual := actual^.siguiente;
  end;
end;

procedure TFormCorreosProgramados.btnRefrescarClick(Sender: TObject);
begin
  // recorrer nuevamente la lista programada por si hubo cambios y refrescar el grid
  RefrescarGrid;
end;

// btnEnviar: toma el correo programado más próximo a expirar DEL USUARIO LOGUEADO,
// lo copia como un nuevo TCorreo en ListaCorreosGlobal y elimina el nodo programado.
// Además asigna un id autoincremental usando ContadorIDCorreosGlobal.
procedure TFormCorreosProgramados.btnEnviarClick(Sender: TObject);
var
  actual, anterior: PNodoCorreoProgramado;
  mejor, antMejor: PNodoCorreoProgramado;
  fhActual, fhMejor: TDateTime;
  nuevoCorreoNodo: PNodoCorreo;
  nuevoCorreo: PCorreo;
  destinatarioUsuario: PUsuario;
begin
  if not Assigned(UsuarioLogueado) then
  begin
    ShowMessage('No hay usuario logueado.');
    Exit;
  end;

  // Buscar el nodo con remitente = UsuarioLogueado^.email y fecha/hora mínima
  actual := ListaCorreosProgramadosGlobal.primero;
  anterior := nil;
  mejor := nil;
  antMejor := nil;
  fhMejor := MaxDateTime; // inicial grande

  while actual <> nil do
  begin
    if Assigned(actual^.datos) and SameText(Trim(actual^.datos^.remitente), Trim(UsuarioLogueado^.email)) then
    begin
      fhActual := actual^.datos^.fecha + actual^.datos^.hora;
      if fhActual < fhMejor then
      begin
        fhMejor := fhActual;
        mejor := actual;
        antMejor := anterior;
      end;
    end;
    anterior := actual;
    actual := actual^.siguiente;
  end;

  if mejor = nil then
  begin
    ShowMessage('No hay correos programados para enviar del usuario logueado.');
    Exit;
  end;

  // Crear nuevo nodo en ListaCorreosGlobal con los datos del programado
  New(nuevoCorreo);
  New(nuevoCorreoNodo);

  // Asignar ID autoincremental usando ContadorIDCorreosGlobal
  ContadorIDCorreosGlobal := ContadorIDCorreosGlobal + 1;
  nuevoCorreo^.id := ContadorIDCorreosGlobal;

  nuevoCorreo^.remitente := nil; // compatibilidad con estructura antigua; puedes dejar nil
  nuevoCorreo^.remitenteUsuario := UsuarioLogueado;

  // encontrar usuario destinatario por email
  destinatarioUsuario := BuscarUsuarioPorEmail(mejor^.datos^.destinatarioEmail);
  nuevoCorreo^.destinatarioUsuario := destinatarioUsuario;

  nuevoCorreo^.estado := 'NL';
  nuevoCorreo^.programado := 'No';
  nuevoCorreo^.asunto := mejor^.datos^.asunto;
  nuevoCorreo^.fecha := mejor^.datos^.fecha;
  nuevoCorreo^.hora := mejor^.datos^.hora;
  nuevoCorreo^.mensaje := mejor^.datos^.mensaje;
  nuevoCorreo^.favorito := mejor^.datos^.favorito;

  // insertar al final de ListaCorreosGlobal (lista doble enlazada)
  nuevoCorreoNodo^.datos := nuevoCorreo;
  nuevoCorreoNodo^.siguiente := nil;
  nuevoCorreoNodo^.anterior := nil;

  if ListaCorreosGlobal.primero = nil then
  begin
    ListaCorreosGlobal.primero := nuevoCorreoNodo;
    ListaCorreosGlobal.ultimo := nuevoCorreoNodo;
  end
  else
  begin
    nuevoCorreoNodo^.anterior := ListaCorreosGlobal.ultimo;
    ListaCorreosGlobal.ultimo^.siguiente := nuevoCorreoNodo;
    ListaCorreosGlobal.ultimo := nuevoCorreoNodo;
  end;

  // Eliminar nodo programado (mejor)
  EliminarNodoProgramado(ListaCorreosProgramadosGlobal, mejor, antMejor);

  ShowMessage('Correo enviado (migrado de programados a lista global). ID asignado: ' + IntToStr(nuevoCorreo^.id));

  // refrescar vista
  RefrescarGrid;
end;

end.

