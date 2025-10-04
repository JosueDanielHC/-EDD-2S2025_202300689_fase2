unit interfazBuscarCorreoEliminado;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids,
  estructuras; // usamos estructuras para tipos de papelera

type

  { TFormInterfazBuscarCorreoEliminado }

  TFormInterfazBuscarCorreoEliminado = class(TForm)
    btnBuscar: TButton;
    btnCerrar: TButton;
    EditAsunto: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    StringGrid1: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure btnBuscarClick(Sender: TObject);
    procedure btnCerrarClick(Sender: TObject);
  private
    procedure SetupGridHeaders;
    procedure MostrarCorreoEnGrid(correoP: PPapelera);
  public

  end;

var
  FormInterfazBuscarCorreoEliminado: TFormInterfazBuscarCorreoEliminado;

implementation

uses
  interfazPapelera, DateUtils; // interfazPapelera en implementation para evitar circularidad

{$R *.lfm}

{ TFormInterfazBuscarCorreoEliminado }

procedure TFormInterfazBuscarCorreoEliminado.FormCreate(Sender: TObject);
begin
  SetupGridHeaders;
end;

procedure TFormInterfazBuscarCorreoEliminado.SetupGridHeaders;
begin
  // Cada fila será un campo: Col0 etiqueta, Col1 valor
  StringGrid1.ColCount := 2;
  StringGrid1.FixedRows := 1;
  StringGrid1.RowCount := 1;
  StringGrid1.Cells[0,0] := 'Campo';
  StringGrid1.Cells[1,0] := 'Valor';
end;

// Rellena el StringGrid con las filas solicitadas (Remitente, Programado, Asunto, Fecha y Hora, Mensaje)
procedure TFormInterfazBuscarCorreoEliminado.MostrarCorreoEnGrid(correoP: PPapelera);
var
  row: Integer;
  fechaHora: string;
  programadoText: string;
begin
  if correoP = nil then Exit;

  if Trim(correoP^.programado) = '' then
    programadoText := 'No'
  else if SameText(correoP^.programado, 'SI') then
    programadoText := 'Si'
  else
    programadoText := correoP^.programado;

  fechaHora := DateTimeToStr(correoP^.fecha) + ' ' + TimeToStr(correoP^.hora);

  // Limpiar y ajustar el grid (fija + 5 filas de datos)
  StringGrid1.RowCount := 1 + 5;

  row := 1;
  StringGrid1.Cells[0,row] := 'Remitente';
  StringGrid1.Cells[1,row] := correoP^.remitente;
  Inc(row);

  StringGrid1.Cells[0,row] := 'Programado';
  StringGrid1.Cells[1,row] := programadoText;
  Inc(row);

  StringGrid1.Cells[0,row] := 'Asunto';
  StringGrid1.Cells[1,row] := correoP^.asunto;
  Inc(row);

  StringGrid1.Cells[0,row] := 'Fecha y Hora';
  StringGrid1.Cells[1,row] := fechaHora;
  Inc(row);

  StringGrid1.Cells[0,row] := 'Mensaje';
  StringGrid1.Cells[1,row] := correoP^.mensaje;
end;

procedure TFormInterfazBuscarCorreoEliminado.btnBuscarClick(Sender: TObject);
var
  actual: PNodoPapelera;
  buscado: string;
  encontrado: Boolean;
begin
  SetupGridHeaders;

  if (UsuarioLogueado = nil) or (UsuarioLogueado^.listaPapelera = nil) then
  begin
    ShowMessage('No hay papelera para el usuario actual.');
    Exit;
  end;

  buscado := Trim(EditAsunto.Text);
  if buscado = '' then
  begin
    ShowMessage('Por favor ingrese un asunto para buscar.');
    Exit;
  end;

  encontrado := False;
  actual := UsuarioLogueado^.listaPapelera^.tope;
  while actual <> nil do
  begin
    if Pos(LowerCase(buscado), LowerCase(actual^.datos^.asunto)) > 0 then
    begin
      // mostrar primer correo que coincide
      MostrarCorreoEnGrid(actual^.datos);
      encontrado := True;
      Break;
    end;
    actual := actual^.siguiente;
  end;

  if not encontrado then
    ShowMessage('No se encontró ningún correo en la papelera con ese asunto.');
end;

procedure TFormInterfazBuscarCorreoEliminado.btnCerrarClick(Sender: TObject);
begin
  // Volver a la pantalla de la papelera
  if Assigned(FormInterfazPapelera) then
  begin
    // refrescar la papelera antes de mostrarla (opcional)
    FormInterfazPapelera.CargarCorreosEnGrid;
    FormInterfazPapelera.Show;
  end;
  Self.Hide;
end;

end.

