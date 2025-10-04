unit interfazPapelera;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids,
  estructuras; // no referenciar interfazBuscarCorreoEliminado aquí (evitar circularidad en interface)

type
  { TFormInterfazPapelera }
  TFormInterfazPapelera = class(TForm)
    btnBuscar: TButton;
    btnEliminar: TButton;
    btnRefrescar: TButton;
    btnCerrar: TButton;
    lblPapelera: TLabel;
    StringGrid1: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure btnBuscarClick(Sender: TObject);
    procedure btnEliminarClick(Sender: TObject);
    procedure btnRefrescarClick(Sender: TObject);
    procedure btnCerrarClick(Sender: TObject);
  private
    procedure SetupGridHeaders;
  public
    // Hacemos pública la rutina para poder llamarla desde otras unidades
    procedure CargarCorreosEnGrid;
  end;

var
  FormInterfazPapelera: TFormInterfazPapelera;

implementation

uses
  interfazBuscarCorreoEliminado, interfazUsuario; // en implementation para evitar circularidad

{$R *.lfm}

{ TFormInterfazPapelera }

procedure TFormInterfazPapelera.SetupGridHeaders;
begin
  // Tres columnas: Asunto, Remitente, Mensaje (vista general)
  StringGrid1.ColCount := 3;
  StringGrid1.FixedRows := 1;
  StringGrid1.RowCount := 1;
  StringGrid1.Cells[0,0] := 'Asunto';
  StringGrid1.Cells[1,0] := 'Remitente';
  StringGrid1.Cells[2,0] := 'Mensaje';
end;

procedure TFormInterfazPapelera.FormCreate(Sender: TObject);
begin
  SetupGridHeaders;
  CargarCorreosEnGrid;
end;

procedure TFormInterfazPapelera.CargarCorreosEnGrid;
var
  actual: PNodoPapelera;
  row: Integer;
begin
  SetupGridHeaders;

  if (UsuarioLogueado = nil) or (UsuarioLogueado^.listaPapelera = nil) then
    Exit;

  actual := UsuarioLogueado^.listaPapelera^.tope;
  row := 1;
  while actual <> nil do
  begin
    StringGrid1.RowCount := row + 1;
    StringGrid1.Cells[0,row] := actual^.datos^.asunto;
    StringGrid1.Cells[1,row] := actual^.datos^.remitente;
    StringGrid1.Cells[2,row] := actual^.datos^.mensaje;
    Inc(row);
    actual := actual^.siguiente;
  end;
end;

procedure TFormInterfazPapelera.btnCerrarClick(Sender: TObject);
begin
  // Volver a la interfazUsuario
  if Assigned(FormUsuario) then
  begin
    FormUsuario.Show;
    Self.Hide;
  end
  else
    Self.Hide;
end;

procedure TFormInterfazPapelera.btnRefrescarClick(Sender: TObject);
begin
  CargarCorreosEnGrid;
end;

procedure TFormInterfazPapelera.btnEliminarClick(Sender: TObject);
var
  tmp: PNodoPapelera;
begin
  if (UsuarioLogueado = nil) or (UsuarioLogueado^.listaPapelera = nil) then
  begin
    ShowMessage('No hay papelera para el usuario actual.');
    Exit;
  end;

  if UsuarioLogueado^.listaPapelera^.tope = nil then
  begin
    ShowMessage('La papelera está vacía.');
    Exit;
  end;

  // Pop: eliminar el tope de la pila del usuario
  tmp := UsuarioLogueado^.listaPapelera^.tope;
  UsuarioLogueado^.listaPapelera^.tope := tmp^.siguiente;

  // liberar memoria
  Dispose(tmp^.datos);
  Dispose(tmp);

  ShowMessage('Correo eliminado de la papelera.');
  CargarCorreosEnGrid;
end;

procedure TFormInterfazPapelera.btnBuscarClick(Sender: TObject);
begin
  // Abrir la ventana de búsqueda de correo eliminado
  // Si no está creada la creamos (esto espera que exista el .lfm)
  if not Assigned(FormInterfazBuscarCorreoEliminado) then
    Application.CreateForm(TFormInterfazBuscarCorreoEliminado, FormInterfazBuscarCorreoEliminado);

  // limpiar controles de la búsqueda
  FormInterfazBuscarCorreoEliminado.EditAsunto.Text := '';
  FormInterfazBuscarCorreoEliminado.StringGrid1.RowCount := 1;

  // mostrar y ocultar la papelera
  FormInterfazBuscarCorreoEliminado.Show;
  Self.Hide;
end;

end.

