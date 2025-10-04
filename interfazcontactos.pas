unit interfazContactos;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, estructuras;

type
  { TFormContactos }
  TFormContactos = class(TForm)
    btnCerrar: TButton;
    Label1: TLabel;
    ListBox1: TListBox;
    procedure btnCerrarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
  private
    procedure RecorrerPreOrder(raiz: PContactoAVL);
  public
    procedure CargarContactos(usuario: PUsuario);
  end;

var
  FormContactos: TFormContactos;

implementation

uses
  interfazUsuario;

{$R *.lfm}

procedure TFormContactos.FormCreate(Sender: TObject);
begin
  ListBox1.Clear;
end;

procedure TFormContactos.ListBox1Click(Sender: TObject);
begin
  // por ahora sin acción
end;

procedure TFormContactos.RecorrerPreOrder(raiz: PContactoAVL);
var
  sPhone: string;
begin
  if raiz = nil then Exit;

  // Primero el nodo actual
  if Assigned(raiz^.usuarioRef) then
  begin
    if raiz^.usuarioRef^.telefono <= 0 then
      sPhone := ''
    else
      sPhone := IntToStr(raiz^.usuarioRef^.telefono);

    ListBox1.Items.Add(Format('%d | %s | %s | %s',
      [raiz^.usuarioRef^.id, Trim(raiz^.usuarioRef^.nombre),
       Trim(raiz^.usuarioRef^.email), sPhone]));
  end;

  // luego subárbol izquierdo
  RecorrerPreOrder(raiz^.izquierdo);
  // luego subárbol derecho
  RecorrerPreOrder(raiz^.derecho);
end;

procedure TFormContactos.CargarContactos(usuario: PUsuario);
begin
  ListBox1.Clear;
  if not Assigned(usuario) then
  begin
    ShowMessage('No hay usuario logueado o usuario inválido.');
    Exit;
  end;

  if usuario^.listaContactos = nil then Exit;

  RecorrerPreOrder(usuario^.listaContactos);
end;

procedure TFormContactos.btnCerrarClick(Sender: TObject);
begin
  Self.Hide;
  if Assigned(FormUsuario) then
    FormUsuario.Show;
end;

end.

