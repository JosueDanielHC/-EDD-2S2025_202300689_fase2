unit interfazFavorito;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, estructuras;

type

  { TFormInterfazFavorito }

  TFormInterfazFavorito = class(TForm)
    btnEliminar: TButton;
    btnCerrar: TButton;
    EditDestinatario: TEdit;
    EditAsunto: TEdit;
    EditFecha: TEdit;
    LabelDestinatario: TLabel;
    LabelAsunto: TLabel;
    LabelFecha: TLabel;
    Label4: TLabel;
    Memo1: TMemo;
    procedure btnCerrarClick(Sender: TObject);
    procedure btnEliminarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FCorreo: PCorreo;
  public
    procedure SetCorreo(c: PCorreo);
  end;

var
  FormInterfazFavorito: TFormInterfazFavorito;

implementation

uses
  interfazBandejadeFavoritos;

{$R *.lfm}

procedure TFormInterfazFavorito.FormCreate(Sender: TObject);
begin
  FCorreo := nil;
end;

procedure TFormInterfazFavorito.SetCorreo(c: PCorreo);
begin
  FCorreo := c;
  if not Assigned(FCorreo) then
  begin
    EditDestinatario.Text := '';
    EditAsunto.Text := '';
    EditFecha.Text := '';
    Memo1.Lines.Clear;
    Exit;
  end;

  if Assigned(FCorreo^.destinatarioUsuario) then
    EditDestinatario.Text := FCorreo^.destinatarioUsuario^.email
  else
    EditDestinatario.Text := '';

  EditAsunto.Text := FCorreo^.asunto;
  if FCorreo^.fecha <> 0 then
    EditFecha.Text := FormatDateTime('dd/mm/yyyy', FCorreo^.fecha)
  else
    EditFecha.Text := '';
  Memo1.Lines.Text := FCorreo^.mensaje;
end;

procedure TFormInterfazFavorito.btnCerrarClick(Sender: TObject);
begin
  // volver a la bandeja de favoritos (si existe)
  if not Assigned(FormInterfazBandejadeFavoritos) then
    Application.CreateForm(TFormInterfazBandejadeFavoritos, FormInterfazBandejadeFavoritos);

  // forzamos refresco usando SetUsuario con UsuarioLogueado global (si aplica)
  if Assigned(FormInterfazBandejadeFavoritos) then
  begin
    if Assigned(UsuarioLogueado) then
      FormInterfazBandejadeFavoritos.SetUsuario(UsuarioLogueado);
    FormInterfazBandejadeFavoritos.Show;
  end;

  Self.Hide;
end;

procedure TFormInterfazFavorito.btnEliminarClick(Sender: TObject);
begin
  if not Assigned(FCorreo) then
  begin
    ShowMessage('No hay correo cargado.');
    Exit;
  end;

  // desmarcar favorito
  FCorreo^.favorito := False;

  // Reconstruir árbol de favoritos del usuario logueado y refrescar la vista
  if not Assigned(FormInterfazBandejadeFavoritos) then
    Application.CreateForm(TFormInterfazBandejadeFavoritos, FormInterfazBandejadeFavoritos);

  if Assigned(UsuarioLogueado) then
  begin
    FormInterfazBandejadeFavoritos.SetUsuario(UsuarioLogueado);
    FormInterfazBandejadeFavoritos.Show;
  end;

  ShowMessage('Correo removido de favoritos.');

  // ocultar formulario actual
  Self.Hide;
end;

end.

