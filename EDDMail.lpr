program EDDMail;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, login, root, interfazUsuario, estructuras, interfazCrearUsuario,
  interfazBandejadeEntrada, interfazvercorreo, interfazPapelera,
  interfazBuscarCorreoEliminado, interfazProgramarCorreo,
  interfazCorreosProgramados, interfazAgregarContacto, interfazContactos,
interfazActualizarPerfil, interfazEnviarCorreo, interfazBandejadeBorrador,
interfazBorrador, interfazBandejadeFavoritos, interfazFavorito
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;

  // Crear los formularios
  Application.CreateForm(TFormLogin, FormLogin);
  Application.CreateForm(TFormRoot, FormRoot);
  Application.CreateForm(TFormUsuario, FormUsuario);

  // Mostrar el formulario de login al iniciar
  FormLogin.Show;

  // Crear otros formularios (no los mostramos ahora)
  Application.CreateForm(TFormCrearUsuario, FormCrearUsuario);
  Application.CreateForm(TFormInterfazBandejadeEntrada, FormInterfazBandejadeEntrada);
  Application.CreateForm(TFormVerCorreo, FormVerCorreo);
  Application.CreateForm(TFormInterfazBuscarCorreoEliminado, FormInterfazBuscarCorreoEliminado);
  Application.CreateForm(TFormInterfazProgramarCorreo, FormInterfazProgramarCorreo);
  Application.CreateForm(TFormCorreosProgramados, FormCorreosProgramados);
  Application.CreateForm(TFormInterfazAgregarContacto,
    FormInterfazAgregarContacto);
  Application.CreateForm(TFormContactos, FormContactos);
  Application.CreateForm(TFormInterfazActualizarPerfil,
    FormInterfazActualizarPerfil);
  Application.CreateForm(TFormInterfazEnviarCorreo, FormInterfazEnviarCorreo);
  Application.CreateForm(TFormInterfazBandejadeBorrador,
    FormInterfazBandejadeBorrador);
  Application.CreateForm(TFormInterfazBorrador, FormInterfazBorrador);
  Application.CreateForm(TFormInterfazBandejadeFavoritos,
    FormInterfazBandejadeFavoritos);
  Application.CreateForm(TFormInterfazFavorito, FormInterfazFavorito);
  Application.Run;
end.

