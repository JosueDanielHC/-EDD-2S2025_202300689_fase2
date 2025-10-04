unit interfazEliminarCorreo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormInterfazEliminarCorreo }

  TFormInterfazEliminarCorreo = class(TForm)
    btnEliminar: TButton;
    btnCerrar: TButton;
    EditCorreo: TEdit;
    Label1: TLabel;
    Label2: TLabel;
  private

  public

  end;

var
  FormInterfazEliminarCorreo: TFormInterfazEliminarCorreo;

implementation

{$R *.lfm}

end.

