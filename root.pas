unit root;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormRoot }

  TFormRoot = class(TForm)
    btnCargaMasiva: TButton;
    btnComunidades: TButton;
    btnReportes: TButton;
    btnVerMensajedeComunidad: TButton;
    btnCerrar: TButton;
    ROOT: TLabel;
    procedure btnCerrarClick(Sender: TObject);
    procedure btnCargaMasivaClick(Sender: TObject);
  private

  public

  end;

var
  FormRoot: TFormRoot;

implementation

uses
  login,
  estructuras,     // <- contiene las estructuras y listas globales
  fpjson, jsonparser, // para parsear JSON
  DateUtils;

{$R *.lfm}

{ --------------------
  Tipos temporales usados para validar sin tocar las listas globales
  -------------------- }
type
  TTempUser = record
    id: Integer;
    nombre: string;
    usuario: string;
    password: string;
    email: string;
    telefonoStr: string;
    telefonoInt: Integer;
  end;

  TTempCorreo = record
    id: Integer;
    remitente: string;
    destinatario: string;
    estado: string;
    asunto: string;
    mensaje: string;
  end;

{ --------------------
  Helpers y validaciones
  -------------------- }

function EmailValido(const s: string): Boolean;
var
  suf: string;
begin
  Result := False;
  if Trim(s) = '' then Exit;
  if Pos('@', s) <= 1 then Exit;
  if Length(s) < 9 then Exit; // mínimo: x@edd.com => 9 caracteres
  suf := LowerCase(Copy(s, Length(s) - 7, 8)); // toma los últimos 8 caracteres
  if suf <> '@edd.com' then Exit;
  Result := True;
end;

function ExtraerDigitosAInteger(const s: string): Integer;
var
  i: Integer;
  tmp: string;
begin
  tmp := '';
  for i := 1 to Length(s) do
    if s[i] in ['0'..'9'] then
      tmp := tmp + s[i];
  Result := StrToIntDef(tmp, 0);
end;

function BuscarUsuarioPorEmail(const email: string): PUsuario;
var
  nodo: PUsuarioNodo;
begin
  Result := nil;
  if Trim(email) = '' then Exit;
  nodo := ListaUsuariosGlobal.primero;
  while nodo <> nil do
  begin
    if (nodo^.datos <> nil) and (AnsiCompareText(nodo^.datos^.email, email) = 0) then
    begin
      Result := nodo^.datos;
      Exit;
    end;
    nodo := nodo^.siguiente;
  end;
end;

function BuscarUsuarioPorID(const id: Integer): PUsuario;
var
  nodo: PUsuarioNodo;
begin
  Result := nil;
  nodo := ListaUsuariosGlobal.primero;
  while nodo <> nil do
  begin
    if (nodo^.datos <> nil) and (nodo^.datos^.id = id) then
    begin
      Result := nodo^.datos;
      Exit;
    end;
    nodo := nodo^.siguiente;
  end;
end;

function UsuarioExistePorEmail(const email: string): Boolean;
begin
  Result := BuscarUsuarioPorEmail(email) <> nil;
end;

function UsuarioExistePorID(const id: Integer): Boolean;
begin
  Result := BuscarUsuarioPorID(id) <> nil;
end;

function CorreoExistePorID(const id: Integer): Boolean;
var
  nodo: PNodoCorreo;
begin
  Result := False;
  nodo := ListaCorreosGlobal.primero;
  while nodo <> nil do
  begin
    if (nodo^.datos <> nil) and (nodo^.datos^.id = id) then
    begin
      Result := True;
      Exit;
    end;
    nodo := nodo^.siguiente;
  end;
end;

{ --------------------
  Inserciones individuales (devuelven mensaje explicativo)
  -------------------- }

function InsertarUsuarioDesdeJSON(
  const id: Integer;
  const nombre, usuario, password, email: string;
  const telefonoInt: Integer;
  out mensaje: string
): Boolean;
var
  nuevoUser: PUsuario;
  nuevoNodo: PUsuarioNodo;
begin
  mensaje := '';
  Result := False;

  if id <= 0 then
  begin
    mensaje := 'ID inválido';
    Exit;
  end;
  if Trim(nombre) = '' then
  begin
    mensaje := 'Nombre vacío';
    Exit;
  end;
  if Trim(usuario) = '' then
  begin
    mensaje := 'Usuario vacío';
    Exit;
  end;
  if Trim(password) = '' then
  begin
    mensaje := 'Password vacío';
    Exit;
  end;
  if not EmailValido(email) then
  begin
    mensaje := 'Email inválido';
    Exit;
  end;
  if telefonoInt <= 0 then
  begin
    mensaje := 'Teléfono inválido';
    Exit;
  end;
  if UsuarioExistePorID(id) then
  begin
    mensaje := Format('ID duplicado en lista global: %d', [id]);
    Exit;
  end;
  if UsuarioExistePorEmail(email) then
  begin
    mensaje := Format('Email duplicado en lista global: %s', [email]);
    Exit;
  end;

  New(nuevoUser);
  FillChar(nuevoUser^, SizeOf(TUsuario), 0);
  nuevoUser^.id := id;
  nuevoUser^.nombre := nombre;
  nuevoUser^.usuario := usuario;
  nuevoUser^.password := password;
  nuevoUser^.email := email;
  nuevoUser^.telefono := telefonoInt;
  nuevoUser^.listaContactos := nil;
  nuevoUser^.listaPapelera := nil;
  nuevoUser^.listaBorradores := nil;
  nuevoUser^.listaBandejaEntrada := nil;
  nuevoUser^.listaCorreosEnviados := nil;
  nuevoUser^.listaFavoritos := nil;

  New(nuevoNodo);
  FillChar(nuevoNodo^, SizeOf(TUsuarioNodo), 0);
  nuevoNodo^.datos := nuevoUser;
  nuevoNodo^.siguiente := ListaUsuariosGlobal.primero;
  ListaUsuariosGlobal.primero := nuevoNodo;

  mensaje := 'OK';
  Result := True;
end;

function InsertarCorreoDesdeJSON(
  const id: Integer;
  const remitenteEmail, destinatarioEmail, estado, asunto, mensajeTexto: string;
  out mensaje: string
): Boolean;
var
  nuevoCorreo: PCorreo;
  nuevoNodo: PNodoCorreo;
  remitUser, destUser: PUsuario;
  sEstado: string;
begin
  mensaje := '';
  Result := False;

  if id <= 0 then
  begin
    mensaje := 'ID de correo inválido';
    Exit;
  end;
  if CorreoExistePorID(id) then
  begin
    mensaje := Format('ID de correo duplicado en lista global: %d', [id]);
    Exit;
  end;
  if not EmailValido(remitenteEmail) then
  begin
    mensaje := 'Email remitente inválido';
    Exit;
  end;
  if not EmailValido(destinatarioEmail) then
  begin
    mensaje := 'Email destinatario inválido';
    Exit;
  end;

  // Buscar usuarios en lista global
  remitUser := BuscarUsuarioPorEmail(remitenteEmail);
  destUser := BuscarUsuarioPorEmail(destinatarioEmail);
  if (remitUser = nil) or (destUser = nil) then
  begin
    mensaje := 'Remitente o destinatario no encontrados en la lista de usuarios';
    Exit;
  end;

  New(nuevoCorreo);
  FillChar(nuevoCorreo^, SizeOf(TCorreo), 0);
  nuevoCorreo^.id := id;
  nuevoCorreo^.remitente := nil;
  nuevoCorreo^.remitenteUsuario := remitUser;
  nuevoCorreo^.destinatarioUsuario := destUser;
  sEstado := Trim(estado);
  if sEstado = '' then
    nuevoCorreo^.estado := 'NL'
  else
    nuevoCorreo^.estado := sEstado;

  // IMPORTANTE:
  // Cuando se carga desde JSON (no hay campos fecha/hora en tu JSON),
  // entendemos que NO son correos programados -> no asignamos hora.
  nuevoCorreo^.programado := 'No';
  nuevoCorreo^.asunto := asunto;

  // Guardamos sólo la fecha (sin hora) para correos no programados.
  nuevoCorreo^.fecha := DateOf(Now);
  nuevoCorreo^.hora := 0; // indica "sin hora"

  nuevoCorreo^.mensaje := mensajeTexto;
  nuevoCorreo^.favorito := False;

  New(nuevoNodo);
  FillChar(nuevoNodo^, SizeOf(TNodoCorreo), 0);
  nuevoNodo^.datos := nuevoCorreo;
  nuevoNodo^.siguiente := nil;
  nuevoNodo^.anterior := nil;

  if ListaCorreosGlobal.primero = nil then
  begin
    ListaCorreosGlobal.primero := nuevoNodo;
    ListaCorreosGlobal.ultimo := nuevoNodo;
  end
  else
  begin
    nuevoNodo^.anterior := ListaCorreosGlobal.ultimo;
    ListaCorreosGlobal.ultimo^.siguiente := nuevoNodo;
    ListaCorreosGlobal.ultimo := nuevoNodo;
  end;

  // *** ACTUALIZAR EL CONTADOR GLOBAL DE IDS AQUÍ ***
  // Si insertamos un correo con id mayor que el ContadorIDCorreosGlobal actual,
  // actualizamos el contador para mantener la continuidad.
  if id > ContadorIDCorreosGlobal then
    ContadorIDCorreosGlobal := id;

  mensaje := 'OK';
  Result := True;
end;

{ --------------------
  Validaciones previas (sin tocar listas globales). Recolectan todos los errores.
  -------------------- }

procedure ValidarUsuariosDesdeJSON_Array(arr: TJSONArray; out tempUsers: array of TTempUser; out errores: TStringList);
var
  i: Integer;
  item: TJSONObject;
  idVal: Integer;
  tu: TTempUser;
  mapIds: TStringList; // para ids dentro del archivo (evitar duplicados dentro del json)
  mapEmails: TStringList;
begin
  mapIds := TStringList.Create;
  mapEmails := TStringList.Create;
  errores.Clear;
  try
    mapIds.Sorted := True;
    mapIds.Duplicates := dupError;
    mapEmails.Sorted := True;
    mapEmails.Duplicates := dupError;

    for i := 0 to arr.Count - 1 do
    begin
      if arr.Items[i].JSONType <> jtObject then
      begin
        errores.Add(Format('Usuario índice %d: elemento no es objeto', [i]));
        Continue;
      end;
      item := TJSONObject(arr.Items[i]);

      idVal := item.Get('id', 0);
      tu.id := idVal;
      tu.nombre := item.Get('nombre', '');
      tu.usuario := item.Get('usuario', '');
      tu.password := item.Get('password', '');
      tu.email := item.Get('email', '');
      tu.telefonoStr := item.Get('telefono', '');
      tu.telefonoInt := ExtraerDigitosAInteger(tu.telefonoStr);

      if tu.id <= 0 then
        errores.Add(Format('Usuario índice %d: id inválido (%d)', [i, tu.id]));
      if Trim(tu.nombre) = '' then
        errores.Add(Format('Usuario índice %d: nombre vacío', [i]));
      if Trim(tu.usuario) = '' then
        errores.Add(Format('Usuario índice %d: usuario vacío', [i]));
      if Trim(tu.password) = '' then
        errores.Add(Format('Usuario índice %d: password vacío', [i]));
      if not EmailValido(tu.email) then
        errores.Add(Format('Usuario índice %d: email inválido (%s)', [i, tu.email]));
      if tu.telefonoInt <= 0 then
        errores.Add(Format('Usuario índice %d: teléfono inválido (%s)', [i, tu.telefonoStr]));

      try
        mapIds.Add(IntToStr(tu.id));
      except
        on E: Exception do
          errores.Add(Format('Usuario índice %d: id duplicado dentro del archivo (%d)', [i, tu.id]));
      end;
      try
        mapEmails.Add(tu.email);
      except
        on E: Exception do
          errores.Add(Format('Usuario índice %d: email duplicado dentro del archivo (%s)', [i, tu.email]));
      end;

      if UsuarioExistePorID(tu.id) then
        errores.Add(Format('Usuario índice %d: id ya existe en sistema (%d)', [i, tu.id]));
      if UsuarioExistePorEmail(tu.email) then
        errores.Add(Format('Usuario índice %d: email ya existe en sistema (%s)', [i, tu.email]));

      tempUsers[i] := tu;
    end;

  finally
    mapIds.Free;
    mapEmails.Free;
  end;
end;

procedure ValidarCorreosDesdeJSON_Array(arr: TJSONArray; const tempUsers: array of TTempUser; out tempCorreos: array of TTempCorreo; out errores: TStringList);
var
  i: Integer;
  item: TJSONObject;
  tc: TTempCorreo;
  mapIds: TStringList;
  function EmailExisteEnTempOGlobal(const email: string): Boolean;
  var
    j: Integer;
  begin
    Result := False;
    if not EmailValido(email) then Exit;
    for j := 0 to High(tempUsers) do
      if AnsiCompareText(tempUsers[j].email, email) = 0 then
      begin
        Result := True;
        Exit;
      end;
    if UsuarioExistePorEmail(email) then
    begin
      Result := True;
      Exit;
    end;
  end;
begin
  mapIds := TStringList.Create;
  errores.Clear;
  try
    mapIds.Sorted := True;
    mapIds.Duplicates := dupError;

    for i := 0 to arr.Count - 1 do
    begin
      if arr.Items[i].JSONType <> jtObject then
      begin
        errores.Add(Format('Correo índice %d: elemento no es objeto', [i]));
        Continue;
      end;
      item := TJSONObject(arr.Items[i]);

      tc.id := item.Get('id', 0);
      tc.remitente := item.Get('remitente', '');
      tc.destinatario := item.Get('destinatario', '');
      tc.estado := item.Get('estado', '');
      tc.asunto := item.Get('asunto', '');
      tc.mensaje := item.Get('mensaje', '');

      if tc.id <= 0 then
        errores.Add(Format('Correo índice %d: id inválido (%d)', [i, tc.id]));
      if not EmailValido(tc.remitente) then
        errores.Add(Format('Correo índice %d: remitente inválido (%s)', [i, tc.remitente]));
      if not EmailValido(tc.destinatario) then
        errores.Add(Format('Correo índice %d: destinatario inválido (%s)', [i, tc.destinatario]));
      if (Trim(tc.remitente) <> '') and (not EmailExisteEnTempOGlobal(tc.remitente)) then
        errores.Add(Format('Correo índice %d: remitente no encontrado entre usuarios (%s)', [i, tc.remitente]));
      if (Trim(tc.destinatario) <> '') and (not EmailExisteEnTempOGlobal(tc.destinatario)) then
        errores.Add(Format('Correo índice %d: destinatario no encontrado entre usuarios (%s)', [i, tc.destinatario]));

      try
        mapIds.Add(IntToStr(tc.id));
      except
        on E: Exception do
          errores.Add(Format('Correo índice %d: id duplicado dentro del archivo (%d)', [i, tc.id]));
      end;

      if CorreoExistePorID(tc.id) then
        errores.Add(Format('Correo índice %d: id de correo ya existe en sistema (%d)', [i, tc.id]));

      tempCorreos[i] := tc;
    end;

  finally
    mapIds.Free;
  end;
end;

{ --------------------
  Evento del botón Carga Masiva (versión atómica con mensajes claros)
  -------------------- }
procedure TFormRoot.btnCargaMasivaClick(Sender: TObject);
var
  usuariosPath, correosPath: string;
  sl: TStringList;
  js: TJSONData;
  arrUsuarios, arrCorreos: TJSONArray;
  i: Integer;
  tempUsersArr: array of TTempUser;
  tempCorreosArr: array of TTempCorreo;
  errores: TStringList;
  msg: string;
  okInsert: Boolean;
  outMsg: string;
begin
  usuariosPath := '/home/edd/Desktop/EDD2S2025_202300689_Fase2_1/data/Usuarios.json';
  correosPath  := '/home/edd/Desktop/EDD2S2025_202300689_Fase2_1/data/Correos.json';

  errores := TStringList.Create;
  sl := TStringList.Create;
  try
    if not FileExists(usuariosPath) then
    begin
      ShowMessage('Error: archivo de Usuarios no encontrado en: ' + usuariosPath);
      Exit;
    end;
    if not FileExists(correosPath) then
    begin
      ShowMessage('Error: archivo de Correos no encontrado en: ' + correosPath);
      Exit;
    end;

    // PARSEO USUARIOS
    sl.LoadFromFile(usuariosPath);
    js := GetJSON(sl.Text);
    try
      if (js = nil) or (js.JSONType <> jtObject) then
      begin
        ShowMessage('Error: JSON de Usuarios inválido (no es un objeto)');
        Exit;
      end;
      if TJSONObject(js).GetPath('usuarios') = nil then
      begin
        ShowMessage('Error: JSON de Usuarios no contiene la clave "usuarios"');
        Exit;
      end;
      arrUsuarios := TJSONArray(TJSONObject(js).FindPath('usuarios'));
      SetLength(tempUsersArr, arrUsuarios.Count);
      errores.Clear;
      ValidarUsuariosDesdeJSON_Array(arrUsuarios, tempUsersArr, errores);
      if errores.Count > 0 then
      begin
        msg := 'Errores en archivo Usuarios.json:' + sLineBreak + errores.Text;
        ShowMessage(msg);
        Exit;
      end;
    finally
      js.Free;
    end;

    // PARSEO CORREOS
    sl.Clear;
    sl.LoadFromFile(correosPath);
    js := GetJSON(sl.Text);
    try
      if (js = nil) or (js.JSONType <> jtObject) then
      begin
        ShowMessage('Error: JSON de Correos inválido (no es un objeto)');
        Exit;
      end;
      if TJSONObject(js).GetPath('correos') = nil then
      begin
        ShowMessage('Error: JSON de Correos no contiene la clave "correos"');
        Exit;
      end;
      arrCorreos := TJSONArray(TJSONObject(js).FindPath('correos'));
      SetLength(tempCorreosArr, arrCorreos.Count);
      errores.Clear;
      ValidarCorreosDesdeJSON_Array(arrCorreos, tempUsersArr, tempCorreosArr, errores);
      if errores.Count > 0 then
      begin
        msg := 'Errores en archivo Correos.json:' + sLineBreak + errores.Text;
        ShowMessage(msg);
        Exit;
      end;
    finally
      js.Free;
    end;

    // Insertar usuarios
    for i := 0 to High(tempUsersArr) do
    begin
      okInsert := InsertarUsuarioDesdeJSON(
        tempUsersArr[i].id,
        tempUsersArr[i].nombre,
        tempUsersArr[i].usuario,
        tempUsersArr[i].password,
        tempUsersArr[i].email,
        tempUsersArr[i].telefonoInt,
        outMsg
      );
      if not okInsert then
      begin
        ShowMessage('Error inesperado al insertar usuario: ' + outMsg + sLineBreak + 'Carga masiva abortada.');
        Exit;
      end;
    end;

    // Insertar correos
    for i := 0 to High(tempCorreosArr) do
    begin
      okInsert := InsertarCorreoDesdeJSON(
        tempCorreosArr[i].id,
        tempCorreosArr[i].remitente,
        tempCorreosArr[i].destinatario,
        tempCorreosArr[i].estado,
        tempCorreosArr[i].asunto,
        tempCorreosArr[i].mensaje,
        outMsg
      );
      if not okInsert then
      begin
        ShowMessage('Error inesperado al insertar correo: ' + outMsg + sLineBreak + 'Carga masiva abortada.');
        Exit;
      end;
    end;

    // Después de insertar correos desde JSON, recalculamos el contador global de IDs
    // (esto asegura que ContadorIDCorreosGlobal siempre sea el máximo id actual).
    RecalcularContadorIDCorreosGlobal;

    ShowMessage(Format('Carga masiva exitosa. Usuarios insertados: %d. Correos insertados: %d',
      [Length(tempUsersArr), Length(tempCorreosArr)]));

  finally
    errores.Free;
    sl.Free;
  end;
end;

procedure TFormRoot.btnCerrarClick(Sender: TObject);
begin
  // Volver al login
  FormLogin.Show;
  Self.Hide;
end;

end.

