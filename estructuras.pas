unit estructuras;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DateUtils;

type
  //--- Declaraciones adelantadas de todos los punteros ---
  PUsuario = ^TUsuario;
  PUsuarioNodo = ^TUsuarioNodo;
  PListaUsuarios = ^TListaUsuarios;
  PContactoAVL = ^TContactoAVL;
  PBorradorAVL = ^TBorradorAVL;
  PFavoritoB = ^TFavoritoB;
  PCorreo = ^TCorreo;
  PNodoCorreo = ^TNodoCorreo;
  PListaCorreos = ^TListaCorreos;
  PPapelera = ^TPapelera;
  PNodoPapelera = ^TNodoPapelera;
  PListaPapelera = ^TListaPapelera;
  PCorreoProgramado = ^TCorreoProgramado;
  PNodoCorreoProgramado = ^TNodoCorreoProgramado;
  PListaCorreoProgramado = ^TListaCorreoProgramado;
  PNodoBandejaEntrada = ^TNodoBandejaEntrada;
  PListaBandejaEntrada = ^TListaBandejaEntrada;
  PNodoCorreosEnviados = ^TNodoCorreosEnviados;
  PListaCorreosEnviados = ^TListaCorreosEnviados;

  //==================== CORREO (lista doblemente enlazada) ============================
  TCorreo = record
    id: Integer;
    remitente: PContactoAVL;        // compatibilidad con código antiguo (ahora es BST)
    remitenteUsuario: PUsuario;     // referencia directa al usuario remitente
    destinatarioUsuario: PUsuario;  // referencia directa al usuario destinatario
    estado: string;                 // "NL" o "L"
    programado: string;             // "SI" o "No"
    asunto: string;
    fecha: TDateTime;               // fecha de la computadora
    hora: TTime;                    // inicializa vacía
    mensaje: string;
    favorito: Boolean;              // por defecto es false
  end;

  TNodoCorreo = record
    datos: PCorreo;
    siguiente: PNodoCorreo;
    anterior: PNodoCorreo;
  end;

  TListaCorreos = record
    primero: PNodoCorreo;
    ultimo: PNodoCorreo;
  end;

  //==================== BANDEJA ENTRADA (Lista doblemente enlazada) ======================
  TNodoBandejaEntrada = record
    correoRef: PCorreo;          // puntero al correo en lista global
    siguiente: PNodoBandejaEntrada;
    anterior: PNodoBandejaEntrada;
  end;

  TListaBandejaEntrada = record
    primero: PNodoBandejaEntrada;
    ultimo: PNodoBandejaEntrada;
  end;

  //==================== CORREOS ENVIADOS (Lista doblemente enlazada) ======================
  TNodoCorreosEnviados = record
    correoRef: PCorreo;          // puntero al correo en lista global
    siguiente: PNodoCorreosEnviados;
    anterior: PNodoCorreosEnviados;
  end;

  TListaCorreosEnviados = record
    primero: PNodoCorreosEnviados;
    ultimo: PNodoCorreosEnviados;
  end;

  //==================== CONTACTO (Árbol BST por email) ======================
  TContactoAVL = record
    usuarioRef: PUsuario;        // apunta al usuario correspondiente
    email: string;               // clave del BST (email del contacto)
    izquierdo: PContactoAVL;     // hijo izquierdo
    derecho: PContactoAVL;       // hijo derecho
    altura: Integer;             // altura del nodo (para balanceo)
  end;

  //==================== BORRADOR (Árbol AVL indexado por ID) ======================
  TBorradorAVL = record
    id: Integer;                 // clave del AVL
    remitente: string;           // email del remitente
    destinatarioEmail: string;   // email del destinatario
    asunto: string;
    mensaje: string;
    fecha: TDateTime;
    izquierdo: PBorradorAVL;     // hijo izquierdo
    derecho: PBorradorAVL;       // hijo derecho
    altura: Integer;             // altura del nodo
  end;

  //==================== FAVORITO (Árbol B de orden 5 indexado por ID) ======================
  // Implementación: orden = 5 => max hijos = 5, max claves por nodo = 4
  TFavoritoB = record
    keys: array[1..4] of Integer;     // claves (ids)
    corrs: array[1..4] of PCorreo;    // punteros a correos correspondientes
    hijos: array[0..4] of PFavoritoB; // 5 hijos posibles
    numKeys: Integer;                 // número de claves actuales (0..4)
    esHoja: Boolean;
  end;

  //======================== USUARIO (lista simple) ======================
  TUsuario = record
    id: Integer;
    nombre: string;
    usuario: string;
    password: string;
    email: string;
    telefono: Integer;
    listaContactos: PContactoAVL;           // Árbol BST de contactos
    listaPapelera: PListaPapelera;          // Pila de papelera
    listaBorradores: PBorradorAVL;          // Árbol AVL de borradores
    listaBandejaEntrada: PListaBandejaEntrada; // Lista doble de bandeja entrada
    listaCorreosEnviados: PListaCorreosEnviados; // Lista doble de correos enviados
    listaFavoritos: PFavoritoB;             // Árbol B de favoritos (raíz)
  end;

  TUsuarioNodo = record
    datos: PUsuario;
    siguiente: PUsuarioNodo; // lista simple
  end;

  TListaUsuarios = record
    primero: PUsuarioNodo;
  end;

  //==================== PAPELERA (pila) ============================
  TPapelera = record
    id: Integer;                    // inicia en 100, autoincremental
    remitente: string;              // copia del email remitente
    estado: string;                 // "Eliminado"
    programado: string;
    asunto: string;
    fecha: TDateTime;
    hora: TTime;
    mensaje: string;
  end;

  TNodoPapelera = record
    datos: PPapelera;
    siguiente: PNodoPapelera;
  end;

  TListaPapelera = record
    tope: PNodoPapelera;
    contadorID: Integer;            // inicia en 100, sube con cada apilado
  end;

  //==================== CORREO PROGRAMADO (cola) ============================
  TCorreoProgramado = record
    id: Integer;                    // inicia en 200
    remitenteID: Integer;           // ID del usuario remitente
    remitente: string;              // correo del remitente (email)
    destinatarioEmail: string;      // email del destinatario
    estado: string;                 // "Programado"
    programado: string;             // "SI"
    asunto: string;
    fecha: TDateTime;
    hora: TTime;
    mensaje: string;
    favorito: Boolean;              // por defecto es false
  end;

  TNodoCorreoProgramado = record
    datos: PCorreoProgramado;
    siguiente: PNodoCorreoProgramado;
  end;

  TListaCorreoProgramado = record
    primero: PNodoCorreoProgramado;
    ultimo: PNodoCorreoProgramado;
    contadorID: Integer;            // inicia en 200
  end;

var
  ListaUsuariosGlobal: TListaUsuarios;
  ListaCorreosGlobal: TListaCorreos;
  ListaPapeleraGlobal: TListaPapelera; // fallback/global
  ListaCorreosProgramadosGlobal: TListaCorreoProgramado;

  // ** variable global que apunta al usuario actualmente logueado **
  UsuarioLogueado: PUsuario = nil;

  // ** contador autoincremental para ids de correos globales **
  ContadorIDCorreosGlobal: Integer = 0;

  // ** contador autoincremental para ids de borradores (AVL) **
  ContadorIDBorradoresGlobal: Integer = 0;

{ Public API }
procedure RecalcularContadorIDCorreosGlobal;

implementation

{ RecalcularContadorIDCorreosGlobal:
  Recorre ListaCorreosGlobal y establece ContadorIDCorreosGlobal al máximo id encontrado.
  Útil después de cargar datos desde archivo para mantener continuidad del id. }
procedure RecalcularContadorIDCorreosGlobal;
var
  nodo: PNodoCorreo;
  maxId: Integer;
begin
  maxId := 0;
  nodo := ListaCorreosGlobal.primero;
  while nodo <> nil do
  begin
    if Assigned(nodo^.datos) then
      if nodo^.datos^.id > maxId then
        maxId := nodo^.datos^.id;
    nodo := nodo^.siguiente;
  end;
  ContadorIDCorreosGlobal := maxId;
end;

initialization
  ListaUsuariosGlobal.primero := nil;
  ListaCorreosGlobal.primero := nil;
  ListaCorreosGlobal.ultimo := nil;
  ListaPapeleraGlobal.tope := nil;
  ListaPapeleraGlobal.contadorID := 100;
  ListaCorreosProgramadosGlobal.primero := nil;
  ListaCorreosProgramadosGlobal.ultimo := nil;
  ListaCorreosProgramadosGlobal.contadorID := 200;
  UsuarioLogueado := nil;
  ContadorIDCorreosGlobal := 0;
  ContadorIDBorradoresGlobal := 0;

  // recalcular (si hay correos cargados antes de iniciar el programa)
  RecalcularContadorIDCorreosGlobal;

end.

