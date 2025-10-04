
# 📘 Manual de Usuario  
**Proyecto: EDDMail**  
**Autor:** Josue Daniel Herrera Cotton  
**Carnet:** 202300689  

---

## Introducción  
EDDMail es una aplicación desarrollada como parte de la fase 2 del curso de **Estructuras de Datos**.  
El sistema simula un cliente de correo electrónico que permite:  
- Crear usuarios.  
- Iniciar sesión.  
- Enviar correos.  
- Administrar bandejas (entrada, borradores, papelera, favoritos).  
- Gestionar contactos.  
- Programar correos con una cola de envíos.  

Este manual está dirigido a cualquier persona que desee utilizar la aplicación, sin necesidad de conocimientos técnicos.  

---

## Índice  
1. Requisitos del sistema  
2. Instalación y ejecución  
3. Interfaz principal  
   - Pantalla de inicio de sesión  
   - Creación de cuenta  
   - Bandeja de entrada  
   - Envío de correos  
   - Contactos  
   - Papelera y borradores  
   - Correos programados  
4. Ejemplos de uso  
5. Ilustraciones de interfaz  
6. Recomendaciones  
7. Conclusiones  

---

## 1. Requisitos del sistema  
- **Sistema operativo:** Windows, Linux o macOS (ejecutable incluido o compilación con Lazarus).  
- **Ejecutable:** Archivo `EDDMail`.  
- **Memoria:** 2 GB RAM mínimo.  
- **Almacenamiento:** 200 MB libres.  

---

## 2. Instalación y ejecución  
1. Descargue la carpeta del proyecto.  
2. Ejecute el archivo `EDDMail` directamente.  
3. Para compilar:  
   - Abra **Lazarus**.  
   - Cargue `EDDMail.lpi`.  
   - Compile y ejecute.  

---

## 3. Interfaz principal  

### 🔑 Pantalla de inicio de sesión  
Permite ingresar con email y contraseña.  
- **Iniciar Sesión** → abre la ventana principal.  
- **Crear Cuenta** → abre el registro de usuarios.  

### 🆕 Creación de cuenta  
Formulario donde se ingresan los datos del nuevo usuario. Se guarda en `Usuarios.json`.  

### 📥 Bandeja de entrada  
Muestra correos recibidos con opciones de **ver, responder, eliminar, mover a favoritos**.  

### ✉️ Envío de correos  
Formulario con campos de: destinatario, asunto y cuerpo. Botones:  
- **Enviar**  
- **Guardar en borradores**  

### 📒 Contactos  
Permite agregar, editar o eliminar contactos. Se maneja como **lista circular**.  

### 🗑 Papelera y borradores  
- Correos eliminados → se guardan en **Papelera**.  
- Correos no enviados → se almacenan en **Borradores** (AVL).  

### ⏰ Correos programados  
Sistema de **cola FIFO** para correos que se envían automáticamente.  

---

## 4. Ejemplos de uso  

**Ejemplo 1: Enviar un correo**  
1. Inicie sesión.  
2. Seleccione **Nuevo correo**.  
3. Complete los campos.  
4. Pulse **Enviar**.  

**Ejemplo 2: Recuperar correo eliminado**  
1. Abra la **Papelera**.  
2. Seleccione un correo.  
3. Pulse **Restaurar**.  

---

## 5. Ilustraciones de interfaz  

```plaintext
╔════════════════════════════════╗
║            LOGIN               ║
╠════════════════════════════════╣
║ Email: [___________________]   ║
║ Pass:  [___________________]   ║
║                                ║
║ [ Iniciar sesión ] [ Registrar ]║
╚════════════════════════════════╝
```

```plaintext
╔════════════════════════════════╗
║     BANDEJA DE ENTRADA         ║
╠════════════════════════════════╣
║ [Correo 1 - remitente]         ║
║ [Correo 2 - remitente]         ║
║ [Correo 3 - remitente]         ║
║                                ║
║ [Ver] [Responder] [Eliminar]   ║
╚════════════════════════════════╝
```

```plaintext
╔════════════════════════════════╗
║        NUEVO CORREO            ║
╠════════════════════════════════╣
║ Destinatario: [___________]    ║
║ Asunto:       [___________]    ║
║--------------------------------║
║ Cuerpo:                        ║
║ [___________________________]  ║
║ [___________________________]  ║
║ [___________________________]  ║
║                                ║
║ [Enviar]   [Guardar Borrador]  ║
╚════════════════════════════════╝
```

---

## 6. Recomendaciones  
- Mantenga actualizada la lista de contactos.  
- Verifique la bandeja de borradores regularmente.  
- No cierre la app mientras se envían correos programados.  
- No modifique manualmente los archivos JSON.  

---

## 7. Conclusiones  
EDDMail brinda una simulación realista de un gestor de correos. Permite comprender cómo funcionan listas, colas, pilas y árboles en un entorno práctico.  
Es ideal para reforzar el aprendizaje de **estructuras de datos**.  
