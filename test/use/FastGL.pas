{
 ***************************************************************
 *
 * Unit Name: FastGL
 * Purpose  : Simplification of common tasks in OpenGL.
 * Author   : Sebastian H.
 * History  :
 *
 ****************************************************************
}

unit FastGL;

{$MODE Delphi}

interface

uses dglOpenGL, LCLIntf, LCLType, LMessages,GLHelper,Graphics
{$IF DEFINED(Win32) or DEFINED(Win64)}
  ,Windows
{$ENDIF};

const
  WGL_FONT_LINES = 0;
  WGL_FONT_POLYGONS = 1;

type
  TgluRenderContext = packed record
    DC: HDC;
    RC: HGLRC;
    Handle: HWND;
  end;

  TgluPixelFormatSettings = packed record
    Options: TRCOptions;
    ColorBits: Integer;
    AlphaBits: Integer;
    DepthBits: Integer;
    StencilBits: Integer;
    AccumBits: Integer;
    AuxBuffers: Integer;
    Layer: Integer;
  end;

var
  glFOV          : single =   45.0;
  glNearClipping : single =    1.0;
  glFarClipping  : single = 1000.0;
  gluLastError: Cardinal;

function  gluGetPixelFormat(const Handle: HWND; const Options: TRCOptions;
  const ColorBits, DepthBits, StencilBits, AccumBits, AuxBuffers, Layer: Integer): Integer; overload;
function  gluGetPixelFormat(const Handle: HWND; const aFormatSettings: TgluPixelFormatSettings): Integer; overload;
function  gluGetAntiAliasingPixelFormats(const aFormatSettings: TgluPixelFormatSettings;
  PFList, SampleList: PglInt; const MaxCount: Integer; var Count: Integer): Boolean; overload;
function  gluGetAntiAliasingPixelFormats(const PFList, SampleList: PglInt; const MaxCount: Integer; var Count: Integer): Boolean; overload;
function  gluCreateRenderContext(const Handle: HWND; const PixelFormat: Integer): TgluRenderContext;
function  gluDestroyRenderContext(const RC: TgluRenderContext): Boolean;
function  gluActivateRenderContext(const RC: TgluRenderContext): Boolean;
function  gluDeactivateRenderContext: Boolean;
procedure gluSetupVSync(const Enable: Boolean);

procedure glOpenRender(DC:HDC;W,H:integer;var RC:HGLRC);      overload;
procedure glOpenRender(Wnd:HWND;W,H:integer;var DC:HDC; var RC:HGLRC);  overload;
procedure glInit;
procedure glResizeWnd(Width, Height : Integer);
function glInitFont(Name:String;DC:HDC; Height:integer;Style:TFontStyles=[]):TGLuint;
function glInit3DFont(Name:String;DC:HDC; Height:integer;Extrusion:TGLfloat;Style:TFontStyles=[]; Mode:integer=WGL_FONT_POLYGONS):TGLuint;

procedure glPreDraw(R:TGLFloat=0.1;G:TGLFloat=0.1;B:TGLFloat=0.3;A:TGLFloat=1);
//procedure glPosition(X,Y,Z,Theta,Phi:single);
procedure glLight(Pos:TVector; ambient, diffuse, Specular:TRGBA; Number:integer; SourceSize:GLFloat);
procedure glTextOut(Font:TGLuint; Text:String);
procedure glRotater(R,X,Y,Z:GLFloat);
procedure glAfterDraw(DC:HDC);

procedure glCloseRender(RC:HGLRC);

function Normalize(X:Double; isDegrees:boolean=false):double;

const
  GLU_ERROR_NONE                =  $0;
  
  GLU_ERROR_INVALID_HANDLE      = $10;
  GLU_ERROR_INVALID_OBJECT_TYPE = $11;
  GLU_ERROR_INVALID_RC          = $12;
  GLU_ERROR_INVALID_DC          = $13;
  GLU_ERROR_INVALID_PIXELFORMAT = $14;

  GLU_ERROR_ACTIVATE_RC         = $20;
  GLU_ERROR_AA_NOT_SUPPORTED    = $21;
  GLU_ERROR_INIT_OPENGL         = $22;
  GLU_ERROR_CREATE_DC           = $23;
  GLU_ERROR_SET_PIXELFORMAT     = $24;
  GLU_ERROR_CREATE_RC           = $25;
  GLU_ERROR_DEACTIVATE_RC       = $26;
  GLU_ERROR_DESTROY_RC          = $27;
  GLU_ERROR_RELEASE_DC          = $28;


  GLU_WARNING_INVALID_HANDLE      = $1010;
  GLU_WARNING_INVALID_OBJECT_TYPE = $1011;
  GLU_WARNING_INVALID_RC          = $1012;
  GLU_WARNING_INVALID_DC          = $1013;
  GLU_WARNING_INVALID_PIXELFORMAT = $1014;

  GLU_WARNING_ACTIVATE_RC         = $1020;
  GLU_WARNING_AA_NOT_SUPPORTED    = $1021;
  GLU_WARNING_INIT_OPENGL         = $1022;
  GLU_WARNING_CREATE_DC           = $1023;
  GLU_WARNING_SET_PIXELFORMAT     = $1024;
  GLU_WARNING_CREATE_RC           = $1025;
  GLU_WARNING_DEACTIVATE_RC       = $1026;
  GLU_WARNING_DESTROY_RC          = $1027;
  GLU_WARNING_RELEASE_DC          = $1028;

  FNT_CHAR_COUNT    =   256;

  IdentityHmgMatrix: TMatrix = ((1, 0, 0, 0),
                                (0, 1, 0, 0),
                                (0, 0, 1, 0),
                                (0, 0, 0, 1));

implementation

uses Math, Forms;

function gluGetPixelFormat(const Handle: HWND; const Options: TRCOptions; const ColorBits, DepthBits,
  StencilBits, AccumBits, AuxBuffers, Layer: Integer): Integer;
var
  Settings: TgluPixelFormatSettings;
begin
  Settings.Options     := Options;
  Settings.ColorBits   := ColorBits;
  Settings.AlphaBits   := 0;
  Settings.DepthBits   := DepthBits;
  Settings.StencilBits := StencilBits;
  Settings.AccumBits   := AccumBits;
  Settings.AuxBuffers  := AuxBuffers;
  Settings.Layer       := Layer;
  result := gluGetPixelFormat(Handle, Settings);
end;

function gluGetPixelFormat(const Handle: HWND; const aFormatSettings: TgluPixelFormatSettings): Integer;
const
  MemoryDCs = [OBJ_MEMDC, OBJ_METADC, OBJ_ENHMETADC];
var
  //DeviceContext
  DC: HDC;
  //Objekttyp des DCs
  AType: DWord;
  //Beschreibung zum passenden Pixelformat
  PFDescriptor: TPixelFormatDescriptor;
begin
  result := 0;
  gluLastError := GLU_ERROR_NONE;
  DC := GetDC(Handle);
  if DC = 0 then begin
    gluLastError := GLU_ERROR_INVALID_HANDLE;
    exit;
  end;
  FillChar(PFDescriptor{%H-}, SizeOf(PFDescriptor), #0);
  with PFDescriptor do begin
    nSize    := SizeOf(PFDescriptor);
    nVersion := 1;
    dwFlags  := PFD_SUPPORT_OPENGL;
    AType    := GetObjectType(DC);
    if AType = 0 then begin
      gluLastError := GLU_ERROR_INVALID_OBJECT_TYPE;
      exit;
    end;
    if opDoubleBuffered in aFormatSettings.Options then dwFlags := dwFlags or PFD_DOUBLEBUFFER;
    if opGDI            in aFormatSettings.Options then dwFlags := dwFlags or PFD_SUPPORT_GDI;
    if opStereo         in aFormatSettings.Options then dwFlags := dwFlags or PFD_STEREO;
    if AType            in MemoryDCs then dwFlags := dwFlags or PFD_DRAW_TO_BITMAP
                                     else dwFlags := dwFlags or PFD_DRAW_TO_WINDOW;

    iPixelType   := PFD_TYPE_RGBA;
    cColorBits   := aFormatSettings.ColorBits;
    cAlphaBits   := aFormatSettings.AlphaBits;
    cDepthBits   := aFormatSettings.DepthBits;
    cStencilBits := aFormatSettings.StencilBits;
    cAccumBits   := aFormatSettings.AccumBits;
    cAuxBuffers  := aFormatSettings.AuxBuffers;

    if aFormatSettings.Layer = 0 then
      iLayerType := PFD_MAIN_PLANE
    else if aFormatSettings.Layer > 0 then
      iLayerType := PFD_OVERLAY_PLANE
    else
      iLayerType := Byte(PFD_UNDERLAY_PLANE);
  end;
  result := ChoosePixelFormat(DC, @PFDescriptor);
end;

function  gluGetAntiAliasingPixelFormats(const aFormatSettings: TgluPixelFormatSettings; PFList, SampleList: PglInt; const MaxCount: Integer; var Count: Integer): Boolean;
var
  //temporäres Fenster zum erzeugen des RC
  Form: TForm;
  //ARB_Erweiterung vorhanden
  //|          EXT_Erweiterung vorhanden
  MultiARBSup, MultiEXTSup: Boolean;
  //Liste der Integer Attribute
  IAttrib: array[0..22] of Integer;
  //Liste der Float Attribute (nur 0, da kein Wert)
  FAttrib: GLFloat;
  //temp. PixelFormat
  //| Schleifenvariable
  //| |  AttributName
  PF, i, QueryAtrib: Integer;
  //Zeiger auf zum hochzählen ni der Liste
  PPosiblePF, PSample: PglInt;
  //temp. RenderKontext
  //| RenderContext, der zurvor aktiviert war
  RC, OldRC: TgluRenderContext;
begin
  result       := false;
  OldRC.DC     := wglGetCurrentDC;
  OldRC.RC     := wglGetCurrentContext;

  Form := TForm.Create(nil);
  PF := gluGetPixelFormat(Form.Handle, aFormatSettings);
  RC := gluCreateRenderContext(Form.Handle, PF);
  if RC.RC = 0 then begin
    Form.Free;
    if (OldRC.DC <> 0) and (OldRC.RC <> 0) then
      gluActivateRenderContext(OldRC);
    gluLastError := GLU_ERROR_INVALID_RC;
    exit;
  end;
  if not gluActivateRenderContext(RC) then begin
    Form.Free;
    gluDestroyRenderContext(RC);
    if (OldRC.DC <> 0) and (OldRC.RC <> 0) then
      gluActivateRenderContext(OldRC);
    gluLastError := GLU_ERROR_ACTIVATE_RC;
    exit;
  end;

  //Pixelformate mit AA auslesen
  MultiARBSup := false;
  MultiEXTSup := false;
  if WGL_ARB_extensions_string and
     WGL_ARB_pixel_format and
     (WGL_ARB_MULTISAMPLE or GL_ARB_MULTISAMPLE) then
    multiARBSup := true;
  if WGL_EXT_extensions_string and
     WGL_EXT_pixel_format and
     (WGL_EXT_MULTISAMPLE or GL_EXT_MULTISAMPLE) then
    multiEXTSup := true;

  if multiARBSup then
    Read_WGL_ARB_pixel_format
  else if multiEXTSup then
    Read_WGL_EXT_pixel_format;

  if not (MultiARBSup or MultiEXTSup) then begin
    Form.Free;
    gluDestroyRenderContext(RC);
    if (OldRC.DC <> 0) and (OldRC.RC <> 0) then
      gluActivateRenderContext(OldRC);
    gluLastError := GLU_ERROR_AA_NOT_SUPPORTED;
  end;

  IAttrib[00] := WGL_DRAW_TO_WINDOW_ARB;
  IAttrib[01] := 1;

  IAttrib[02] := WGL_SUPPORT_OPENGL_ARB;
  IAttrib[03] := 1;

  IAttrib[04] := WGL_DOUBLE_BUFFER_ARB;
  if (opDoubleBuffered in aFormatSettings.Options) then
    IAttrib[05] := 1
  else
    IAttrib[05] := 0;

  IAttrib[06] := WGL_PIXEL_TYPE_ARB;
  IAttrib[07] := WGL_TYPE_RGBA_ARB;

  IAttrib[08] := WGL_COLOR_BITS_ARB;
  IAttrib[09] := aFormatSettings.ColorBits;

  IAttrib[10] := WGL_ALPHA_BITS_ARB;
  IAttrib[11] := aFormatSettings.AlphaBits;

  IAttrib[12] := WGL_DEPTH_BITS_ARB;
  IAttrib[13] := aFormatSettings.DepthBits;

  IAttrib[14] := WGL_STENCIL_BITS_ARB;
  IAttrib[15] := aFormatSettings.StencilBits;

  IAttrib[16] := WGL_ACCUM_BITS_ARB;
  IAttrib[17] := aFormatSettings.AccumBits;

  IAttrib[18] := WGL_AUX_BUFFERS_ARB;
  IAttrib[19] := aFormatSettings.AuxBuffers;

  IAttrib[20] := WGL_SAMPLE_BUFFERS_ARB;
  IAttrib[21] := 1;

  IAttrib[22] := 0;
  FAttrib     := 0;

  if multiARBSup then
    wglChoosePixelFormatARB(RC.DC, @IAttrib, @FAttrib, MaxCount, PFList, @Count)
  else if multiEXTSup then
    wglChoosePixelFormatEXT(RC.DC, @IAttrib, @FAttrib, MaxCount, PFList, @Count);

  if Count > MaxCount then
    Count := MaxCount;

  QueryAtrib := WGL_SAMPLES_ARB;
  PSample    := SampleList;
  PPosiblePF := PFList;
  for i := 0 to Count-1 do begin
    if multiARBSup then
      wglGetPixelFormatAttribivARB(RC.DC, PPosiblePF^, 0, 1, @QueryAtrib, PSample)
    else if multiEXTSup then
      wglGetPixelFormatAttribivEXT(RC.DC, PPosiblePF^, 0, 1, @QueryAtrib, PSample);
    inc(PSample);
    inc(PPosiblePF);
  end;

  if not gluDestroyRenderContext(RC) then begin
    Form.Free;
    exit;
  end;
  Form.Free;

  if (OldRC.DC <> 0) and (OldRC.RC <> 0) then
    if not gluActivateRenderContext(OldRC) then
      exit;

  result := True;
end;

function gluGetAntiAliasingPixelFormats(const PFList, SampleList: PglInt; const MaxCount: Integer; var Count: Integer): Boolean;
var
  PFSettings: TgluPixelFormatSettings;
begin
  PFSettings.Options     := [opDoubleBuffered];
  PFSettings.ColorBits   := 32;
  PFSettings.AlphaBits   := 0;
  PFSettings.DepthBits   := 24;
  PFSettings.StencilBits := 0;
  PFSettings.AccumBits   := 0;
  PFSettings.AuxBuffers  := 0;
  PFSettings.Layer       := 0;
  result := gluGetAntiAliasingPixelFormats(PFSettings, PFList, SampleList, MaxCount, Count);
end;

function gluCreateRenderContext(const Handle: HWND; const PixelFormat: Integer): TgluRenderContext;
begin
  result.DC     := 0;
  result.RC     := 0;
  result.Handle := 0;

  if GL_LibHandle = nil then
    if not InitOpenGL then begin
      gluLastError := GLU_ERROR_INIT_OPENGL;
      exit;
    end;

  if PixelFormat = 0 then begin
    gluLastError := GLU_ERROR_INVALID_PIXELFORMAT;
    exit;
  end;

  result.Handle := Handle;
  result.DC := GetDC(Handle);
  if result.DC = 0 then begin
    gluLastError := GLU_ERROR_CREATE_DC;
    exit;
  end;

  if not SetPixelFormat(result.DC, PixelFormat, nil) then begin
    ReleaseDC(result.Handle, result.DC);
    gluLastError := GLU_ERROR_SET_PIXELFORMAT;
    exit;
  end;

  result.RC := wglCreateContext(result.DC);
  if result.RC = 0 then begin
    ReleaseDC(result.Handle, result.DC);
    gluLastError := GLU_ERROR_CREATE_RC;
    exit;
  end;
end;

function gluDestroyRenderContext(const RC: TgluRenderContext): Boolean;
var
  CurrentRC: HGLRC;
begin
  result := False;
  CurrentRC := wglGetCurrentContext;
  if (CurrentRC = RC.RC) and (CurrentRC <> 0) then
    if not wglMakeCurrent(0, 0) then begin
      gluLastError := GLU_ERROR_DEACTIVATE_RC;
      exit;
    end;

  if RC.RC <> 0 then
    if not wglDeleteContext(RC.RC) then begin
      gluLastError := GLU_ERROR_DESTROY_RC;
      exit;
    end;
    
  if RC.DC <> 0 then
    if ReleaseDC(RC.Handle, RC.DC) = 0 then begin
      gluLastError := GLU_ERROR_RELEASE_DC;
      exit;
    end;
  result := True;
end;

function gluActivateRenderContext(const RC: TgluRenderContext): Boolean;
begin
  result := false;
  if RC.DC = 0 then begin
    gluLastError := GLU_ERROR_INVALID_DC;
    exit;    
  end;

  if RC.RC = 0 then begin
    gluLastError := GLU_ERROR_INVALID_RC;
    exit;
  end;
  
  if not wglMakeCurrent(RC.DC, RC.RC) then begin
    gluLastError := GLU_ERROR_ACTIVATE_RC;
    exit;
  end;
  ReadImplementationProperties;
  ReadExtensions;
  result := True;
end;

function gluDeactivateRenderContext: Boolean;
begin
  result := wglMakeCurrent(0, 0);
end;

procedure gluSetupVSync(const Enable: Boolean);
var i : Integer;
begin
  if WGL_EXT_swap_control then begin
    i := wglGetSwapIntervalExt();
    if enable then begin
      if i <> 1 then wglSwapIntervalExt(1);
    end else begin
      if i <> 0 then wglSwapIntervalExt(0);
    end;
  end;
end;

function Normalize(X:Double; isDegrees:boolean=false):double;
begin
  if isDegrees then begin
    while X<0   do X:= X+360;
    while X>360 do X:= X-360;
  end else begin
    while X<0    do X:= X+2*pi;
    while X>2*pi do X:= X-2*pi;
  end;
  Result:= X;
end;


{------------------------------------------------------------------}
{  Initialisierung von OpenGL                                      }
{------------------------------------------------------------------}
procedure glInit();
begin
  glEnable(GL_TEXTURE_2D);	       // Aktiviert Texture Mapping
  glShadeModel(GL_SMOOTH);	       // Aktiviert weiches Shading
  glClearColor(0.0, 0.0, 0.0, 0.5);    // Bildschirm löschen (schwarz)
  glClearDepth(1.0);		       // Depth Buffer Setup
  glEnable(GL_DEPTH_TEST);	       // Aktiviert Depth Testing
  glDepthFunc(GL_LEQUAL);	       // Bestimmt den Typ des Depth Testing
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
  glEnable(GL_LIGHTING);
end;


{------------------------------------------------------------------}
{  Behandelt Größenveränderung des Fensters                        }
{------------------------------------------------------------------}
procedure glResizeWnd(Width, Height : Integer);
begin
  if (Height = 0) then Height := 1;
  glViewport(0, 0, Width, Height);    // Setzt den Viewport für das OpenGL Fenster
  glMatrixMode(GL_PROJECTION);        // Matrix Mode auf Projection setzen
  glLoadIdentity();                   // Reset View
  gluPerspective(glFOV, Width/Height, glNearClipping, glFarClipping);  // Perspektive den neuen Maßen anpassen.

  glMatrixMode(GL_MODELVIEW);         // Zurück zur Modelview Matrix
  glLoadIdentity();                   // Reset View
end;


{------------------------------------------------------------------}
{  Font vorbereiten                                                }
{------------------------------------------------------------------}
function glInitFont(Name:String;DC:HDC; Height:integer;Style:TFontStyles=[]):TGLuint;
var
  CustomFont:HFont;
  w:integer;
begin
  Result := glGenLists(FNT_CHAR_COUNT);
  w:= IfThen(fsBold in Style,700,0);
  (*CustomFont := GetStockObject (SYSTEM_FONT);*)//Eine Alternative zu CreateFont
  CustomFont := CreateFont(Height,              // Höhe
                           0,                   // Breite 0=Keine Vorgabe
                           0,
                           0,
                           w,                         // Fett?
                           Byte(fsItalic    in Style),// Kursiv?
                           Byte(fsUnderline in Style),// Unterstrichen?
                           Byte(fsStrikeout in Style),// Durchgestrichen?
                           ANSI_CHARSET,
                           OUT_TT_PRECIS,
                           CLIP_DEFAULT_PRECIS,
                           NONANTIALIASED_QUALITY,
                           FF_DONTCARE or DEFAULT_PITCH,
                           PChar(Name));        // Name der Schrift
  SelectObject(DC, CustomFont);
  wglUseFontBitmaps (DC, 0, FNT_CHAR_COUNT-1, Result);
  DeleteObject(CustomFont);
end;

function glInit3DFont(Name:String;DC:HDC; Height:integer;Extrusion:TGLfloat;Style:TFontStyles=[]; Mode:integer=WGL_FONT_POLYGONS):TGLuint;
var
  CustomFont:HFont;
  w:integer;
  agmf:array [0..255] of GLYPHMETRICSFLOAT ;
begin
  Result := glGenLists(FNT_CHAR_COUNT);
  w:= IfThen(fsBold in Style,700,0);
  (*CustomFont := GetStockObject (SYSTEM_FONT);*)//Eine Alternative zu CreateFont
  CustomFont := CreateFont(Height,              // Höhe
                           0,                   // Breite 0=Keine Vorgabe
                           0,
                           0,
                           w,                         // Fett?
                           Byte(fsItalic    in Style),// Kursiv?
                           Byte(fsUnderline in Style),// Unterstrichen?
                           Byte(fsStrikeout in Style),// Durchgestrichen?
                           ANSI_CHARSET,
                           OUT_TT_PRECIS,
                           CLIP_DEFAULT_PRECIS,
                           NONANTIALIASED_QUALITY,
                           FF_DONTCARE or DEFAULT_PITCH,
                           PChar(Name));        // Name der Schrift
  SelectObject(DC, CustomFont);
  wglUseFontOutlines(DC,
                     0,   //Von Zeichen #0
                     FNT_CHAR_COUNT-1, //Bis Zeichen #255
                     Result,
                     0, //So genau wie möglich
                     Extrusion, //0.2 LE tiefe Buchstaben
                     Mode,//Linen keine Polygone
                     @agmf);//Speichere dort die Informationen
  DeleteObject(CustomFont);
end;

{------------------------------------------------------------------}
{  Zeichnen der Szene                                              }
{------------------------------------------------------------------}
procedure glPreDraw(R:TGLFloat;G:TGLFloat;B:TGLFloat;A:TGLFloat);
begin
  glClearColor(R,G,B,A);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
end;

{------------------------------------------------------------------}
{  "Kamera" o.ä. positionieren                                     }
{------------------------------------------------------------------}
{
procedure glPosition(X,Y,Z,Theta,Phi:single);
var RotMatrix: TMatrix;
begin
  Theta:= Normalize(Theta);
  Rotmatrix:= IdentityHmgMatrix;
  RotMatrix:= Turn(RotMatrix,Theta);
  RotMatrix:= Pitch(RotMatrix,Phi);

  glTranslatef(X,Y, Z);
  glRotater(Theta,0,1,0);
  glRotater(Phi,1,0,0);
  //glMultMatrixf(@RotMatrix[0,0]);
end;
}

{------------------------------------------------------------------}
{  "Light" positionieren                                          }
{------------------------------------------------------------------}
procedure glLight(Pos:TVector;
       ambient,
       diffuse,
       Specular:TRGBA;
       Number:integer;
       SourceSize:GLFloat);
var quadObj:PGLUquadric;
begin
  glLightfv(GL_LIGHT0+Number, GL_AMBIENT,  @ambient.arr[0]);
  glLightfv(GL_LIGHT0+Number, GL_DIFFUSE,  @diffuse.arr[0]);
  glLightfv(GL_LIGHT0+Number, GL_SPECULAR, @Specular.arr[0]);
  glLightfv(GL_LIGHT0+Number, GL_POSITION, @Pos[0]);

//  glLightf(GL_LIGHT0+Number, GL_CONSTANT_ATTENUATION, 1.0);
//  glLightf(GL_LIGHT0+Number, GL_LINEAR_ATTENUATION, 0.001);
//  glLightf(GL_LIGHT0+Number, GL_QUADRATIC_ATTENUATION, 0.004);

  glEnable(GL_LIGHT0+Number);
  
  if SourceSize>0 then begin
    quadObj := gluNewQuadric;
    glPushMatrix;
      glTranslatef(Pos[0],Pos[1],Pos[2]);
      gluQuadricDrawStyle(quadObj, GLU_FILL);
      gluQuadricNormals(quadObj, GLU_SMOOTH);
      gluSphere(quadObj, SourceSize, 36, 36);
    glPopMatrix;
    gluDeleteQuadric(quadObj);
  end;
end;


{------------------------------------------------------------------}
{  Text ausgeben                                                   }
{------------------------------------------------------------------}
procedure glTextOut(Font:TGLuint; Text:String);
begin
  glPushMatrix;
    glListBase(Font); //Liste auswählen
    glCallLists(Length(Text), GL_UNSIGNED_BYTE, PChar(Text));//Entsprechende Listen aufrufen
  glPopMatrix;
end;


{------------------------------------------------------------------}
{  Rotate mt Rad                                                   }
{------------------------------------------------------------------}
procedure glRotater(R,X,Y,Z:GLFloat);
begin
  glRotatef(R*180/pi,X,Y,Z);
end;


{------------------------------------------------------------------}
{  Zeichnen der Szene II                                           }
{------------------------------------------------------------------}
procedure glAfterDraw(DC:HDC);
begin
  SwapBuffers(DC);
end;

{------------------------------------------------------------------}
{  Anlegen von Context...                                          }
{------------------------------------------------------------------}
procedure glOpenRender(DC:HDC;W,H:integer;var RC:HGLRC);
begin
  InitOpenGL;
  RC:= CreateRenderingContext( DC,
                               [opDoubleBuffered],
                               32,
                               24,
                               0,0,0,
                               0);
  ActivateRenderingContext(DC, RC);
  ReadExtensions;
  glResizeWnd(W,H);
  glInit;
end;

procedure glOpenRender(Wnd:HWND;W,H:integer;var DC:HDC; var RC:HGLRC);
begin
  DC:= GetDC(Wnd);
  glOpenRender(DC,W,H,RC);
end;



{------------------------------------------------------------------}
{  Freigeben von Context...                                        }
{------------------------------------------------------------------}
procedure glCloseRender(RC:HGLRC);
begin
  DeactivateRenderingContext;
  DestroyRenderingContext(RC);
end;


end.
