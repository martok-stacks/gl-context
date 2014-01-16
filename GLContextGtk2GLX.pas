unit GLContextGtk2GLX;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

{$DEFINE USE_FBGLX}
{$DEFINE GDK_SET_COLORMAP}

interface

uses
  SysUtils, Controls, LCLType, XUtil, XLib, gdk2x, gtk2, gdk2, Gtk2Int, GLContext, dglOpenGL,
  LMessages, glib2;

type
  EGLXError = class(EGLError);

  { TRenderControl }

  TRenderControl = class(TCustomControl)
  private
    fTarget: TWinControl;
  protected
    procedure WndProc(var Message: TLMessage); override;
  public
    property Target: TWinControl read fTarget write fTarget;
  end;

  { TGLContextGtk2GLX }

  TGLContextGtk2GLX = class(TGLContext)
  private
    FDisplay: PDisplay;
    FWidget: PGtkWidget;
    FContext: GLXContext;
    FRenderControl: TRenderControl;
  protected
    procedure CloseContext; override;
    procedure OpenContext(pf: TglcContextPixelFormatSettings); override;
  public
    procedure Activate; override;
    procedure Deactivate; override;
    function IsActive: boolean; override;
    procedure SwapBuffers; override;
    procedure SetSwapInterval(const aInterval: GLint); override;
    procedure Share(const aContext: TGLContext); override;

    class function ChangeDisplaySettings(const aWidth, aHeight,
      aBitPerPixel, aFreq: Integer; const aFlags: TglcDisplayFlags): Boolean; override;
    class function IsAnyContextActive: boolean; override;
  end;

implementation

type
  TGLIntArray = packed array of GLInt;

{$region messages -fold}
procedure TRenderControl.WndProc(var Message: TLMessage);
var
  handled: Boolean;
begin
  handled := false;
  case Message.msg of
    LM_ACTIVATEITEM,
    LM_CHANGED,
    LM_FOCUS,
    LM_CLICKED,
    LM_RELEASED,
    LM_ENTER,
    LM_LEAVE,
    LM_CHECKRESIZE,
    LM_SETEDITABLE,
    LM_MOVEWORD,
    LM_MOVEPAGE,
    LM_MOVETOROW,
    LM_MOVETOCOLUMN,
    LM_KILLCHAR,
    LM_KILLWORD,
    LM_KILLLINE,
    LM_CLOSEQUERY,
    LM_DRAGSTART,
    LM_MONTHCHANGED,
    LM_YEARCHANGED,
    LM_DAYCHANGED,
    LM_LBUTTONTRIPLECLK,
    LM_LBUTTONQUADCLK,
    LM_MBUTTONTRIPLECLK,
    LM_MBUTTONQUADCLK,
    LM_RBUTTONTRIPLECLK,
    LM_RBUTTONQUADCLK,
    LM_MOUSEENTER,
    LM_MOUSELEAVE,
    LM_XBUTTONTRIPLECLK,
    LM_XBUTTONQUADCLK,

    SC_SIZE,
    SC_MOVE,
    SC_MINIMIZE,
    SC_MAXIMIZE,
    SC_NEXTWINDOW,
    SC_PREVWINDOW,
    SC_CLOSE,
    SC_VSCROLL,
    SC_HSCROLL,
    SC_MOUSEMENU,
    SC_KEYMENU,
    SC_ARRANGE,
    SC_RESTORE,
    SC_TASKLIST,
    SC_SCREENSAVE,
    SC_HOTKEY,
    SC_DEFAULT,
    SC_MONITORPOWER,
    SC_CONTEXTHELP,
    SC_SEPARATOR,

    LM_MOVE,
    LM_SIZE,
    LM_ACTIVATE,
    LM_SETFOCUS,
    LM_KILLFOCUS,
    LM_ENABLE,
    LM_GETTEXTLENGTH,
    LM_SHOWWINDOW,
    LM_CANCELMODE,
    LM_DRAWITEM,
    LM_MEASUREITEM,
    LM_DELETEITEM,
    LM_VKEYTOITEM,
    LM_CHARTOITEM,
    LM_COMPAREITEM,
    LM_WINDOWPOSCHANGING,
    LM_WINDOWPOSCHANGED,
    LM_NOTIFY,
    LM_HELP,
    LM_NOTIFYFORMAT,
    LM_CONTEXTMENU,
    LM_NCCALCSIZE,
    LM_NCHITTEST,
    LM_NCPAINT,
    LM_NCACTIVATE,
    LM_GETDLGCODE,
    LM_NCMOUSEMOVE,
    LM_NCLBUTTONDOWN,
    LM_NCLBUTTONUP,
    LM_NCLBUTTONDBLCLK,
    LM_KEYDOWN,
    LM_KEYUP,
    LM_CHAR,
    LM_SYSKEYDOWN,
    LM_SYSKEYUP,
    LM_SYSCHAR,
    LM_COMMAND,
    LM_SYSCOMMAND,
    LM_TIMER,
    LM_HSCROLL,
    LM_VSCROLL,
    LM_CTLCOLORMSGBOX,
    LM_CTLCOLOREDIT,
    LM_CTLCOLORLISTBOX,
    LM_CTLCOLORBTN,
    LM_CTLCOLORDLG,
    LM_CTLCOLORSCROLLBAR,
    LM_CTLCOLORSTATIC,
    LM_MOUSEMOVE,
    LM_LBUTTONDOWN,
    LM_LBUTTONUP,
    LM_LBUTTONDBLCLK,
    LM_RBUTTONDOWN,
    LM_RBUTTONUP,
    LM_RBUTTONDBLCLK,
    LM_MBUTTONDOWN,
    LM_MBUTTONUP,
    LM_MBUTTONDBLCLK,
    LM_MOUSEWHEEL,
    LM_XBUTTONDOWN,
    LM_XBUTTONUP,
    LM_XBUTTONDBLCLK,
    LM_PARENTNOTIFY,
    LM_CAPTURECHANGED,
    LM_DROPFILES,
    LM_SELCHANGE,
    LM_CUT,
    LM_COPY,
    LM_PASTE,
    LM_CLEAR,

    CM_ACTIVATE,
    CM_DEACTIVATE,
    CM_FOCUSCHANGED,
    CM_PARENTFONTCHANGED,
    CM_PARENTCOLORCHANGED,
    CM_HITTEST,
    CM_VISIBLECHANGED,
    CM_ENABLEDCHANGED,
    CM_COLORCHANGED,
    CM_FONTCHANGED,
    CM_CURSORCHANGED,
    CM_TEXTCHANGED,
    CM_MOUSEENTER,
    CM_MOUSELEAVE,
    CM_MENUCHANGED,
    CM_APPSYSCOMMAND,
    CM_BUTTONPRESSED,
    CM_SHOWINGCHANGED,
    CM_ENTER,
    CM_EXIT,
    CM_DESIGNHITTEST,
    CM_ICONCHANGED,
    CM_WANTSPECIALKEY,
    CM_RELEASE,
    CM_FONTCHANGE,
    CM_TABSTOPCHANGED,
    CM_UIACTIVATE,
    CM_CONTROLLISTCHANGE,
    CM_GETDATALINK,
    CM_CHILDKEY,
    CM_HINTSHOW,
    CM_SYSFONTCHANGED,
    CM_CONTROLCHANGE,
    CM_CHANGED,
    CM_BORDERCHANGED,
    CM_BIDIMODECHANGED,
    CM_PARENTBIDIMODECHANGED,
    CM_ALLCHILDRENFLIPPED,
    CM_ACTIONUPDATE,
    CM_ACTIONEXECUTE,
    CM_HINTSHOWPAUSE,
    CM_DOCKNOTIFICATION,
    CM_MOUSEWHEEL,
    CM_APPSHOWBTNGLYPHCHANGED,
    CM_APPSHOWMENUGLYPHCHANGED,

    CN_BASE,
    CN_CHARTOITEM,
    CN_COMMAND,
    CN_COMPAREITEM,
    CN_CTLCOLORBTN,
    CN_CTLCOLORDLG,
    CN_CTLCOLOREDIT,
    CN_CTLCOLORLISTBOX,
    CN_CTLCOLORMSGBOX,
    CN_CTLCOLORSCROLLBAR,
    CN_CTLCOLORSTATIC,
    CN_DELETEITEM,
    CN_DRAWITEM,
    CN_HSCROLL,
    CN_MEASUREITEM,
    CN_PARENTNOTIFY,
    CN_VKEYTOITEM,
    CN_VSCROLL,
    CN_KEYDOWN,
    CN_KEYUP,
    CN_CHAR,
    CN_SYSKEYUP,
    CN_SYSKEYDOWN,
    CN_SYSCHAR,
    CN_NOTIFY:
    begin
      if Assigned(fTarget) then begin
        Message.Result := fTarget.Perform(Message.msg, Message.wParam, Message.lParam);
        handled := true;
      end;
    end;

    LM_CONFIGUREEVENT,
    LM_EXIT,
    LM_QUIT,
    LM_NULL,
    LM_PAINT,
    LM_ERASEBKGND,
    LM_SETCURSOR,
    LM_SETFONT:
    begin
      inherited WndProc(Message);
      if Assigned(fTarget) then
        Message.Result := fTarget.Perform(Message.msg, Message.wParam, Message.lParam);
      handled := true;
    end;
  end;
  if not handled then
    inherited WndProc(Message);     
end;

{$endregion}

function CreateOpenGLContextAttrList(UseFB: boolean; pf: TglcContextPixelFormatSettings): TGLIntArray;
var
  p: integer;

  procedure Add(i: integer);
  begin
    SetLength(Result, p+1);
    Result[p]:=i;
    inc(p);
  end;

  procedure CreateList;
  begin
    if UseFB then begin Add(GLX_X_RENDERABLE); Add(1); end;
    if pf.DoubleBuffered then begin
      if UseFB then begin
        Add(GLX_DOUBLEBUFFER); Add(1);
      end else
        Add(GLX_DOUBLEBUFFER);
    end;
    if not UseFB and (pf.ColorBits>24) then Add(GLX_RGBA);
    if UseFB then begin
      Add(GLX_DRAWABLE_TYPE);
      Add(GLX_WINDOW_BIT);
    end;
    Add(GLX_RED_SIZE);  Add(8);
    Add(GLX_GREEN_SIZE);  Add(8);
    Add(GLX_BLUE_SIZE);  Add(8);
    if pf.ColorBits>24 then
      Add(GLX_ALPHA_SIZE);  Add(8);
    Add(GLX_DEPTH_SIZE);  Add(pf.DepthBits);
    Add(GLX_STENCIL_SIZE);  Add(pf.StencilBits);
    Add(GLX_AUX_BUFFERS);  Add(pf.AUXBuffers);

    if pf.MultiSampling > 1 then begin
      Add(GLX_SAMPLE_BUFFERS_ARB); Add(1);
      Add(GLX_SAMPLES_ARB); Add(pf.MultiSampling);
    end;

    Add(0); { 0 = X.None (be careful: GLX_NONE is something different) }
  end;

begin
  SetLength(Result, 0);
  p:=0;
  CreateList;
end;

function FBglXChooseVisual(dpy:PDisplay; screen:longint; attrib_list:Plongint):PXVisualInfo;
type
  PGLXFBConfig = ^GLXFBConfig;
var
  FBConfigsCount: integer;
  FBConfigs: PGLXFBConfig;
  FBConfig: GLXFBConfig;
begin
  Result:= nil;
  FBConfigsCount:=0;
  FBConfigs:= glXChooseFBConfig(dpy, screen, attrib_list, @FBConfigsCount);
  if FBConfigsCount = 0 then
    exit;

  { just choose the first FB config from the FBConfigs list.
    More involved selection possible. }
  FBConfig := FBConfigs^;
  Result:=glXGetVisualFromFBConfig(dpy, FBConfig);
end;


{ TGLContextGtk2GLX }

{$region debug -fold}

function gdk_x11_visual_get_xvinfo (scr:longint; xdisplay: PXDisplay; visual: PGdkVisual): PXVisualInfo;
var
  xvinfo_template: TXVisualInfo;
  xvinfo_list: PXVisualInfo;
  nitems_return: integer;
begin
  xvinfo_template.visualid:= XVisualIDFromVisual (gdk_x11_visual_get_xvisual(visual));
  xvinfo_template.screen:=scr;

  xvinfo_list:= XGetVisualInfo (xdisplay,
                                VisualIDMask or VisualScreenMask,
                                @xvinfo_template,
                                @nitems_return);

  { Returned XVisualInfo needs to be unique }
  if (nitems_return<>1) then
    Result:= nil
  else
    Result:= xvinfo_list;
end;

procedure DebugglxGetConfig(vi: PXVisualInfo; dpy: PXDisplay);
var
  Value: GLint;
const
  GLX_BAD_SCREEN      =1;
  GLX_BAD_ATTRIBUTE   =2;
  GLX_NO_EXTENSION    =3;
  GLX_BAD_VISUAL      =4;
  GLX_BAD_CONTEXT     =5;
  GLX_BAD_VALUE       =6;
  GLX_BAD_ENUM        =7;

  function Query(attrib: GLenum): boolean;
  var
    e: integer;
  begin
    Result:= false;
    e:= glXGetConfig(dpy, vi, attrib, @Value);
    case e of
      GLX_NO_EXTENSION: WriteLn('GLX_NO_EXTENSION');
      GLX_BAD_SCREEN: WriteLn('GLX_BAD_SCREEN');
      GLX_BAD_ATTRIBUTE: WriteLn('GLX_BAD_ATTRIBUTE');
      GLX_BAD_VISUAL: WriteLn('GLX_BAD_VISUAL && GLX_USE_GL');
      GLX_BAD_CONTEXT: WriteLn('GLX_BAD_CONTEXT');
      GLX_BAD_VALUE: WriteLn('GLX_BAD_VALUE');
      GLX_BAD_ENUM: WriteLn('GLX_BAD_ENUM');
      0: Result:= true;
    else
      WriteLn('Unknown Error: ', e);
    end;
  end;

begin
  if Query(GLX_USE_GL) then
    WriteLn('Using GL')
  else
    exit;

  Write('Bits per Pixel: '); if Query(GLX_BUFFER_SIZE) then WriteLn(Value);
  Write('Bits per R: '); if Query(GLX_RED_SIZE) then WriteLn(Value);
  Write('Bits per G: '); if Query(GLX_GREEN_SIZE) then WriteLn(Value);
  Write('Bits per B: '); if Query(GLX_BLUE_SIZE) then WriteLn(Value);
  Write('Bits per A: '); if Query(GLX_ALPHA_SIZE) then WriteLn(Value);
  Write('FB Level: '); if Query(GLX_LEVEL) then WriteLn(Value);
  Write('Is RGBA: '); if Query(GLX_RGBA) then WriteLn(BoolToStr(Value=GL_TRUE, true));
  Write('Is DoubleBuffered: '); if Query(GLX_DOUBLEBUFFER) then WriteLn(BoolToStr(Value=GL_TRUE, true));
  Write('Is Stereo: '); if Query(GLX_STEREO) then WriteLn(BoolToStr(Value=GL_TRUE, true));
  Write('Aux Buffers: '); if Query(GLX_AUX_BUFFERS) then WriteLn(Value);
  Write('Depth Bits: '); if Query(GLX_DEPTH_SIZE) then WriteLn(Value);
  Write('Stencil Bits: '); if Query(GLX_STENCIL_SIZE) then WriteLn(Value);
  Write('Accum Bits R: '); if Query(GLX_ACCUM_RED_SIZE) then WriteLn(Value);
  Write('Accum Bits G: '); if Query(GLX_ACCUM_GREEN_SIZE) then WriteLn(Value);
  Write('Accum Bits B: '); if Query(GLX_ACCUM_BLUE_SIZE) then WriteLn(Value);
  Write('Accum Bits A: '); if Query(GLX_ACCUM_ALPHA_SIZE) then WriteLn(Value);
end;

procedure DebugXVisualInfo(const vi: PXVisualInfo);
begin
  with vi^ do begin
    WriteLn('Screen: ', screen, ' VisualID: ', hexStr(visualid,4));
    WriteLn('Class: ', _class);
    Writeln('Bits RGB: ', bits_per_rgb);
    Writeln('Depth: ', depth);
    WriteLn(Format('Masks (RGB): %.8x %.8x, %.8x', [red_mask, green_mask,blue_mask]));
    Writeln('Colormap Size: ', colormap_Size);
  end;
end;

procedure DebugDrawableAndVisuals(const drawable: PGdkDrawable; const dpy: PXDisplay; const vi: PXVisualInfo);
var
  gv: PGdkVisual;
  gvi: PXVisualInfo;
begin
  WriteLn('==== GDKDrawable GDKVisual ====');
  gv:= gdk_drawable_get_visual(drawable);
  with gv^ do begin
    Write('Type: ');
    case TheType of
     GDK_VISUAL_STATIC_GRAY: Write('GDK_VISUAL_STATIC_GRAY');
     GDK_VISUAL_GRAYSCALE: Write('GDK_VISUAL_GRAYSCALE');
     GDK_VISUAL_STATIC_COLOR: Write('GDK_VISUAL_STATIC_COLOR');
     GDK_VISUAL_PSEUDO_COLOR: Write('GDK_VISUAL_PSEUDO_COLOR');
     GDK_VISUAL_TRUE_COLOR: Write('GDK_VISUAL_TRUE_COLOR');
     GDK_VISUAL_DIRECT_COLOR: Write('GDK_VISUAL_DIRECT_COLOR');
    end;
    WriteLn('(',Ord(TheType),')');
    WriteLn('Depth: ', depth);
    Write('ByteOrder: ');
    Case byte_order of
     GDK_LSB_FIRST: Write('GDK_LSB_FIRST');
     GDK_MSB_FIRST: Write('GDK_MSB_FIRST');
    end;
    WriteLn('(',Ord(byte_order),')');
    WriteLn('Colormap entries: ', colormap_size);
    WriteLn('Bits RGB: ', bits_per_rgb);
    WriteLn(Format('Mask R: %.8x << %d @ %d', [red_mask, red_shift, red_prec]));
    WriteLn(Format('Mask G: %.8x << %d @ %d', [green_mask, green_shift, green_prec]));
    WriteLn(Format('Mask B: %.8x << %d @ %d', [blue_mask, blue_shift, blue_prec]));
  end;

  WriteLn('==== GDKVisual XVisual ====');
  gvi:= gdk_x11_visual_get_xvinfo(DefaultScreen(dpy), dpy, gv);
  if Assigned(gvi) then
    DebugXVisualInfo(gvi)
  else begin
    WriteLn('(Can''t get)');
    WriteLn('XVisualID: ', hexstr(XVisualIDFromVisual (gdk_x11_visual_get_xvisual(gv)), 4));
  end;

  WriteLn('==== glXChooseVisual XVisualInfo ====');
  DebugXVisualInfo(vi);

  WriteLn('==== glXGetConfig ====');
  DebugglXGetConfig(vi, dpy);
end;

{$endregion}

procedure TGLContextGtk2GLX.OpenContext(pf: TglcContextPixelFormatSettings);
var
  attrList: TGLIntArray;
  drawable: PGdkDrawable;
  vi: PXVisualInfo;
  cmap: PGdkColormap;
  gdkvis: PGdkVisual;
  gdkvisp: PGdkVisualPrivate;
begin
  {
    Temporary (realized) widget to get to display
  }
  FWidget:= {%H-}PGtkWidget(PtrUInt(Control.Handle));
  WriteLn('Temporary Widget: ', hexStr(FWidget));
  gtk_widget_realize(FWidget);
  drawable:= GTK_WIDGET(FWidget)^.window;

  WriteLn('Temporary Drawable: ', hexstr(drawable));
  FDisplay:= GDK_WINDOW_XDISPLAY(drawable);

  WriteLn('Have XDisplay: ', hexstr(FDisplay));
  {
    Find a suitable visual from PixelFormat
  }
{$IFDEF USE_FBGLX}
  attrList:=CreateOpenGLContextAttrList(true, pf);
  vi:= FBglXChooseVisual(FDisplay, DefaultScreen(FDisplay), @attrList[0]);
  if vi=nil then begin
    pf.MultiSampling:= 1;
    attrList:=CreateOpenGLContextAttrList(true, pf);
    vi:= FBglXChooseVisual(FDisplay, DefaultScreen(FDisplay), @attrList[0]);
  end;
{$ELSE}
  attrList:=CreateOpenGLContextAttrList(false, pf);
  vi:= glXChooseVisual(FDisplay, DefaultScreen(FDisplay), @attrList[0]);
  if vi=nil then begin
    pf.MultiSampling:= 1;
    attrList:=CreateOpenGLContextAttrList(false, pf);
    vi:= glXChooseVisual(FDisplay, DefaultScreen(FDisplay), @attrList[0]);
  end;
{$ENDIF}
  WriteLn('XVID:', hexstr(vi^.visualid, 4));
  if vi=nil then
    raise EGLXError.Create('Failed to find Visual');

  {
    Create Context
  }
  try
    FContext := glXCreateContext(FDisplay, vi, nil, true);

    if (FContext = nil) then
      raise EGLXError.Create('Failed to create Context');

    {
      Most widgets inherit the drawable of their parent. In contrast to Windows, descending from
      TWinControl does not mean it's actually always a window of its own.
      Famous example: TPanel is just a frame painted on a canvas.
      We need to create something where we know it works here to make sure.
    }
    FRenderControl := TRenderControl.Create(Control);
    try
      FRenderControl.Parent := Control;
      {FRenderControl.Align  := alClient;
      FRenderControl.Target := Control;}
    except
      FreeAndNil(FRenderControl);
      raise;
    end;

    {
      Real Widget handle, unrealized!!!
    }
    FWidget:= {%H-}PGtkWidget(PtrUInt(FRenderControl.Handle));
    WriteLn('Real Widget: ', hexStr(FWidget));

{$IFDEF GDK_SET_COLORMAP}
    cmap  :=gdk_colormap_get_system;
    gdkvis:=gdk_colormap_get_visual(cmap);
    WriteLn('System Colormap: ', hexstr(cmap));
    WriteLn('System Colormap VisualID: ', hexstr(XVisualIDFromVisual(gdk_x11_visual_get_xvisual(gdkvis)), 4));
    if XVisualIDFromVisual(gdk_x11_visual_get_xvisual(gdkvis))<>vi^.visual.visualid then begin
      gdkvis:=gdkx_visual_get(vi^.visualid);
      WriteLn('Want VisualID: ', hexstr(vi^.visualid, 4), ' got ', hexstr(XVisualIDFromVisual(gdk_x11_visual_get_xvisual(gdkvis)), 4));
      cmap:=gdk_colormap_new(gdkvis, false);
      WriteLn('New Colormap: ', hexstr(cmap));
      WriteLn('New Colormap VisualID: ', hexstr(XVisualIDFromVisual(gdk_x11_visual_get_xvisual(gdkvis)), 4));
    end;

    gtk_widget_set_colormap(FWidget, cmap);
    gtk_widget_set_visual(FWidget, gdkvis);
    WriteLn('Colormap for Widget set to ', hexstr(cmap));
{$ENDIF}

    WriteLn('Colormap for Widget before realize ', hexstr(XVisualIDFromVisual(gdk_x11_visual_get_xvisual(gdk_colormap_get_visual(gtk_widget_get_colormap(FWidget)))), 4));
    WriteLn('Widget before realize ', hexstr(XVisualIDFromVisual(gdk_x11_visual_get_xvisual(gtk_widget_get_visual(FWidget))), 4));
    gtk_widget_realize(FWidget);
    WriteLn('Colormap for Widget realized as ', hexstr(XVisualIDFromVisual(gdk_x11_visual_get_xvisual(gdk_colormap_get_visual(gtk_widget_get_colormap(FWidget)))), 4));
    WriteLn('Widget realized as ', hexstr(XVisualIDFromVisual(gdk_x11_visual_get_xvisual(gtk_widget_get_visual(FWidget))), 4));
    drawable:= GTK_WIDGET(FWidget)^.window;

    WriteLn('Real Drawable: ', hexstr(drawable));
    FDisplay:= GDK_WINDOW_XDISPLAY(drawable);

    WriteLn('Real XDisplay: ', hexstr(FDisplay));


    //DebugDrawableAndVisuals(drawable, FDisplay, vi);
  finally
    XFree(vi);
  end;
end;

procedure TGLContextGtk2GLX.CloseContext;
begin
  glXDestroyContext(FDisplay, FContext);
  FreeAndNil(FRenderControl);
end;

procedure TGLContextGtk2GLX.Activate;
begin
  // make sure the widget is realized
  gtk_widget_realize(FWidget);
  if not GTK_WIDGET_REALIZED(FWidget) then exit;

  // make current

  glXMakeCurrent(FDisplay, GDK_DRAWABLE_XID(GTK_WIDGET(FWidget)^.window), FContext);
end;

procedure TGLContextGtk2GLX.Deactivate;
begin
  glXMakeCurrent(FDisplay, GDK_DRAWABLE_XID(GTK_WIDGET(FWidget)^.window), nil);
end;

function TGLContextGtk2GLX.IsActive: boolean;
begin
  Result:= (FContext = glXGetCurrentContext()) and
           (GDK_DRAWABLE_XID(GTK_WIDGET(FWidget)^.window) = glXGetCurrentDrawable());
end;

procedure TGLContextGtk2GLX.SwapBuffers;
var
  drawable: PGdkDrawable;
begin
  drawable:= GTK_WIDGET(FWidget)^.window;
  glXSwapBuffers(FDisplay, GDK_DRAWABLE_XID(drawable));
end;

procedure TGLContextGtk2GLX.SetSwapInterval(const aInterval: GLint);
var
  drawable: PGdkDrawable;
begin
  drawable:= GTK_WIDGET(FWidget)^.window;
  if GLX_EXT_swap_control then
    glXSwapIntervalEXT(FDisplay, GDK_WINDOW_XWINDOW(drawable), aInterval);
end;

procedure TGLContextGtk2GLX.Share(const aContext: TGLContext);
begin
  raise Exception.Create('not yet implemented');
end;

class function TGLContextGtk2GLX.ChangeDisplaySettings(const aWidth, aHeight,
  aBitPerPixel, aFreq: Integer; const aFlags: TglcDisplayFlags): Boolean;
begin
  raise Exception.Create('not yet implemented');
end;

class function TGLContextGtk2GLX.IsAnyContextActive: boolean;
begin
  Result:= (glXGetCurrentContext()<>nil) and (glXGetCurrentDrawable()<>0);
end;

procedure glib_log(log_domain:Pgchar; log_level:TGLogLevelFlags; TheMessage:Pgchar; user_data:gpointer);cdecl;
begin
  WriteLn('#glib# [',log_domain,'] ', theMessage);
end;

initialization
  g_log_set_handler('GLib',G_LOG_LEVEL_MASK, @glib_log, nil);
  g_log_set_handler(nil,G_LOG_LEVEL_MASK, @glib_log, nil);
  g_log_set_handler('Gdk',G_LOG_LEVEL_MASK, @glib_log, nil);
  g_log_set_handler('Gtk',G_LOG_LEVEL_MASK, @glib_log, nil);
  g_warning('test message');
end.
