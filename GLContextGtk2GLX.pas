unit GLContextGtk2GLX;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  XUtil, XLib, gdk2x, gtk2, gdk2, Gtk2Int, GLContext, dglOpenGL;

type
  EGLXError = class(EGLError);

  { TGLContextGtk2GLX }

  TGLContextGtk2GLX = class(TGLContext)
  private
    FDisplay: PDisplay;
    FWidget: PGtkWidget;
    FContext: GLXContext;
  protected
    procedure CloseContext; override;
    procedure OpenContext(pf: TContextPixelFormatSettings); override;
  public
    procedure Activate; override;
    procedure Deactivate; override;
    procedure SwapBuffers; override;
  end;

implementation

type
  TGLIntArray = packed array of GLInt;

function CreateOpenGLContextAttrList(UseFB: boolean; pf: TContextPixelFormatSettings): TGLIntArray;
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
    if pf.DoubleBuffered then
    begin
      if UseFB then begin
        Add(GLX_DOUBLEBUFFER); Add(1);
      end else
        Add(GLX_DOUBLEBUFFER);
    end;
    if not UseFB then Add(GLX_RGBA);
    Add(GLX_RED_SIZE);  Add(1);
    Add(GLX_GREEN_SIZE);  Add(1);
    Add(GLX_BLUE_SIZE);  Add(1);
    Add(GLX_ALPHA_SIZE);  Add(0);//TODO: Add(AlphaBits);
    Add(GLX_DEPTH_SIZE);  Add(pf.DepthBits);
    Add(GLX_STENCIL_SIZE);  Add(pf.StencilBits);
    Add(GLX_AUX_BUFFERS);  Add(pf.AUXBuffers);

    if pf.MultiSampling > 1 then
    begin
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


{ TGLContextGtk2GLX }

procedure TGLContextGtk2GLX.OpenContext(pf: TContextPixelFormatSettings);
var
  attrList: TGLIntArray;
  drawable: PGdkDrawable;
  vi: PXVisualInfo;
begin
  FWidget:= {%H-}PGtkWidget(PtrUInt(Control.Handle));
  gtk_widget_realize(FWidget);
  drawable:= GTK_WIDGET(FWidget)^.window;

  FDisplay:= GDK_WINDOW_XDISPLAY(drawable);

  attrList:=CreateOpenGLContextAttrList(false, pf);
  vi:= glXChooseVisual(FDisplay, DefaultScreen(FDisplay), @attrList[0]);
  if vi=nil then begin
    pf.MultiSampling:= 1;
    attrList:=CreateOpenGLContextAttrList(false, pf);
    vi:= glXChooseVisual(FDisplay, DefaultScreen(FDisplay), @attrList[0]);
  end;
  if vi=nil then
    raise EGLXError.Create('Failed to find Visual');

  try
    FContext := glXCreateContext(FDisplay, vi, nil, false);
  finally
    XFree(vi);
  end;

  if (FContext = nil) then
    raise EGLXError.Create('Failed to create Context');
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
    raise EGLXError.Create('Could not find FB config');

  { just choose the first FB config from the FBConfigs list.
    More involved selection possible. }
  FBConfig := FBConfigs^;
  Result:=glXGetVisualFromFBConfig(dpy, FBConfig);
end;

procedure TGLContextGtk2GLX.CloseContext;
begin
  glXDestroyContext(FDisplay, FContext);
end;

procedure TGLContextGtk2GLX.Activate;
begin
  // make sure the widget is realized
  gtk_widget_realize(FWidget);
  if not GTK_WIDGET_REALIZED(FWidget) then exit;

  // make current

  glXMakeCurrent(FDisplay, GDK_WINDOW_XWINDOW(GTK_WIDGET(FWidget)^.window), FContext);
end;

procedure TGLContextGtk2GLX.Deactivate;
begin
  glXMakeCurrent(FDisplay, GDK_WINDOW_XWINDOW(GTK_WIDGET(FWidget)^.window), nil);
end;

procedure TGLContextGtk2GLX.SwapBuffers;
var
  drawable: PGdkDrawable;
begin
  drawable:= GTK_WIDGET(FWidget)^.window;
  glXSwapBuffers(FDisplay, GDK_WINDOW_XWINDOW(drawable));
end;

end.

