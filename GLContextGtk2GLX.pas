unit GLContextGtk2GLX;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  X, XUtil, XLib, gdk2x, glib2, gtk2, Gtk2Int, GLContext, dglOpenGL;

type

  { TGLContextGtk2GLX }

  TGLContextGtk2GLX = class(TGLContext)
  private
    FDisplay: PDisplay;
    FContext: GLXContext;
    procedure CreateContextInternal(pf: TContextPixelFormatSettings);
  protected
    procedure CloseContext; override;
    procedure OpenContext(pf: TContextPixelFormatSettings); override;
  public
    procedure Activate; override;
    procedure Deactivate; override;
    procedure SwapBuffers; override;
  end;

implementation


{ TGLContextGtk2GLX }

procedure TGLContextGtk2GLX.OpenContext(pf: TContextPixelFormatSettings);
begin
  if (pf.MultiSampling > 1) and
     GLX_ARB_multisample(GetDefaultXDisplay, DefaultScreen(GetDefaultXDisplay))  then
  try
    Result := CreateContextInternal(pf);
  except
    { retry without MultiSampling }
    pf.MultiSampling:= 1;
    Result := CreateContextInternal(pf);
  end else begin
    { no multi-sampling requested (or GLX_ARB_multisample not available),
      just pass to LOpenGLCreateContextCore }
    pf.MultiSampling:= 1;
    Result := CreateContextInternal(pf);
  end;
end;

function CreateOpenGLContextAttrList(pf: TContextPixelFormatSettings): PInteger;
var
  p: integer;

  procedure Add(i: integer);
  begin
    if Result<>nil then
      Result[p]:=i;
    inc(p);
  end;

  procedure CreateList;
  begin
    Add(GLX_X_RENDERABLE); Add(1);
    if pf.DoubleBuffered then
    begin
      Add(GLX_DOUBLEBUFFER); Add(1);
    end;
    Add(GLX_RED_SIZE);  Add(1);
    Add(GLX_GREEN_SIZE);  Add(1);
    Add(GLX_BLUE_SIZE);  Add(1);
    Add(GLX_ALPHA_SIZE);  Add(1);//TODO: Add(AlphaBits);
    Add(GLX_DEPTH_SIZE);  Add(pf.DepthBits);
    Add(GLX_STENCIL_SIZE);  Add(pf.StencilBits);
    Add(GLX_AUX_BUFFERS);  Add(pf.AUXBuffers);

    if MultiSampling > 1 then
    begin
      Add(GLX_SAMPLE_BUFFERS_ARB); Add(1);
      Add(GLX_SAMPLES_ARB); Add(pf.MultiSampling);
    end;

    Add(0); { 0 = X.None (be careful: GLX_NONE is something different) }
  end;

begin
  Result:=nil;
  p:=0;
  CreateList;
  GetMem(Result,SizeOf(integer)*p);
  p:=0;
  CreateList;
end;

procedure TGLContextGtk2GLX.CreateContextInternal(pf: TContextPixelFormatSettings);
var
  Widget: PGtkWidget;
  SharedArea: PGtkGLArea;
  attrList: PInteger;
  drawable: PGdkDrawable;
begin
  attrList:=CreateOpenGLContextAttrList(pf);
  try
    Widget:= PGtkWidget(PtrUInt(Control.Handle));
    drawable:= GTK_WIDGET(Widget)^.window;

    FDisplay:= GDK_WINDOW_XDISPLAY(drawable);
    vi:= glXChooseVisual(FDisplay, DefaultScreen(dpy), @attrList[0]);
    if vi=nil then
      raise Exception.Create('gdk_gl_context_share_new no visual found');

    FContext := glXCreateContext(FDisplay, vi, nil, direct);

    XFree(vi);
    if (glxcontext = nil) then
      exit;
  finally
    FreeMem(attrList);
  end;
end;

procedure TGLContextGtk2GLX.CloseContext;
begin
  glXDestroyContext(FDisplay, FContext);
end;

procedure TGLContextGtk2GLX.Activate;
var
  glarea: PGtkGLArea;
begin
  // make sure the widget is realized
  gtk_widget_realize(FWidget);
  if not GTK_WIDGET_REALIZED(FWidget) then exit;

  // make current

  glXMakeCurrent(FDisplay, GDK_WINDOW_XWINDOW(PGtkWidget(Control.Handle)^.window), FContext);
end;

procedure TGLContextGtk2GLX.Deactivate;
begin
  glXMakeCurrent(FDisplay, nil, nil);
end;

procedure TGLContextGtk2GLX.SwapBuffers;
var
  drawable: PGdkDrawable;
begin
  drawable:= GTK_WIDGET(PtrUInt(Control.Handle))^.window;
  glXSwapBuffers(GDK_WINDOW_XDISPLAY(drawable),
                 GDK_WINDOW_XWINDOW(drawable));
end;

end.

