unit GLContextGtk2GLX;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  GLContext, dglOpenGL;

type

  { TGLContextGtk2GLX }

  TGLContextGtk2GLX = class(TGLContext)
  private
    fCtx: pointer;
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
 // fCtx:= glXCreateNewContext(??,??,GLX_RGBA_TYPE,nil, true);
end;

procedure TGLContextGtk2GLX.CloseContext;
begin

end;

procedure TGLContextGtk2GLX.Activate;
begin

end;

procedure TGLContextGtk2GLX.Deactivate;
begin

end;

procedure TGLContextGtk2GLX.SwapBuffers;
begin

end;

end.

