unit GLContextGLX;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  GLContext, dglOpenGL;

type

  { TGLContextGLX }

  TGLContextGLX = class(TGLContext)
  private
    fCtx: pointer;
  protected
    procedure CloseContext; override;
    function OpenContext(pf: TContextPixelFormatSettings): boolean; override;
  public
    procedure Activate; override;
    procedure Deactivate; override;
    procedure SwapBuffers; override;
  end;

implementation


{ TGLContextGLX }

function TGLContextGLX.OpenContext(pf: TContextPixelFormatSettings): boolean;
begin
 // fCtx:= glXCreateNewContext(??,??,GLX_RGBA_TYPE,nil, true);
end;

procedure TGLContextGLX.CloseContext;
begin

end;

procedure TGLContextGLX.Activate;
begin

end;

procedure TGLContextGLX.Deactivate;
begin

end;

procedure TGLContextGLX.SwapBuffers;
begin

end;

end.

