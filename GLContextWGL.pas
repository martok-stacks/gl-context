unit GLContextWGL;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, Forms, Windows, GLContext, dglOpenGL;

type

  { TGLContextWGL }

  TGLContextWGL = class(TGLContext)
  private
    FDC: HDC;
    FRC: HGLRC;
  protected
    procedure CloseContext; override;
    procedure OpenContext(pf: TContextPixelFormatSettings); override;
  public
    procedure Activate; override;
    procedure Deactivate; override;
    procedure SwapBuffers; override;
  end;

implementation

{ TGLContextWGL }

procedure TGLContextWGL.OpenContext(pf: TContextPixelFormatSettings);
var
  opt: TRCOptions;
begin
  fDC:= GetDC(Control.Handle);
  opt:= [];
  if pf.DoubleBuffered then include(opt, opDoubleBuffered);
  if pf.Stereo then include(opt, opStereo);
  FRC:= CreateRenderingContext(FDC, opt, pf.ColorBits, pf.DepthBits, pf.StencilBits, pf.AccumBits, pf.AuxBuffers, pf.Layer);
end;

procedure TGLContextWGL.CloseContext;
begin
  Deactivate;
  DestroyRenderingContext(FRC);
  inherited Destroy;
end;

procedure TGLContextWGL.Activate;
begin
  ActivateRenderingContext(FDC, FRC);
end;

procedure TGLContextWGL.Deactivate;
begin
  if wglGetCurrentContext()=FRC then
    DeactivateRenderingContext;
end;

procedure TGLContextWGL.SwapBuffers;
begin
  Windows.SwapBuffers(FDC);
end;

end.

