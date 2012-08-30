unit GLContext;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  SysUtils, Controls, dglOpenGL;

type
  TContextPixelFormatSettings = packed record
    DoubleBuffered: boolean;
    Stereo: boolean;
    AA: boolean;
    ColorBits: Integer;
    DepthBits: Integer;
    StencilBits: Integer;
    AccumBits: Integer;
    AuxBuffers: Integer;
    Layer: Integer;
  end;

  EGLError = class(Exception);

  { TGLContext }

  TGLContext = class
  private
    FControl: TWinControl;
  protected
    procedure OpenContext(pf: TContextPixelFormatSettings); virtual; abstract;
    procedure CloseContext; virtual; abstract;
  public
    class function MakePF(DoubleBuffered: boolean = true;
                          Stereo: boolean=false;
                          AA: boolean=false;
                          ColorBits: Integer=32;
                          DepthBits: Integer=24;
                          StencilBits: Integer=0;
                          AccumBits: Integer=0;
                          AuxBuffers: Integer=0;
                          Layer: Integer=0): TContextPixelFormatSettings;

    constructor Create(aControl: TWinControl);
    destructor Destroy; override;

    property Control: TWinControl read FControl;

    procedure BuildContext(pf: TContextPixelFormatSettings);
    procedure Activate; virtual; abstract;
    procedure Deactivate; virtual; abstract;
    procedure SwapBuffers; virtual; abstract;
  end;

implementation

{ TGLContext }

class function TGLContext.MakePF(DoubleBuffered: boolean; Stereo: boolean;
  AA: boolean; ColorBits: Integer; DepthBits: Integer; StencilBits: Integer;
  AccumBits: Integer; AuxBuffers: Integer; Layer: Integer
  ): TContextPixelFormatSettings;
begin
  Result.DoubleBuffered:= DoubleBuffered;
  Result.Stereo:= Stereo;
  Result.AA:= AA;
  Result.ColorBits:= ColorBits;
  Result.DepthBits:= DepthBits;
  Result.StencilBits:= StencilBits;
  Result.AccumBits:= AccumBits;
  Result.AuxBuffers:= AuxBuffers;
  Result.Layer:= Layer;
end;

constructor TGLContext.Create(aControl: TWinControl);
begin
  inherited Create;
  FControl:= aControl;
  InitOpenGL();
end;

destructor TGLContext.Destroy;
begin
  inherited Destroy;
end;

procedure TGLContext.BuildContext(pf: TContextPixelFormatSettings);
begin
  OpenContext(pf);
  Activate;
  ReadExtensions;
end;

end.

