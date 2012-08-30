unit uCrosstest;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  dglOpenGL, GLContext, FastGL;

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { private declarations }
    FContext: TGLContext;
    runtime: double;
  public
    { public declarations }
    procedure Idle(Sender: TObject; var Done: boolean);
    procedure Render;
  end;

var
  Form1: TForm1;

implementation

uses
{$IFDEF WINDOWS}
  GLContextWGL
{$ENDIF}
{$IFDEF LINUX}
  GLContextGtk2GLX
{$ENDIF}
;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  pf: TContextPixelFormatSettings;
begin
  Application.OnIdle:= Idle;
  {$IFDEF WINDOWS}
  FContext:= TGLContextWGL.Create(Self);
  {$ENDIF}
  {$IFDEF LINUX}
  FContext:= TGLContextGtk2GLX.Create(Self);
  {$ENDIF}
  pf:= TGLContext.MakePF();
  FContext.BuildContext(pf);
  runtime:= 0;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FContext);
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  glResizeWnd(ClientWidth, ClientHeight);
end;

procedure TForm1.Idle(Sender: TObject; var Done: boolean);
begin
  Done:= false;
  runtime:= runtime + 1;
  Render;
end;

procedure TForm1.Render;
const
  FOV = 45;
  CLIP_NEAR = 0.1;
  CLIP_FAR  = 1000;
begin
  glMatrixMode(GL_PROJECTION);
  glViewport(0, 0, ClientWidth, ClientHeight);
  glLoadIdentity();
  gluPerspective(FOV, ClientWidth / ClientHeight, CLIP_NEAR, CLIP_FAR);

  glMatrixMode(GL_MODELVIEW);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity();
  glEnable(GL_DEPTH_TEST);

  glTranslatef(0,0,-5);
  glRotatef(runtime*0.01,0,1,0);
  glBegin(GL_TRIANGLES);
    glColor3f(1, 0, 0); glVertex3f(-1,-1, 0);
    glColor3f(0, 0, 1); glVertex3f( 1,-1, 0);
    glColor3f(0, 1, 0); glVertex3f( 0, 1, 0);
  glEnd;

  FContext.SwapBuffers;
end;

end.

