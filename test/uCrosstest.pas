unit uCrosstest;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  dglOpenGL, GLContext;

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
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

{$R *.lfm}
var
  glFOV          : single =   45.0;
  glNearClipping : single =    1.0;
  glFarClipping  : single = 1000.0;

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

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Application.OnIdle:= Idle;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FContext);
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  if not Assigned(FContext) then exit;
  glResizeWnd(ClientWidth, ClientHeight);
end;

procedure TForm1.FormShow(Sender: TObject);
var
  pf: TglcContextPixelFormatSettings;
begin
  FContext:= TGLContext.GetPlatformClass.Create(Self);
  pf:= TGLContext.MakePF();
  pf.MultiSampling:= 16;
  FContext.BuildContext(pf);
  runtime:= 0;
end;

procedure TForm1.Idle(Sender: TObject; var Done: boolean);
begin
  Done:= false;
  runtime:= runtime + 1;
  if not Assigned(FContext) then exit;
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

