{
 ***************************************************************
 *
 * Unit Name: GLHelper
 * Purpose  : Misc. Helper functions like color conversion and
 *            2d/3d switching
 * Author   : Sebastian H.
 * History  :
 *
 ****************************************************************
}

unit GLHelper;

{$MODE Delphi}

interface

uses Graphics,dglOpenGL, LCLIntf, LCLType, LMessages, SysUtils, Types;

type TVertex = array[0..2] of Single;
     TMatrix = array[0..3, 0..3] of Single;
     TVector = array[0..3] of Single;
     TRGBA   = record
               case boolean of
                 true  : ( R,G,B,A:TGLfloat);
                 false : ( Arr:array [0..3] of TGLfloat);
               end;

  TArrMatrix = array [0..15] of TGLFloat;


function TextToFloat(X:String):TGLfloat;
Function ColorToRGBA(color : TColor) : TRGBA;   overload;
function ColorToRGBA(R,G,B:TGLfloat; A:TGLFloat=1.0):TRGBA;   overload;
function StringToColorDef(const S: string; Def:TColor): TColor;
function StringToRGBA(S:String):TRGBA;
function StringToRGBADef(S:String; Def:TRGBA):TRGBA;
function RGBToColor(RGBA:TRGBA):TColor;         overload;
function RGBToColor(R,G,B:TGlFloat):TColor;     overload;
function MixRGBA(A,B:TRGBA):TRGBA;

procedure SetGLColor(Col:TRGBA);
procedure SetGLMaterial(Col:TRGBA);

var W_2d,H_2d:TGLfloat;
procedure Enter2dMode(W,H:TGLfloat);
procedure Exit2dMode;


  function GetMatrixX (matrix: TArrMatrix): TVertex;
  function GetMatrixY (matrix: TArrMatrix): TVertex;
  function GetMatrixZ (matrix: TArrMatrix): TVertex;
  function GetMatrixPos (matrix: TArrMatrix): TVertex;
  procedure SetMatrixZ (var matrix: TArrMatrix; v: TVertex);
  procedure SetMatrixPos (var matrix: TArrMatrix; v: TVertex);
  function Multiply (M1, M2: TArrMatrix): TArrMatrix;


  function GetIdentity(Matrix:TMatrix):TMatrix;overload;
  function GetIdentity(Matrix:TArrMatrix):TArrMatrix;overload;
  function InvertMatrix (src: TArrMatrix; var inverse: TArrMatrix): boolean;
  procedure InitVector (var V1: TVertex; x, y, z: TGLdouble);

  function Vector(X,Y,Z:TGLFloat; W:TGLFloat=1):TVector;
  function MakeVector(X,Y,Z:TGLFloat):TVertex;

  function AddVector (Vec1, Vec2: TVertex): TVertex;
  function SubtractVector (Vec1, Vec2: TVertex): TVertex;
  function Magnitude(V1 : TVertex) : GLdouble;

  function RandomVector(MaxX,MaxY,MaxZ:TGLdouble):TVertex;
  function VectorToString(Vec:TVertex):string;

  function TranslateRect(R:TRect; X,Y:integer):TRect;

  function VectorMake(const v : TVector; w : Single = 0) : TVector; overload;
  function VectorMake(const x, y, z : Single; w : Single = 0) : TVector; overload;

const
  C_X = 0;
  C_Y = 1;
  C_Z = 2;

  c_Black      : TRGBA = (Arr:(0,0,0,1));
  c_White      : TRGBA = (Arr:(1,1,1,1));
  c_Red        : TRGBA = (Arr:(1,0,0,1));
  c_Yellow     : TRGBA = (Arr:(1,1,0,1));
  c_Green      : TRGBA = (Arr:(0,1,0,1));



implementation

uses Math;


// VectorMake
//
function VectorMake(const v : TVector; w : Single = 0) : TVector;
begin
	Result[0]:=v[0];
	Result[1]:=v[1];
	Result[2]:=v[2];
	Result[3]:=w;
end;

// VectorMake
//
function VectorMake(const x, y, z : Single; w : Single = 0) : TVector;
begin
	Result[0]:=x;
	Result[1]:=y;
	Result[2]:=z;
	Result[3]:=w;
end;

function Random(Max:TGLdouble):TGLdouble;
var n:integer;
begin
  n:= -round(Log10(Max));
  if n=0 then n:= 10;
  if n>0 then
    Result:= System.Random(round(max*n))/n
  else
    Result:= System.Random(round(max))
end;

function RandomRange(const AFrom, ATo: TGLdouble): TGLdouble;
begin
  if AFrom > ATo then
    Result := Random(AFrom - ATo) + ATo
  else
    Result := Random(ATo - AFrom) + AFrom;
end;

function MyPower (Base: extended; Exp: integer): extended;
begin
  result := Base * Base;
end;

function TextToFloat(X:String):TGLfloat;
begin
  X:= StringReplace(X,'.',DecimalSeparator,[]);
  X:= StringReplace(X,',',DecimalSeparator,[]);
  Result:= StrToFloat(X);
end;

Function ColorToRGBA(color : TColor) : TRGBA;
begin
  result.R := getrvalue(colorToRgb(color)) / 255;
  result.G := getgvalue(colorToRgb(color)) / 255;
  result.B := getbvalue(colorToRgb(color)) / 255;
  result.A := 1.0;
end;

function ColorToRGBA(R,G,B:TGLfloat; A:TGLFloat=1.0):TRGBA;
begin
  result.R := R;
  result.G := G;
  result.B := B;
  result.A := A;
end;

function StringToColorDef(const S: string; Def:TColor): TColor;
begin
  if not IdentToColor(S, Longint(Result)) then
    Result := TColor(StrToIntDef(S,Def));
end;

function StringToRGBA(S:String):TRGBA;
begin
  Result:= ColorToRGBA(StringToColor(S));
end;

function StringToRGBADef(S:String; Def:TRGBA):TRGBA;
begin
  Result:= ColorToRGBA(StringToColor(S));
end;

function RGBToColor(RGBA:TRGBA):TColor;
begin
  Result:= RGB(trunc(rgba.R*255),trunc(rgba.G*255),trunc(rgba.B*255));
end;

function RGBToColor(R,G,B:TGlFloat):TColor;
begin
  Result:= RGBToColor(ColorToRGBA(R,G,B,1.0));
end;


function MixRGBA(A,B:TRGBA):TRGBA;
begin
  Result.R:= A.R * B.R;
  Result.G:= A.G * B.G;
  Result.B:= A.B * B.B;
end;

procedure SetGLColor(Col:TRGBA);
begin
  with Col do
  glColor4f(R,G,B,A);
end;

procedure SetGLMaterial(Col:TRGBA);
var
  mat_specular   : TRGBA;
  mat_shininess  : Array[0..0] of TGlFloat;
  mat_ambient    : TRGBA;
  mat_diffuse    : TRGBA;
begin
  mat_specular:= MixRGBA(ColorToRGBA(1.0, 1.0, 1.0, 1.0),col);
  mat_ambient:= MixRGBA(ColorToRGBA(0.4, 0.4, 0.4, 1.0), col);
  mat_diffuse:= MixRGBA(ColorToRGBA(0.8, 0.8, 0.8, 1.0), col);
  mat_shininess[0]:= 50.0;


  glMaterialfv(GL_FRONT, GL_SPECULAR,  @mat_specular.arr[0]);
  glMaterialfv(GL_FRONT, GL_SHININESS, @mat_shininess[0]);
  glMaterialfv(GL_FRONT, GL_AMBIENT,   @mat_ambient.arr[0]);
  glMaterialfv(GL_FRONT, GL_DIFFUSE,   @mat_diffuse.arr[0]);
end;

procedure Enter2dMode(W,H:TGLfloat);
begin
  W_2d:= w;
  H_2d:= h;

  glPushAttrib(GL_DEPTH_BUFFER_BIT or GL_ENABLE_BIT);
    glDisable(GL_DEPTH_TEST);
    glDisable(GL_LIGHTING);
    glDisable(GL_TEXTURE_2D);

  //Wir brauchen eine Orthogonale ansicht für Text
    // Aktuelle Projektionsmatrix auf den Stack
    glMatrixMode(GL_PROJECTION);
      glPushMatrix();
        glLoadIdentity();
        //Ortho Matrix
        glOrtho(0.0, W_2d, H_2d, 0, 0, 128);
        // Aktuelle Modelviewmatrix auf den Stack
        glMatrixMode(GL_MODELVIEW);
        glPushMatrix();
          glLoadIdentity();
end;

procedure Exit2dMode;
begin
        //Projektionsmatrix wiederherstellen
        glMatrixMode(GL_PROJECTION);
        glPopMatrix();
      //Modelview Matrix wiederherstellen
      glMatrixMode(GL_MODELVIEW);
      glPopMatrix();
  glPopAttrib()
end;


function GetMatrixX (matrix: TArrMatrix): TVertex;
// holt den X-vektor aus matrix und gibt ihn zurück
begin
  result[C_X] := matrix[00];
  result[C_Y] := matrix[01];
  result[C_Z] := matrix[02];
end;

function GetMatrixY (matrix: TArrMatrix): TVertex;
// holt den Y-vektor aus matrix und gibt ihn zurück
begin
  result[C_X] := matrix[04];
  result[C_Y] := matrix[05];
  result[C_Z] := matrix[06];
end;

function GetMatrixZ (matrix: TArrMatrix): TVertex;
// holt den Z-vektor aus matrix und gibt ihn zurück
begin
  result[C_X] := matrix[08];
  result[C_Y] := matrix[09];
  result[C_Z] := matrix[10];
end;

function GetMatrixPos (matrix: TArrMatrix): TVertex;
// holt den Position-vektor aus matrix und gibt ihn zurück
begin
  result[C_X] := matrix[12];
  result[C_Y] := matrix[13];
  result[C_Z] := matrix[14];
end;

procedure SetMatrixZ (var matrix: TArrMatrix; v: TVertex);
// setzt den durch v bestimmten Z-vektor in matrix
begin
  matrix[08] := v[C_X];
  matrix[09] := v[C_Y];
  matrix[10] := v[C_Z];
end;

procedure SetMatrixPos (var matrix: TArrMatrix; v: TVertex);
// setzt den durch v bestimmten Position-vektor in matrix
begin
  matrix[12] :=  v[C_X];
  matrix[13] :=  v[C_Y];
  matrix[14] :=  v[C_Z];
end;

function Multiply(M1, M2: TArrMatrix): TArrMatrix;
// multiplies two 4x4 matrices
var
  ret: TArrMatrix;
begin
  glPushMatrix();
  glLoadMatrixf(@M1);
  glMultMatrixf(@M2);
  glGetFloatv(GL_MODELVIEW_MATRIX,@ret);
  glPopMatrix();
  result := ret;
end;

function GetIdentity(Matrix:TMatrix):TMatrix;
begin
  result[0,0]:=1.0;result[0,1]:=0.0;result[0,2]:=0.0;result[0,3]:=0.0;
  result[1,0]:=0.0;result[1,1]:=1.0;result[1,2]:=0.0;result[1,3]:=0.0;
  result[2,0]:=0.0;result[2,1]:=0.0;result[2,2]:=1.0;result[2,3]:=0.0;
  result[3,0]:=0.0;result[3,1]:=0.0;result[3,2]:=0.0;result[3,3]:=1.0;
end;

function GetIdentity(Matrix:TArrMatrix):TArrMatrix;
begin
  result[0]:=1.0;result[1]:=0.0;result[2]:=0.0;result[3]:=0.0;
  result[4]:=0.0;result[5]:=1.0;result[6]:=0.0;result[7]:=0.0;
  result[8]:=0.0;result[9]:=0.0;result[10]:=1.0;result[11]:=0.0;
  result[12]:=0.0;result[13]:=0.0;result[14]:=0.0;result[15]:=1.0;
end;

function InvertMatrix (src: TArrMatrix; var inverse: TArrMatrix): boolean;
var
  t: TGLdouble;
  i, j, k, swap: integer;
  tmp: TMatrix;
begin
  result := false;
  inverse := GetIdentity(inverse);

  for i := 0 to 3 do
  begin
    for j := 0 to 3 do
    begin
      tmp[i][j] := src[i*4+j];
    end;
  end;

  for i := 0 to 3 do
  begin
    // look for largest element in column.
    swap := i;
    for j := i+1 to 3 do
    begin
      if abs(tmp[j][i]) > abs(tmp[i][i]) then
      begin
        swap := j;
      end;
    end;

    if not (swap = i) then
    begin
      // swap rows.
      for k := 0 to 3 do
      begin
        t := tmp[i][k];
        tmp[i][k] := tmp[swap][k];
        tmp[swap][k] := t;

        t := inverse[i*4+k];
        inverse[i*4+k] := inverse[swap*4+k];
        inverse[swap*4+k] := t;
      end;
    end;

    if tmp[i][i] = 0 then
    begin
    { no non-zero pivot.  the matrix is singular, which
      shouldn't happen.  This means the user gave us a bad
      matrix. }
      exit;
    end;

    t := tmp[i][i];
    for k := 0 to 3 do
    begin
      tmp[i][k] := tmp[i][k]/t;
      inverse[i*4+k] := inverse[i*4+k]/t;
    end;

    for j := 0 to 3 do
    begin
      if not (j = i) then
      begin
        t := tmp[j][i];
        for k := 0 to 3 do
        begin
          tmp[j][k] := tmp[j][k]-tmp[i][k]*t;
          inverse[j*4+k] := inverse[j*4+k]-inverse[i*4+k]*t;
        end;
      end;
    end;
  end;
  result := true;
end;

procedure InitVector (var V1: TVertex; x, y, z: TGLdouble);
begin
  V1[C_X] := x;
  V1[C_Y] := y;
  V1[C_Z] := z;
end;

function Vector(X,Y,Z,W:TGLFloat):TVector;
begin
  Result[0]:= X;
  Result[1]:= Y;
  Result[2]:= Z;
  Result[3]:= W;
end;

function MakeVector(X,Y,Z:TGLFloat):TVertex;
begin
  result[0]:=x;
  result[1]:=y;
  result[2]:=z;
end;

function AddVector (Vec1, Vec2: TVertex): TVertex;
// addiert Vec2 auf vec1 und gibt das ergebnis in vec3 zurück
var
  Vec3: TVertex;
begin
  Vec3[C_X] := Vec1[C_X] + Vec2[C_X];
  Vec3[C_Y] := Vec1[C_Y] + Vec2[C_Y];
  Vec3[C_Z] := Vec1[C_Z] + Vec2[C_Z];
  result := Vec3;
end;

function SubtractVector (Vec1, Vec2: TVertex): TVertex;
// subtrahiert Vec2 von vec1 und gibt das ergebnis in vec3 zurück
var
  Vec3: TVertex;
begin
  Vec3[C_X] := Vec1[C_X] - Vec2[C_X];
  Vec3[C_Y] := Vec1[C_Y] - Vec2[C_Y];
  Vec3[C_Z] := Vec1[C_Z] - Vec2[C_Z];
  result := Vec3;
end;

function Magnitude(V1 : TVertex) : GLdouble;
var
  Ergebnis: GLdouble;
begin
// gibt die länge des vektors zurück
  Ergebnis := MyPower(V1[C_X],2)+MyPower(V1[C_Y],2)+MyPower(V1[C_Z],2);
  try
    result := sqrt(Ergebnis);
  except
    result := 0;
  end;
end;

function RandomVector(MaxX,MaxY,MaxZ:TGLdouble):TVertex;
begin
  Result[C_X] := RandomRange(-MaxX,MaxX);
  Result[C_Y] := RandomRange(-MaxY,MaxY);
  Result[C_Z] := RandomRange(-MaxZ,MaxZ);
end;

function VectorToString(Vec:TVertex):string;
begin
  Result:= format('(%f,%f,%f)',[Vec[C_X],Vec[C_Y],Vec[C_Z]]);
end;

function TranslateRect(R:TRect; X,Y:integer):TRect;
begin
  Result.Left:= R.Left+X;
  Result.Right:= R.Right+X;
  Result.Top:= R.Top+Y;
  Result.Bottom:= R.Bottom+Y;
end;

initialization
  Randomize;
end.
