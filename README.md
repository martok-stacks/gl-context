gl-context
==========

Larazus/FPC/Delphi OpenGL Context abstraction for cross-platform development,
published as an independent part of the bitSpaceEngine developed for [Massive Universe Online](http://muo-game.de).

Purpose
-------
Provides a helper class to [dglOpenGL](http://wiki.delphigl.com/index.php/dglOpenGL.pas) for unified cross-platform
handling of OpenGL rendercontexts across different platforms and toolkits.

Currently supported GL bindings are WGL on Windows/Native and GLX on Linux/GTK2.

A modified version of dglOpenGL.pas version 4.4 can be found in test/use/. This version adds the missing
import for GLX_EXT_swap_control, which will be added in later versions of dglOpenGL.

Usage
-----
Initialization:

```Delphi
var
  pf: TglcContextPixelFormatSettings;
begin
  // construct the helper object for the current target platform
  FContext:= TGLContext.GetPlatformClass.Create(Self);
  // prepare a PixelFormat with all-default settings
  pf:= TGLContext.MakePF();
  // modify PixelFormat as required
  pf.MultiSampling:= 16;
  // Create and Activate context, also initializes dglOpenGL
  FContext.BuildContext(pf);

```

Render Loop:
```Delphi
  FContext.SwapBuffers;

```

Methods
-------
- `MakePF`: Initialize a PixelFormat description structure
- `GetPlatformClass`: return the implementation best suited for the current compilation target
- `ChangeDisplaySettings`: Change global display settings (i.e. for fullscreen rendering)
- `IsAnyContextActive`: check if any OpenGL context is active

- `BuildContext`: create a rendercontext using a specified PixelFormat description
- `Activate`: activate the context
- `Deactivate`: deactivate the context
- `IsActive`: check if the current context is the active one
- `SwapBuffers`: Draw a frame
- `SetSwapInterval`: set VSync properties
- `Share`: enable sharing of DisplayLists with another context


License
-------
Copyright (c) 2012-14 Martok & BitSpace Development Group

This software is provided 'as-is', without any express or implied warranty. In no event will the authors be held liable for any damages arising from the use of this software.

Permission is granted to anyone to use this software for any purpose, including commercial applications, and to alter it and redistribute it freely, subject to the following restrictions:

1. The origin of this software must not be misrepresented; you must not claim that you wrote the original software. If you use this software in a product, an acknowledgment in the product documentation would be appreciated but is not required.
2. Altered source versions must be plainly marked as such, and must not be misrepresented as being the original software.
3. This notice may not be removed or altered from any source distribution.


