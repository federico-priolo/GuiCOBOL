rem cobc agarcob.cbl -v -fno-gen-c-decl-static-call -LC:\cygwin64\usr\local\lib -lag_gui -lag_core -I"C:\agar\include\agar\" 

rem cobc agarcob.cbl -v -fno-gen-c-decl-static-call '-LC:\cygwin64\usr\local\lib' '-lag_gui' '-lag_core' -I "C:\agar\include\agar\"

cobc agarcob.cbl -v -fno-gen-c-decl-static-call -LC:/cygwin64/usr/local/lib -lag_gui -lag_core  -I"C:/cygwin64/usr/local/include/agar" -I"C:/cygwin64/usr/include/SDL" -I"c:/cygwin64/usr/include/freetype2" -I"C:/cygwin64/usr/include/libpng16" -I"C:/cygwin64/usr/include/freetype2" -I"C:/cygwin64/usr/include/uuid" -L"C:/cygwin64/usr/lib"  -lSDL.dll -lfreetype -lfontconfig -lfreetype -lopengl32 -lgdi32 -lX11 -lXinerama -lm -ljpeg -lpng16 -lwinmm 

guicobol %1.cbl

cobc %1.gui -x -v -fno-gen-c-decl-static-call -LC:/cygwin64/usr/local/lib -lag_gui -lag_core  -I"C:/cygwin64/usr/local/include/agar" -I"C:/cygwin64/usr/include/SDL" -I"c:/cygwin64/usr/include/freetype2" -I"C:/cygwin64/usr/include/libpng16" -I"C:/cygwin64/usr/include/freetype2" -I"C:/cygwin64/usr/include/uuid" -L"C:/cygwin64/usr/lib"  -lSDL.dll -lfreetype -lfontconfig -lfreetype -lopengl32 -lgdi32 -lX11 -lXinerama -lm -ljpeg -lpng16 -lwinmm 
rem cobc  -x -v %1.gui -fno-gen-c-decl-static-call   -L"C:\cygwin64\usr\local\lib"  -I"C:\cygwin64\usr\local\include\agar\agar" -I"C:\cygwin64\usr\local\include\agar\agar\gui" -I"C:\cygwin64\usr\local\include\agar\agar\gui" 


