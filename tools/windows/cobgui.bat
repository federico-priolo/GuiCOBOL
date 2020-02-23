echo off
cls
echo "**********************************************************
echo "* compila un programma cbl e lo trasforma in .gui ..." %1
echo "**********************************************************


rem cobc agarcob.cbl -v -fno-gen-c-decl-static-call -LC:/cygwin64/usr/local/lib -lag_gui -lag_core  -I"C:/cygwin64/usr/local/include/agar" -I"C:/cygwin64/usr/include/SDL" -I"c:/cygwin64/usr/include/freetype2" -I"C:/cygwin64/usr/include/libpng16" -I"C:/cygwin64/usr/include/freetype2" -I"C:/cygwin64/usr/include/uuid" -L"C:/cygwin64/usr/lib"  -lSDL.dll -lfreetype -lfontconfig -lfreetype -lopengl32 -lgdi32 -lX11 -lXinerama -lm -ljpeg -lpng16 -lwinmm  

rem cobc agarcob.cbl -v -LC:/cygwin64/usr/local/lib -lag_gui -lag_core  -I"C:/cygwin64/usr/local/include/agar" -I"C:/cygwin64/usr/include/SDL" -I"c:/cygwin64/usr/include/freetype2" -I"C:/cygwin64/usr/include/libpng16" -I"C:/cygwin64/usr/include/freetype2" -I"C:/cygwin64/usr/include/uuid" -L"C:/cygwin64/usr/lib"  -lSDL.dll -lfreetype -lfontconfig -lfreetype -lopengl32 -lgdi32 -lX11 -lXinerama -lm -ljpeg -lpng16 -lwinmm 

guicobol %1.cbl

rem cobc %1.gui -x -v -fno-gen-c-decl-static-call  -LC:/cygwin64/usr/local/lib -lag_gui -lag_core  -I"C:/cygwin64/usr/local/include/agar" -I"C:/cygwin64/usr/include/SDL" -I"c:/cygwin64/usr/include/freetype2" -I"C:/cygwin64/usr/include/libpng16" -I"C:/cygwin64/usr/include/freetype2" -I"C:/cygwin64/usr/include/uuid" -L"C:/cygwin64/usr/lib"  -lSDL.dll -lfreetype -lfontconfig -lfreetype -lopengl32 -lgdi32 -lX11 -lXinerama -lm -ljpeg -lpng16 -lwinmm 

cobc %1.gui -x -v  -fno-gen-c-decl-static-call -LC:/cygwin64/usr/local/lib -lag_gui -lag_core  -I"C:/cygwin64/usr/local/include/agar" -I"C:/cygwin64/usr/include/SDL" -I"c:/cygwin64/usr/include/freetype2" -I"C:/cygwin64/usr/include/libpng16" -I"C:/cygwin64/usr/include/freetype2" -I"C:/cygwin64/usr/include/uuid" -L"C:/cygwin64/usr/lib"  -lSDL.dll -lfreetype -lfontconfig -lfreetype -lopengl32 -lgdi32 -lX11 -lXinerama -lm -ljpeg -lpng16 -lwinmm 




