echo off
cls
echo "**********************************************************
echo "* animatore di agarcob.ani che viene generato
echo "**********************************************************

animator agarcob.cbl -k   -c

cobc agarcob.ani -v  -LC:/cygwin64/usr/local/lib -lag_gui -lag_core  -I"C:/cygwin64/usr/local/include/agar" -I"C:/cygwin64/usr/include/SDL" -I"c:/cygwin64/usr/include/freetype2" -I"C:/cygwin64/usr/include/libpng16" -I"C:/cygwin64/usr/include/freetype2" -I"C:/cygwin64/usr/include/uuid" -L"C:/cygwin64/usr/lib"  -lSDL.dll -lfreetype -lfontconfig -lfreetype -lopengl32 -lgdi32 -lX11 -lXinerama -lm -ljpeg -lpng16 -lwinmm  







