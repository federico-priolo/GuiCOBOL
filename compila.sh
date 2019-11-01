set -x

export COB_LDFLAGS='-Wl,--no-as-needed'

echo rimuovo $1.gui
rm $1.gui

echo compilo...$1
#cobc agarcob.cbl   $(agar-config --libs)  $(agar-core-config --libs) -fno-gen-c-decl-static-call   

cobc agarcob.cbl   $(agar-config --libs)  $(agar-core-config --libs)    

echo "compilato...."
cobc -x guicobol.cbl

./guicobol $1   

#cobc -x  $1.gui $(agar-config --libs)  $(agar-core-config --libs) -fno-gen-c-decl-static-call   -Wall

cobc -x  $1.gui $(agar-config --libs)  $(agar-core-config --libs)     

./$1
