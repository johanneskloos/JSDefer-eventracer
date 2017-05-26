#! /bin/bash
set -x
for FILE in $(cat tools.itarget); do
    FILE2=${FILE%.native}
    mv $FILE $FILE2
done
FILES_TO_INSTALL="META $(sed -e s/.native// tools.itarget)"
ocamlfind remove JSDefer && true
ocamlfind install JSDefer $FILES_TO_INSTALL
