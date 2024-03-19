#!/bin/sh

example_dylib=$(basename -- $(find $(ghc --print-libdir) -name libHS* -not -name *.a | head -n1))
dylib_ext="${example_dylib##*.}"
# we try .out instead of using the correct extension.

i=0
while [ $i -lt 500 ]; do
    j=0
    while [ $j -lt 100 ]; do
        echo "int lib${i}_$j(void) { return $i; }" >> "lib$i.c"
        j=$(( j + 1 ))
    done
    cc -o "lib$i.o" -c "lib$i.c" -fPIC
    cc -shared "lib$i.o" -o "lib$i.out" # "lib$i.$dylib_ext"
    rm "lib$i.c" "lib$i.o"
    i=$(( i + 1 ))
done


