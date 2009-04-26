#! /bin/sh
set -e

# Check that we have all boot packages.
for dir in `grep "^[^# ][^ ]*  *[^ ][^ ]*  *[^ ][^ ]*$" packages | sed "s/ .*//"`
do
    if test ! -f $dir/LICENSE
    then
        echo "Error: $dir/LICENSE doesn't exist." >&2
        echo "Maybe you haven't done './darcs-all get'?" >&2
        exit 1
    fi
done

# autoreconf everything that needs it.
for dir in . libraries/*
do
    if test -f $dir/configure.ac
    then
        echo "Booting $dir"
        ( cd $dir && autoreconf )
    fi
done

# Alas, darcs doesn't handle file permissions, so fix a few of them.
for f in boot darcs-all push-all validate
do
    if test -f $f
    then
        chmod +x $f
    fi
done

for f in libraries/*; do
   dir=`basename $f`
   cabals=`echo $f/*.cabal`
   if test -f $cabals; then
       echo "Creating $f/ghc.mk"
       rm -f $f/ghc.mk
       pkg=`basename ${cabals%.cabal}`
       echo "${f}_PACKAGE = ${pkg}" >> $f/ghc.mk
       echo "\$(eval \$(call build-package,${f},dist-install,1))" >> $f/ghc.mk
       rm -f $f/GNUmakefile
       echo "Creating $f/GNUmakefile"
       echo "dir = ${f}" >> $f/GNUmakefile
       echo "TOP = ../.." >> $f/GNUmakefile
       echo "include \$(TOP)/mk/sub-makefile.mk" >> $f/GNUmakefile
   fi
done
