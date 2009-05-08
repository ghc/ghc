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

libraries=

for f in libraries/*; do
  pkgs=$f/ghc-packages
  if test -f $pkgs; then
    for p in `cat $pkgs`; do
      libraries="$libraries $f/$p"
    done
  else
    libraries="$libraries $f"
  fi
done

for f in $libraries; do
   dir=`basename $f`
   cabals=`echo $f/*.cabal`
   if test -f $cabals; then
       echo "Creating $f/ghc.mk"
       rm -f $f/ghc.mk
       pkg=`echo "$cabals" | sed -e 's#.*/##' -e 's#\.cabal$##'`
       if test -f $f/ghc-stage; then
           stage=`cat $f/ghc-stage`
       else
           stage=1
       fi
       top=`echo $f | sed 's#[^/]\+#..#g'`
       echo "${f}_PACKAGE = ${pkg}" >> $f/ghc.mk
       echo "${f}_dist-install_GROUP = libraries" >> $f/ghc.mk
       echo "\$(eval \$(call build-package,${f},dist-install,${stage}))" >> $f/ghc.mk
       rm -f $f/GNUmakefile
       echo "Creating $f/GNUmakefile"
       echo "dir = ${f}" >> $f/GNUmakefile
       echo "TOP = ${top}" >> $f/GNUmakefile
       echo "include \$(TOP)/mk/sub-makefile.mk" >> $f/GNUmakefile
   fi
done
