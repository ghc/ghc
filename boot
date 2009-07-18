#! /bin/sh
set -e

# Create libraries/*/{ghc.mk,GNUmakefile}
sh boot-pkgs

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
