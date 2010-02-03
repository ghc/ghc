#! /bin/sh
set -e

# Create libraries/*/{ghc.mk,GNUmakefile}
bash boot-pkgs

# Check that we have all boot packages.
for dir in `grep "^[^# ][^ ]*  *[^ ][^ ]*  *[^ ][^ ]*$" packages | sed "s/ .*//"`
do
    # We would like to just check for an _darcs directory here, but in
    # an lndir tree we avoid making _darcs directories, so it doesn't
    # exist. We therefore require that every repo has a LICENSE file
    # instead.
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
