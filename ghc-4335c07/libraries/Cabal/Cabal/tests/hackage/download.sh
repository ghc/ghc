#!/bin/sh

if test ! -f archive/archive.tar; then

  wget http://hackage.haskell.org/cgi-bin/hackage-scripts/archive.tar
  mkdir -p archive
  mv archive.tar archive/
  tar -C archive -xf archive/archive.tar    

fi

if test ! -f archive/00-index.tar.gz; then

  wget http://hackage.haskell.org/packages/archive/00-index.tar.gz
  mkdir -p archive
  mv 00-index.tar.gz archive/
  tar -C archive -xzf archive/00-index.tar.gz

fi
