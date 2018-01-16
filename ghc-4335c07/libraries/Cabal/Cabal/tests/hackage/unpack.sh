#!/bin/sh

for tarball in archive/*/*/*.tar.gz; do

  pkgdir=$(dirname ${tarball})
  pkgname=$(basename ${tarball} .tar.gz)

  if tar -tzf ${tarball} ${pkgname}/Setup.hs 2> /dev/null; then
    tar -xzf ${tarball} ${pkgname}/Setup.hs -O > ${pkgdir}/Setup.hs
  elif tar -tzf ${tarball} ${pkgname}/Setup.lhs 2> /dev/null; then
    tar -xzf ${tarball} ${pkgname}/Setup.lhs -O > ${pkgdir}/Setup.lhs
  else
    echo "${pkgname} has no Setup.hs or .lhs at all!!?!"
  fi

done
