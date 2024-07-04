#!/usr/bin/env bash

set -eu

ghc_cmd="$1"
ghc_opts="$2"
ghc_pkg_cmd="$3"
archive="$4"

base="$PWD"
lib="$base/dep"
# TODO see if this can just be stored in pwd. $lib as well
db="$base/db"
conf="${lib}/dep.conf"

ghc_pkg()
{
  eval "${ghc_pkg_cmd@Q} --no-user-package-db --package-db=${db@Q} $@"
}

ghc()
{
  eval "${ghc_cmd@Q} $ghc_opts $@"
}

mkdir -p "$lib" "$db"
mv CrossDep.hs CrossDepApi.hs "$lib/"
cp dep.conf "$lib/"

ghc_pkg recache

ghc "-package-db ${db@Q} -hidir ${lib@Q} -O0 -this-unit-id dep-1.0 -fbyte-code-and-object-code -c ${lib@Q}/CrossDep.hs ${lib@Q}/CrossDepApi.hs"

if [[ "$archive" == 1 ]]
then
  $AR cqs "${lib}/libHSdep-1.0.a" "${lib}/CrossDep.o" "${lib}/CrossDepApi.o"
  echo 'hs-libraries: HSdep-1.0' >> "$conf"
elif [[ "$archive" == 2 ]]
then
  $AR cqs "${lib}/libHSdep-1.0.a"
  echo 'hs-libraries: HSdep-1.0' >> "$conf"
fi

ghc_pkg -v0 register "${conf@Q}"
