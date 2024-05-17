#!/usr/bin/env bash

set -eu

ghc_cmd="$1"
ghc_opts="$2"
ghc_pkg_cmd="$3"

base="$PWD"
lib="$base/dep"
# TODO see if this can just be stored in pwd. $lib as well
db="$base/db"

ghc_pkg()
{
  eval "${ghc_pkg_cmd@Q} --no-user-package-db --package-db=${db@Q} $@"
}

ghc()
{
  eval "${ghc_cmd@Q} $ghc_opts $@"
}

mkdir -p "$lib" "$db"
mv CrossDep.hs CrossDepApi.hs dep.conf "$lib/"

ghc_pkg recache

ghc "-package-db ${db@Q} -hidir ${lib@Q} -O0 -this-unit-id dep-1.0 -fbyte-code-and-object-code -c ${lib@Q}/CrossDep.hs ${lib@Q}/CrossDepApi.hs"
$AR cqs "${lib}/libHSdep-1.0.a" "${lib}/CrossDep.o" "${lib}/CrossDepApi.o"

ghc_pkg -v0 register "${lib@Q}/dep.conf"

tree >&2
