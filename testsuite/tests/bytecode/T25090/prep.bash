#!/usr/bin/env bash

set -eu

ghc_cmd="$1"
ghc_opts="$2"
ghc_pkg_cmd="$3"
library="$4"

base="$PWD"
db="$base/db"
dep="$base/dep"
conf_dep="${dep}/dep.conf"

mkdir -p "$dep" "$db"
mv Dep.hs DepApi.hs "$dep/"
cp dep.conf "$dep/"

ghc_pkg()
{
  eval "${ghc_pkg_cmd@Q} --no-user-package-db --package-db=${db@Q} $@"
}

ghc()
{
  eval "${ghc_cmd@Q} ${ghc_opts/-rtsopts/} -package-db ${db@Q} -hidir ${dep@Q} -O0 -this-unit-id dep-1.0 -fbyte-code-and-object-code $@"
}

version=$(ghc "--numeric-version")

ghc_pkg recache

ghc "-dynamic-too -c ${dep@Q}/Dep.hs ${dep@Q}/DepApi.hs"

if [[ "$library" == 'shared' ]]
then
  ghc "-dynamic -shared -o ${dep@Q}/libHSdep-1.0-ghc$version.so ${dep@Q}/Dep.dyn_o ${dep@Q}/DepApi.dyn_o"
  echo 'hs-libraries: HSdep-1.0' >> "$conf_dep"
elif [[ "$library" == 'shared-empty' ]]
then
  echo 'module Dummy where' > Dummy.hs
  ghc "-dynamic-too -c Dummy.hs"
  ghc "-dynamic -shared -o ${dep@Q}/libHSdep-1.0-ghc$version.so Dummy.dyn_o"
  echo 'hs-libraries: HSdep-1.0' >> "$conf_dep"
elif [[ "$library" == 'archive' ]]
then
  $AR cqs "${dep}/libHSdep-1.0.a" "${dep}/Dep.o" "${dep}/DepApi.o"
  echo 'hs-libraries: HSdep-1.0' >> "$conf_dep"
elif [[ "$library" == 'none' ]]
then
  :
else
  echo "Invalid argument for 'library': $library"
  exit 1
fi

ghc_pkg -v0 register "${conf_dep@Q}"
