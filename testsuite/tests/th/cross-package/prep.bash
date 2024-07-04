#!/usr/bin/env bash

set -eu

ghc_cmd="$1"
ghc_opts="$2"
ghc_pkg_cmd="$3"
archive="$4"

base="$PWD"
db="$base/db"
dep="$base/dep"
conf_dep="${dep}/dep.conf"
obj="$base/obj"
conf_obj="${obj}/obj.conf"

ghc_pkg()
{
  eval "${ghc_pkg_cmd@Q} --no-user-package-db --package-db=${db@Q} $@"
}

ghc()
{
  eval "${ghc_cmd@Q} $ghc_opts $@"
}

mkdir -p "$dep" "$obj" "$db"
mv CrossDep.hs CrossDepApi.hs "$dep/"
cp dep.conf "$dep/"
mv CrossObj.hs "$obj/"
cp obj.conf "$obj/"

ghc_pkg recache

ghc "-package-db ${db@Q} -hidir ${dep@Q} -O0 -this-unit-id dep-1.0 -fbyte-code-and-object-code -c ${dep@Q}/CrossDep.hs ${dep@Q}/CrossDepApi.hs"

ghc "-package-db ${db@Q} -hidir ${obj@Q} -O0 -this-unit-id obj-1.0 -c ${obj@Q}/CrossObj.hs"
$AR cqs "${obj}/libHSobj-1.0.a" "${obj}/CrossObj.o"
echo 'hs-libraries: HSobj-1.0' >> "$conf_obj"

if [[ "$archive" == 1 ]]
then
  $AR cqs "${dep}/libHSdep-1.0.a" "${dep}/CrossDep.o" "${dep}/CrossDepApi.o"
  echo 'hs-libraries: HSdep-1.0' >> "$conf_dep"
elif [[ "$archive" == 2 ]]
then
  $AR cqs "${dep}/libHSdep-1.0.a"
  echo 'hs-libraries: HSdep-1.0' >> "$conf_dep"
fi

ghc_pkg -v0 register "${conf_dep@Q}"
ghc_pkg -v0 register "${conf_obj@Q}"
