#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
area=$(cd "$(dirname "$0")" && pwd)
d=$(mktemp -d)
trap 'rm -rf "$d"' EXIT
cd "$d"
cp "$area"/NativePercentArrow*.hs .
g=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -Werror=unrecognised-modifiers -fdiagnostics-color=never)
reject() {
  local m=$1 msg=$2 status=0
  "${g[@]}" -c "$m.hs" >bad.log 2>&1 || status=$?
  if [[ "$status" != 1 ]] || ! grep -Fq "$msg" bad.log; then cat bad.log >&2; exit 1; fi
}
"${g[@]}" -c NativePercentArrowProvider.hs
"${g[@]}" -c NativePercentArrowOnlyModifiers.hs
"${g[@]}" --show-iface NativePercentArrowProvider.hi >NativePercentArrowProvider.iface
grep -Fq 'type Variable m p a b = a %p -> @(m) b' NativePercentArrowProvider.iface
reject NativePercentArrowDuplicate 'NativePercentArrowDuplicate.hs:4:19: error: [GHC-64627]'
reject NativePercentArrowWrong 'NativePercentArrowWrong.hs:6:7: error: [GHC-83865]'
reject NativePercentArrowNoModifiers 'Multiplicity'

cat >Record.hs <<'HS'
{-# LANGUAGE Modifiers #-}
module Record where
import GHC.Exts (M)
data R = R { unR %M :: Int }
HS
reject Record 'GHC-49969'
