#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
area=$(cd "$(dirname "$0")" && pwd)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
cd "$tmp"
cp "$area"/Native*.hs .
g=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)
reject() {
  local file=$1 span=$2 message=$3 status=0
  "${g[@]}" -c "$file.hs" >bad.log 2>&1 || status=$?
  if [[ $status != 1 ]] || ! grep -Fq "$file.hs:$span:" bad.log ||
      ! grep -Fq "$message" bad.log; then cat bad.log >&2; exit 1; fi
}
"${g[@]}" -c NativeCase.hs
"${g[@]}" --show-iface NativeCase.hi >case.iface
if sed -n '/^exports:/,/^defaults:/p' case.iface | grep -F '$case:'; then exit 1; fi
mkdir interfaces
cp NativeCase.hi NativeTypeCase{Client,Rigid}.hs interfaces/
(cd interfaces && "${g[@]}" -c NativeTypeCaseClient.hs &&
   reject NativeTypeCaseRigid 8:13 'case a of {True -> Int; False -> Int}' &&
   grep -Fq "Just x1 -> '(x, x1)" bad.log)
reject NativeTypeCaseInvalid 6:3 'Not in scope: data constructor'
reject NativeTypeCaseEscape 6:14 'Not in scope: type variable'
reject NativeTypeCaseBadPattern 5:3 'A type case pattern needs a data constructor'
grep -Fq '[GHC-55789]' bad.log
reject NativeTypeCaseFallback 5:3 'A type case pattern needs a data constructor'
reject NativeTypeCaseDisabled 4:12 'GHC-98931'
reject NativeTypeCaseBranchKind 6:12 "In the type case in"
reject NativeTypeCaseScrutinee 4:15 'Not in scope: type constructor or class'
