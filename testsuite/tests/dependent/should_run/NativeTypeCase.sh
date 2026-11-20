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
"${g[@]}" -fomit-interface-pragmas -c NativeNestedCase.hs
"${g[@]}" --show-iface NativeNestedCase.hi >nested.iface
if sed -n '/^exports:/,/^defaults:/p' nested.iface | grep -E '\$(case(-node)?|lambda):'; then exit 1; fi
"${g[@]}" --show-iface NativeCase.hi >case.iface
if sed -n '/^exports:/,/^defaults:/p' case.iface | grep -E '\$case(:|-let-rhs:)'; then exit 1; fi
mkdir interfaces
cp NativeCase.hi NativeNestedCase.hi NativeTypeCase{Client,Rigid,LetWrong}.hs interfaces/
(cd interfaces && "${g[@]}" -c NativeTypeCaseClient.hs &&
   reject NativeTypeCaseRigid 10:13 'case a of {True -> Int; False -> Int}' &&
   grep -Fq "Just x1 -> '(x, x1)" bad.log &&
   grep -Fq 'NativeTypeCaseRigid.hs:16:12: error: [GHC-83865]' bad.log &&
   grep -Fq 'NativeTypeCaseRigid.hs:19:10: error: [GHC-83865]' bad.log &&
   grep -Fq 'NativeTypeCaseRigid.hs:22:13: error: [GHC-83865]' bad.log &&
   grep -Fq 'NativeTypeCaseRigid.hs:27:14: error: [GHC-83865]' bad.log &&
   grep -Fq 'NativeTypeCaseRigid.hs:30:19: error: [GHC-83865]' bad.log &&
   ! grep -Eq '\$(case(-node)?|lambda):' bad.log &&
   reject NativeTypeCaseLetWrong 8:9 'GHC-83865')
"${g[@]}" -fomit-interface-pragmas -c NativeTypeCaseHeadProvider.hs
cp NativeTypeCaseHeadProvider.hi NativeTypeCaseHeads.hs NativeTypeCaseSynonymCatchAll.hs interfaces/
(cd interfaces && "${g[@]}" -Werror=unused-imports -c NativeTypeCaseHeads.hs)
reject NativeTypeCaseFamilyHead 4:24 'GHC-73138'
grep -Fq 'Illegal type synonym family application' bad.log
(cd interfaces && reject NativeTypeCaseSynonymCatchAll 8:39 'GHC-55789'
grep -Fq 'After expanding type synonyms, the pattern has no constructor head' bad.log
grep -Fq 'NativeTypeCaseSynonymCatchAll.hs:9:39: error: [GHC-55789]' bad.log
grep -Fq 'NativeTypeCaseSynonymCatchAll.hs:10:35: error: [GHC-55789]' bad.log
grep -Fq 'NativeTypeCaseSynonymCatchAll.hs:11:41: error: [GHC-55789]' bad.log)
reject NativeTypeCaseInvalid 6:3 'Not in scope: data constructor'
reject NativeTypeCaseEscape 6:14 'Not in scope: type variable'
reject NativeTypeCaseBadPattern 5:3 'A type case pattern needs a constructor head'
grep -Fq '[GHC-55789]' bad.log
reject NativeTypeCaseFallback 5:3 'A type case pattern needs a constructor head'
reject NativeTypeCaseDisabled 4:12 'GHC-98931'
reject NativeTypeCaseBranchKind 6:12 "In the type case in"
reject NativeTypeCaseScrutinee 4:15 'Not in scope: type constructor or class'
reject NativeTypeCaseLetMonomorphic 6:22 'GHC-83865'
reject NativeTypeCaseLetUnowned 4:26 'A type let used in a case body cannot generalize the kind of its right-hand side.'

reject NativeTypeCaseDominatedRhs 6:20 'GHC-76037'
grep -Fq 'Not in scope: type constructor or class' bad.log
reject NativeTypeCaseDominatedWrongKind 8:20 'GHC-83865'
grep -Eq 'has kind .*Bool' bad.log
reject NativeTypeCaseNestedAliasHead 5:8 'GHC-55789'
reject NativeTypeCaseOpaqueIndex 12:3 'GHC-79642'
grep -Fq 'Cannot determine which constructors are possible' bad.log
reject NativeTypeCaseReorderedAlias 7:8 'GHC-78416'
grep -Fq 'Cannot use this constructor pattern in a nested type case' bad.log
