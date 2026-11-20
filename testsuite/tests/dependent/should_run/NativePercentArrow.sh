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

# An alias's default U arrow is valid for terms but unsuitable for a data kind.
cat >ArrowAlias.hs <<'HS'
{-# LANGUAGE GADTs, TypeOperators #-}
module ArrowAlias where
import Data.Kind (Type)
data TyFun a b
type a ~> b = TyFun a b -> Type
data Symbol :: Type ~> Type
HS
reject ArrowAlias 'ArrowAlias.hs:6:1: error: [GHC-83865]'
grep -Fq 'supplies an unmatchable (U) arrow.' bad.log
grep -Fq 'This declaration requires a matchable (M) arrow.' bad.log
grep -Fq "in the synonym's definition" bad.log

sed -e 's/module ArrowAlias/module AliasProvider/' -e '/^data Symbol/d' ArrowAlias.hs >AliasProvider.hs
"${g[@]}" -c AliasProvider.hs
rm AliasProvider.hs
sed -e 's/module ArrowAlias/module ImportedAlias/' \
    -e '/^data TyFun/,+1c import AliasProvider (type (~>))' ArrowAlias.hs >ImportedAlias.hs
reject ImportedAlias 'ImportedAlias.hs:5:1: error: [GHC-83865]'
grep -Fq 'supplies an unmatchable (U) arrow.' bad.log
grep -Fq 'import a matchable synonym' bad.log

sed -e '1s/TypeOperators/TypeApplications, DataKinds/' \
    -e 's/module ImportedAlias/module WiredArrow/' \
    -e 's/import AliasProvider.*/import GHC.Exts (U)/' \
    -e 's/Type ~> Type/(->) @U Type Type/' ImportedAlias.hs >WiredArrow.hs
reject WiredArrow 'WiredArrow.hs:5:1: error: [GHC-83865]'
if grep -Fq 'kind synonym' bad.log; then cat bad.log >&2; exit 1; fi

# A trailing synonym can supply the bad arrow. A forwarding alias does not.
sed -e 's/module ArrowAlias/module TrailingAlias/' \
    -e 's/data Symbol :: Type ~> Type/data Symbol :: Type -> (Type ~> Type)/' \
    ArrowAlias.hs >TrailingAlias.hs
reject TrailingAlias 'TrailingAlias.hs:6:1: error: [GHC-83865]'
grep -Fq 'supplies an unmatchable (U) arrow.' bad.log

sed -e 's/module WiredArrow/module ForwardedArrow/' \
    -e '/^data Symbol/i type Identity a = a' \
    -e 's/Symbol :: (->) @U Type Type/Symbol :: Identity ((->) @U Type Type)/' \
    WiredArrow.hs >ForwardedArrow.hs
reject ForwardedArrow 'ForwardedArrow.hs:6:1: error: [GHC-83865]'
if grep -Fq 'kind synonym' bad.log; then cat bad.log >&2; exit 1; fi

# The mismatch can come from a forall in the synonym's kind.
cat >ForallAlias.hs <<'HS'
{-# LANGUAGE DataKinds, GADTs, Modifiers, PolyKinds, RankNTypes #-}
{-# LANGUAGE RequiredTypeArguments, StandaloneKindSignatures, TypeApplications #-}
module ForallAlias where
import Data.Kind (Type)
import GHC.Exts (M)
type InvisibleKind = forall (a :: Type). a %M -> Type
data InvisibleSymbol :: InvisibleKind
type VisibleKind = forall (a :: Type) -> Type
type VisibleSymbol :: VisibleKind
data VisibleSymbol a
HS
reject ForallAlias 'error: [GHC-83865]'
grep -Fq 'supplies an unmatchable (U) invisible forall.' bad.log
grep -Fq "Use 'forall ... . @M' in the synonym's definition" bad.log
grep -Fq 'supplies an unmatchable (U) visible forall.' bad.log
grep -Fq "Use 'forall ... -> @M' in the synonym's definition" bad.log
sed -e 's/module ForallAlias/module FixedForallAlias/' \
    -e '/^type InvisibleKind/s/\. a %M/. @M a %M/' \
    -e '/^type VisibleKind/s/) -> Type/) -> @M Type/' ForallAlias.hs >FixedForallAlias.hs
"${g[@]}" -c FixedForallAlias.hs
