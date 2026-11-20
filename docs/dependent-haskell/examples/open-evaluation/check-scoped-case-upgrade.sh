#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
if [[ $# != 4 ]]; then
    echo "Usage: $0 LEXICAL_RULE_GHC FIXED_RULE_GHC SCOPED_CASE_GHC COMMON_LIBDIR" >&2
    exit 2
fi
old=$(realpath "$1")
fixed=$(realpath "$2")
new=$(realpath "$3")
args=("-B$(realpath "$4")" -O1 -v1 -fhide-source-paths -i -i. -package-env -
      -dcore-lint -dstg-lint --make -no-link Client.hs LexicalPackageRules.hs)
src=$(cd "$(dirname "$0")/../erased-existentials" && pwd)
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
cd "$work"
cp "$src"/{LexicalPackageRules,PackageRules}.hs .
cat >Mixed.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, PolyKinds, TypeFamilies #-}
module Mixed (Gate(..), Pick, hideInt) where
import Data.Kind (Type)
import PackageRules (Hidden)

data Gate k where
    Plain :: Bool -> Gate k
    Other :: Gate k
type Pick (k :: Type) (d :: Maybe (Gate k)) = case d of
    Just (Plain True) -> 'Nothing
    Just _ -> 'Nothing
    Nothing -> 'Nothing

{-# NOINLINE hideInt #-}
hideInt :: Int -> Hidden
hideInt x = (x, id)
{-# NOINLINE callback #-}
callback :: Int -> Int
callback x = x
{-# RULES "mixed/hide" forall x. hideInt x = ((x, callback) :: Hidden) #-}
HS
cat >Client.hs <<'HS'
{-# LANGUAGE DataKinds, GADTs, TypeFamilies, TypeOperators #-}
module Client where
import Data.Type.Equality ((:~:)(Refl))
import Mixed
proof :: (Pick Bool ('Just ('Plain 'True)) :: Maybe Bool) :~: 'Nothing
proof = Refl
HS
for compiler in "$old" "$fixed"; do
    dir=$(mktemp -d "$work/run.XXXXXX")
    cp "$work/"*.hs "$dir/"
    cd "$dir"
    "$compiler" "${args[@]}" >old.log 2>&1 || { cat old.log >&2; exit 1; }
    "$new" "${args[@]}" >upgrade.log 2>&1 || { cat upgrade.log >&2; exit 1; }
    if [[ "$compiler" == "$old" ]]; then
        reason="Compiler now retains private helpers in erased package rules"
        grep -Fq "Compiling LexicalPackageRules [$reason]" upgrade.log
    else
        reason="Compiler now checks dependent type case captures"
        if grep -Fq 'Compiling LexicalPackageRules ' upgrade.log; then cat upgrade.log >&2; exit 1; fi
    fi
    grep -Fq "Compiling Mixed [$reason]" upgrade.log || { cat upgrade.log >&2; exit 1; }
    grep -Fq 'Compiling Client [Mixed changed]' upgrade.log || { cat upgrade.log >&2; exit 1; }
    if grep -Fq 'Compiling PackageRules ' upgrade.log; then cat upgrade.log >&2; exit 1; fi
    "$new" "${args[@]}" >warm.log 2>&1 || { cat warm.log >&2; exit 1; }
    if grep -Fq 'Compiling ' warm.log; then cat warm.log >&2; exit 1; fi
    echo "Rebuilt Mixed and Client once ($reason)."
done
