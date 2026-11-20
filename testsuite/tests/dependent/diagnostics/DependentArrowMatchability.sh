#!/usr/bin/env bash
set -euo pipefail

src=$(cd "$(dirname "$0")" && pwd)
ghc=("$1" "-B$2" -O -v0 -package-env - -dcore-lint -fdiagnostics-color=never)

# Compile the consumer with the provider's interface alone.
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
cd "$out"
mkdir provider
cp "$src/DependentArrowProvider.hs" provider/
cp "$src/DependentArrowMatchability.hs" .
(cd provider && "${ghc[@]}" -fomit-interface-pragmas -fno-write-if-simplified-core \
    -c DependentArrowProvider.hs)
cp provider/DependentArrowProvider.hi .
"${ghc[@]}" -c DependentArrowMatchability.hs
"${ghc[@]}" -fforce-recomp -XExperimentalDependentTypes -c DependentArrowMatchability.hs

reject() {
    local line=$1 message=$2
    shift 2
    if "${ghc[@]}" "$@" -c Rejected.hs > reject.log 2>&1 ||
       ! grep -Eq "^Rejected\\.hs:$line:[0-9]+(-[0-9]+)?: error:" reject.log ||
       ! grep -Fq "$message" reject.log; then
        cat reject.log >&2
        exit 1
    fi
}

# Keep the declaration and use identical except for the head matchability.
sed -e 's/^module DependentArrowMatchability/module Rejected/' \
    -e 's/(f :: FUN @M/(f :: FUN @U/' DependentArrowMatchability.hs > Rejected.hs
line=$(grep -n '^inverseM _ Refl = Refl$' Rejected.hs | cut -d: -f1)
reject "$line" 'In the expression: Refl'

for bad in "P.D (FUN @U 'Many Type Type) Maybe" \
           "P.D (FUN @M 'Many Type Type) P.Abstract"; do
    sed 's/^module DependentArrowMatchability/module Rejected/' DependentArrowMatchability.hs > Rejected.hs
    printf '\ntype Bad = %s\n' "$bad" >> Rejected.hs
    line=$(wc -l < Rejected.hs | tr -d ' ')
    reject "$line" '[GHC-83865]'
    grep -Fq 'Matchable' reject.log
    grep -Fq 'Unmatchable' reject.log
done

# An explicit annotation conflicts with the subject. An undetermined bare
# kind arrow still defaults to M before a later declaration uses it.
for mode in NoExperimentalDependentTypes ExperimentalDependentTypes; do
    sed 's/^module DependentArrowMatchability/module Rejected/' \
        DependentArrowMatchability.hs > Rejected.hs
    printf '\ntype BadAnnotation = (P.Abstract :: Type %%M -> Type)\n' >> Rejected.hs
    reject "$(wc -l < Rejected.hs | tr -d ' ')" '[GHC-83865]' -X"$mode"

    sed 's/^module DependentArrowMatchability/module Rejected/' \
        DependentArrowMatchability.hs > Rejected.hs
    cat >> Rejected.hs <<'HS'

badDefault :: Proxy P.Abstract -> Proxy P.Abstract
badDefault = ascribedDefault @P.Abstract
HS
    reject "$(wc -l < Rejected.hs | tr -d ' ')" '[GHC-83865]' -X"$mode"

    sed 's/^module DependentArrowMatchability/module Rejected/' \
        DependentArrowMatchability.hs > Rejected.hs
    printf '\ntype BadAppliedDefault = AppliedDefault P.Abstract\n' >> Rejected.hs
    reject "$(wc -l < Rejected.hs | tr -d ' ')" '[GHC-83865]' -X"$mode"

    sed 's/^module DependentArrowMatchability/module Rejected/' \
        DependentArrowMatchability.hs > Rejected.hs
    printf '\ntype BadMixedArrows f = (f Bool, P.D (Type %%M -> Type) f, P.D (Type %%U -> Type) f)\n' \
        >> Rejected.hs
    reject "$(wc -l < Rejected.hs | tr -d ' ')" '[GHC-83865]' -X"$mode"

    # A written modifier wins, even when the signature needs a different arrow.
    sed -e 's/^module DependentArrowMatchability/module Rejected/' \
        -e '/^laterBinder ::/s/(f :: Type -> Type)/(f :: Type %M -> Type)/' \
        DependentArrowMatchability.hs > Rejected.hs
    line=$(grep -n '^laterBinder ::' Rejected.hs | cut -d: -f1)
    reject "$line" '[GHC-83865]' -X"$mode"

    # The binder's kind cannot depend on a variable later in the telescope.
    sed -e 's/^module DependentArrowMatchability/module Rejected/' \
        -e '/^scopedBinder ::/s/(m :: Matchability) (f :: Type -> Type)/(f :: Type -> Type) (m :: Matchability)/' \
        DependentArrowMatchability.hs > Rejected.hs
    line=$(grep -n '^scopedBinder ::' Rejected.hs | cut -d: -f1)
    reject "$line" '[GHC-97739]' -X"$mode"

    sed 's/^module DependentArrowMatchability/module Rejected/' \
        DependentArrowMatchability.hs > Rejected.hs
    cat >> Rejected.hs <<'HS'

badBinderDefault :: Proxy P.Abstract -> Proxy P.Abstract
badBinderDefault = binderDefault @P.Abstract
HS
    reject "$(wc -l < Rejected.hs | tr -d ' ')" '[GHC-83865]' -X"$mode"

    sed -e 's/^module DependentArrowMatchability/module Rejected/' \
        -e '/^nestedBinder ::/s/(Type -> Type)/(Type %M -> Type)/' \
        DependentArrowMatchability.hs > Rejected.hs
    line=$(grep -n '^ *Proxy (g P.Abstract)' Rejected.hs | cut -d: -f1)
    reject "$line" '[GHC-83865]' -X"$mode"

    sed 's/^module DependentArrowMatchability/module Rejected/' \
        DependentArrowMatchability.hs > Rejected.hs
    cat >> Rejected.hs <<'HS'

type family NestedAbstract :: (Type %U -> Type) %U -> Type
badNestedDefault :: Proxy (NestedAbstract P.Abstract) -> Proxy NestedAbstract -> Proxy NestedAbstract
badNestedDefault = nestedBinder @NestedAbstract
HS
    reject "$(wc -l < Rejected.hs | tr -d ' ')" '[GHC-83865]' -X"$mode"
done
