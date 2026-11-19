#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

# Both compiler binaries must read interfaces from this fork's same format.
if [[ $# -ne 3 || $1 != /* || $2 != /* || $3 != /* ]]; then
    printf 'usage: %s ABS_M142_GHC ABS_M144_GHC ABS_LIBDIR\n' "$0" >&2
    exit 2
fi
old=$1
new=$2
lib=$3
t=$(mktemp -d)
trap 'rm -rf -- "$t"' EXIT
cd "$t"

cat >Worker.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module Worker (Choice(..), pick) where
data Choice a b = First a b | Second b a
pick :: Choice a b -> b
pick (First _ y) = y
pick (Second y _) = y
HS
sed -e 's/NoExperimentalDependentTypes/ExperimentalDependentTypes/' \
    -e 's/module Worker /module ExtendedWorker /' Worker.hs > ExtendedWorker.hs
sed -e 's/module Worker /module StrictWorker /' \
    -e 's/pick (First _ y) = y/pick (First x y) = x `seq` y/' Worker.hs > StrictWorker.hs
sed -e 's/module Worker /module AbstractWorker /' \
    -e 's/Choice(\.\.)/Choice/' Worker.hs > AbstractWorker.hs
cat >Plain.hs <<'HS'
module Plain (plain) where
plain :: Integer
plain = 4
HS

flags=("-B$lib" -package-env - -O0 -fomit-interface-pragmas
       -fno-write-if-simplified-core -dcore-lint -dstg-lint -fdiagnostics-color=never)
"$old" "${flags[@]}" -v0 -c Worker.hs ExtendedWorker.hs StrictWorker.hs AbstractWorker.hs Plain.hs

check_family() {
    local module=$1 expected=$2
    "$new" "${flags[@]}" --show-iface "$module.hi" > "$module.iface"
    if grep -Fq 'T:pick' "$module.iface"; then
        if [[ $expected == present ]]; then return; fi
    elif [[ $expected == absent ]]; then
        return
    fi
    printf 'Expected T:pick %s in %s.hi\n' "$expected" "$module" >&2
    cat "$module.iface" >&2
    exit 1
}
for module in Worker ExtendedWorker StrictWorker AbstractWorker; do
    check_family "$module" absent
done

mkdir binary-only
cp Worker.hi binary-only/
(
    cd binary-only
    cat >Consumer.hs <<'HS'
module Consumer where
import Worker (Choice(First), pick)
field :: Char
field = pick (First True 'x')
HS
    "$new" "${flags[@]}" -v0 -i -i. -c Consumer.hs
)

cat >Use.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, StandaloneKindSignatures, TypeOperators #-}
module Use where
import Data.Type.Equality ((:~:)(Refl))
import qualified Worker as W
import qualified ExtendedWorker as E
import qualified StrictWorker as S
import qualified AbstractWorker as A
import Plain (plain)

type Pick :: W.Choice Bool Char -> Char
type Pick p = W.pick p
ordinary :: Pick ('W.First 'True 'x') :~: 'x'
ordinary = Refl

type Extended :: E.Choice Bool Char -> Char
type Extended p = E.pick p
extended :: Extended ('E.Second 'x' 'True) :~: 'x'
extended = Refl

strictUse :: S.Choice Bool Char -> Char
strictUse = S.pick
abstractUse :: A.Choice Bool Char -> Char
abstractUse = A.pick
n :: Integer
n = plain
HS

"$new" "${flags[@]}" -v1 --make -no-link Use.hs >upgrade.log 2>&1 || { cat upgrade.log; exit 1; }
cat upgrade.log
for module in Worker ExtendedWorker StrictWorker; do
    grep -Eq "Compiling $module[[:space:]].*\\[Compiler now checks sum projections\\]" upgrade.log
done
if grep -Eq 'Compiling (AbstractWorker|Plain)[[:space:]]' upgrade.log; then
    printf '%s\n' 'The upgrade rebuilt a module that cannot publish a sum projection.' >&2
    exit 1
fi
check_family Worker present
check_family ExtendedWorker present
check_family StrictWorker absent
check_family AbstractWorker absent

"$new" "${flags[@]}" -v1 --make -no-link Use.hs >warm.log 2>&1 || { cat warm.log; exit 1; }
if grep -Fq 'Compiling ' warm.log; then cat warm.log >&2; exit 1; fi
echo 'GHC read an old binary-only interface, upgraded the eligible producers, and reused all modules on the warm build.'
