#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
ghc=("$1" "-B$2" -v0 -O0 -package-env - -fomit-interface-pragmas
     -fno-write-if-simplified-core -fdiagnostics-color=never -dcore-lint -dstg-lint)
shift 2
ghc+=("$@")
ways=()
if [[ " $* " != *" -dynamic "* ]] && "${ghc[@]}" --info | grep -F '"Support dynamic-too","YES"' >/dev/null; then
    ways=(-dynamic-too)
fi
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
mkdir "$out"/{producer,interfaces,client}
cd "$out/producer"
cp "$src/NativeClosedFamilyProjection.hs" .
cat >Builder.hs <<'HS'
{-# LANGUAGE BangPatterns, ExplicitForAll, NoExperimentalDependentTypes #-}
module Builder where
import Numeric.Natural (Natural)

left :: Natural -> (Natural, Natural)
left n = (n, errorWithoutStackTrace "unused right field")
right :: forall b a. b -> a -> (a, a)
right _ x = (errorWithoutStackTrace "unused left field", x)
strict :: Natural -> (Natural, Natural)
strict !n = (n, errorWithoutStackTrace "unused right field")
defined :: Natural -> Natural -> (Natural, Natural)
defined n _ = (n, n + 1)
bad :: Natural -> Natural
bad _ = errorWithoutStackTrace "ignored argument was forced"

firstWorker, secondWorker :: Natural -> (Natural, Char)
firstWorker n = (n + 1, if n <= 7 then 'x' else 'a')
secondWorker n = (n + 1, if odd n then 'x' else 'b')
HS
"${ghc[@]}" "${ways[@]}" -c Builder.hs NativeClosedFamilyProjection.hs
"${ghc[@]}" --show-iface Builder.hi >producer.iface
for name in 'P2:left' 'P1:right' 'T:left' 'T:right'; do grep -Fq "$name" producer.iface; done
if grep -Fq 'T:strict' producer.iface; then exit 1; fi
cp ./*.hi ../interfaces/
if compgen -G '*.dyn_hi' >/dev/null; then cp ./*.dyn_hi ../interfaces/; fi
cd ../interfaces
cat >Alias.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module Alias (LeftField, RightField) where
import qualified Builder as W
import qualified NativeClosedFamilyProjection as F
type LeftField = F.pick (W.left 7)
type RightField = F.pickRight (W.right 'False 'x')
HS
"${ghc[@]}" -i -i. -c Alias.hs
"${ghc[@]}" --show-iface Alias.hi >alias.iface
if grep -Eq 'addDependentFile ".*(Builder|NativeClosedFamilyProjection)\.(dyn_o|o)"' alias.iface; then exit 1; fi
sed 's/^module Alias /module AliasRule /' Alias.hs >AliasRule.hs
sed -i '/^module AliasRule /a import Numeric.Natural (Natural)' AliasRule.hs
cat >>AliasRule.hs <<'HS'
{-# RULES "left/changed" forall (x :: Natural). W.left x = (x+1,x) #-}
HS
"${ghc[@]}" -i -i. -XScopedTypeVariables -fenable-rewrite-rules -Wno-inline-rule-shadowing -c AliasRule.hs
cp ./*.hi ../client/
cd ../client
cat >Proof.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
module Proof where
import Alias (LeftField, RightField)
import Data.Type.Equality ((:~:)(Refl))
left :: LeftField :~: 7
left = Refl
right :: RightField :~: 'x'
right = Refl
HS
"${ghc[@]}" -i -i. -c Proof.hs
for owner in left pick; do
    sed 's/module Proof where/module Rule where/' Proof.hs >Rule.hs
    sed -i '/^module Rule where/a import Numeric.Natural (Natural)\nimport qualified NativeClosedFamilyProjection as F\nimport qualified Builder as W' Rule.hs
    if [[ $owner == left ]]; then
        echo '{-# RULES "left/changed" forall (x :: Natural). W.left x = (x+1,x) #-}' >>Rule.hs
    else
        echo '{-# RULES "pick/changed" forall (a :: Natural) (b :: Natural). F.pick (a,b) = b #-}' >>Rule.hs
    fi
    if "${ghc[@]}" -i -i. -XScopedTypeVariables -fenable-rewrite-rules -Wno-inline-rule-shadowing -c Rule.hs >err 2>&1 ||
       ! grep -Fq '[GHC-83605]' err || ! grep -Fq "$owner" err; then cat err >&2; exit 1; fi
done
cat >Disjoint.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, ScopedTypeVariables, TypeOperators #-}
module Disjoint where
import Alias (RightField)
import Data.Type.Equality ((:~:)(Refl))
import qualified Builder as W
proof :: RightField :~: 'x'
proof = Refl
{-# RULES "right/char" forall (b :: Char) (a :: Char). W.right b a = (a,b) #-}
HS
"${ghc[@]}" -i -i. -fenable-rewrite-rules -Wno-inline-rule-shadowing -c Disjoint.hs

# Running the builder can still establish the value of a defined opaque field.
cd ../interfaces
cp ../producer/Builder.o .
if [[ -f ../producer/Builder.dyn_o ]]; then cp ../producer/Builder.dyn_o .; fi
cat >Executable.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeOperators #-}
module Executable where
import qualified Builder as W
import qualified NativeClosedFamilyProjection as F
import Data.Type.Equality ((:~:)(Refl))
computed :: F.pickRight (W.defined 7 3) :~: 8
computed = Refl
ignored :: F.pick (W.defined 7 (W.bad 1)) :~: 7
ignored = Refl
HS
"${ghc[@]}" -i -i. -c Executable.hs

# Identical operands allow reflexivity without using the projection equation.
cat >Reflexive.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, RoleAnnotations, ScopedTypeVariables #-}
module Reflexive where
import qualified Builder as W
import qualified NativeClosedFamilyProjection as F
import Numeric.Natural (Natural)
data N (n :: Natural) = N
type role N nominal
{-# RULES "pick/changed" forall (p :: (Natural, Char)). F.pick p = 17 #-}
same :: N (F.pick (W.firstWorker 7)) -> N (F.pick (W.secondWorker 7))
same x = x
HS
"${ghc[@]}" -i -i. -fenable-rewrite-rules -Wno-inline-rule-shadowing -c Reflexive.hs
# At eight the selected numbers still agree but the complete operands differ.
sed -e 's/module Reflexive where/module Distinct where/' -e 's/Worker 7/Worker 8/g' \
    Reflexive.hs >Distinct.hs
if "${ghc[@]}" -i -i. -fenable-rewrite-rules -Wno-inline-rule-shadowing -c Distinct.hs >err 2>&1 ||
   ! grep -Fq '[GHC-83605]' err || ! grep -Fq 'pick' err; then cat err >&2; exit 1; fi

# Both projected types can also arrive through ordinary exported values.
cat >Values.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, RoleAnnotations #-}
module Values (value, consume) where
import qualified Builder as W
import qualified NativeClosedFamilyProjection as F
import Numeric.Natural (Natural)
data N (n :: Natural) = N
type role N nominal
type A = F.pick (W.firstWorker 7)
type B = F.pick (W.secondWorker 7)
value :: N A
value = N
consume :: N B -> Bool
consume _ = True
HS
"${ghc[@]}" -i -i. -c Values.hs
cp Values.hi ../client/
cd ../client
cat >UseValues.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes, ScopedTypeVariables #-}
module UseValues where
import qualified Values
import qualified NativeClosedFamilyProjection as F
import Numeric.Natural (Natural)
{-# RULES "pick/changed" forall (p :: (Natural, Char)). F.pick p = 17 #-}
result :: Bool
result = Values.consume Values.value
HS
"${ghc[@]}" -i -i. -fenable-rewrite-rules -Wno-inline-rule-shadowing -c UseValues.hs

# The hidden field has the same family declaration after its implementation changes.
# The object used to compute that field must still invalidate the old proof.
cd ../producer
sed -i 's/^defined n _ = (n, n + 1)$/defined n _ = (n, n + 2)/' Builder.hs
"${ghc[@]}" "${ways[@]}" -c Builder.hs
cp Builder.hi Builder.o ../interfaces/
if [[ -f Builder.dyn_o ]]; then cp Builder.dyn_hi Builder.dyn_o ../interfaces/; fi
cd ../interfaces
"${ghc[@]}" -i -i. -v1 -c Alias.hs >reuse.log 2>&1
grep -Fq 'compilation IS NOT required' reuse.log
if "${ghc[@]}" -i -i. -c Executable.hs >err 2>&1 ||
   ! grep -Eq 'Executable.hs:7:.*error: \[GHC-(83865|25897)\]' err; then cat err >&2; exit 1; fi

# Change the copied field while preserving the ordinary type of the builder.
cd ../producer
sed -i 's/^left n = .*/left n = (n + 1, n)/' Builder.hs
"${ghc[@]}" "${ways[@]}" -c Builder.hs
cp Builder.hi Builder.o ../interfaces/
if [[ -f Builder.dyn_o ]]; then cp Builder.dyn_hi Builder.dyn_o ../interfaces/; fi
cd ../interfaces
"${ghc[@]}" -i -i. -c Alias.hs
cp Alias.hi Builder.hi ../client/
cd ../client
if "${ghc[@]}" -i -i. -c Proof.hs >err 2>&1 ||
   ! grep -Eq 'Proof.hs:6:.*error: \[GHC-(83865|25897)\]' err ||
   ! grep -Fq "Couldn't match" err; then cat err >&2; exit 1; fi
