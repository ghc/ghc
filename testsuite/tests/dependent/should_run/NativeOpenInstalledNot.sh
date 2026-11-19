#!/usr/bin/env bash
set -euo pipefail
src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
ghc=("$1" "-B$2" -O0 -v0 -package-env - -fomit-interface-pragmas
     -fno-write-if-simplified-core -dcore-lint -dstg-lint -fdiagnostics-color=never)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
mkdir "$out/db" "$out/interfaces"
source "$src/NativeOpenInstalledInterfaces.sh" "$2" "$3"
cd "$out"
cp "$src"/{OpenMapWorker,InstalledNotSource,InstalledNotProof}.hs .
"${ghc[@]}" -c OpenMapWorker.hs
rm OpenMapWorker.hs OpenMapWorker.o
"${ghc[@]}" "${db[@]}" -c InstalledNotSource.hs
rm InstalledNotSource.hs InstalledNotSource.o
"${ghc[@]}" "${db[@]}" -c InstalledNotProof.hs

# Import an orphan rule through an ordinary re-export.
cat >Override.hs <<'HS'
module Override () where
import qualified Prelude as P
{-# RULES "not/override" forall x. P.not x = P.True #-}
HS
cat >Bridge.hs <<'HS'
module Bridge (NegateAll) where
import Override ()
import InstalledNotSource (NegateAll)
HS
rules=(-fenable-rewrite-rules -fno-omit-interface-pragmas -fno-ignore-interface-pragmas
       -Wno-orphans -Wno-inline-rule-shadowing)
"${ghc[@]}" "${db[@]}" "${rules[@]}" -c Override.hs Bridge.hs
rm Override.o Bridge.o

# Check that the imported rule fires in an ordinary call in the same environment.
cat >Call.hs <<'HS'
module Call where
import Bridge ()
import qualified Prelude as P
witness :: P.Bool
witness = P.not P.True
HS
"${ghc[@]}" "${db[@]}" -O1 "${rules[@]}" -ddump-rule-firings -c Call.hs >fired.log
grep -Fq 'Rule fired: not/override' fired.log

cat >Empty.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
module Empty where
import Bridge (NegateAll)
import Data.Type.Equality ((:~:)(Refl))
empty :: NegateAll '[] :~: '[]
empty = Refl
HS
"${ghc[@]}" "${db[@]}" "${rules[@]}" -c Empty.hs
sed -e 's/module InstalledNotProof/module Ruled/' -e 's/import InstalledNotSource/import Bridge/' \
    InstalledNotProof.hs >Ruled.hs
if "${ghc[@]}" "${db[@]}" "${rules[@]}" -c Ruled.hs >rule.log 2>&1 ||
   ! grep -Fq '[GHC-83605]' rule.log || ! grep -Fq 'not' rule.log; then
    cat rule.log >&2
    exit 1
fi
