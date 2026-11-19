#!/usr/bin/env bash
set -euo pipefail

src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
ghc=("$1" "-B$2" -O0 -v0 -package-env - -dcore-lint -fdiagnostics-color=never)
shift 2
ghc+=("$@")
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
mkdir "$out"/{producer,client}
cd "$out/producer"
cp "$src/NativeLocalOpenProducer.hs" .
"${ghc[@]}" -c NativeLocalOpenProducer.hs
cp NativeLocalOpenProducer.hi ../client/
cd ../client
cp "$src/NativeLocalOpen.hs" .
"${ghc[@]}" -c NativeLocalOpen.hs
"${ghc[@]}" NativeLocalOpen.o ../producer/NativeLocalOpenProducer.o -o client
./client

cd ../producer
sed 's/^select x _ = x$/select _ y = y/' NativeLocalOpenProducer.hs >changed
mv changed NativeLocalOpenProducer.hs
"${ghc[@]}" -c NativeLocalOpenProducer.hs
cp NativeLocalOpenProducer.hi ../client/
cd ../client
if "${ghc[@]}" -c NativeLocalOpen.hs >changed.log 2>&1 ||
   ! grep -Fq 'GHC-83865' changed.log || ! grep -Fq 'backward = True' changed.log; then
    cat changed.log >&2
    exit 1
fi

cd ../producer
expect_rejection() {
    local mod=$1 line=$2 code=$3 message=$4
    if "${ghc[@]}" -fforce-recomp -c "$mod.hs" > reject.log 2>&1 ||
       ! grep -Eq "^$mod\\.hs:$line:[0-9]+(-[0-9]+)?: error:" reject.log ||
       ! grep -Eq "$code" reject.log ||
       ! grep -Fq "$message" reject.log; then
        cat reject.log >&2
        exit 1
    fi
}

cat > NativeLocalOpenNames.hs <<'SOURCE'
{-# LANGUAGE ExperimentalDependentTypes, ExplicitForAll, TypeApplications #-}
module NativeLocalOpenNames where
pick x = x
explicit :: forall a. a -> pick a
explicit x = x
implicit :: pick a -> pick a
implicit x = x
twoBinders :: Maybe Bool
twoBinders = implicit @Maybe @Bool (Just True)
SOURCE
"${ghc[@]}" -c NativeLocalOpenNames.hs

cat > NativeLocalOpenCycle.hs <<'SOURCE'
{-# LANGUAGE ExperimentalDependentTypes, StandaloneKindSignatures #-}
module NativeLocalOpenCycle where
import Data.Kind (Type)
type Result :: Type -> Type
type Result a = NativeLocalOpenCycle.keep a
keep :: a -> Result a
keep x = x
SOURCE
expect_rejection NativeLocalOpenCycle 5 '\[GHC-70566\]' 'dependency cycle'

cat > NativeLocalOpenTermCycle.hs <<'SOURCE'
{-# LANGUAGE ExperimentalDependentTypes, StandaloneKindSignatures #-}
module NativeLocalOpenTermCycle where
import Data.Kind (Type)
type Result :: Type -> Type
type Result a = NativeLocalOpenTermCycle.choose a
choose x = loop x
loop x = loop x
SOURCE
expect_rejection NativeLocalOpenTermCycle 5 '\[GHC-70566\]' 'Reduction exceeded the open evaluation step limit'

cat > NativeLocalOpenHelperType.hs <<'SOURCE'
{-# LANGUAGE ExperimentalDependentTypes, StandaloneKindSignatures #-}
module NativeLocalOpenHelperType where
import Data.Kind (Type)
type Result :: Type -> Type
type Result a = NativeLocalOpenHelperType.choose a
type Payload a = a
choose x = helper x
helper :: a -> Result a
helper x = x
SOURCE
expect_rejection NativeLocalOpenHelperType 5 '\[GHC-70566\]' 'dependency cycle through helper and Result'
sed 's/^helper :: a -> Result a$/helper :: a -> Payload a/' NativeLocalOpenHelperType.hs >changed
mv changed NativeLocalOpenHelperType.hs
"${ghc[@]}" -c NativeLocalOpenHelperType.hs
sed 's/^type Payload a = a$/type Payload a = Result a/' NativeLocalOpenHelperType.hs >changed
mv changed NativeLocalOpenHelperType.hs
expect_rejection NativeLocalOpenHelperType 5 '\[GHC-70566\]' 'dependency cycle through helper and Result'

cat > NativeLocalOpenChain.hs <<'SOURCE'
{-# LANGUAGE ExperimentalDependentTypes, StandaloneKindSignatures, TypeOperators #-}
module NativeLocalOpenChain where
import Data.Kind (Type)
import Data.Type.Equality
type Final :: Type -> Type
type Final a = NativeLocalOpenChain.outer a
outer :: a -> Middle a
outer x = x
type Middle :: Type -> Type
type Middle a = NativeLocalOpenChain.inner a
inner :: a -> a
inner x = via End x
data Stamp = End | More Side
data Side = Side Stamp
via :: Stamp -> a -> a
via _ x = x
proof :: Final Char :~: Char
proof = Refl
SOURCE
"${ghc[@]}" -c NativeLocalOpenChain.hs

cp "$src/NativeLocalOpenDerived.hs" .
"${ghc[@]}" -c NativeLocalOpenDerived.hs
cp NativeLocalOpenDerived.hi ../client/
(
    cd ../client
    cat > DerivedConsumer.hs <<'SOURCE'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module Main where
import GHC.Generics (from, to)
import NativeLocalOpenDerived
selectedAtBool :: Later Char Bool
selectedAtBool = to (from Dusk) > Noon
main :: IO ()
main = print (selectedAtBool, show (Ticket Dusk), label (Ticket Noon), earlyLabel (), lateLabel)
SOURCE
    "${ghc[@]}" -c DerivedConsumer.hs
    "${ghc[@]}" DerivedConsumer.o ../producer/NativeLocalOpenDerived.o -o derived
    test "$(./derived)" = '(True,"Ticket Dusk","Dusk","1.0","2.0")'
)

# The instance head is needed while checking keep. Its methods see both
# the first type synonym and the producer checked in the next source group.
cp "$src/LocalInstance.hs" "$src/LocalAssociated.hs" "$src/LocalDataFamily.hs" .
"${ghc[@]}" -c LocalInstance.hs LocalAssociated.hs LocalDataFamily.hs
cp LocalInstance.hi LocalAssociated.hi LocalDataFamily.hi ../client/
(
    cd ../client
    cat > InstanceConsumer.hs <<'SOURCE'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module Main where
import LocalInstance (passValue)
import LocalAssociated (result)
import qualified LocalDataFamily
main :: IO ()
main = print (passValue (7 :: Int), passValue True, result, LocalDataFamily.result)
SOURCE
    "${ghc[@]}" -c InstanceConsumer.hs
    "${ghc[@]}" InstanceConsumer.o ../producer/LocalInstance.o ../producer/LocalAssociated.o \
        ../producer/LocalDataFamily.o -o instance-consumer
    test "$(./instance-consumer)" = '(7,True,True,(True,False,"StampToken {marked = False}"))'
)

sed -e 's/LocalInstance/StrictInstance/g' \
    -e 's/const x (enabled IdentityPass)/if enabled IdentityPass then x else x/' \
    "$src/LocalInstance.hs" > StrictInstance.hs
line=$(awk '/^type Input a =/ {print NR}' StrictInstance.hs)
expect_rejection StrictInstance "$line" '\[GHC-70566\]' 'local class method whose definition is unavailable'

# An ignored method still has to typecheck. A later splice closes its group.
sed -e 's/LocalInstance/BadInstance/g' \
    -e "s/(True :: Input Bool)/'x'/" "$src/LocalInstance.hs" > BadInstance.hs
line=$(awk '/^    enabled _ =/ {print NR}' BadInstance.hs)
expect_rejection BadInstance "$line" '\[GHC-(83865|18872)\]' "Couldn't match"
sed -e 's/LocalInstance/SplicedInstance/g' \
    -e '/^passValue ::/i $(pure [])' "$src/LocalInstance.hs" > SplicedInstance.hs
"${ghc[@]}" -XTemplateHaskell -c SplicedInstance.hs

sed -e 's/LocalAssociated/EquationCycle/g' \
    -e 's/type Flag Token = Answer/type Flag Token = EquationCycle.keep Bool/' \
    "$src/LocalAssociated.hs" >EquationCycle.hs
line=$(awk '/^    type Flag Token =/ {print NR}' EquationCycle.hs)
expect_rejection EquationCycle "$line" '\[GHC-70566\]' 'unfinished local function keep'

# A family instance may need the very producer that uses its constructors.
for family in AssociatedData StandaloneData StandaloneType; do
    case "$family" in
        AssociatedData) pattern='marked :: Payload'; replacement='marked :: AssociatedData.keep Bool'
                        reason='An associated data instance' ;;
        StandaloneData) pattern='contents :: Flag Token'; replacement='contents :: StandaloneData.keep Bool'
                        reason='This data instance' ;;
        StandaloneType) pattern='type instance Flag Token = Payload'; replacement='type instance Flag Token = StandaloneType.keep Bool'
                        reason='This type instance' ;;
    esac
    sed -e "s/LocalDataFamily/$family/g" -e "s/$pattern/$replacement/" "$src/LocalDataFamily.hs" >"$family.hs"
    line=$(awk -v term="$family.keep Bool" 'index($0,term) {print NR}' "$family.hs")
    expect_rejection "$family" "$line" '\[GHC-70566\]' "$reason requires the unfinished local function keep"
    [[ $(grep -c 'error: \[GHC-70566\]' reject.log) == 1 ]]
done

cat > DerivingCycle.hs <<'SOURCE'
{-# LANGUAGE ExperimentalDependentTypes, StandaloneKindSignatures #-}
module DerivingCycle where
import Data.Kind (Type)
type Result :: Type -> Type
type Result a = DerivingCycle.choose a
data Payload = Payload (Result Bool) deriving Show
choose x = const x (show (undefined :: Payload))
SOURCE
expect_rejection DerivingCycle 5 '\[GHC-70566\]' 'whose declaration uses the local function choose'

cat > NativeLocalOpenStrict.hs <<'SOURCE'
{-# LANGUAGE BangPatterns, ExperimentalDependentTypes, StandaloneKindSignatures #-}
module NativeLocalOpenStrict where
import Data.Kind (Type)
type Result :: Type -> Type
type Result a = NativeLocalOpenStrict.keep a
keep :: a -> a
keep !x = x
SOURCE
expect_rejection NativeLocalOpenStrict 5 '\[GHC-70566\]' 'rigid argument a'

cat > NativeLocalOpenRules.hs <<'SOURCE'
{-# LANGUAGE ExperimentalDependentTypes, StandaloneKindSignatures #-}
{-# OPTIONS_GHC -fenable-rewrite-rules -Wno-inline-rule-shadowing #-}
module NativeLocalOpenRules where
import Data.Kind (Type)
type Selected :: Type -> Type -> Type
type Selected a b = NativeLocalOpenRules.choose a b
choose :: a -> a -> a
choose x y = select x (unused y)
select :: a -> a -> a
select x _ = x
unused :: a -> a
unused x = unused x
{-# RULES "unused/strict" forall x. unused x = x `seq` x #-}
SOURCE
"${ghc[@]}" -c NativeLocalOpenRules.hs
cat >>NativeLocalOpenRules.hs <<'SOURCE'
{-# RULES "select/second" forall x y. select x y = y #-}
SOURCE
expect_rejection NativeLocalOpenRules 14 '\[GHC-83605\]' 'select'

cp "$src/NativeLocalOpenSibling.hs" .
"${ghc[@]}" -c NativeLocalOpenSibling.hs
sed -e 's/^left x = keep x (right x)$/left x = right x/' \
    -e 's/^right x = left x$/right x = keep x (left x)/' NativeLocalOpenSibling.hs >changed
mv changed NativeLocalOpenSibling.hs
expect_rejection NativeLocalOpenSibling 10 '\[GHC-83605\]' 'right'

cat > NativeLocalOpenPriorHelper.hs <<'SOURCE'
{-# LANGUAGE ExperimentalDependentTypes, StandaloneKindSignatures, TemplateHaskell #-}
module NativeLocalOpenPriorHelper where
import Data.Kind (Type)
choose :: a -> a
choose x = helper x
helper :: a -> a
helper = $$([|| \x -> x ||])
$(pure [])
type Selected :: Type -> Type
type Selected a = NativeLocalOpenPriorHelper.choose a
SOURCE
expect_rejection NativeLocalOpenPriorHelper 10 '\[GHC-70566\]' 'pending typed splice'

cat > NativeLocalOpenSplice.hs <<'SOURCE'
{-# LANGUAGE ExperimentalDependentTypes, StandaloneKindSignatures, TemplateHaskell #-}
module NativeLocalOpenSplice where
import Data.Kind (Type)
type Result :: Type -> Type
type Result a = NativeLocalOpenSplice.keep a
$(pure [])
keep :: a -> a
keep x = x
SOURCE
expect_rejection NativeLocalOpenSplice 5 '\[GHC-76037\]' 'NativeLocalOpenSplice.keep'

cat > NativeLocalOpenStale.hs <<'SOURCE'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeOperators #-}
module NativeLocalOpenStale where
import Data.Type.Equality ((:~:)(Refl))
import Numeric.Natural (Natural)
size :: [Natural] -> Natural
size xs = fromIntegral (length xs)
SOURCE
"${ghc[@]}" -c NativeLocalOpenStale.hs
test -s NativeLocalOpenStale.o
cat >> NativeLocalOpenStale.hs <<'SOURCE'
proof :: NativeLocalOpenStale.size '[1,2] :~: 2
proof = Refl
SOURCE
expect_rejection NativeLocalOpenStale 7 '\[GHC-70565\]' \
    'Closed type evaluation requires an imported definition.'
