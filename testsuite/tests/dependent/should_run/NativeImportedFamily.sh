#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
ghc=("$1" "-B$2" -v0 -O0 -i -i. -package-env - -dcore-lint -dstg-lint
     -fdiagnostics-color=never)
shift 2
ghc+=("$@")
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
mkdir "$out"/{producer,client}
cd "$out/producer"
cp "$src"/{ImportedFamilyProvider,ImportedFamily,MixedFamily,FamilySignature}.hs .
"${ghc[@]}" -c ImportedFamilyProvider.hs ImportedFamily.hs MixedFamily.hs FamilySignature.hs

"${ghc[@]}" --show-iface MixedFamily.hi >mixed.iface
grep -Fq 'type role Envelope nominal' mixed.iface

# The consumer checks the producer interfaces before linking their objects.
cp ./*.hi ../client/
cd ../client
cat >Client.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module Main where
import ImportedFamily (importedFamilyResults)
import FamilySignature (observed, directObserved)
import MixedFamily (Stored(..), mixedResult)
main :: IO ()
main = case mixedResult of
    Stored True | observed && directObserved -> print (importedFamilyResults :: (Bool, Ordering, Char, Maybe Bool, Ordering))
    Stored True -> fail "expected a True payload from the signed producer"
    Stored False -> fail "expected a True payload"
HS
"${ghc[@]}" -c Client.hs
"${ghc[@]}" Client.o ../producer/{ImportedFamily,ImportedFamilyProvider,MixedFamily,FamilySignature}.o -o client
./client

expect_cycle() {
    local mod=$1 site
    site=$(awk -v call="$mod.keep" '/^type instance/ { print NR ":" index($0, call); exit }' "$mod.hs")
    if "${ghc[@]}" -c "$mod.hs" >cycle.log 2>&1 ||
       ! grep -Eq "^$mod\.hs:$site(-[0-9]+)?: error: \[GHC-70566\]" cycle.log ||
       ! grep -Fq 'This type instance requires the unfinished local function keep' cycle.log; then
        cat cycle.log >&2
        exit 1
    fi
}

cat >BlindCycle.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, TypeFamilies #-}
module BlindCycle where
import Data.Proxy (Proxy(..))
import ImportedFamilyProvider (Payload, hidden)
type instance Payload Int = BlindCycle.keep Bool
keep :: forall a. a -> a
keep x = const x (hidden Proxy)
HS
expect_cycle BlindCycle

sed -e 's/BlindCycle/ErasedCycle/g' \
    -e '/^import Data.Proxy/d' -e 's/(Payload, hidden)/(Payload)/' \
    -e 's/(hidden Proxy)/(True :: Payload Int)/' BlindCycle.hs >ErasedCycle.hs
expect_cycle ErasedCycle

sed -e 's/ErasedCycle/AliasAnnotationCycle/g' \
    -e '/^keep ::/i type Dependency = Payload Int' \
    -e 's/True :: Payload Int/True :: Dependency/' ErasedCycle.hs >AliasAnnotationCycle.hs
expect_cycle AliasAnnotationCycle

sed -e 's/BlindCycle/NestedTypeCycle/g' -e '/^import Data.Proxy/d' \
    -e 's/(Payload, hidden)/(Payload, hiddenNested)/' \
    -e 's/(hidden Proxy)/(hiddenNested Nothing)/' BlindCycle.hs >NestedTypeCycle.hs
expect_cycle NestedTypeCycle

sed -e 's/NestedTypeCycle/UnsignedNestedTypeCycle/g' -e '/^keep ::/d' \
    NestedTypeCycle.hs >UnsignedNestedTypeCycle.hs
expect_cycle UnsignedNestedTypeCycle

# The Wrapped alternative needs this equation even though keep uses Envelope.
sed -e 's/MixedFamily/TransitiveFieldCycle/g' \
    -e 's/data Wrap a = Wrap a/data Wrap a = Wrap (Payload Double) a/' \
    ../producer/MixedFamily.hs >TransitiveFieldCycle.hs
expect_cycle TransitiveFieldCycle

# The only route to Wrap is through the helper signature's closed family.
sed -e '/^type instance Payload Word/,$d' -e 's/, directObserved//' \
    -e 's/FamilySignature/SignatureFieldCycle/g' \
    -e 's/data Wrap a = Wrap a/data Wrap a = Wrap (Payload Float)/' \
    ../producer/FamilySignature.hs >SignatureFieldCycle.hs
expect_cycle SignatureFieldCycle

# The standalone kind signature depends on the equation scheduled after keep.
cat >SignatureKindCycle.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, StandaloneKindSignatures, TypeFamilies #-}
module SignatureKindCycle where
import Data.Kind (Type)
import ImportedFamilyProvider (Payload)
type instance Payload Int = SignatureKindCycle.keep Bool
keep :: forall a. Input a -> a
keep x = x
type Input :: Payload Int -> Type
type family Input a where Input _ = Bool
HS
expect_cycle SignatureKindCycle

# The input to Discard still depends on the postponed equation.
cat >AliasDependencyCycle.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, TypeFamilies #-}
module AliasDependencyCycle where
import ImportedFamilyProvider (Payload)
type instance Payload Int = AliasDependencyCycle.keep Bool
keep :: forall a. Input a -> a
keep x = x
type Input a = Discard a (Payload Int)
type Discard a b = a
HS
expect_cycle AliasDependencyCycle

# A typed adapter still observes demand and records the RULE owner.
cat >StrictDirect.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, TypeFamilies #-}
module StrictDirect where
type family Payload a
type instance Payload Word = StrictDirect.direct Bool
type family Input a where
  Input a = a
direct :: forall a. Input a -> a
direct x = seq x x
HS
if "${ghc[@]}" -c StrictDirect.hs >strict-direct.log 2>&1 ||
   ! grep -Fq 'StrictDirect.hs:4:30: error: [GHC-70566]' strict-direct.log ||
   ! grep -Fq 'Reduction cannot inspect the value of a type operand.' strict-direct.log; then
    cat strict-direct.log >&2
    exit 1
fi

sed -e '1s/TypeFamilies/TypeFamilies, TypeApplications/' \
    -e 's/StrictDirect/RuledDirect/g' \
    -e '/^module /a import Data.Kind (Type)' \
    -e 's/direct x = seq x x/direct x = x/' StrictDirect.hs >RuledDirect.hs
cat >>RuledDirect.hs <<'HS'
{-# RULES "dependent/direct" forall (x :: Input Type). direct @Type x = x #-}
HS
if "${ghc[@]}" -fenable-rewrite-rules -Wno-inline-rule-shadowing -c RuledDirect.hs >rule-direct.log 2>&1 ||
   ! grep -Fq 'RuledDirect.hs:10:11: error: [GHC-83605]' rule-direct.log ||
   ! grep -Fq 'direct @Type' rule-direct.log; then
    cat rule-direct.log >&2
    exit 1
fi

cat >RecursiveProducer.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, TypeFamilies #-}
module RecursiveProducer where
import ImportedFamilyProvider (Payload)
type instance Payload Int = RecursiveProducer.keep Bool
keep :: forall a. a -> a
keep x = const x (helper x)
helper :: forall a. a -> a
helper x = keep x
HS
expect_cycle RecursiveProducer

cat >StrictImport.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, TypeFamilies #-}
module StrictImport where
import ImportedFamilyProvider (Payload, importedStrict)
type instance Payload Int = StrictImport.keep Bool Char
keep :: forall a b. a -> b -> b
keep x y = importedStrict x y
HS
if "${ghc[@]}" -c StrictImport.hs >strict.log 2>&1 ||
   ! grep -Eq '^StrictImport\.hs:4:[0-9]+(-[0-9]+)?: error: \[GHC-70566\]' strict.log ||
   ! grep -Fq 'Reduction cannot inspect the value of a type operand.' strict.log; then
    cat strict.log >&2
    exit 1
fi

sed -e 's/StrictImport/RuleImport/g' -e 's/importedStrict/importedFirst/g' \
    -e 's/forall a b. a -> b -> b/forall a. a -> a -> a/' StrictImport.hs > RuleImport.hs
cat >>RuleImport.hs <<'HS'
{-# RULES "imported-family/change" forall x y. importedFirst x y = y `seq` x #-}
HS
if "${ghc[@]}" -fenable-rewrite-rules -Wno-inline-rule-shadowing -c RuleImport.hs >rule.log 2>&1 ||
   ! grep -Eq '^RuleImport\.hs:7:[0-9]+(-[0-9]+)?: error: \[GHC-83605\]' rule.log ||
   ! grep -Fq 'importedFirst' rule.log; then
    cat rule.log >&2
    exit 1
fi

# A hidden dictionary is a genuine dependency even when observation ignores it.
cat >Dictionary.hs <<'HS'
{-# LANGUAGE CPP, TypeFamilies #-}
module Main where
import Data.Proxy (Proxy(..))
import ImportedFamilyProvider (Payload, hidden)
#ifdef WITH_EQUATION
type instance Payload Int = Bool
#endif
keep :: a -> a
keep x = const x (hidden Proxy)
choose :: a -> a -> a
choose x y = if hidden Proxy then x else y
main :: IO ()
main = print (keep False, choose True False)
HS
"${ghc[@]}" -c Dictionary.hs
"${ghc[@]}" Dictionary.o ../producer/ImportedFamilyProvider.o -o without
"${ghc[@]}" -DWITH_EQUATION -fforce-recomp -c Dictionary.hs
"${ghc[@]}" Dictionary.o ../producer/ImportedFamilyProvider.o -o with
without=$(./without)
with=$(./with)
if [[ $without != '(False,False)' || $with != '(False,True)' ]]; then
    printf 'Dictionary control produced %s without the equation and %s with it\n' "$without" "$with" >&2
    exit 1
fi

# An independent local producer can supply an associated equation before
# the normal instance pass checks the method.
mkdir "$out"/{associated-provider,associated-reader}
cat > "$out/associated-reader/Main.hs" <<'HS'
{-# LANGUAGE GADTs, NoExperimentalDependentTypes, TypeFamilies, TypeOperators #-}
module Main where
import AssociatedLocal
import Data.Type.Equality ((:~:) (Refl))

result :: Payload Token :~: Bool
result = Refl

main :: IO ()
main = case result of Refl -> print (value Token :: Bool)
HS
(
    cd "$out/associated-provider"
    cat > header <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, TypeFamilies #-}
module AssociatedLocal where
class Has t where
    type Payload t
    value :: t -> Payload t

data Token = Token
HS
    cat > worker <<'HS'
keep :: a -> a
keep x = x
HS
    cat > instance <<'HS'
instance Has Token where
    type Payload Token = AssociatedLocal.keep Bool
    value _ = True
HS
    for order in worker-first equation-first; do
        if [[ $order == worker-first ]]; then
            cat header worker instance > AssociatedLocal.hs
        else
            cat header instance worker > AssociatedLocal.hs
        fi
        "${ghc[@]}" -fforce-recomp -fomit-interface-pragmas             -fno-write-if-simplified-core -c AssociatedLocal.hs
        cp AssociatedLocal.hi "$out/associated-reader/"
        ( cd "$out/associated-reader"
          "${ghc[@]}" -fforce-recomp -c Main.hs
          "${ghc[@]}" Main.o ../associated-provider/AssociatedLocal.o -o read-associated
          test "$(./read-associated)" = True )
    done

    # Typechecking this producer would require the instance it supplies.
    sed -i 's/^keep x = x$/keep x = const x (value Token)/' AssociatedLocal.hs
    site=$(awk '/^    type Payload Token/ {print NR ":" index($0, "AssociatedLocal.keep")}' AssociatedLocal.hs)
    if "${ghc[@]}" -fforce-recomp -c AssociatedLocal.hs > cycle.log 2>&1 ||
       ! grep -Eq "^AssociatedLocal\.hs:$site(-[0-9]+)?: error: \[GHC-70566\]" cycle.log ||
       ! grep -Fq 'An associated type equation requires the unfinished local function keep.' cycle.log; then
        cat cycle.log >&2
        exit 1
    fi
)

# With the same signature, the provider can choose the other opaque type.
cd ../producer
sed 's/^importedFirst x _ = x$/importedFirst _ y = y/' ImportedFamilyProvider.hs > changed
mv changed ImportedFamilyProvider.hs
"${ghc[@]}" -fforce-recomp -c ImportedFamilyProvider.hs
line=$(awk '/^  observed _ = True$/ {print NR}' ImportedFamily.hs)
if "${ghc[@]}" -fforce-recomp -c ImportedFamily.hs >changed.log 2>&1 ||
   ! grep -Eq "^ImportedFamily\.hs:$line:[0-9]+(-[0-9]+)?: error: \[GHC-(83865|18872)\]" changed.log ||
   ! grep -Fq "Couldn't match" changed.log || ! grep -Fq 'Char' changed.log; then
    cat changed.log >&2
    exit 1
fi
