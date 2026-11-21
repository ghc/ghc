#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
ghc=$(realpath "$1")
lib=$(realpath "$2")
src=$(cd "$(dirname "$0")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
flags=("-B$lib" -v0 -i -i. -package-env - -fforce-recomp -dcore-lint -dstg-lint -fdiagnostics-color=never)
mkdir "$out/provider" "$out/client"
cp "$src/VisibleRuleArguments.hs" "$out/provider/"
cp "$src/VisibleRuleClient.hs" "$out/client/"
cat >"$out/client/OtherRequired.hs" <<'HS'
{-# LANGUAGE ExplicitNamespaces, NoExperimentalDependentTypes, RequiredTypeArguments #-}
module OtherRequired (readRequiredOther) where
import VisibleRuleArguments (TaggedReading(..), keepRequired)

readRequiredOther :: Bool -> Maybe Char -> Int -> Int
readRequiredOther tag m value =
    case keepRequired tag (type Char) m (TaggedReading value) of TaggedReading n -> n
HS
cat >"$out/client/Main.hs" <<'HS'
module Main where
import Data.IORef
import OtherRequired
import VisibleRuleClient
main :: IO ()
main = do
    m <- newIORef (Just True) >>= readIORef
    bottom <- newIORef (error "forced reading index" :: Maybe Bool) >>= readIORef
    other <- newIORef (error "forced Char reading index" :: Maybe Char) >>= readIORef
    ignored <- newIORef (error "forced first argument") >>= readIORef
    print (readAt m 37, readAt bottom 19, readAfter ignored m 43, readTagged ignored bottom 47,
           readRequired ignored bottom 53, readRequiredOther ignored other 59)
HS

for opt in 0 2; do
    cd "$out/provider"
    "$ghc" "${flags[@]}" -O"$opt" -c VisibleRuleArguments.hs
    cp VisibleRuleArguments.hi ../client/
    cd "$out/client"
    "$ghc" "${flags[@]}" -O"$opt" -ddump-rule-firings -c VisibleRuleClient.hs >firings
    "$ghc" "${flags[@]}" -O"$opt" -ddump-rule-firings -ddump-simpl -dsuppress-all -c OtherRequired.hs >other
    if [[ $opt == 2 ]]; then
        grep -Fq 'Rule fired: keepRequired/Bool (VisibleRuleArguments)' firings || { cat firings; exit 1; }
        grep -Fq 'Rule fired: keepTagged/Bool (VisibleRuleArguments)' firings || { cat firings; exit 1; }
        grep -Fq 'Rule fired: keepAfter/Bool (VisibleRuleArguments)' firings || { cat firings; exit 1; }
        grep -Fq 'Rule fired: keepReading/Bool (VisibleRuleArguments)' firings || { cat firings; exit 1; }
        if grep -Fq 'Rule fired: keepRequired/Bool' other || ! grep -Ewq 'keepRequired[[:digit:]]*' other; then
            cat other >&2
            exit 1
        fi
    fi
    "$ghc" "${flags[@]}" -O"$opt" -c Main.hs
    "$ghc" "-B$lib" Main.o OtherRequired.o VisibleRuleClient.o ../provider/VisibleRuleArguments.o -o observe
    [[ $(./observe) == '(37,19,43,47,53,59)' ]]
done

cd "$out/provider"
for rule in 'forall (m :: Maybe a) x. keepReading @a m x = x' \
            'forall (m :: Maybe Bool) x. keepReading @_ m x = x' \
            'forall (a :: Type). forall m x. keepReading @(Maybe a) m x = x'; do
    sed "s/forall m x. keepReading @Bool m x = x/$rule/" "$src/VisibleRuleArguments.hs" >VisibleRuleArguments.hs
    "$ghc" "${flags[@]}" -O2 -c VisibleRuleArguments.hs
done

# The prefix uses an earlier runtime Id in a noninjective family argument.
# A second specified parameter must keep its own instantiation.
cat >ScopedRule.hs <<'HS'
{-# LANGUAGE AllowAmbiguousTypes, DataKinds, ExperimentalDependentTypes, PolyKinds, ScopedTypeVariables, TypeAbstractions, TypeFamilies #-}
module ScopedRule where
import Data.Kind (Type)
import Data.Proxy (Proxy)
type family Payload a c (b :: Bool) :: Type
{-# NOINLINE keepAt #-}
keepAt :: forall (a :: Type) (c :: Type). foreach (b :: Bool) -> Payload a c b -> Payload a c b
keepAt _ x = x
{-# RULES "scoped" forall (b :: Bool) x. keepAt @(Proxy b) @Char b x = x #-}
{-# NOINLINE keepBetween #-}
keepBetween :: forall a. Proxy a -> forall c. foreach (b :: Bool) -> Payload a c b -> Payload a c b
keepBetween _ @c = keepAt @a @c
{-# RULES "between" forall p b x. keepBetween @Int p @Char b x = x #-}
HS
"$ghc" "${flags[@]}" -O2 -fwrite-ide-info -c ScopedRule.hs

reject() {
    local rule=$1 diagnostic=$2
    sed "s/forall m x. keepReading @Bool m x = x/$rule/" "$src/VisibleRuleArguments.hs" >VisibleRuleArguments.hs
    if "$ghc" "${flags[@]}" -O2 -c VisibleRuleArguments.hs >bad.log 2>&1 ||
       ! grep -Fq "$diagnostic" bad.log; then cat bad.log; exit 1; fi
}
reject 'forall x m. keepReading @Bool m x = x' '[GHC-25897]'
reject 'forall m x. keepReading @Maybe m x = x' '[GHC-83865]'

# The written type argument comes after a value operand.
sed 's/keepAfter ignored @Bool m x = x/keepAfter ignored @Maybe m x = x/' \
    "$src/VisibleRuleArguments.hs" >VisibleRuleArguments.hs
if "$ghc" "${flags[@]}" -O2 -c VisibleRuleArguments.hs >bad.log 2>&1 ||
   ! grep -Fq 'Expecting one more argument' bad.log ||
   ! grep -Fq 'keepAfter ignored @Maybe m x' bad.log; then
    cat bad.log >&2
    exit 1
fi

cat >Independent.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, PolyKinds, ScopedTypeVariables, TypeAbstractions #-}
module Independent where

data Reading (m :: Maybe a) = Reading Int

keepTwo :: forall a. foreach (m :: Maybe a) -> Maybe a -> Reading m -> Reading m
keepTwo _ _ x = x

{-# NOINLINE keepAfter #-}
keepAfter :: Bool -> forall a. foreach (m :: Maybe a) -> Maybe a -> Reading m -> Reading m
keepAfter _ @a = keepTwo @a

{-# RULES "independent" forall b m (other :: Maybe Bool) (x :: Reading other).
    keepAfter b @Bool m other x = x #-}
HS
if "$ghc" "${flags[@]}" -O2 -c Independent.hs >bad.log 2>&1 ||
   ! grep -Fq 'Expected: Reading m' bad.log ||
   ! grep -Fq 'Actual: Reading other' bad.log ||
   ! grep -Fq 'In the fifth argument' bad.log; then
    cat bad.log >&2
    exit 1
fi
sed 's/x :: Reading other/x :: Reading m/' Independent.hs >SameIndex.hs
"$ghc" "${flags[@]}" -O2 -c SameIndex.hs

sed 's/"keepRequired\/Bool" forall tag m x\./"keepRequired\/Bool" forall tag (m :: Maybe Bool) (x :: TaggedReading tag m)./' \
    "$src/VisibleRuleArguments.hs" >VisibleRuleArguments.hs
"$ghc" "${flags[@]}" -O2 -c VisibleRuleArguments.hs

cp "$src/VisibleRuleArguments.hs" WrongRequiredIndex.hs
cat >>WrongRequiredIndex.hs <<'HS'
{-# NOINLINE independent #-}
independent :: foreach (left :: Bool) -> Bool -> forall (a :: Type) ->
               foreach (m :: Maybe a) -> TaggedReading left m -> TaggedReading left m
independent left _ (type a) m x = keepRequired left (type a) m x

{-# RULES "independent" forall left (right :: Bool) m x.
    independent left right (type Bool) m x = keepRequired right (type Bool) m x #-}
HS
if "$ghc" "${flags[@]}" -O2 -c WrongRequiredIndex.hs >bad.log 2>&1 ||
   ! grep -Fq 'Expected: TaggedReading left m' bad.log ||
   ! grep -Fq 'Actual: TaggedReading right m' bad.log ||
   ! grep -Fq 'When checking the rewrite rule "independent"' bad.log; then
    cat bad.log >&2
    exit 1
fi

cd "$out/client"
cat >WrongRequired.hs <<'HS'
{-# LANGUAGE CPP, ExplicitNamespaces, NoExperimentalDependentTypes, RequiredTypeArguments, TypeApplications #-}
module WrongRequired where
import VisibleRuleArguments

#if CASE == 4
ordinary :: forall a. Bool -> forall b -> (a, b) -> Int
ordinary _ (type b) _ = 0
#endif

bad :: Bool -> Maybe Bool -> Int
#if CASE == 1
bad tag m = case keepRequired tag m (TaggedReading 0) of TaggedReading n -> n
#elif CASE == 2
bad tag m = case keepRequired tag (type Bool) m (TaggedReading 0) True of TaggedReading n -> n
#elif CASE == 3
bad tag m = case keepRequired tag @Bool m (TaggedReading 0) of TaggedReading n -> n
#else
bad tag _ = ordinary @Int tag (type Bool) (0, True) False
#endif
HS
for n in 1 2 3 4; do
    if "$ghc" "${flags[@]}" "-DCASE=$n" -c WrongRequired.hs >bad.log 2>&1; then
        echo "Accepted malformed required argument case $n" >&2
        exit 1
    fi
    case $n in
        1) grep -Fq '[GHC-45510]' bad.log || { cat bad.log; exit 1; } ;;
        2) grep -Fq 'applied to five visible arguments' bad.log &&
           grep -Fq 'has only four' bad.log || { cat bad.log; exit 1; } ;;
        3) grep -Fq '[GHC-95781]' bad.log || { cat bad.log; exit 1; } ;;
        4) grep -Fq 'applied to four visible arguments' bad.log &&
           grep -Fq 'has only three' bad.log || { cat bad.log; exit 1; } ;;
    esac
done
