#!/usr/bin/env bash
set -euo pipefail

ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint
     -fdiagnostics-color=never)
shift 2
ghc+=("$@")
src=$PWD
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
mkdir "$out"/{exporter,client,producer}

# An optimized module can export a type alias without using an equation.
cd "$out/exporter"
cp "$src/PreludeMapSource.hs" .
"${ghc[@]}" -O1 -fenable-rewrite-rules -c PreludeMapSource.hs
cd "$out/client"
cp ../exporter/PreludeMapSource.hi "$src/PreludeMapProof.hs" .

# Only the alias interface is present, and the client has no dependent syntax.
# The module pragma keeps the proof valid in an optimized build with rules on.
"${ghc[@]}" -O1 -fenable-rewrite-rules -c PreludeMapProof.hs
# Remove only the pragma in the negative control and keep the source spans.
sed 's/^{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}//' PreludeMapProof.hs >Proof.tmp
mv Proof.tmp PreludeMapProof.hs
line=$(awk '/^preludeNil =/ { print NR }' PreludeMapProof.hs)
if "${ghc[@]}" -O0 -fenable-rewrite-rules -fforce-recomp -c PreludeMapProof.hs >rules.log 2>&1 ||
   ! grep -Fq "PreludeMapProof.hs:$line:" rules.log ||
   ! grep -Fq '[GHC-83605]' rules.log ||
   ! grep -Fq 'with the current rewrite rules' rules.log ||
   ! grep -Fq 'Use -fno-enable-rewrite-rules in the module that needs this equation.' rules.log; then
    cat rules.log >&2
    exit 1
fi

# A no-code build must not issue final-Core provenance. An ordinary producer
# can publish the certificate, or withdraw it and compile after a real rule.
cd "$out/producer"
cp "$src/NativeOpenInstalledMapProducer.hs" .
producer=("${ghc[@]}" -O1 -fforce-recomp -c NativeOpenInstalledMapProducer.hs)
interface=NativeOpenInstalledMapProducer.hi
"${producer[@]}" -fno-code -fwrite-interface
"${ghc[@]}" --show-iface "$interface" >producer.iface
if grep -Eq 'FinalCoreSourceTable|T:mapper' producer.iface; then
    exit 1
fi
"${producer[@]}"
"${ghc[@]}" --show-iface "$interface" >producer.iface
grep -Fq 'FinalCoreSourceTable T:mapper' producer.iface
"${producer[@]}" -DCHANGE_MAPPER -ddump-rule-firings >producer.rules
grep -Fq 'Rule fired: drop mapper traversal' producer.rules
"${ghc[@]}" --show-iface "$interface" >producer.iface
if grep -Eq 'FinalCoreSourceTable|T:mapper' producer.iface; then
    exit 1
fi

# A user rule may also fire during specialisation. Disabling rules must
# protect both the ordinary runtime call and a fresh source equality.
mkdir "$out"/{specialise,rewrite}
cd "$out/specialise"
cp "$src/NativeOpenInstalledMapProducer.hs" .
cat >PreludeMapSource.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, StandaloneKindSignatures #-}
module PreludeMapSource (PreludeNegateAll) where
import qualified NativeOpenInstalledMapProducer as W
import qualified Prelude as P
type PreludeNegateAll :: [P.Bool] -> [P.Bool]
type PreludeNegateAll xs = W.mapper P.not xs
HS
cat >Orphan.hs <<'HS'
{-# OPTIONS_GHC -Wno-orphans -Wno-inline-rule-shadowing #-}
module Orphan () where
import qualified NativeOpenInstalledMapProducer as W
{-# RULES "override demanded negation" [~1] forall xs. W.mapper not xs = xs #-}
HS
cat >Main.hs <<'HS'
{-# LANGUAGE CPP, DataKinds, GADTs, NoExperimentalDependentTypes, TypeOperators #-}
module Main where
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import qualified NativeOpenInstalledMapProducer as W
import Orphan ()
import PreludeMapSource

#ifdef CHECK_PROOF
proof :: Proxy xs -> PreludeNegateAll ('True ': xs) :~: ('False ': PreludeNegateAll xs)
proof _ = Refl
#endif

{-# NOINLINE demanded #-}
demanded :: [Bool] -> Bool
demanded xs = case W.mapper not xs of
    x : _ -> x
    [] -> error "empty"

main :: IO ()
#ifdef CHECK_PROOF
main = case proof (Proxy :: Proxy '[]) of Refl -> print (False, demanded [True])
#else
main = print (demanded [True])
#endif
HS
cp ./*.hs "$out/rewrite/"
"${ghc[@]}" -O1 -fno-enable-rewrite-rules -DCHECK_PROOF --make Main.hs -o run
[[ $(./run) == '(False,False)' ]]
cd "$out/rewrite"
"${ghc[@]}" -O1 -fenable-rewrite-rules -ddump-rule-firings --make Main.hs -o run >fired.log
grep -Fq 'Rule fired: override demanded negation' fired.log
[[ $(./run) == True ]]
line=$(awk '/^proof _ = Refl/ { print NR }' Main.hs)
if "${ghc[@]}" -O1 -fenable-rewrite-rules -DCHECK_PROOF -fforce-recomp -c Main.hs >orphan.log 2>&1 ||
   ! grep -Fq "Main.hs:$line:" orphan.log ||
   ! grep -Fq '[GHC-83605]' orphan.log; then
    cat orphan.log >&2
    exit 1
fi

# A checked identity rule admits an optimized proof. Adding an orphan
# to a previously imported module invalidates the unchanged proof.
mkdir "$out/identity"
cd "$out/identity"
cp "$out/rewrite/"*.hs .
printf 'module Orphan () where\n' >Orphan.hs
identity=("${ghc[@]}" -O1 -fenable-rewrite-rules -DCHECK_PROOF)
"${identity[@]}" --make Main.hs -o run
[[ $(./run) == '(False,False)' ]]
"${identity[@]}" -v1 --make Main.hs -o run >warm.log 2>&1
if grep -Fq 'Compiling ' warm.log; then cat warm.log >&2; exit 1; fi
cp "$out/rewrite/Orphan.hs" Orphan.hs
if "${identity[@]}" -v1 --make Main.hs -o run >changed.log 2>&1 ||
   ! grep -Fq 'Compiling Main' changed.log ||
   ! grep -Fq '[Orphan changed]' changed.log ||
   ! grep -Fq "Main.hs:$line:" changed.log ||
   ! grep -Fq '[GHC-83605]' changed.log; then
    cat changed.log >&2
    exit 1
fi

# A rule can expose the checked cons equation with an opaque callback.
# The ordinary proof client checks the head with an unknown tail.
mkdir "$out"/{cons,cons-client}
cd "$out/cons"
cp "$out/identity/PreludeMapSource.hs" "$src/NativeOpenInstalledMapProducer.hs" .
cat >Cons.hs <<'HS'
module Main where
import Control.Exception (SomeException, evaluate, try)
import qualified NativeOpenInstalledMapProducer as W

{-# NOINLINE observe #-}
observe :: (Bool -> Bool) -> Bool -> [Bool] -> Bool
observe f x xs = case W.mapper f (x : xs) of
    _ : _ : _ -> True
    _ -> False

main :: IO ()
main = do
    result <- try (evaluate (observe undefined undefined [True]))
    print (either (const "forced") show (result :: Either SomeException Bool))
HS
cons=("${ghc[@]}" -O1 -fenable-rewrite-rules -DCONS_RULE)
"${cons[@]}" -c NativeOpenInstalledMapProducer.hs
"${cons[@]}" -c PreludeMapSource.hs
"${cons[@]}" --make Cons.hs -ddump-rule-firings -o run >cons.log
grep -Fq 'Rule fired: mapper/cons' cons.log
[[ $(./run) == '"True"' ]]
"${ghc[@]}" --show-iface NativeOpenInstalledMapProducer.hi >cons.iface
grep -Fq 'FinalCoreSourceTable T:mapper' cons.iface
grep -Fq '"mapper/cons"' cons.iface

cd "$out/cons-client"
cp ../cons/{NativeOpenInstalledMapProducer,PreludeMapSource}.hi .
cat >ConsProof.hs <<'HS'
{-# LANGUAGE CPP, DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
module ConsProof where
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import PreludeMapSource (PreludeNegateAll)
#ifdef SECOND_RULE
import Orphan ()
#endif

proof :: Proxy xs -> PreludeNegateAll ('True ': xs) :~: ('False ': PreludeNegateAll xs)
proof _ = Refl
HS
proof=("${ghc[@]}" -O1 -fenable-rewrite-rules -c ConsProof.hs)
"${proof[@]}" >proof.log 2>&1 || { cat proof.log >&2; exit 1; }
line=$(awk '/^proof _ = Refl/ { print NR }' ConsProof.hs)

# A checked early rule does not justify a second rule in a later phase.
sed 's/\[~1\]/[1]/' "$out/rewrite/Orphan.hs" >Orphan.hs
"${ghc[@]}" -O1 -fenable-rewrite-rules -c Orphan.hs
if "${proof[@]}" -DSECOND_RULE >orphan.log 2>&1 ||
   ! grep -Fq "ConsProof.hs:$line:" orphan.log ||
   ! grep -Fq '[GHC-83605]' orphan.log; then
    cat orphan.log >&2
    exit 1
fi

for control in FORCE_HEAD DROP_TAIL; do
    cd "$out/cons"
    "${cons[@]}" -D"$control" -fforce-recomp -c NativeOpenInstalledMapProducer.hs
    "${cons[@]}" -D"$control" --make Cons.hs -ddump-rule-firings -o run >cons.log
    grep -Fq 'Rule fired: mapper/cons' cons.log
    case "$control" in
        FORCE_HEAD) [[ $(./run) == '"forced"' ]] ;;
        DROP_TAIL) [[ $(./run) == '"False"' ]] ;;
    esac
    cd "$out/cons-client"
    cp ../cons/NativeOpenInstalledMapProducer.hi .
    if "${proof[@]}" >changed.log 2>&1 ||
       ! grep -Fq "ConsProof.hs:$line:" changed.log ||
       ! grep -Fq '[GHC-83605]' changed.log; then
        cat changed.log >&2
        exit 1
    fi
done
