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

# A downstream rule can invalidate a proof obtained from an erased equation.
(
    mkdir "$out"/{rule-proof-provider,rule-proof-client}
    cd "$out/rule-proof-provider"
    cp "$src/NativeRuleProofProvider.hs" .
    "${ghc[@]}" -O2 -fno-enable-rewrite-rules -fomit-interface-pragmas \
        -fno-write-if-simplified-core -c NativeRuleProofProvider.hs

    cd "$out/rule-proof-client"
    cp ../rule-proof-provider/NativeRuleProofProvider.hi "$src/NativeRuleProofClient.hs" .
    provider=../rule-proof-provider/NativeRuleProofProvider.o
    for form in FAMILY ALIAS IMPORTED_PROOF; do
        expression='observation = Refl'
        if [[ $form == IMPORTED_PROOF ]]; then expression='print imageProof'; fi
        line=$(awk -v expression="$expression" 'index($0, expression) {print NR}' NativeRuleProofClient.hs)
        if "${ghc[@]}" -O2 -fenable-rewrite-rules -fforce-recomp "-D$form" \
            -c NativeRuleProofClient.hs >proof-rule.log 2>&1 ||
           ! grep -Fq "NativeRuleProofClient.hs:$line:" proof-rule.log ||
           ! grep -Fq '[GHC-83605]' proof-rule.log; then
            echo "Expected a checked rule diagnostic for $form" >&2
            cat proof-rule.log >&2
            exit 1
        fi

        "${ghc[@]}" -O2 -fno-enable-rewrite-rules -fforce-recomp "-D$form" -c NativeRuleProofClient.hs
        "${ghc[@]}" NativeRuleProofClient.o "$provider" -o rule-proof-client
        diff -u <(printf 'Refl\nJust False\n') <(./rule-proof-client)
    done

    "${ghc[@]}" -O2 -fenable-rewrite-rules -fforce-recomp -DOTHER -c NativeRuleProofClient.hs
    "${ghc[@]}" NativeRuleProofClient.o "$provider" -o rule-proof-client
    diff -u <(printf '42\nNothing\n') <(./rule-proof-client)
)

# An ordinary client uses the installed map and size equations with a recursive proof.
mkdir "$out"/{length-source,length-client}
cd "$out/length-source"
cp "$src"/PreludeMapLength{Size,Source}.hs .
"${ghc[@]}" -O1 -fenable-rewrite-rules -fomit-interface-pragmas -c PreludeMapLengthSize.hs PreludeMapLengthSource.hs
"${ghc[@]}" --show-iface PreludeMapLengthSize.hi >size.iface
grep -Fxq '  SourceTable T:size' size.iface
cd "$out/length-client"
cp ../length-source/PreludeMapLength{Size,Source}.hi "$src/PreludeMapLengthProof.hs" .
"${ghc[@]}" -O1 -fenable-rewrite-rules -c PreludeMapLengthProof.hs

sed "s/:~: Count xs$/:~: 'S.Succ (Count xs)/" PreludeMapLengthProof.hs >FalseLength.hs
line=$(awk '/^lengthMap _ Empty = Refl/ { print NR }' FalseLength.hs)
if "${ghc[@]}" -O1 -c FalseLength.hs >false-length.log 2>&1 ||
   ! grep -Fq "FalseLength.hs:$line:" false-length.log ||
   ! grep -Fq '[GHC-83865]' false-length.log ||
   ! grep -Eq 'Zero.*Succ.*Zero' false-length.log; then
    cat false-length.log >&2
    exit 1
fi

sed -e 's/^lengthMap f (Cons xs) = .*$/lengthMap _ (Cons _) = Refl/' \
    -e '/^    Refl -> Refl$/d' PreludeMapLengthProof.hs >MissingHypothesis.hs
line=$(awk '/^lengthMap _ \(Cons _\) = Refl/ { print NR }' MissingHypothesis.hs)
if "${ghc[@]}" -O1 -c MissingHypothesis.hs >missing-hypothesis.log 2>&1 ||
   ! grep -Fq "MissingHypothesis.hs:$line:" missing-hypothesis.log ||
   ! grep -Fq '[GHC-05617]' missing-hypothesis.log ||
   ! grep -Eq 'S\.size \(map f .*\) ~ S\.size ' missing-hypothesis.log; then
    cat missing-hypothesis.log >&2
    exit 1
fi

cat >Main.hs <<'HS'
module Main where
import PreludeMapLengthProof (showPreludeMapLength)
main :: IO ()
main = putStrLn showPreludeMapLength
HS
"${ghc[@]}" -O1 -c Main.hs
"${ghc[@]}" Main.o PreludeMapLengthProof.o ../length-source/PreludeMapLength{Size,Source}.o -o length-run
[[ $(./length-run) == '(Refl,Succ (Succ Zero))' ]]

# The ordinary provider can change this option without changing its source.
mkdir "$out/length-no-code"
cd "$out/length-no-code"
sed '/^{-# OPTIONS_GHC -fwrite-source-equations #-}$/d' \
    "$src/PreludeMapLengthSize.hs" >PreludeMapLengthSize.hs
no_code=("${ghc[@]}" -O1 -fno-code -fwrite-interface -c PreludeMapLengthSize.hs)
"${no_code[@]}" -fno-write-source-equations
"${ghc[@]}" --show-iface PreludeMapLengthSize.hi >size.iface
if grep -Fxq '  SourceTable T:size' size.iface; then
    cat size.iface >&2
    exit 1
fi
"${no_code[@]}" -fwrite-source-equations
"${ghc[@]}" --show-iface PreludeMapLengthSize.hi >size.iface
grep -Fxq '  SourceTable T:size' size.iface

# Recheck the ordinary proof when its producer changes through a type alias.
mkdir "$out/length-rebuild"
cd "$out/length-rebuild"
cp "$src"/PreludeMapLength{Size,Source}.hs .
cat >CountOne.hs <<'HS'
{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, TypeOperators #-}
module CountOne where
import Data.Type.Equality ((:~:)(Refl))
import PreludeMapLengthSize (Size(..))
import PreludeMapLengthSource (Count)

one :: Count '[ 'True] :~: 'Succ 'Zero
one = Refl
HS
rebuild=("${ghc[@]}" -O1 -fomit-interface-pragmas -fno-code -fwrite-interface --make CountOne.hs)
"${rebuild[@]}"
sed 's/= Succ (size xs)$/= Succ (Succ (size xs))/' "$src/PreludeMapLengthSize.hs" >PreludeMapLengthSize.hs
if "${rebuild[@]}" >changed.log 2>&1 || ! grep -Fq 'CountOne.hs:8:7: error: [GHC-83865]' changed.log; then
    cat changed.log >&2
    exit 1
fi
sed '/^{-# OPTIONS_GHC -fwrite-source-equations #-}$/d' "$src/PreludeMapLengthSize.hs" >PreludeMapLengthSize.hs
if "${rebuild[@]}" >withdrawn.log 2>&1 || ! grep -Eq 'PreludeMapLengthSource.hs:[0-9]+:[0-9]+: error: \[GHC-70566\]' withdrawn.log; then
    cat withdrawn.log >&2
    exit 1
fi
cp "$src/PreludeMapLengthSize.hs" .
"${rebuild[@]}"

# The installed filter needs a known predicate result before choosing a branch.
# The ordinary client has only the producer interface and keeps the tail open.
mkdir "$out"/{filter,filter-client}
cd "$out/filter"
cp "$src/PreludeFilterSource.hs" .
filter=("${ghc[@]}" -O1 -fenable-rewrite-rules)
"${filter[@]}" -fomit-interface-pragmas -c PreludeFilterSource.hs
cd "$out/filter-client"
cp ../filter/PreludeFilterSource.hi ../length-source/PreludeMapLength{Size,Source}.hi \
   "$src"/PreludeFilter{Proof,LengthProof}.hs .
"${filter[@]}" -c PreludeFilterProof.hs PreludeFilterLengthProof.hs

# Each rejected proof must reach Refl. The KnownTrue sibling must succeed.
cp "$src/NativeOpenFilterGivenControls.hs" .
proofs=(withoutResult wrongResult otherPredicate otherHead representationalResult
        newtypeHead invertHead invertPredicate 'staleGiven _ _ _ Unknown')
if "${filter[@]}" -fzap-coercions -fmax-errors=20 -c NativeOpenFilterGivenControls.hs >filter-givens.log 2>&1 ||
   [[ $(grep -Ec '^NativeOpenFilterGivenControls\.hs:[0-9]+:[0-9]+: error:' filter-givens.log) != "${#proofs[@]}" ]]; then
    cat filter-givens.log >&2
    exit 1
fi
for proof in "${proofs[@]}"; do
    line=$(awk -v proof="$proof" '$0 ~ "^" proof " .*Refl$" { print NR }' NativeOpenFilterGivenControls.hs)
    if ! grep -Eq "^NativeOpenFilterGivenControls\.hs:$line:[0-9]+: error:" filter-givens.log; then
        cat filter-givens.log >&2
        exit 1
    fi
done

cat >UnknownFilter.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
module UnknownFilter where
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import PreludeFilterSource (FilterNegated, FilterWith)

unknown :: Proxy p -> Proxy xs
        -> FilterWith p ('False ': xs) :~: ('False ': FilterWith p xs)
unknown _ _ = Refl

unknownHead :: Proxy x -> Proxy xs
            -> FilterNegated (x ': xs) :~: (x ': FilterNegated xs)
unknownHead _ _ = Refl
HS
if "${filter[@]}" -fno-enable-rewrite-rules -c UnknownFilter.hs >unknown.log 2>&1 ||
   ! grep -Fq '[GHC-83865]' unknown.log ||
   ! grep -Fq 'unknown _ _ = Refl' unknown.log ||
   ! grep -Fq 'unknownHead _ _ = Refl' unknown.log; then
    cat unknown.log >&2
    exit 1
fi

cat >ReverseBound.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module ReverseBound where
import PreludeFilterLengthProof (AtMost(..))
import PreludeFilterSource (FilterNegated)
import PreludeMapLengthSource (Count)

wrong :: AtMost (Count '[ 'True]) (Count (FilterNegated '[ 'True]))
wrong = BothSucc ZeroBound
HS
line=$(awk '/^wrong =/ { print NR }' ReverseBound.hs)
if "${filter[@]}" -c ReverseBound.hs >reverse-bound.log 2>&1 ||
   ! grep -Fq "ReverseBound.hs:$line:" reverse-bound.log ||
   ! grep -Fq '[GHC-83865]' reverse-bound.log ||
   ! grep -Eq "Couldn.t match type.*PreludeMapLengthSize\.Succ" reverse-bound.log ||
   ! grep -Eq 'with.*PreludeMapLengthSize\.Zero' reverse-bound.log; then
    cat reverse-bound.log >&2
    exit 1
fi

cat >FilterRun.hs <<'HS'
module Main where
import PreludeFilterLengthProof (showPreludeFilterLength)
import PreludeFilterProof (lazyHead, lazyIdentityHead)
main :: IO ()
main = do
    let bottom = error "filter forced the tail"
    print (lazyHead bottom, lazyIdentityHead bottom)
    putStrLn showPreludeFilterLength
HS
"${filter[@]}" -c FilterRun.hs
"${filter[@]}" FilterRun.o PreludeFilter{Proof,LengthProof}.o ../filter/PreludeFilterSource.o \
    ../length-source/PreludeMapLength{Size,Source}.o -o filter-run
[[ $(./filter-run) == $'(True,True)\n(BothSucc (RightSucc ZeroBound),Succ Zero,Succ (Succ Zero))' ]]

# The proof uses the certified predicate call as a retained argument.
cp "$src/NativeRetainedSourceOperand.hs" .
for level in 1 2; do
    "${filter[@]}" "-O$level" -fforce-recomp -c "$src/PreludeFilterRuntime.hs" -odir . -hidir .
    "${filter[@]}" "-O$level" -fforce-recomp -c "$src/PreludeFilterDecider.hs" -odir . -hidir .
    "${filter[@]}" "-O$level" -fforce-recomp -ddump-simpl -ddump-to-file \
        -dsuppress-uniques -dsuppress-idinfo -dsuppress-module-prefixes -c NativeRetainedSourceOperand.hs
    # Ordinary calls to not still inline. The retained applications use the named call.
    awk '/^ordinaryNot$/,/^$/' NativeRetainedSourceOperand.dump-simpl | grep -Fq 'case x of'
    grep -Fq '(not x)' NativeRetainedSourceOperand.dump-simpl
done
cat >DirectCase.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}
module DirectCase where
import PreludeFilterLengthProof
import PreludeFilterSource (Negation)

steps :: foreach (xs :: [Bool]) -> FilterSteps Negation xs
steps [] = End
steps (x : xs) = case not x of
    True -> keepIfTrue (steps xs)
    False -> skipIfFalse (steps xs)
HS
if "${filter[@]}" -c DirectCase.hs >direct-case.log 2>&1 ||
   ! grep -Fq '[GHC-05617]' direct-case.log ||
   ! grep -Fq 'True -> keepIfTrue' direct-case.log ||
   ! grep -Fq 'False -> skipIfFalse' direct-case.log; then
    cat direct-case.log >&2
    exit 1
fi

mkdir "$out/runtime-client"
cd "$out/runtime-client"
cp ../filter-client/*.hi .
cat >ForwardFilter.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes, RequiredTypeArguments #-}
module ForwardFilter (check) where
import PreludeFilterRuntime (runtimeFilterBoundFor)
import PreludeFilterSource (Negation)
check = runtimeFilterBoundFor (type Negation)
HS
cat >RuntimeFilterRun.hs <<'HS'
{-# LANGUAGE GADTs, NoExperimentalDependentTypes #-}
module Main where
import Control.Exception (ErrorCall(..), evaluate, try)
import Data.IORef (newIORef, readIORef, writeIORef)
import ForwardFilter (check)
import NativeRetainedSourceOperand (ordinaryNot, select)
import PreludeFilterLengthProof (AtMost(..))
import PreludeFilterRuntime
import System.Environment (getArgs)

outer :: RuntimeFilterBound -> String
outer (RuntimeFilterBound _ bound) = case bound of
    ZeroBound -> "empty"
    BothSucc _ -> "keep"
    RightSucc _ -> "skip"

main :: IO ()
main = do
    ref <- newIORef []
    args <- getArgs
    mapM_ (\input -> do
        writeIORef ref (read input)
        xs <- readIORef ref
        putStrLn (showRuntimeBound (packRuntimeBound xs))) args
    print (case packRuntimeBound (error "package forced list") of RuntimeFilterBound _ _ -> True)
    putStrLn (outer (packRuntimeBound (False : error "keep forced tail")))
    putStrLn (outer (packRuntimeBound (True : error "skip forced tail")))
    let xs = False : error "forwarded checker forced tail"
    print (outer (RuntimeFilterBound xs (check negationResult xs)))
    result <- try (evaluate (outer (packRuntimeBound [error "predicate head"])))
    print (case result of Left (ErrorCall "predicate head") -> True; _ -> False)
    refHead <- newIORef (error "strict worker forced its head")
    x <- readIORef refHead
    print (select False x)
    selected <- try (evaluate (select True x))
    print (ordinaryNot False, case selected of Left (ErrorCall "strict worker forced its head") -> True; _ -> False)
HS
"${filter[@]}" -c ForwardFilter.hs RuntimeFilterRun.hs
"${filter[@]}" RuntimeFilterRun.o ForwardFilter.o ../filter-client/PreludeFilter{LengthProof,Runtime}.o \
    ../filter-client/NativeRetainedSourceOperand.o \
    ../filter/PreludeFilterSource.o ../length-source/PreludeMapLength{Size,Source}.o -o runtime-filter-run
cat >runtime-filter.expected <<'OUT'
(ZeroBound,Zero,Zero)
(BothSucc ZeroBound,Succ Zero,Succ Zero)
(RightSucc ZeroBound,Zero,Succ Zero)
(BothSucc (RightSucc (BothSucc ZeroBound)),Succ (Succ Zero),Succ (Succ (Succ Zero)))
True
keep
skip
"keep"
True
'l'
(True,True)
OUT
./runtime-filter-run '[]' '[False]' '[True]' '[False,True,False]' >runtime-filter.stdout
diff -u runtime-filter.expected runtime-filter.stdout

cat >WrongDecider.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes #-}
module WrongDecider where
import PreludeFilterDecider

wrong :: FilterDecider Always
wrong = case negationDecider of FilterDecider decide -> FilterDecider decide

unchecked :: FilterDecider Always
unchecked = FilterDecider id
HS
if "${filter[@]}" -c WrongDecider.hs >wrong-decider.log 2>&1 ||
   ! grep -Fq 'WrongDecider.hs:6:' wrong-decider.log ||
   ! grep -Fq 'WrongDecider.hs:9:' wrong-decider.log ||
   ! grep -Fq '[GHC-83865]' wrong-decider.log ||
   ! grep -Fq 'Bool -> Bool' wrong-decider.log; then
    cat wrong-decider.log >&2
    exit 1
fi

cat >DeciderRun.hs <<'HS'
{-# LANGUAGE GADTs, NoExperimentalDependentTypes #-}
module Main where
import Control.Exception (ErrorCall(..), evaluate, try)
import Data.IORef (newIORef, readIORef)
import PreludeFilterDecider
import PreludeFilterLengthProof (AtMost(..))
import System.Environment (getArgs)

first :: DecidedFilterBound p -> String
first (DecidedFilterBound _ bound) = case bound of
    ZeroBound -> "empty"
    BothSucc _ -> "keep"
    RightSucc _ -> "skip"

main :: IO ()
main = do
    [choice, input] <- getArgs
    ref <- newIORef (if choice == "not" then SomeFilterDecider negationDecider
                                       else SomeFilterDecider alwaysDecider)
    decider <- readIORef ref
    xs <- newIORef (read input) >>= readIORef
    putStrLn (showDecidedBound decider xs)
    putStrLn (first (packDecidedBound (error "empty forced its decider") []))
    putStrLn (first (packDecidedBound negationDecider (False : error "keep forced its tail")))
    putStrLn (first (packDecidedBound negationDecider (True : error "skip forced its tail")))
    putStrLn (first (packDecidedBound alwaysDecider (error "forced head" : error "forced tail")))
    bad <- try (evaluate (first (packDecidedBound (error "cons needs its decider") [False])))
    print (case bad of Left (ErrorCall "cons needs its decider") -> True; _ -> False)
HS
"${filter[@]}" -c DeciderRun.hs
"${filter[@]}" DeciderRun.o ../filter-client/PreludeFilter{Decider,LengthProof,Runtime}.o \
    ../filter/PreludeFilterSource.o ../length-source/PreludeMapLength{Size,Source}.o -o decider-run
[[ $(./decider-run not '[False,True,True]') == $'(BothSucc (RightSucc (RightSucc ZeroBound)),Succ Zero,Succ (Succ (Succ Zero)))\nempty\nkeep\nskip\nkeep\nTrue' ]]
[[ $(./decider-run always '[False,True,True]') == $'(BothSucc (BothSucc (BothSucc ZeroBound)),Succ (Succ (Succ Zero)),Succ (Succ (Succ Zero)))\nempty\nkeep\nskip\nkeep\nTrue' ]]
cd "$out/filter-client"

sed -e 's/^{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}//' \
    -e 's/, showPreludeFilterProofs//' \
    -e '/^showPreludeFilterProofs ::/,$d' PreludeFilterProof.hs >RuleFilter.hs
if "${filter[@]}" -fforce-recomp -c RuleFilter.hs >filter-rules.log 2>&1 ||
   ! grep -Eq '(keepFalse|skipTrue|keepTrue|skipFalse) _ = Refl' filter-rules.log ||
   ! grep -Fq '[GHC-83605]' filter-rules.log ||
   ! grep -Fq 'with the current rewrite rules' filter-rules.log; then
    cat filter-rules.log >&2
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

# Compose two traversals on an open list. A strict intermediate changes
# the result when the outer callback ignores the inner callback's result.
mkdir "$out"/{compose,compose-client}
cd "$out/compose"
cp "$src/NativeOpenInstalledMapProducer.hs" .
cat >TwiceSource.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, StandaloneKindSignatures #-}
module TwiceSource (Twice) where
import qualified NativeOpenInstalledMapProducer as W
import qualified Prelude as P

type Twice :: [P.Bool] -> [P.Bool]
type Twice xs = W.mapper P.not (W.mapper P.not xs)
HS
cat >Compose.hs <<'HS'
module Main where
import Control.Exception (SomeException, evaluate, try)
import qualified NativeOpenInstalledMapProducer as W

{-# NOINLINE observe #-}
observe :: (Bool -> Bool) -> (Bool -> Bool) -> [Bool] -> Bool
observe f g xs = case W.mapper f (W.mapper g xs) of
    x : _ -> x
    [] -> False

main :: IO ()
main = do
    result <- try (evaluate (observe (const True) undefined (False : undefined)))
    print (either (const "forced") show (result :: Either SomeException Bool))
HS
compose=("${ghc[@]}" -O1 -fenable-rewrite-rules -DCOMPOSE_RULE)
"${compose[@]}" -c NativeOpenInstalledMapProducer.hs
"${compose[@]}" -c TwiceSource.hs
"${compose[@]}" --make Compose.hs -ddump-rule-firings -o run >compose.log
grep -Fq 'Rule fired: mapper/compose' compose.log
[[ $(./run) == '"True"' ]]

cd "$out/compose-client"
cp ../compose/{NativeOpenInstalledMapProducer,TwiceSource}.hi .
cat >TwiceProof.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
module TwiceProof where
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import TwiceSource (Twice)

proof :: Proxy xs -> Twice ('True ': xs) :~: ('True ': Twice xs)
proof _ = Refl
HS
proof=("${ghc[@]}" -O1 -fenable-rewrite-rules -c TwiceProof.hs)
"${proof[@]}"

cd "$out/compose"
"${compose[@]}" -DFORCE_INTERMEDIATE -fforce-recomp -c NativeOpenInstalledMapProducer.hs
"${compose[@]}" -DFORCE_INTERMEDIATE --make Compose.hs -ddump-rule-firings -o run >compose.log
grep -Fq 'Rule fired: mapper/compose' compose.log
[[ $(./run) == '"forced"' ]]
cd "$out/compose-client"
cp ../compose/NativeOpenInstalledMapProducer.hi .
line=$(awk '/^proof _ = Refl/ { print NR }' TwiceProof.hs)
if "${proof[@]}" >changed.log 2>&1 ||
   ! grep -Fq "TwiceProof.hs:$line:" changed.log ||
   ! grep -Fq '[GHC-83605]' changed.log; then
    cat changed.log >&2
    exit 1
fi

# The same predicate decision applies before and after appending to the tail.
stripped=(-fomit-interface-pragmas -fno-write-if-simplified-core)
mkdir "$out"/{filter-append-provider,filter-append-client,filter-append-observer}
cd "$out/filter-append-provider"
cp ../length-source/PreludeMapLength{Size,Source}.hi ../filter/PreludeFilterSource.hi .
"${filter[@]}" "${stripped[@]}" -odir . -hidir . -c \
    "$src/PreludeFilterLengthProof.hs" "$src/PreludeFilterRuntime.hs" "$src/PreludeAppendLengthSource.hs"
cd "$out/filter-append-client"
cp ../filter-append-provider/*.hi "$src"/PreludeFilterAppend{Proof,Runtime}.hs .
filter_append_sources=(../filter-append-provider/*.o ../filter/PreludeFilterSource.o
                       ../length-source/PreludeMapLength{Size,Source}.o)
cat >Main.hs <<'HS'
module Main where
import PreludeFilterAppendProof (showPreludeFilterAppend)
main :: IO ()
main = putStrLn showPreludeFilterAppend
HS
for level in -O0 -O2; do
    "${filter[@]}" "$level" "${stripped[@]}" -fforce-recomp -c PreludeFilterAppendProof.hs Main.hs
    "${ghc[@]}" Main.o PreludeFilterAppendProof.o "${filter_append_sources[@]}" -o filter-append-run
    [[ $(./filter-append-run) == '(Refl,[1,3],[1,3],(True,True))' ]]
done

sed -e 's/Decide decision xs/Decide _ xs/' -e 's/^    Refl -> case decision of$/    Refl -> Refl/' \
    -e '/^        IsTrue -> Refl$/d' -e '/^        IsFalse -> Refl$/d' \
    PreludeFilterAppendProof.hs >WithoutDecision.hs
line=$(awk '/^    Refl -> Refl$/ { print NR }' WithoutDecision.hs)
if "${filter[@]}" -c WithoutDecision.hs >without-decision.log 2>&1 ||
   ! grep -Fq "WithoutDecision.hs:$line:" without-decision.log ||
   ! grep -Fq '[GHC-05617]' without-decision.log ||
   ! grep -Fq '#guard:filter (p x)' without-decision.log; then
    cat without-decision.log >&2
    exit 1
fi

# Check that each runtime decision belongs to the head in this proof step.
"${filter[@]}" -O2 "${stripped[@]}" -c PreludeFilterAppendRuntime.hs
sed 's/negationResult x/negationResult False/' PreludeFilterAppendRuntime.hs >WrongHead.hs
line=$(awk '/^decisionsForNot .*negationResult False/ { print NR }' WrongHead.hs)
if "${filter[@]}" -c WrongHead.hs >wrong-head.log 2>&1 ||
   ! grep -Fq "WrongHead.hs:$line:" wrong-head.log ||
   ! grep -Fq '[GHC-05617]' wrong-head.log ||
   ! grep -Fq 'not x ~ True' wrong-head.log; then
    cat wrong-head.log >&2
    exit 1
fi

cd "$out/filter-append-observer"
cp ../filter-append-client/{PreludeFilterAppendProof,PreludeFilterAppendRuntime}.hi ../filter-append-provider/*.hi .
cat >ObserveFilterAppend.hs <<'HS'
{-# LANGUAGE GADTs, NoExperimentalDependentTypes #-}
module Main where

import Data.IORef (newIORef, readIORef)
import Data.Type.Equality ((:~:)(Refl))
import PreludeFilterAppendRuntime (RuntimeFilterAppend(..), packFilterAppend)

observe :: RuntimeFilterAppend -> ([Bool], [Bool])
observe (RuntimeFilterAppend xs ys Refl) =
    (filter not (xs ++ ys), filter not xs ++ filter not ys)

proofOnly :: RuntimeFilterAppend -> Bool
proofOnly (RuntimeFilterAppend _ _ Refl) = True

packageOnly :: RuntimeFilterAppend -> Bool
packageOnly (RuntimeFilterAppend _ _ _) = True

main :: IO ()
main = do
    xs <- newIORef [True, False] >>= readIORef
    ys <- newIORef [False, True] >>= readIORef
    print ( observe (packFilterAppend xs ys)
          , proofOnly (packFilterAppend [] undefined)
          , proofOnly (packFilterAppend xs undefined)
          , packageOnly (packFilterAppend undefined undefined) )
HS
for level in -O0 -O2; do
    "${filter[@]}" "$level" -fforce-recomp -c ObserveFilterAppend.hs
    "${ghc[@]}" ObserveFilterAppend.o ../filter-append-client/PreludeFilterAppend{Proof,Runtime}.o \
        "${filter_append_sources[@]}" -o filter-append-observer
    [[ $(./filter-append-observer) == '(([False,False],[False,False]),True,True,True)' ]]
done
