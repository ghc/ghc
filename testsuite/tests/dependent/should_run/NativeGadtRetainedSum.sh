#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C.UTF-8

src=$(cd "$(dirname "$0")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint)
provider=NativeGadtRetainedSumProvider
client=NativeGadtRetainedSumClient

for mode in O0 O1-stripped O1; do
    case $mode in
        O0) flags=(-O0 -fomit-interface-pragmas) ;;
        O1-stripped) flags=(-O1 -fomit-interface-pragmas) ;;
        O1) flags=(-O1) ;;
    esac
    flags+=(-fno-write-if-simplified-core)
    work=$out/$mode
    mkdir -p "$work"/{provider,client}
    cd "$work/provider"
    cp "$src/$provider.hs" .
    "${ghc[@]}" "${flags[@]}" -c "$provider.hs"
    mv "$provider.hs" "$work/"
    cp "$provider.hi" "$src/$client.hs" ../client/
    cd ../client
    "${ghc[@]}" "${flags[@]}" -c "$client.hs"
    "${ghc[@]}" "${flags[@]}" "$client.o" "../provider/$provider.o" -o run
    ./run

    cat >BadLength.hs <<'HS'
{-# LANGUAGE CPP, ExperimentalDependentTypes #-}
module BadLength where
import NativeGadtRetainedSumProvider
bad :: Nested Bool
#ifdef BAD_INNER
bad = makeNested (S Z) (S Z) (VCons True VNil) (VCons False VNil) VNil
#else
bad = makeNested (S Z) Z (VCons True VNil) VNil VNil
#endif
HS
    for which in BAD_OUTER BAD_INNER; do
        if "${ghc[@]}" "${flags[@]}" -D"$which" -c BadLength.hs >reject.log 2>&1; then
            echo "Accepted $which" >&2
            exit 1
        fi
        grep -Fq '[GHC-83865]' reject.log || { cat reject.log >&2; exit 1; }
    done

    cat >BadBranch.hs <<'HS'
{-# LANGUAGE GADTs #-}
module BadBranch where
import GHC.Sigma (Sigma(Pair))
import NativeGadtRetainedSumProvider
bad :: Later a b -> Vec a requested
bad (Later actual (Pair _ (xs, _))) = xs
HS
    if "${ghc[@]}" "${flags[@]}" -ferror-spans -c BadBranch.hs >reject.log 2>&1; then
        echo "Accepted an escaping constructor witness" >&2
        exit 1
    fi
    grep -Eq 'BadBranch.hs:6:[0-9]+-[0-9]+: error: \[GHC-25897\]' reject.log || { cat reject.log >&2; exit 1; }

    cat >BadPayload.hs <<'HS'
{-# LANGUAGE CPP, GADTs, NoExperimentalDependentTypes #-}
module BadPayload where
import GHC.Sigma (Sigma(Pair))
import NativeGadtRetainedSumProvider
#ifdef ESCAPE
bad :: Packed Bool -> Vec Bool caller
bad (Pair _ xs) = forward xs
#elif defined(INDEPENDENT)
bad :: Packed Bool -> Packed Bool -> Bool
bad (Pair _ xs) (Pair _ ys) = sameLength xs ys
#elif defined(WRONG_ELEMENT)
bad :: DirectVec a n -> Vec b n -> DirectVec a n
bad (DirectVec n _) xs = DirectVec n xs
#else
bad :: DirectVec a n -> Vec a m -> DirectVec a n
bad (DirectVec n _) xs = DirectVec n xs
#endif
HS
    for which in ESCAPE INDEPENDENT WRONG_ELEMENT WRONG_LENGTH; do
        if "${ghc[@]}" "${flags[@]}" -D"$which" -ferror-spans -c BadPayload.hs >reject.log 2>&1; then
            echo "Accepted $which" >&2
            exit 1
        fi
        case $which in
            ESCAPE) expected='BadPayload.hs:7:19-[0-9]+: error: \[GHC-25897\]' ;;
            INDEPENDENT) expected='BadPayload.hs:10:45-46: error: \[GHC-83865\]' ;;
            WRONG_ELEMENT) expected='BadPayload.hs:13:[0-9]+(-[0-9]+)?: error: \[GHC-25897\]' ;;
            WRONG_LENGTH) expected='BadPayload.hs:16:[0-9]+(-[0-9]+)?: error: \[GHC-25897\]' ;;
        esac
        grep -Eq "$expected" reject.log || { cat reject.log >&2; exit 1; }
    done

    cat >WrongDirectIndex.hs <<'HS'
{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes #-}
module WrongDirectIndex where
import Data.Type.Equality ((:~:)(Refl))
import NativeGadtRetainedSumProvider
wrong :: DirectVec a n -> Maybe (n :~: 'Z)
wrong (DirectVec (S _) _) = Just Refl
wrong (DirectVec Z _) = Just Refl
HS
    if "${ghc[@]}" "${flags[@]}" -ferror-spans -c WrongDirectIndex.hs >reject.log 2>&1; then
        echo "Accepted a zero proof for a successor index" >&2
        exit 1
    fi
    grep -Fq 'WrongDirectIndex.hs:6:34-37: error: [GHC-05617]' reject.log ||
        { cat reject.log >&2; exit 1; }

    if [[ $mode == O0 ]]; then
        cat >DirectIndexCoverage.hs <<'HS'
{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes #-}
module DirectIndexCoverage where
import NativeGadtRetainedSumProvider

knownZero :: DirectVec a 'Z -> Bool
knownZero (DirectVec Z _) = True

knownSuccessor :: DirectVec a ('S n) -> a
knownSuccessor (DirectVec (S _) (VCons x _)) = x

unknownZero :: DirectVec a n -> Bool
unknownZero (DirectVec Z _) = True

knownUnreachable :: DirectVec a 'Z -> Bool
knownUnreachable (DirectVec (S _) _) = False
knownUnreachable (DirectVec Z _) = True
HS
        "${ghc[@]}" "${flags[@]}" -ferror-spans -Wincomplete-patterns -Woverlapping-patterns \
            -c DirectIndexCoverage.hs >coverage.log 2>&1
        for span in 6:1-32 9:1-48 12:1-34; do
            grep -Fq "DirectIndexCoverage.hs:$span: warning: [GHC-62161]" coverage.log ||
                { cat coverage.log >&2; exit 1; }
        done
        test "$(grep -c 'warning:' coverage.log)" -eq 3 || { cat coverage.log >&2; exit 1; }
    fi

    cat >BadScope.hs <<'HS'
{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs, RankNTypes #-}
module BadScope where
import NativeGadtRetainedSumProvider
#ifdef HIDDEN_KIND
data BadScope where
    BadScope :: foreach (n :: Nat)
             -> (foreach (n :: FlagAt n) -> (some (m :: Nat) | (KindTag n, Vec Char m)))
             -> BadScope
#elif defined(RESULT_SCOPE)
data BadScope (a :: Nat) where
    BadScope :: forall (a :: Nat). foreach (n :: Nat)
             -> (foreach (n :: Nat) -> (some (m :: Nat) | Vec Char m))
             -> BadScope m
#else
data BadScope a (n :: Nat) where
    BadScope :: foreach (n :: Nat) -> Vec b n -> BadScope a n
#endif
HS
    for which in HIDDEN_KIND RESULT_SCOPE UNMATCHED_ERASED; do
        if "${ghc[@]}" "${flags[@]}" -D"$which" -ferror-spans -c BadScope.hs >reject.log 2>&1; then
            echo "Accepted $which" >&2
            exit 1
        fi
        case $which in
            HIDDEN_KIND)
                expected='BadScope.hs:7:46-86: error:'
                grep -Fq 'A retained dependent sum is supported' reject.log || { cat reject.log >&2; exit 1; }
                ;;
            RESULT_SCOPE) expected='BadScope.hs:13:26: error: \[GHC-76037\]' ;;
            UNMATCHED_ERASED) expected='BadScope.hs:16:[0-9]+(-[0-9]+)?: error: \[GHC-83605\]' ;;
        esac
        grep -Eq "$expected" reject.log || { cat reject.log >&2; exit 1; }
    done
done
