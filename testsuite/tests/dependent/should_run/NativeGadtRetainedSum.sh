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

    cat >CrossedWitness.hs <<'HS'
{-# LANGUAGE BangPatterns, GADTs, NoExperimentalDependentTypes #-}
module CrossedWitness where
import NativeGadtRetainedSumProvider
crossed :: DirectVec a n -> DirectVec a m -> Vec a m
crossed (DirectVec !size xs) right = case right of
    DirectVec !size _ -> xs
HS
    if "${ghc[@]}" "${flags[@]}" -ferror-spans -c CrossedWitness.hs >reject.log 2>&1; then
        echo 'Accepted the payload of a different retained witness' >&2
        exit 1
    fi
    grep -Eq 'CrossedWitness.hs:6:26-27: error: \[GHC-05617\]' reject.log || { cat reject.log >&2; exit 1; }

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

    cat >WrongTile.hs <<'HS'
{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes #-}
module WrongTile where

import NativeGadtRetainedSumProvider

wrong :: Tile a ('Rectangle 'Z ('S 'Z)) -> Vec (Vec a 'Z) ('S 'Z)
wrong (Tile _ _ xs) = xs
HS
    if "${ghc[@]}" "${flags[@]}" -ferror-spans -c WrongTile.hs >reject.log 2>&1; then
        echo "Accepted a tile payload with its dimensions swapped" >&2
        exit 1
    fi
    grep -Fq 'WrongTile.hs:7:23-24: error: [GHC-05617]' reject.log ||
        { cat reject.log >&2; exit 1; }

    cat >RepeatedMatrix.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}
module RepeatedMatrix where
import NativeGadtRetainedSumProvider (Nat, Vec)
data RepeatedMatrix a (rows :: Nat) (columns :: Nat) where
    RepeatedMatrix :: foreach (rows :: Nat) (columns :: Nat)
                   -> Vec (Vec a columns) rows -> RepeatedMatrix a rows rows
HS
    if "${ghc[@]}" "${flags[@]}" -ferror-spans -c RepeatedMatrix.hs >reject.log 2>&1; then
        echo "Accepted a constructor result that omits a retained dimension" >&2
        exit 1
    fi
    grep -Fq 'RepeatedMatrix.hs:6:51-76: error: [GHC-83605]' reject.log ||
        { cat reject.log >&2; exit 1; }

    cat >RepeatedBlock.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}
module RepeatedBlock where
import NativeGadtRetainedSumProvider (BlockShape(..), Nat, Vec)
data RepeatedBlock a (shape :: BlockShape) where
    RepeatedBlock :: foreach (mark :: Bool) (depth :: Nat) (width :: Nat)
                  -> Vec a width -> Vec a depth -> RepeatedBlock a ('Block width mark width)
HS
    if "${ghc[@]}" "${flags[@]}" -ferror-spans -c RepeatedBlock.hs >reject.log 2>&1; then
        echo "Accepted a repeated argument that omits a retained field" >&2
        exit 1
    fi
    grep -Fq 'RepeatedBlock.hs:6:52-92: error: [GHC-83605]' reject.log || \
        { cat reject.log >&2; exit 1; }

    if [[ $mode == O0 ]]; then
        cat >DirectIndexCoverage.hs <<'HS'
{-# LANGUAGE DataKinds, EmptyCase, GADTs, NoExperimentalDependentTypes #-}
module DirectIndexCoverage where
import GHC.Sigma (Sigma(Pair))
import NativeGadtRetainedSumProvider

knownZero :: DirectVec a 'Z -> Bool
knownZero (DirectVec Z _) = True

knownSuccessor :: DirectVec a ('S n) -> a
knownSuccessor (DirectVec (S _) (VCons x _)) = x

unknownZero :: DirectVec a n -> Bool
unknownZero (DirectVec Z _) = True

knownUnreachable :: DirectVec a 'Z -> Bool
knownUnreachable (DirectVec (S _) _) = False
knownUnreachable (DirectVec _ _) = True

knownMatrix :: Matrix a 'Z ('S columns) -> Bool
knownMatrix (Matrix Z (S _) _) = True
knownMatrix (MatrixByColumns (S _) Z _) = True

knownMarked :: MarkedRows a ('S rows) 'True -> a
knownMarked (MarkedRows True (S _) (VCons x _)) = x

knownSquare :: SquareMatrix a 'Z 'Z -> Bool
knownSquare (Square Z _) = True

disjointSquare :: SquareMatrix a 'Z ('S 'Z) -> ()
disjointSquare x = case x of {}

disjointSquareMatch :: SquareMatrix a 'Z ('S 'Z) -> ()
disjointSquareMatch (Square _ _) = ()

impossibleSuccessor :: SuccVec a 'Z -> ()
impossibleSuccessor x = case x of {}

knownPredecessor :: SuccVec a ('S 'Z) -> Bool
knownPredecessor (SuccVec Z _) = True

unknownPredecessor :: SuccVec a n -> Bool
unknownPredecessor (SuccVec Z _) = True

knownTile :: Tile a ('Rectangle 'Z ('S 'Z)) -> Bool
knownTile (Tile Z (S _) _) = True

unknownTile :: Tile a shape -> Bool
unknownTile (Tile Z _ _) = True

knownNested :: DirectVec a ('S n) -> a
knownNested (DirectVec _ (VCons x _)) = x

knownDifferentFields :: Blocked a ('Block ('S n) mark 'Z) -> a
knownDifferentFields (Blocked _ _ _ (VCons x _) VNil) = x

unknownNested :: Blocked a ('Block n mark depth) -> a
unknownNested (Blocked _ _ _ (VCons x _) _) = x

knownNestedBool :: Blocked Bool ('Block ('S n) mark depth) -> Bool
knownNestedBool (Blocked _ _ _ (VCons True _) _) = True

knownNestedInaccessible :: Blocked a ('Block 'Z mark depth) -> Bool
knownNestedInaccessible (Blocked _ _ _ (VCons _ _) _) = False
knownNestedInaccessible (Blocked _ _ _ _ _) = True

missingNamed :: Phantom -> Bool
missingNamed (Pair Z _) = False

missingApplied :: Packed Bool -> Bool
missingApplied (Pair Z _) = False

missingOrdinary :: OrdinaryOptional Bool -> Bool
missingOrdinary (Just _) = False
HS
        "${ghc[@]}" "${flags[@]}" -ferror-spans -Wincomplete-patterns -Woverlapping-patterns \
            -c DirectIndexCoverage.hs >coverage.log 2>&1
        grep -Fq 'DirectIndexCoverage.hs:13:1-34: warning: [GHC-62161]' coverage.log &&
            grep -Fq 'not matched: DirectVec (S _) _' coverage.log &&
            grep -Fq 'DirectIndexCoverage.hs:16:1-44: warning: [GHC-94210]' coverage.log &&
            grep -Fq 'DirectIndexCoverage.hs:33:1-37: warning: [GHC-94210]' coverage.log &&
            grep -Fq 'DirectIndexCoverage.hs:42:1-39: warning: [GHC-62161]' coverage.log &&
            grep -Fq 'not matched: SuccVec (S _) _' coverage.log &&
            grep -Fq 'DirectIndexCoverage.hs:48:1-31: warning: [GHC-62161]' coverage.log &&
            grep -Fq 'not matched: Tile (S _) _ _' coverage.log &&
            grep -Fq 'DirectIndexCoverage.hs:57:1-47: warning: [GHC-62161]' coverage.log &&
            grep -Fq 'Blocked _ _ _ VNil _' coverage.log &&
            grep -Fq 'DirectIndexCoverage.hs:60:1-55: warning: [GHC-62161]' coverage.log &&
            grep -Fq 'Blocked _ _ _ (VCons False _) _' coverage.log &&
            grep -Fq 'DirectIndexCoverage.hs:63:1-61: warning: [GHC-94210]' coverage.log &&
            grep -F -A3 'DirectIndexCoverage.hs:67:1-31: warning: [GHC-62161]' coverage.log |
                grep -Fq 'Patterns of type ‘Phantom’ not matched: Pair (S _) _' &&
            grep -F -A3 'DirectIndexCoverage.hs:70:1-33: warning: [GHC-62161]' coverage.log |
                grep -Fq 'Patterns of type ‘Packed Bool’ not matched: Pair (S _) _' &&
            grep -F -A3 'DirectIndexCoverage.hs:73:1-32: warning: [GHC-62161]' coverage.log |
                grep -Fq 'Patterns of type ‘Maybe Bool’ not matched: Nothing' &&
            test "$(grep -c 'warning:' coverage.log)" -eq 11 || { cat coverage.log >&2; exit 1; }
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
                expected='BadScope.hs:7:46-86: error: \[GHC-83605\]'
                grep -Fq 'GHC cannot check a retained dependent sum in this position.' reject.log || { cat reject.log >&2; exit 1; }
                ;;
            RESULT_SCOPE) expected='BadScope.hs:13:26: error: \[GHC-76037\]' ;;
            UNMATCHED_ERASED) expected='BadScope.hs:16:[0-9]+(-[0-9]+)?: error: \[GHC-83605\]' ;;
        esac
        grep -Eq "$expected" reject.log || { cat reject.log >&2; exit 1; }
    done
done
