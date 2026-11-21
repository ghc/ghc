#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
src=$(cd "$(dirname "$0")" && pwd)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint)
for opt in -O0 -O1 -O2; do
    out="$tmp/$opt"
    mkdir -p "$out"/{indices,proof,client}
    cd "$out/indices"
    cp "$src"/{PackedVecLibrary,ParameterizedIndexLibrary}.hs .
    "${ghc[@]}" "$opt" -fwrite-source-equations -c PackedVecLibrary.hs
    "${ghc[@]}" "$opt" -Werror=incomplete-patterns -c ParameterizedIndexLibrary.hs
    cd ../proof
    cp "$src/"{FinitePosition,ListCursor,AlignedList,NativeIndexedFinGate,NativeLinearIndexColumns}.hs ../indices/*.hi .
    "${ghc[@]}" "$opt" -Werror=incomplete-patterns -c FinitePosition.hs AlignedList.hs NativeIndexedFinGate.hs
    lint=()
    if [[ $opt == -O0 ]]; then lint=(-dlinear-core-lint); fi
    "${ghc[@]}" "$opt" "${lint[@]}" -Werror=incomplete-patterns -c ListCursor.hs NativeLinearIndexColumns.hs
    cd ../client
    cp "$src/"{FinitePositionClient,CursorClient}.hs ../proof/*.hi .
    "${ghc[@]}" "$opt" -Werror=incomplete-patterns -c FinitePositionClient.hs CursorClient.hs
    cat >Main.hs <<'HS'
{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, TypeApplications #-}
module Main where
import Control.Exception (ErrorCall, displayException, evaluate, try)
import AlignedList (zipWithExact)
import Data.IORef (newIORef, readIORef)
import Data.Maybe (isJust)
import CursorClient (observeBoth, observeZip, observeAligned, observeCursors, observeIndexedCallback, zipTwoPairs, zipInt8Literals)
import NativeLinearIndexColumns (choose)
import FinitePositionClient (lookupVecViaView, observeConstructedViews, observeEmptyIndex, observeFinitePosition, observeListIndices)
import NativeIndexedFinGate (Gate(..), Ticket(..), grant)
import PackedVecLibrary (Nat(..), Vec(..), toList)
import ParameterizedIndexLibrary (indexVec, lookupVecChecked, mkFinGeneric, mkFinVec)

written = toList (zipWithExact [1,2] [10,20] (+)) :: [Int]

scopedVector =
    let a = 1 :: Int; b = 2 :: Int; c = 10 :: Int; d = 20 :: Int
    in zipWithExact [a,b] [c,d] (+)

scopedLambda =
    toList ((\ ~a ~b ~c ~d -> zipWithExact [a,b] [c,d] (+)) 1 2 10 20) :: [Int]

main :: IO ()
main = do
    observeFinitePosition >>= print
    observeListIndices >>= print
    let lazyOperands =
            take 1 (toList ((zipWithExact @Int @Int @Int [5 + 1, error "forced later left field"])
                [10, error "forced later right field"] (+))) :: [Int]
    if (written, toList scopedVector, scopedLambda, lazyOperands, zipInt8Literals) ==
        ([11,22], [11,22], [11,22], [16], [137,-108])
    then pure ()
    else fail "the retained list spine changed a field or closed result"
    both <- observeBoth
    if both == [Just ('b', 10), Just ('b', 20), Nothing] then pure () else fail (show both)
    zipped <- observeZip
    if zipped == (Just [('a', 10), ('b', 20)], Nothing, Just 30, [('x', 'x')]) then pure () else fail (show zipped)
    case zipTwoPairs 'a' (error "forced second left field") True (error "forced second right field") of
        ('a', True) : _ -> pure ()
        _ -> fail "wrong first pair"
    aligned <- observeAligned
    if aligned == [Just ('b', 20), Nothing, Nothing] then pure () else fail (show aligned)
    callbacks <- observeIndexedCallback
    if callbacks == (Just "cc", Nothing, True) then pure () else fail (show callbacks)
    word <- newIORef "xz" >>= readIORef
    case mkFinGeneric word 1 of
        Nothing -> fail "missing position"
        Just pos -> do
            let selected = choose word pos True (error "forced explicitly lazy argument")
                fallback = choose word pos False '!'
            if (selected, fallback) == ('z', '!') then pure () else fail "wrong branch"
    (ordinary, linear) <- observeCursors
    let expectedCursors = [Just 'c', Nothing, Nothing, Just 'b', Just 'x', Just 'y']
    if ordinary == expectedCursors && linear == expectedCursors then pure ()
        else fail (show (ordinary, linear))
    empty <- observeEmptyIndex
    gate <- newIORef (Shut (error "gate field")) >>= readIORef
    denied <- try (evaluate (grant gate (error "ticket"))) :: IO (Either ErrorCall Bool)
    case denied of
        Left err | empty && grant (Open True) Granted
                 , takeWhile (/= '\n') (displayException err) == "ticket" -> pure ()
        _ -> fail "wrong demand for an omitted retained equation"
    views <- observeConstructedViews
    if views == (Just 1, True) then pure () else fail (show views)
    positions <- newIORef [minBound, -1, 0, 1, 2, 3, maxBound] >>= readIORef
    let cells = VCons (7 :: Int) (VCons 9 (VCons 13 VNil))
        other = VCons (11 :: Int) (VCons 19 (VCons 23 VNil))
        expected = [Nothing, Nothing, Just 7, Just 9, Just 13, Nothing, Nothing]
    case (map (lookupVecChecked cells) positions, map (lookupVecViaView cells) positions,
          fmap (\i -> indexVec i other) (mkFinVec cells 2)) of
        (direct, viaView, Just 23) | direct == expected && viaView == expected -> pure ()
        actual -> fail (show actual)
    let absent = error "forced vector for negative position" :: Vec Int 'Z
        unusedTail = error "forced suffix past checked position" :: Vec Int ('S 'Z)
        prefix = VCons (error "forced skipped element") (VCons (29 :: Int) unusedTail)
        selectedBottom = VCons (7 :: Int) (VCons (error "forced selected element") VNil)
    case (lookupVecViaView absent minBound, lookupVecViaView (VNil :: Vec Int 'Z) 0,
          lookupVecViaView prefix 1, isJust (lookupVecViaView selectedBottom 1)) of
        (Nothing, Nothing, Just 29, True) -> pure ()
        actual -> fail (show actual)
HS
    "${ghc[@]}" "$opt" -c Main.hs
    "${ghc[@]}" Main.o FinitePositionClient.o CursorClient.o ../proof/*.o ../indices/*.o -o run
    ./run
done
cd "$out/client"
cat >EscapingComposedResult.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, ScopedTypeVariables #-}
module EscapingComposedResult where
import AlignedList (checkedZipWith)
import PackedVecLibrary (Vec)

escape :: forall n. [Char] -> [Int] -> Maybe (Vec Int n)
escape left numbers = fmap ((.) id id) (checkedZipWith left numbers (flip const))
HS
if "${ghc[@]}" -O0 -fdiagnostics-color=never -c EscapingComposedResult.hs >compose-escape.log 2>&1; then
    echo 'Accepted a runtime vector length at an unrelated result index'
    exit 1
fi
grep -Eq '^EscapingComposedResult.hs:7:[0-9]+: error: .GHC-25897.' compose-escape.log
grep -Fq 'PackedVecLibrary.sourceLength left' compose-escape.log
grep -Fq 'is a rigid type variable bound by' compose-escape.log
cat >MissingRows.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeFamilies #-}
module MissingRows where
import NativeIndexedFinGate

type family Unknown (g :: Gate) :: Bool where
    Unknown ('Open b) = 'True

inhabited :: foreach (g :: Gate) -> Ticket (Always g) -> Bool
inhabited (Open b) Granted = b

stuck :: foreach (g :: Gate) -> Ticket (Unknown g) -> Bool
stuck (Open b) Granted = b

noSplit :: foreach (g :: Gate) -> Ticket (Ready g) -> Bool
noSplit (Open b) _ = b

lazySplit :: foreach (g :: Gate) -> Ticket (Ready g) -> Bool
lazySplit (Open b) ~Granted = b

type family Always (g :: Gate) :: Bool where
    Always ('Open b) = 'True
    Always ('Shut b) = 'True
HS
if "${ghc[@]}" -O0 -fdiagnostics-color=never -c MissingRows.hs >missing.log 2>&1; then
    echo 'Accepted an uncovered retained equation'
    exit 1
fi
for line in 9 12 15 18; do
    grep -Eq "^MissingRows.hs:$line:[0-9]+: error: .GHC-83605." missing.log
done
test "$(grep -c 'Missing equation for the retained constructor .*Shut' missing.log)" -eq 4
cat >IndependentLengths.hs <<'HS'
{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs, LinearTypes, ScopedTypeVariables #-}
module IndependentLengths where
import Data.Proxy (Proxy)
import PackedVecLibrary (sourceLength)
import ParameterizedIndexLibrary (Fin)

data Positions a b where
    Positions :: foreach (xs :: [a]) -> Fin (sourceLength xs) %1
              -> foreach (ys :: [b]) -> Fin (sourceLength ys) %1 -> Positions a b

ordinary :: Proxy (xs :: [a]) -> Proxy (ys :: [b])
         -> Proxy (sourceLength xs) -> Proxy (sourceLength ys)
         -> (Proxy (sourceLength xs), Proxy (sourceLength ys))
ordinary _ _ p q = (p, q)
#ifdef BAD_INDEX
data Wrong where
    Wrong :: Proxy (sourceLength True) -> Wrong
#endif
HS
for opt in -O0 -O2; do
    "${ghc[@]}" "$opt" -fforce-recomp -c IndependentLengths.hs
done
if "${ghc[@]}" -O0 -fforce-recomp -DBAD_INDEX -fdiagnostics-color=never -c IndependentLengths.hs >wrong-length.log 2>&1; then
    echo 'Generalized a certified source function over a wrong argument kind'
    exit 1
fi
grep -Eq '^IndependentLengths.hs:17:[0-9]+: error: .GHC-83865.' wrong-length.log
cat >CheckedList.hs <<'HS'
{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module CheckedList where
import Data.Type.Equality ((:~:)(Refl))

data CheckedList (xs :: [Bool]) where
#if defined(DIRECT_RESULT)
    CheckedList :: foreach (ys :: [Bool]) -> CheckedList ys
#elif defined(COMPOUND_RESULT)
    CheckedList :: foreach (ys :: [Bool]) -> CheckedList (True ': ys)
#elif defined(TWO_FIELDS_RESULT)
    CheckedList :: foreach (ys :: [Bool]) (zs :: [Bool]) -> CheckedList ys
#else
    CheckedList :: forall xs. foreach (ys :: [Bool]) -> (xs :~: ys) -> CheckedList xs

checked :: CheckedList '[True]
checked = CheckedList [True] Refl
#endif

#ifdef DIRECT_RESULT
data Tagged (tag :: Ordering) where
    Dynamic :: foreach (tag :: Ordering) -> Tagged tag
    Fixed :: Tagged 'EQ
#endif
HS
"${ghc[@]}" -O0 -c CheckedList.hs
for shape in COMPOUND_RESULT TWO_FIELDS_RESULT; do
    if "${ghc[@]}" -O0 -fforce-recomp "-D$shape" -ferror-spans \
            -fdiagnostics-color=never -c CheckedList.hs >result.log 2>&1; then
        echo "Accepted an unsupported retained constructor result ($shape)"
        exit 1
    fi
    case $shape in
        COMPOUND_RESULT)
            grep -Fq 'CheckedList.hs:9:46-69: error: [GHC-83605]' result.log
            grep -Eq 'The result type is .CheckedList \(True : ys\).\.' result.log ;;
        TWO_FIELDS_RESULT)
            grep -Fq 'CheckedList.hs:11:61-74: error: [GHC-83605]' result.log
            grep -Eq 'The result type is .CheckedList ys.\.' result.log ;;
    esac
    grep -Fq 'A constructor may use its only field as the sole result index.' result.log
    grep -Fq 'propositional equality field' result.log
done
if "${ghc[@]}" -O0 -fforce-recomp -XStrictData -fdiagnostics-color=never \
        -c CheckedList.hs >strict.log 2>&1; then
    echo 'Accepted a retained constructor with StrictData'
    exit 1
fi
grep -Fq 'A retained constructor requires a lazy ordinary data declaration' strict.log
mkdir -p "$tmp/result"/{provider,client}
cp CheckedList.hs "$tmp/result/provider/"
cat >"$tmp/result/client/ResultClient.hs" <<'HS'
{-# LANGUAGE GADTs, NoExperimentalDependentTypes #-}
module Main where

import Data.IORef (newIORef, readIORef)
import CheckedList

roundList :: CheckedList xs -> CheckedList xs
roundList (CheckedList stored) = CheckedList stored

roundTag :: Tagged tag -> Tagged tag
roundTag (Dynamic stored) = Dynamic stored
roundTag Fixed = Fixed

dynamic :: Tagged tag -> Bool
dynamic (Dynamic _) = True
dynamic Fixed = False

main :: IO ()
main = do
    xs <- newIORef (error "forced retained list" :: [Bool]) >>= readIORef
    tag <- newIORef (error "forced retained ordering" :: Ordering) >>= readIORef
    let listLazy = case CheckedList xs of
            indexed@(CheckedList _) -> case roundList indexed of CheckedList _ -> True
        tagLazy = case Dynamic tag of
            indexed -> dynamic (roundTag indexed)
    if listLazy && tagLazy && not (dynamic (roundTag Fixed))
        then pure ()
        else fail "retained constructor result changed its runtime witness"
HS
for opt in -O0 -O2; do
    (
        cd "$tmp/result/provider"
        "${ghc[@]}" "$opt" -fforce-recomp -DDIRECT_RESULT -fomit-interface-pragmas \
            -fno-write-if-simplified-core -c CheckedList.hs
        cp CheckedList.hi ../client/
        cd ../client
        "${ghc[@]}" "$opt" -fforce-recomp -Werror=incomplete-patterns -c ResultClient.hs
        "${ghc[@]}" "$opt" ResultClient.o ../provider/CheckedList.o -o result-client
        ./result-client
    )
done
cat >ScopedLength.hs <<'HS'
{-# LANGUAGE CPP, EmptyCase, ExperimentalDependentTypes, GADTs, LinearTypes, ScopedTypeVariables #-}
module ScopedLength where
import PackedVecLibrary (sourceLength)
import ParameterizedIndexLibrary (Fin(..))
#ifdef UNRELATED
lookupAt :: forall n m a. foreach (xs :: [a]) -> (m ~ sourceLength xs) => Fin n %1 -> a
#else
lookupAt :: forall n m a. foreach (xs :: [a]) -> (m ~ sourceLength xs, n ~ m) => Fin n %1 -> a
#endif
#ifndef OMIT
lookupAt [] i = case i of {}
#endif
lookupAt (x : _) FZ = x
lookupAt (_ : xs) (FS i) = lookupAt xs i
HS
for clause in explicit omitted; do
    flags=()
    if [[ $clause == omitted ]]; then flags=(-DOMIT); fi
    "${ghc[@]}" -O0 -fforce-recomp -dlinear-core-lint -Werror=incomplete-patterns "${flags[@]}" -c ScopedLength.hs
done
if "${ghc[@]}" -O0 -fforce-recomp -DUNRELATED -DOMIT -fdefer-type-errors -fdiagnostics-color=never -c ScopedLength.hs >unrelated.log 2>&1; then
    echo 'Used an unrelated list length to omit a retained equation'
    exit 1
fi
grep -Fq 'ScopedLength.hs:13:1: error: [GHC-83605]' unrelated.log
cat >WrittenLists.hs <<'HS'
{-# LANGUAGE CPP, NoExperimentalDependentTypes #-}
module WrittenLists where
import AlignedList (zipWithExact)
import PackedVecLibrary (toList)
pairs :: a -> a -> b -> b -> [(a, b)]
#ifdef WRONG_ELEMENT
pairs x y u v = toList (zipWithExact [x, u] [u, v] (,))
#else
pairs x y u v = toList (zipWithExact [x, y] [u] (,))
#endif
HS
if "${ghc[@]}" -O0 -fdiagnostics-color=never -c WrittenLists.hs >length.log 2>&1; then
    echo 'Accepted two list literals of different lengths'
    exit 1
fi
grep -Fq 'WrittenLists.hs:9:25: error: [GHC-18872]' length.log
grep -Fq "Couldn't match type" length.log
if "${ghc[@]}" -O0 -DWRONG_ELEMENT -fdiagnostics-color=never -c WrittenLists.hs >element.log 2>&1; then
    echo 'Accepted a list element of the wrong type'
    exit 1
fi
grep -Fq 'WrittenLists.hs:7:42: error: [GHC-25897]' element.log
cat >WrittenListTypes.hs <<'HS'
{-# LANGUAGE CPP, NoExperimentalDependentTypes, TypeApplications #-}
module WrittenListTypes where
import AlignedList (zipWithExact)
import PackedVecLibrary (toList)
wrong :: [Int]
#ifdef WRONG_KIND
wrong = toList (zipWithExact @Maybe [1,2] [10,20] (+))
#elif defined(TOO_MANY)
wrong = toList (zipWithExact @Int @Int @Int @Int [1,2] [10,20] (+))
#else
wrong = toList (zipWithExact @Char @Int @Int [5 + 1, 'x'] [10,20] (\_ y -> y))
#endif
HS
for mode in WRONG_KIND TOO_MANY WRONG_ELEMENT_TYPE; do
    if "${ghc[@]}" -O0 -D"$mode" -fdiagnostics-color=never -c WrittenListTypes.hs >type.log 2>&1; then
        echo "Accepted an invalid visible type application ($mode)"
        exit 1
    fi
    case "$mode" in
        WRONG_KIND) grep -Fq 'WrittenListTypes.hs:7:31: error: [GHC-83865]' type.log ;;
        TOO_MANY) grep -Fq 'WrittenListTypes.hs:9:17: error: [GHC-95781]' type.log ;;
        WRONG_ELEMENT_TYPE)
            grep -Fq 'WrittenListTypes.hs:11:47: error: [GHC-39999]' type.log
            grep -Fq 'Num Char' type.log ;;
    esac
done
cat >ScopedFields.hs <<'HS'
{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, TypeFamilies #-}
module ScopedFields where
import Data.Proxy

remember :: foreach (xs :: [Int]) -> Proxy xs
remember _ = Proxy

sameContents :: foreach (xs :: [Int]) -> foreach (ys :: [Int]) -> (xs ~ ys) => Bool
sameContents _ _ = True

#ifdef ESCAPE
result = let element = 1 :: Int in remember [element]
#elif defined(DIFFERENT)
result = let left = 1 :: Int; right = 2 :: Int in sameContents [left] [right]
#else
result = let element = 1 :: Int in sameContents [element] [element]
#endif
HS
"${ghc[@]}" -O0 -c ScopedFields.hs
if "${ghc[@]}" -O0 -fforce-recomp -DESCAPE -c ScopedFields.hs >escape-field.log 2>&1; then
    echo 'Accepted a returned local element index'
    exit 1
fi
grep -Eq '^ScopedFields.hs:12:[0-9]+: error: .GHC-83605.' escape-field.log
grep -Eq 'The runtime index .element. is out of scope here\.' escape-field.log
if "${ghc[@]}" -O0 -fforce-recomp -DDIFFERENT -c ScopedFields.hs >field-contents.log 2>&1; then
    echo 'Equated different list contents'
    exit 1
fi
grep -Eq '^ScopedFields.hs:14:[0-9]+: error: .GHC-18872.' field-contents.log

cat >MatrixLength.hs <<'HS'
{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs, ScopedTypeVariables, TypeFamilies #-}
module MatrixLength where
import AlignedList (zipWithExact)
import PackedVecLibrary (Nat(..), Vec(..), sourceLength, toList)

#ifdef OMIT
onlyNil :: forall a b. foreach (xs :: [a]) -> foreach (ys :: [b])
        -> (sourceLength xs ~ sourceLength ys) => Vec (a, b) (sourceLength xs)
onlyNil [] [] = VNil
#elif defined(WRONG)
wrong :: forall a b. foreach (xs :: [a]) -> foreach (ys :: [b])
      -> (sourceLength xs ~ sourceLength ys) => Vec (a, b) (sourceLength xs)
wrong [] [] = VNil
wrong (x : xs) (y : ys) = VNil
#else
useFallback :: forall a b. foreach (xs :: [a]) -> foreach (ys :: [b])
            -> (sourceLength xs ~ sourceLength ys) => [(a, b)]
useFallback [] [] = []
useFallback xs ys = contents
  where contents = toList (zipWithExact xs ys (,))

oneSided :: foreach (n :: Nat) -> (n ~ 'Z) => foreach (m :: Nat) -> Vec Bool m
oneSided Z Z = VNil
oneSided Z (S k) = VCons False (oneSided Z k)

ambient :: forall n. (n ~ 'Z) => foreach (x :: Nat) -> foreach (y :: Nat)
        -> (x ~ n, y ~ n) => Vec Bool x
ambient Z Z = VNil

#ifdef UNRELATED_AMBIENT
unrelated :: forall n p. (n ~ 'Z) => foreach (x :: Nat) -> foreach (y :: Nat)
          -> (x ~ p, y ~ p) => Vec Bool x
unrelated Z Z = VNil
#endif
#endif
HS
"${ghc[@]}" -O0 -Werror=incomplete-patterns -c MatrixLength.hs
if "${ghc[@]}" -O0 -DUNRELATED_AMBIENT -fforce-recomp -fdiagnostics-color=never -c MatrixLength.hs >ambient.log 2>&1; then
    echo 'Used an unrelated outer equality to omit a retained pair'
    exit 1
fi
grep -Fq 'Missing equation for the retained constructor pair (S, S).' ambient.log
if "${ghc[@]}" -O0 -DOMIT -fforce-recomp -fdefer-type-errors -fdiagnostics-color=never -c MatrixLength.hs >pair.log 2>&1; then
    echo 'Accepted an omitted cons/cons equation'
    exit 1
fi
grep -Fq 'MatrixLength.hs:9:1: error: [GHC-83605]' pair.log
grep -Fq 'Missing equation for the retained constructor pair (:, :).' pair.log
if "${ghc[@]}" -O0 -DWRONG -fforce-recomp -fdiagnostics-color=never -c MatrixLength.hs >result.log 2>&1; then
    echo 'Accepted an empty result for two nonempty lists'
    exit 1
fi
grep -Fq 'MatrixLength.hs:14:27: error: [GHC-83865]' result.log
if grep -Fq '$d~' result.log; then
    echo 'Reported an implicit dictionary as a source binding'
    exit 1
fi
cat >MissingSuccessor.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, ScopedTypeVariables #-}
module MissingSuccessor where
import PackedVecLibrary (sourceLength)
import ParameterizedIndexLibrary (Fin(..))
index :: forall a. foreach (xs :: [a]) -> Fin (sourceLength xs) -> a
index (x : _) FZ = x
HS
if "${ghc[@]}" -O0 -Werror=incomplete-patterns -c MissingSuccessor.hs >successor.log 2>&1; then
    echo 'Accepted an uncovered successor position'
    exit 1
fi
grep -Fq 'MissingSuccessor.hs:6:1: error: [GHC-62161]' successor.log
grep -Fq 'FS' successor.log
for variant in DISCARD DUPLICATE; do
    cp "$src/NativeLinearIndexColumns.hs" .
    if "${ghc[@]}" -O0 -fforce-recomp "-D$variant" -fdiagnostics-color=never -c NativeLinearIndexColumns.hs >bad-columns.log 2>&1; then
        echo "Accepted $variant of a linear callback"
        exit 1
    fi
    grep -Fq '[GHC-18872]' bad-columns.log
    test "$(grep -Fc 'In an equation for' bad-columns.log)" = 1
    grep -Fq "multiplicity of" bad-columns.log
done

cat >BadCursor.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, LinearTypes, ScopedTypeVariables, TypeApplications #-}
module BadCursor where
import GHC.Types (Multiplicity)
import PackedVecLibrary (sourceLength)
import ParameterizedIndexLibrary (Fin, indexGeneric, mkFinGeneric)

onlyOnce :: forall a. foreach (xs :: [a]) -> Fin (sourceLength xs) %1 -> a
onlyOnce xs i = indexGeneric xs i

fixed :: [Char] -> Int -> Maybe Char
fixed xs n = fmap (onlyOnce xs) (mkFinGeneric xs n)

independent :: [Char] -> [Char] -> Int -> Maybe Char
independent xs ys n = fmap (indexGeneric xs) (mkFinGeneric ys n)

rigid :: forall (p :: Multiplicity) a b. (a %p -> b) -> Maybe a -> Maybe b
rigid f = fmap f
HS
if "${ghc[@]}" -O0 -fdiagnostics-color=never -c BadCursor.hs >bad-cursor.log 2>&1; then
    echo 'Accepted a mismatched callback or an independent position'
    exit 1
fi
grep -Fq 'BadCursor.hs:11:19: error: [GHC-83865]' bad-cursor.log
grep -Fq 'BadCursor.hs:14:28: error: [GHC-18872]' bad-cursor.log
grep -Fq 'sourceLength xs' bad-cursor.log
grep -Fq 'sourceLength ys' bad-cursor.log
grep -Fq 'BadCursor.hs:17:16: error: [GHC-25897]' bad-cursor.log
cat >InvalidListIndex.hs <<'HS'
{-# LANGUAGE DataKinds, LinearTypes, NoExperimentalDependentTypes, ScopedTypeVariables, TypeApplications #-}
module InvalidListIndex where
import ParameterizedIndexLibrary (Fin(FZ), indexGeneric, mkFinGeneric)

empty :: Char
empty = indexGeneric @Char [] FZ

linear :: [Char] %1 -> Bool
linear xs = case mkFinGeneric xs 0 of
    Just _ -> True
    Nothing -> False

rigid :: forall a. a -> Bool
rigid xs = case mkFinGeneric @Char xs 0 of
    Just _ -> True
    Nothing -> False
HS
if "${ghc[@]}" -O0 -fdiagnostics-color=never -c InvalidListIndex.hs >invalid.log 2>&1; then
    echo 'Accepted an invalid source-length index'
    exit 1
fi
grep -Fq 'InvalidListIndex.hs:6:31: error: [GHC-83865]' invalid.log
grep -Fq 'InvalidListIndex.hs:9:31: error: [GHC-83605]' invalid.log
grep -Fq 'InvalidListIndex.hs:14:36: error: [GHC-25897]' invalid.log
cat >DifferentPredecessor.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module DifferentPredecessor where
import Data.Type.Equality ((:~:)(Refl))
import ParameterizedIndexLibrary (Fin(..))
wrong :: forall n. foreach (p :: Fin n) -> foreach (q :: Fin n) -> 'FS q :~: 'FS p
wrong _ _ = Refl
HS
if "${ghc[@]}" -O0 -ferror-spans -c DifferentPredecessor.hs >reject.log 2>&1; then
    echo 'Accepted an independent predecessor'
    exit 1
fi
grep -F 'DifferentPredecessor.hs:6:13-16: error:' reject.log >/dev/null

# A previous reader can still read a vector operand with the original shape.
cp "$src"/{NativeGeneralGadtOperandProvider,NativeIndexedOperandWire}.hs .
"${ghc[@]}" -O2 -c NativeGeneralGadtOperandProvider.hs NativeIndexedOperandWire.hs
"${ghc[@]}" --show-iface NativeGeneralGadtOperandProvider.hi > new-wire.log
grep -Fq 'general-indexed-constructor-operand' new-wire.log
"${ghc[@]}" --show-iface NativeIndexedOperandWire.hi > vector-wire.log
grep -Fq 'indexed-constructor-operand' vector-wire.log
if grep -Fq 'general-indexed-constructor-operand' vector-wire.log; then
    echo 'Wrote the extended operand header for a vector'
    exit 1
fi
if [[ $# -ge 3 ]]; then
    "$3" "-B$2" --show-iface NativeIndexedOperandWire.hi > old-vector.log
    if "$3" "-B$2" --show-iface NativeGeneralGadtOperandProvider.hi > old-fields.log 2>&1; then
        echo 'Previous reader accepted the new operand header'
        exit 1
    fi
    grep -Fq 'mismatched interface file versions' old-fields.log
    grep -Fq 'general-indexed-constructor-operand' old-fields.log
fi

mkdir "$tmp/multi-index-client"
cp NativeGeneralGadtOperandProvider.hi "$tmp/multi-index-client/"
cd "$tmp/multi-index-client"
cat >Main.hs <<'HS'
{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes #-}
module Main where

import Data.Proxy (Proxy(..))
import NativeGeneralGadtOperandProvider (Both(..), make)

fixed :: (Both (Maybe Bool) [Ordering], Proxy ('Mk2 'LT 'False))
fixed = make False LT

lazy :: Bool -> Char -> Bool
lazy x y = case fst (make x y) of Mk2 _ _ -> True

main :: IO ()
main = case (fixed, lazy undefined undefined) of
    ((Mk2 LT False, Proxy), True) -> pure ()
    _ -> fail "wrong indexed constructor fields"
HS
"${ghc[@]}" -O2 -ddump-inlinings -dsuppress-module-prefixes -c Main.hs > inlinings.log
grep -Fxq 'Inlining done: make' inlinings.log
"${ghc[@]}" Main.o "$out/client/NativeGeneralGadtOperandProvider.o" -o run
./run
