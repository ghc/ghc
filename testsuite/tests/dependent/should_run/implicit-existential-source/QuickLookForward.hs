{-# LANGUAGE DataKinds, ImpredicativeTypes, NoExperimentalDependentTypes, ScopedTypeVariables, TypeFamilies #-}
module QuickLookForward where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import Control.Monad (unless)
import Data.Bool (bool)
import Data.Proxy (Proxy(..))
import Filter (Hidden, filterVec, fromList, partitionVec, toList)
import ForwardSchemas
import Nested (packPair)
import NestedSignature (PackedPair)
import Vector (Vec(..), consumeVector)

apply :: (a -> Int) -> a -> Int
apply f x = f x

missing :: SlotBox Int
missing = error "unused package"

listCells :: Vec n a -> [a]
listCells = foldr (:) []

partitionHeads :: Vec n Int -> ([Int], [Int])
partitionHeads xs =
    (take 1 (listCells (fst (partitionVec odd xs))),
     take 1 (listCells (snd (partitionVec odd xs))))

refilter :: Vec n Int -> Hidden Int
refilter xs =
    let ys = consumeVector (filterVec (> 3)) (filterVec odd xs)
    in ys

-- | Open the vector selected by the conditional.
firstSelected :: Bool -> Hidden a -> Hidden a -> [a]
firstSelected flag left right =
    take 1 (listCells (if flag then id left else right))

-- | Read one cell from the selected list.
firstFromList :: Bool -> [a] -> [a] -> [a]
firstFromList flag left right =
    take 1 (listCells (if flag then fromList left else fromList right))

type family Forwarded (tag :: Bool) a where
    Forwarded 'True a = a

-- | Keep a package when the formal type reduces to that package.
forwardConditional :: Proxy (Hidden Int) -> Bool -> Hidden Int -> Hidden Int -> Bool
forwardConditional shape flag left right =
    accepts (Proxy :: Proxy 'True) shape (if flag then left else right)
  where
    accepts :: Proxy tag -> Proxy a -> Forwarded tag a -> Bool
    accepts _ _ _ = True

-- | Infer the result of a callback whose element type is fixed.
refilterMapped :: Vec n Int -> Hidden Int
refilterMapped xs =
    let kept = consumeVector mapThenKeep (filterVec odd xs)
    in kept
  where
    mapThenKeep :: Vec j Int -> Hidden Int
    mapThenKeep = filterVec (> 6) . increment

    increment :: Vec j Int -> Vec j Int
    increment Nil = Nil
    increment (Cons x rest) = Cons (x + 1) (increment rest)

-- | Keep both hidden lengths in the result of a locally declared callback.
refilterBoth :: forall a i j. Integral a => Vec i a -> Vec j a -> ([a], [a])
refilterBoth xs ys =
    let rows = withBoth (producer id) (filterVec odd xs) (filterVec even ys)
    in rowCells rows
  where
    producer :: forall n. (forall k. Vec k a -> Vec k a)
             -> forall m. Vec n a -> Vec m a -> PackedPair a
    producer transform left right =
        packPair (filterVec (> 3) (transform left)) (filterVec (< 10) (transform right))

    withBoth :: (forall n m. Vec n a -> Vec m a -> r) -> Vec l a -> Vec k a -> r
    withBoth use left right = use left right

    rowCells :: PackedPair a -> ([a], [a])
    rowCells rows = case rows of
        (left, right) -> (listCells left, listCells right)

main :: IO ()
main = do
    print (apply (\h -> slotLabel h) (id (slotBox (7 :: Int))),
           apply (\h -> inspectRanked h) (id ranked),
           slotLabel $ bool missing (slotBox (9 :: Int)) True)
    forced <- try (evaluate (slotLabel (bool missing (slotBox (0 :: Int)) False)))
    case forced of
        Left ex | takeWhile (/= '\n') (displayException (ex :: ErrorCall)) == "unused package" ->
            putStrLn "selected package raised the original exception"
        _ -> fail "unexpected package demand"
    print (partitionHeads (Cons 7 (Cons 2 (error "unused partition tail"))))
    let oddCells = filterVec odd (Cons 7 (error "unused odd tail"))
        evenCells = filterVec even (Cons 2 (error "unused even tail"))
        unused = error "unused conditional branch"
    unless (firstSelected True oddCells unused == [7]
         && firstSelected False unused evenCells == [2]) $
        fail "opening a conditional changed branch or tail demand"
    unless (firstFromList True (7 : error "unused list tail") (error "unused right list") == [7 :: Int]
         && firstFromList False (error "unused left list") ('a' : error "unused list tail") == "a") $
        fail "inferring a conditional changed list demand"
    unless (forwardConditional Proxy (error "unused flag") unused unused) $
        fail "forwarding at a family domain forced a package"
    let filteredCells input = toList (refilter input)
    unless (take 1 (filteredCells (Cons 3 (Cons 7 (error "unused refilter tail")))) == [7]
         && filteredCells (Cons 3 (Cons 7 (Cons 8 (Cons 9 Nil)))) == [7, 9]
         && null (filteredCells Nil)) $
        fail "the ranked callback changed filter order or demand"
    unless (take 1 (toList (refilterMapped (Cons 7 (error "unused mapped tail")))) == [8]
         && toList (refilterMapped (Cons 7 (Cons 2 Nil))) == [8]
         && null (toList (refilterMapped (Cons 5 (Cons 2 Nil))))) $
        fail "the concrete callback changed the filtered payload or demand"
    unless (refilterBoth (Cons 3 (Cons 7 Nil)) (Cons 2 (Cons 4 (Cons 10 Nil))) == ([7], [2, 4])
         && take 1 (fst (refilterBoth (Cons 7 (error "unused left tail")) (error "unused right vector"))) == [7]
         && take 1 (snd (refilterBoth (error "unused left vector") (Cons 2 (error "unused right tail")))) == [2]) $
        fail "the local ranked producer changed row lengths or demand"
