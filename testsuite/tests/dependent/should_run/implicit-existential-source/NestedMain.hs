{-# LANGUAGE DataKinds, GADTs, LinearTypes, PolyKinds #-}
module NestedMain where

import Control.Exception
import Data.Proxy (Proxy(..))
import Nested
import NestedSignature
import StrictMatch (strictOrder)
import Vector

list :: Vec n a -> [a]
list Nil = []
list (Cons x xs) = x : list xs

lists :: (Vec l a, Vec r a) -> ([a], [a])
lists (ls, rs) = (list ls, list rs)

cells :: PackedPair a -> ([a], [a])
cells p = lists p

swapLet, swapCase :: PackedPair a -> PackedPair a
swapLet p = let result = case p of (xs, ys) -> (ys, xs) in result
swapCase p = case (case p of (xs, ys) -> (ys, xs)) of result -> result

partial :: PackedPair a -> Int -> ([a], [a])
partial p =
    let selected = prefixes p
    in selected
  where
    prefixes :: (Vec l a, Vec r a) -> Int -> ([a], [a])
    prefixes (ls, rs) n = (take n (list ls), take n (list rs))

built :: a -> ([a], [a])
built a = lists (buildPair a)

observeProduct :: HiddenProduct a -> ([a], [a])
observeProduct p = case p of
    Product ls rs -> (list ls, list rs)

observeEquation :: HiddenProduct a -> ([a], [a])
observeEquation (Product ls rs)
    | null (list ls) = (list rs, [])
observeEquation (Product ls rs) = (list ls, list rs)

observeIndependent :: HiddenProduct Int -> HiddenProduct Bool -> ([Int], [Bool])
observeIndependent (Product ls _) (Product _ rs) = (list ls, list rs)

ordered :: Bool -> HiddenProduct Int -> Bool
ordered True (Product _ _) = True
ordered False _ = False

consume :: HiddenUnit %1 -> ()
consume (Unit u) = u

ignore :: PackedPair Int -> Bool
ignore p = unused p
  where
    unused :: (Vec l Int, Vec r Int) -> Bool
    unused _ = True

kindOK :: PackedKind -> Bool
kindOK p = known p
  where
    known :: Proxy (v :: k) -> Bool
    known Proxy = True

bottom :: PackedPair Int
bottom = abort
  where
    abort :: (Vec 'Z Int, Vec 'Z Int)
    abort = error "forced the pair"

main :: IO ()
main = do
    print (cells (localPartition odd (Cons 7 (Cons 2 (Cons 9 Nil)))))
    print (cells (roundTrip (packPair (Cons 7 Nil) (Cons 2 (Cons 4 Nil)))))
    let unequal = packPair (Cons (7 :: Int) Nil) (Cons 2 (Cons 4 Nil))
        lazy = packPair (error "forced the other vector")
                        (Cons (7 :: Int) (error "forced the swapped tail"))
    print (cells (swapLet unequal), cells (swapCase unequal))
    print (take 1 (fst (cells (swapLet lazy))), take 1 (fst (cells (swapCase lazy))),
           ignore (swapLet bottom), ignore (swapCase bottom))
    let yes = partitionPair odd (Cons 7 (error "forced the unseen passing tail"))
        no = partitionPair odd (Cons 2 (error "forced the unseen failing tail"))
    print (take 1 (fst (cells yes)), take 1 (snd (cells no)),
           built True, observeProduct (productPair Nil (Cons 9 Nil)))
    print (ignore bottom, kindOK hiddenKind)
    _ <- evaluate (partial bottom)
    print (partial (partitionPair odd (Cons 7 (Cons 2 Nil))) 1)
    checkDemand bottom
    checkDemand (swapLet bottom)
    checkDemand (swapCase bottom)
    putStrLn "observing the payload demanded the pair"

    let rows = productPair (Cons (7 :: Int) (Cons 9 Nil)) (Cons 2 Nil)
        missing = error "forced the rows" :: HiddenProduct Int
    print (observeEquation rows, observeEquation (productPair Nil (Cons (9 :: Int) Nil)))
    print (observeIndependent rows (productPair Nil (Cons True Nil)))
    print (take 1 (fst (observeEquation (productPair (Cons (7 :: Int) (error "forced the unseen row tail")) Nil))))
    print (ordered False missing, strictOrder False missing, consume (hiddenUnit ()))
    demanded <- try (evaluate (ordered True missing)) :: IO (Either ErrorCall Bool)
    case demanded of
        Left e | "forced the rows" == takeWhile (/= '\n') (displayException e) ->
            putStrLn "a constructor equation demanded the rows"
        _ -> error "the equation missed row demand"
  where
    checkDemand :: PackedPair Int -> IO ()
    checkDemand p = do
        result <- try (evaluate (fst (cells p))) :: IO (Either ErrorCall [Int])
        case result of
            Left e | "forced the pair" == takeWhile (/= '\n') (displayException e) -> pure ()
            _ -> error "the observer missed pair demand"
