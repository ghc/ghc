{-# LANGUAGE DataKinds, GADTs, PolyKinds #-}
module NestedMain where

import Control.Exception
import Data.Proxy (Proxy(..))
import Nested
import NestedSignature
import Vector

list :: Vec n a -> [a]
list Nil = []
list (Cons x xs) = x : list xs

lists :: (Vec l a, Vec r a) -> ([a], [a])
lists (ls, rs) = (list ls, list rs)

cells :: PackedPair a -> ([a], [a])
cells p = lists p

built :: a -> ([a], [a])
built a = lists (buildPair a)

observeProduct :: HiddenProduct a -> ([a], [a])
observeProduct p = case p of
    Product ls rs -> (list ls, list rs)

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
    print (cells (partitionPair odd (Cons 7 (Cons 2 (Cons 9 Nil)))))
    print (cells (roundTrip (packPair (Cons 7 Nil) (Cons 2 (Cons 4 Nil)))))
    let yes = partitionPair odd (Cons 7 (error "forced the unseen passing tail"))
        no = partitionPair odd (Cons 2 (error "forced the unseen failing tail"))
    print (take 1 (fst (cells yes)), take 1 (snd (cells no)),
           built True, observeProduct (productPair Nil (Cons 9 Nil)))
    print (ignore bottom, kindOK hiddenKind)
    result <- try (evaluate (fst (cells bottom))) :: IO (Either ErrorCall [Int])
    case result of
        Left e | "forced the pair" == takeWhile (/= '\n') (displayException e) ->
            putStrLn "observing the payload demanded the pair"
        _ -> error "the observer missed pair demand"
