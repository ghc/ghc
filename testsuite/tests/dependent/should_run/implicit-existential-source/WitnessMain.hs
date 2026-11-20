{-# LANGUAGE GADTs #-}

module WitnessMain where

import Filter (Hidden, filterVec, toList)
import Vector

package :: Bool -> Hidden Int
package False = filterVec odd Nil
package True = filterVec odd (Cons 7 (error "forced the unseen tail"))

firstPair :: Vec n Int -> Vec n Int -> Maybe (Int, Int)
firstPair Nil Nil = Nothing
firstPair (Cons x _) (Cons y _) = Just (x, y)

fromLet :: Bool -> Maybe (Int, Int)
fromLet b = let p = package b in firstPair (p) p

fromArg :: Hidden Int -> Maybe (Int, Int)
fromArg p = firstPair p p

ignored :: Vec n Int -> Vec n Int -> Bool
ignored _ _ = True

bottom :: Hidden Int -> Bool
bottom p = ignored p p

repack :: Hidden Int -> Hidden Int
repack p = keep p p
  where
    keep :: Vec n Int -> Vec n Int -> Vec n Int
    keep xs _ = xs

main :: IO ()
main = do
    print (fromLet True, fromLet False, fromArg (package True))
    print (bottom (error "forced the package"), take 1 (toList (repack (package True))))
