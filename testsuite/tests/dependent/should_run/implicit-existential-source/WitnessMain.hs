{-# LANGUAGE GADTs #-}

module WitnessMain where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.List (isPrefixOf)
import Filter (Hidden, filterVec, toList)
import Vector
import WitnessSchema (ProjectionPair, projectionLazy, projectionPair)

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

projected :: ProjectionPair -> Int
projected p = snd p (fst p)

fromProducer b = snd (projectionPair b) (fst (projectionPair b))

byCase :: ProjectionPair -> Int
byCase p = case p of (x, k) -> k x

ignoreProjection :: Int -> Int
ignoreProjection _ = 19
{-# NOINLINE ignoreProjection #-}

skipProjection :: ProjectionPair -> Int
skipProjection p = ignoreProjection (snd p (fst p))

skipProducer :: Bool -> Int
skipProducer b = ignoreProjection (snd (projectionPair b) (fst (projectionPair b)))

main :: IO ()
main = do
    print (fromLet True, fromLet False, fromArg (package True))
    print (bottom (error "forced the package"), take 1 (toList (repack (package True))))
    let inputs = [projectionPair True, projectionPair False, projectionLazy]
    print (map projected inputs, map byCase inputs)
    print (map fromProducer [True, False], skipProducer (error "forced the producer input"))
    let missing = error "forced the projection package"
    print (skipProjection missing, ignoreProjection (byCase missing))
    result <- try @ErrorCall (evaluate (projected missing))
    case result of
        Left ex | "forced the projection package" `isPrefixOf` displayException ex ->
            putStrLn "the projection demanded the package"
        _ -> error "expected the original projection package exception"
