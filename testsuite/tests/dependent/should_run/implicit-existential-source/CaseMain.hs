{-# LANGUAGE GADTs, LinearTypes, PatternSynonyms #-}

module CaseMain where

import Control.Exception
import Filter
import Vector

first :: Hidden a -> Maybe a
first selection = case selection of
    Nil -> Nothing
    Cons x _ -> Just x

filteredFirst :: (a -> Bool) -> Vec n a -> Maybe a
filteredFirst p xs = case filterVec p xs of
    Nil -> Nothing
    Cons x _ -> Just x

linearForward :: Hidden Int %1 -> Hidden Int
linearForward selection = case selection of
    Nil -> Nil
    Cons x xs -> Cons x xs

repack :: Hidden a -> Hidden a
repack selection = case selection of
    Nil -> Nil
    vec@(Cons _ _) -> vec

pattern Keep :: Hidden a
pattern Keep <- _

forward :: Hidden a -> [a]
forward p = case p of q -> toList q

ignored :: Hidden a -> Bool
ignored p = case p of _ -> True

synonym :: Hidden a -> Bool
synonym p = case p of Keep -> True

main :: IO ()
main = do
    let yes = filterVec odd (Cons (7 :: Int) (error "forced the unseen tail"))
        empty = filterVec odd Nil :: Hidden Int
        bottom = error "forced the selected spine" :: Hidden Int
    print (first yes, first empty, filteredFirst odd (Cons (9 :: Int) (error "forced the computed tail")))
    print (take 1 (toList (linearForward yes)), take 1 (forward yes), take 1 (toList (repack yes)))
    print (ignored bottom, synonym bottom)
    answer <- try (evaluate (first bottom)) :: IO (Either ErrorCall (Maybe Int))
    case answer of
        Left e | "forced the selected spine" == takeWhile (/= '\n') (displayException e) ->
            putStrLn "constructor pattern demanded the package"
        _ -> error "constructor pattern missed demand"
