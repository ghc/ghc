{-# LANGUAGE DataKinds, GADTs, LinearTypes, NoExperimentalDependentTypes, NoImpredicativeTypes, PatternSynonyms #-}

module CaseMain where

import Control.Exception
import Control.Monad (unless)
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

-- | Store a filter result with a reader for its hidden length.
withFirst :: (a -> Bool) -> Vec n a -> FilteredWith a (Maybe a)
withFirst predicate xs = (filterVec predicate xs, firstCell)

firstCell :: Vec m a -> Maybe a
firstCell Nil = Nothing
firstCell (Cons x _) = Just x

withProduct, withProductFunction :: Vec n Int -> ProductWith Int (Maybe Int)
withProduct xs = Product (filterVec odd xs) firstCell
withProductFunction xs = makeProduct (filterVec odd xs) firstCell

withProductSection :: Vec n Int -> ProductWith Int Bool
withProductSection xs = (filterVec odd xs `makeProduct`) (const True)

readProduct :: ProductWith a r -> r
readProduct p = case p of Product xs reader -> reader xs

-- | Store a reader that can ignore the vector entirely.
withIgnored :: (a -> Bool) -> Vec n a -> FilteredWith a Bool
withIgnored predicate xs = (filterVec predicate xs, const True)

linearWith :: Hidden a %1 -> FilteredWith a Bool
linearWith p = (p, const True)

-- | Supply both fields from one opening of the package.
readFiltered :: FilteredWith a r -> r
readFiltered p = let (xs, readCells) = p in readCells xs

main :: IO ()
main = do
    let yes = filterVec odd (Cons (7 :: Int) (error "forced the unseen tail"))
        empty = filterVec odd Nil :: Hidden Int
        bottom = error "forced the selected spine" :: Hidden Int
    print (first yes, first empty, filteredFirst odd (Cons (9 :: Int) (error "forced the computed tail")))
    print (take 1 (toList (linearForward yes)), take 1 (forward yes), take 1 (toList (repack yes)))
    print (ignored bottom, synonym bottom)
    unless (readFiltered (withFirst odd (Cons (2 :: Int) (Cons 7 (error "unused tuple tail")))) == Just 7
         && readFiltered (withFirst odd (Nil :: Vec 'Z Int)) == Nothing
         && readFiltered (withIgnored odd (error "unused filter input" :: Vec 'Z Int))
         && readFiltered (linearWith (error "unused linear package" :: Hidden Int))) $
        fail "the tuple forced the discarded filter or used the wrong length"
    let kept = Cons 7 (error "unused product tail")
    unless (readProduct (withProduct kept) == Just 7
         && readProduct (withProductFunction kept) == Just 7
         && readProduct (withProductSection (error "unused product input" :: Vec 'Z Int))) $
        fail "the product forced the discarded filter or used the wrong length"
    answer <- try (evaluate (first bottom)) :: IO (Either ErrorCall (Maybe Int))
    case answer of
        Left e | "forced the selected spine" == takeWhile (/= '\n') (displayException e) ->
            putStrLn "constructor pattern demanded the package"
        _ -> error "constructor pattern missed demand"
