{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds  #-}
{- |
'Foldable' functions, with wrappers like the "Safe" module.
-}
module Safe.Foldable(
    -- * New functions
    findJust,
    -- * Safe wrappers
    foldl1May, foldl1Def, foldl1Note,
    foldr1May, foldr1Def, foldr1Note,
    findJustDef, findJustNote,
    minimumMay, minimumNote,
    maximumMay, maximumNote,
    minimumByMay, minimumByNote,
    maximumByMay, maximumByNote,
    maximumBoundBy, minimumBoundBy,
    maximumBounded, maximumBound,
    minimumBounded, minimumBound,
    -- * Discouraged
    minimumDef, maximumDef, minimumByDef, maximumByDef,
    -- * Deprecated
    foldl1Safe, foldr1Safe, findJustSafe,
    ) where

import Safe.Util
import Data.Foldable as F
import Data.Maybe
import Data.Monoid
import Prelude
import Safe.Partial


---------------------------------------------------------------------
-- UTILITIES

fromNote :: Partial => String -> String -> Maybe a -> a
fromNote = fromNoteModule "Safe.Foldable"


---------------------------------------------------------------------
-- WRAPPERS

foldl1May, foldr1May :: Foldable t => (a -> a -> a) -> t a -> Maybe a
foldl1May = liftMay F.null . F.foldl1
foldr1May = liftMay F.null . F.foldr1

foldl1Note, foldr1Note :: (Partial, Foldable t) => String -> (a -> a -> a) -> t a -> a
foldl1Note note f x = withFrozenCallStack $ fromNote note "foldl1Note on empty" $ foldl1May f x
foldr1Note note f x = withFrozenCallStack $ fromNote note "foldr1Note on empty" $ foldr1May f x

minimumMay, maximumMay :: (Foldable t, Ord a) => t a -> Maybe a
minimumMay = liftMay F.null F.minimum
maximumMay = liftMay F.null F.maximum

minimumNote, maximumNote :: (Partial, Foldable t, Ord a) => String -> t a -> a
minimumNote note x = withFrozenCallStack $ fromNote note "minimumNote on empty" $ minimumMay x
maximumNote note x = withFrozenCallStack $ fromNote note "maximumNote on empty" $ maximumMay x

minimumByMay, maximumByMay :: Foldable t => (a -> a -> Ordering) -> t a -> Maybe a
minimumByMay = liftMay F.null . F.minimumBy
maximumByMay = liftMay F.null . F.maximumBy

minimumByNote, maximumByNote :: (Partial, Foldable t) => String -> (a -> a -> Ordering) -> t a -> a
minimumByNote note f x = withFrozenCallStack $ fromNote note "minimumByNote on empty" $ minimumByMay f x
maximumByNote note f x = withFrozenCallStack $ fromNote note "maximumByNote on empty" $ maximumByMay f x

-- | The largest element of a foldable structure with respect to the
-- given comparison function. The result is bounded by the value given as the first argument.
maximumBoundBy :: Foldable f => a -> (a -> a -> Ordering) -> f a -> a
maximumBoundBy x f xs = maximumBy f $ x : toList xs

-- | The smallest element of a foldable structure with respect to the
-- given comparison function. The result is bounded by the value given as the first argument.
minimumBoundBy :: Foldable f => a -> (a -> a -> Ordering) -> f a -> a
minimumBoundBy x f xs = minimumBy f $ x : toList xs

-- | The largest element of a foldable structure.
-- The result is bounded by the value given as the first argument.
maximumBound :: (Foldable f, Ord a) => a -> f a -> a
maximumBound x xs = maximum $ x : toList xs

-- | The smallest element of a foldable structure.
-- The result is bounded by the value given as the first argument.
minimumBound :: (Foldable f, Ord a) => a -> f a -> a
minimumBound x xs = minimum $ x : toList xs

-- | The largest element of a foldable structure.
-- The result is bounded by 'minBound'.
maximumBounded :: (Foldable f, Ord a, Bounded a) => f a -> a
maximumBounded = maximumBound minBound

-- | The largest element of a foldable structure.
-- The result is bounded by 'maxBound'.
minimumBounded :: (Foldable f, Ord a, Bounded a) => f a -> a
minimumBounded = minimumBound maxBound

-- |
-- > findJust op = fromJust . find op
findJust :: (Partial, Foldable t) => (a -> Bool) -> t a -> a
findJust f x = withFrozenCallStack $ fromNote "" "findJust, no matching value" $ F.find f x

findJustDef :: Foldable t => a -> (a -> Bool) -> t a -> a
findJustDef def = fromMaybe def .^ F.find

findJustNote :: (Partial, Foldable t) => String -> (a -> Bool) -> t a -> a
findJustNote note f x = withFrozenCallStack $ fromNote note "findJustNote, no matching value" $ F.find f x


---------------------------------------------------------------------
-- DISCOURAGED

-- | New users are recommended to use 'minimumBound' or 'maximumBound' instead.
minimumDef, maximumDef :: (Foldable t, Ord a) => a -> t a -> a
minimumDef def = fromMaybe def . minimumMay
maximumDef def = fromMaybe def . maximumMay

-- | New users are recommended to use 'minimumBoundBy' or 'maximumBoundBy' instead.
minimumByDef, maximumByDef :: Foldable t => a -> (a -> a -> Ordering) -> t a -> a
minimumByDef def = fromMaybe def .^ minimumByMay
maximumByDef def = fromMaybe def .^ maximumByMay

-- | New users are recommended to use 'foldr1May' or 'foldl1May' instead.
foldl1Def, foldr1Def :: Foldable t => a -> (a -> a -> a) -> t a -> a
foldl1Def def = fromMaybe def .^ foldl1May
foldr1Def def = fromMaybe def .^ foldr1May


---------------------------------------------------------------------
-- DEPRECATED

{-# DEPRECATED foldl1Safe "Use @foldl f mempty@ instead." #-}
foldl1Safe :: (Monoid m, Foldable t) => (m -> m -> m) -> t m -> m
foldl1Safe fun = F.foldl fun mempty

{-# DEPRECATED foldr1Safe "Use @foldr f mempty@ instead." #-}
foldr1Safe :: (Monoid m, Foldable t) => (m -> m -> m) -> t m -> m
foldr1Safe fun = F.foldr fun mempty

{-# DEPRECATED findJustSafe "Use @findJustDef mempty@ instead." #-}
findJustSafe :: (Monoid m, Foldable t) => (m -> Bool) -> t m -> m
findJustSafe = findJustDef mempty
