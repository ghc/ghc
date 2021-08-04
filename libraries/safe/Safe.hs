{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds  #-}
{- |
A module wrapping @Prelude@/@Data.List@ functions that can throw exceptions, such as @head@ and @!!@.
Each unsafe function has up to four variants, e.g. with @tail@:

* @'tail' :: [a] -> [a]@, raises an error on @tail []@.

* @'tailMay' :: [a] -> /Maybe/ [a]@, turns errors into @Nothing@.

* @'tailDef' :: /[a]/ -> [a] -> [a]@, takes a default to return on errors.

* @'tailNote' :: 'Partial' => /String/ -> [a] -> [a]@, takes an extra argument which supplements the error message.

* @'tailSafe' :: [a] -> [a]@, returns some sensible default if possible, @[]@ in the case of @tail@.

All functions marked with the @'Partial'@ constraint are not total, and will produce stack traces on error, on GHC
versions which support them (see "GHC.Stack").

This module also introduces some new functions, documented at the top of the module.
-}

module Safe(
    -- * New functions
    abort, at, lookupJust, findJust, elemIndexJust, findIndexJust,
    -- * Safe wrappers
    tailMay, tailDef, tailNote, tailSafe,
    initMay, initDef, initNote, initSafe,
    headMay, headDef, headNote,
    lastMay, lastDef, lastNote,
    minimumMay, minimumNote,
    maximumMay, maximumNote,
    minimumByMay, minimumByNote,
    maximumByMay, maximumByNote,
    minimumBoundBy, maximumBoundBy,
    maximumBounded, maximumBound,
    minimumBounded, minimumBound,
    foldr1May, foldr1Def, foldr1Note,
    foldl1May, foldl1Def, foldl1Note,
    foldl1May', foldl1Def', foldl1Note',
    scanl1May, scanl1Def, scanl1Note,
    scanr1May, scanr1Def, scanr1Note,
    cycleMay, cycleDef, cycleNote,
    fromJustDef, fromJustNote,
    assertNote,
    atMay, atDef, atNote,
    readMay, readDef, readNote, readEitherSafe,
    lookupJustDef, lookupJustNote,
    findJustDef, findJustNote,
    elemIndexJustDef, elemIndexJustNote,
    findIndexJustDef, findIndexJustNote,
    toEnumMay, toEnumDef, toEnumNote, toEnumSafe,
    succMay, succDef, succNote, succSafe,
    predMay, predDef, predNote, predSafe,
    indexMay, indexDef, indexNote,
    -- * Discouraged
    minimumDef, maximumDef, minimumByDef, maximumByDef
    ) where

import Safe.Util
import Data.Ix
import Data.List
import Data.Maybe
import Safe.Partial

---------------------------------------------------------------------
-- UTILITIES

fromNote :: Partial => String -> String -> Maybe a -> a
fromNote = fromNoteModule "Safe"

fromNoteEither :: Partial => String -> String -> Either String a -> a
fromNoteEither = fromNoteEitherModule "Safe"


---------------------------------------------------------------------
-- IMPLEMENTATIONS

-- | Synonym for 'error'. Used for instances where the program
--   has decided to exit because of invalid user input, or the user pressed
--   quit etc. This function allows 'error' to be reserved for programmer errors.
abort :: Partial => String -> a
abort x = withFrozenCallStack (error x)


at_ :: [a] -> Int -> Either String a
at_ xs o | o < 0 = Left $ "index must not be negative, index=" ++ show o
         | otherwise = f o xs
    where f 0 (x:xs) = Right x
          f i (x:xs) = f (i-1) xs
          f i [] = Left $ "index too large, index=" ++ show o ++ ", length=" ++ show (o-i)


---------------------------------------------------------------------
-- WRAPPERS

-- |
-- > tailMay [] = Nothing
-- > tailMay [1,3,4] = Just [3,4]
tailMay :: [a] -> Maybe [a]
tailMay = liftMay null tail

-- |
-- > tailDef [12] [] = [12]
-- > tailDef [12] [1,3,4] = [3,4]
tailDef :: [a] -> [a] -> [a]
tailDef def = fromMaybe def . tailMay

-- |
-- > tailNote "help me" [] = error "Safe.tailNote [], help me"
-- > tailNote "help me" [1,3,4] = [3,4]
tailNote :: Partial => String -> [a] -> [a]
tailNote note x = withFrozenCallStack $ fromNote note "tailNote []" $ tailMay x

-- |
-- > tailSafe [] = []
-- > tailSafe [1,3,4] = [3,4]
tailSafe :: [a] -> [a]
tailSafe = tailDef []


initMay :: [a] -> Maybe [a]
initMay = liftMay null init

initDef :: [a] -> [a] -> [a]
initDef def = fromMaybe def . initMay

initNote :: Partial => String -> [a] -> [a]
initNote note x = withFrozenCallStack $ fromNote note "initNote []" $ initMay x

initSafe :: [a] -> [a]
initSafe = initDef []



headMay, lastMay :: [a] -> Maybe a
headMay = liftMay null head
lastMay = liftMay null last

headDef, lastDef :: a -> [a] -> a
headDef def = fromMaybe def . headMay
lastDef def = fromMaybe def . lastMay

headNote, lastNote :: Partial => String -> [a] -> a
headNote note x = withFrozenCallStack $ fromNote note "headNote []" $ headMay x
lastNote note x = withFrozenCallStack $ fromNote note "lastNote []" $ lastMay x

minimumMay, maximumMay :: Ord a => [a] -> Maybe a
minimumMay = liftMay null minimum
maximumMay = liftMay null maximum

minimumNote, maximumNote :: (Partial, Ord a) => String -> [a] -> a
minimumNote note x = withFrozenCallStack $ fromNote note "minumumNote []" $ minimumMay x
maximumNote note x = withFrozenCallStack $ fromNote note "maximumNote []" $ maximumMay x

minimumByMay, maximumByMay :: (a -> a -> Ordering) -> [a] -> Maybe a
minimumByMay = liftMay null . minimumBy
maximumByMay = liftMay null . maximumBy

minimumByNote, maximumByNote :: Partial => String -> (a -> a -> Ordering) -> [a] -> a
minimumByNote note f x = withFrozenCallStack $ fromNote note "minumumByNote []" $ minimumByMay f x
maximumByNote note f x = withFrozenCallStack $ fromNote note "maximumByNote []" $ maximumByMay f x

-- | The largest element of a list with respect to the
-- given comparison function. The result is bounded by the value given as the first argument.
maximumBoundBy :: a -> (a -> a -> Ordering) -> [a] -> a
maximumBoundBy x f xs = maximumBy f $ x : xs

-- | The smallest element of a list with respect to the
-- given comparison function. The result is bounded by the value given as the first argument.
minimumBoundBy :: a -> (a -> a -> Ordering) -> [a] -> a
minimumBoundBy x f xs = minimumBy f $ x : xs

-- | The largest element of a list.
-- The result is bounded by the value given as the first argument.
maximumBound :: Ord a => a -> [a] -> a
maximumBound x xs = maximum $ x : xs

-- | The smallest element of a list.
-- The result is bounded by the value given as the first argument.
minimumBound :: Ord a => a -> [a] -> a
minimumBound x xs = minimum $ x : xs

-- | The largest element of a list.
-- The result is bounded by 'minBound'.
maximumBounded :: (Ord a, Bounded a) => [a] -> a
maximumBounded = maximumBound minBound

-- | The largest element of a list.
-- The result is bounded by 'maxBound'.
minimumBounded :: (Ord a, Bounded a) => [a] -> a
minimumBounded = minimumBound maxBound

foldr1May, foldl1May, foldl1May' :: (a -> a -> a) -> [a] -> Maybe a
foldr1May = liftMay null . foldr1
foldl1May = liftMay null . foldl1
foldl1May' = liftMay null . foldl1'

foldr1Note, foldl1Note, foldl1Note' :: Partial => String -> (a -> a -> a) -> [a] -> a
foldr1Note note f x = withFrozenCallStack $ fromNote note "foldr1Note []" $ foldr1May f x
foldl1Note note f x = withFrozenCallStack $ fromNote note "foldl1Note []" $ foldl1May f x
foldl1Note' note f x = withFrozenCallStack $ fromNote note "foldl1Note []" $ foldl1May' f x

scanr1May, scanl1May :: (a -> a -> a) -> [a] -> Maybe [a]
scanr1May = liftMay null . scanr1
scanl1May = liftMay null . scanl1

scanr1Def, scanl1Def :: [a] -> (a -> a -> a) -> [a] -> [a]
scanr1Def def = fromMaybe def .^ scanr1May
scanl1Def def = fromMaybe def .^ scanl1May

scanr1Note, scanl1Note :: Partial => String -> (a -> a -> a) -> [a] -> [a]
scanr1Note note f x = withFrozenCallStack $ fromNote note "scanr1Note []" $ scanr1May f x
scanl1Note note f x = withFrozenCallStack $ fromNote note "scanl1Note []" $ scanl1May f x

cycleMay :: [a] -> Maybe [a]
cycleMay = liftMay null cycle

cycleDef :: [a] -> [a] -> [a]
cycleDef def = fromMaybe def . cycleMay

cycleNote :: Partial => String -> [a] -> [a]
cycleNote note x = withFrozenCallStack $ fromNote note "cycleNote []" $ cycleMay x

-- | An alternative name for 'fromMaybe', to fit the naming scheme of this package.
--   Generally using 'fromMaybe' directly would be considered better style.
fromJustDef :: a -> Maybe a -> a
fromJustDef  = fromMaybe

fromJustNote :: Partial => String -> Maybe a -> a
fromJustNote note x = withFrozenCallStack $ fromNote note "fromJustNote Nothing" x

assertNote :: Partial => String -> Bool -> a -> a
assertNote note True val = val
assertNote note False val = withFrozenCallStack $ fromNote note "assertNote False" Nothing


-- | Synonym for '!!', but includes more information in the error message.
at :: Partial => [a] -> Int -> a
at = fromNoteEither "" "at" .^ at_

atMay :: [a] -> Int -> Maybe a
atMay = eitherToMaybe .^ at_

atDef :: a -> [a] -> Int -> a
atDef def = fromMaybe def .^ atMay

atNote :: Partial => String -> [a] -> Int -> a
atNote note f x = withFrozenCallStack $ fromNoteEither note "atNote" $ at_ f x

-- | This function provides a more precise error message than 'readEither' from 'base'.
readEitherSafe :: Read a => String -> Either String a
readEitherSafe s = case [x | (x,t) <- reads s, ("","") <- lex t] of
        [x] -> Right x
        []  -> Left $ "no parse on " ++ prefix
        _   -> Left $ "ambiguous parse on " ++ prefix
    where
        maxLength = 15
        prefix = '\"' : a ++ if length s <= maxLength then b ++ "\"" else "...\""
            where (a,b) = splitAt (maxLength - 3) s

readMay :: Read a => String -> Maybe a
readMay = eitherToMaybe . readEitherSafe

readDef :: Read a => a -> String -> a
readDef def = fromMaybe def . readMay

-- | 'readNote' uses 'readEitherSafe' for the error message.
readNote :: (Partial, Read a) => String -> String -> a
readNote note x = withFrozenCallStack $ fromNoteEither note "readNote" $ readEitherSafe x

-- |
-- > lookupJust key = fromJust . lookup key
lookupJust :: (Eq a, Partial) => a -> [(a,b)] -> b
lookupJust x xs = withFrozenCallStack $ fromNote "" "lookupJust, no matching value" $ lookup x xs

lookupJustDef :: Eq a => b -> a -> [(a,b)] -> b
lookupJustDef def = fromMaybe def .^ lookup

lookupJustNote :: (Partial, Eq a) => String -> a -> [(a,b)] -> b
lookupJustNote note x xs = withFrozenCallStack $ fromNote note "lookupJustNote, no matching value" $ lookup x xs

-- |
-- > findJust op = fromJust . find op
findJust :: (a -> Bool) -> [a] -> a
findJust = fromNote "" "findJust, no matching value" .^ find

findJustDef :: a -> (a -> Bool) -> [a] -> a
findJustDef def = fromMaybe def .^ find

findJustNote :: Partial => String -> (a -> Bool) -> [a] -> a
findJustNote note f x = withFrozenCallStack $ fromNote note "findJustNote, no matching value" $ find f x

-- |
-- > elemIndexJust op = fromJust . elemIndex op
elemIndexJust :: (Partial, Eq a) => a -> [a] -> Int
elemIndexJust x xs = withFrozenCallStack $ fromNote "" "elemIndexJust, no matching value" $ elemIndex x xs

elemIndexJustDef :: Eq a => Int -> a -> [a] -> Int
elemIndexJustDef def = fromMaybe def .^ elemIndex

elemIndexJustNote :: (Partial, Eq a) => String -> a -> [a] -> Int
elemIndexJustNote note x xs = withFrozenCallStack $ fromNote note "elemIndexJustNote, no matching value" $ elemIndex x xs

-- |
-- > findIndexJust op = fromJust . findIndex op
findIndexJust :: (a -> Bool) -> [a] -> Int
findIndexJust f x = withFrozenCallStack $ fromNote "" "findIndexJust, no matching value" $ findIndex f x

findIndexJustDef :: Int -> (a -> Bool) -> [a] -> Int
findIndexJustDef def = fromMaybe def .^ findIndex

findIndexJustNote :: Partial => String -> (a -> Bool) -> [a] -> Int
findIndexJustNote note f x = withFrozenCallStack $ fromNote note "findIndexJustNote, no matching value" $ findIndex f x

-- From http://stackoverflow.com/questions/2743858/safe-and-polymorphic-toenum
-- answer by C. A. McCann
toEnumMay :: (Enum a, Bounded a) => Int -> Maybe a
toEnumMay i =
  let r = toEnum i
      max = maxBound `asTypeOf` r
      min = minBound `asTypeOf` r
  in if i >= fromEnum min && i <= fromEnum max
  then Just r
  else Nothing

toEnumDef :: (Enum a, Bounded a) => a -> Int -> a
toEnumDef def = fromMaybe def . toEnumMay

toEnumNote :: (Partial, Enum a, Bounded a) => String -> Int -> a
toEnumNote note x = withFrozenCallStack $ fromNote note "toEnumNote, out of range" $ toEnumMay x

toEnumSafe :: (Enum a, Bounded a) => Int -> a
toEnumSafe = toEnumDef minBound

succMay :: (Enum a, Eq a, Bounded a) => a -> Maybe a
succMay = liftMay (== maxBound) succ

succDef :: (Enum a, Eq a, Bounded a) => a -> a -> a
succDef def = fromMaybe def . succMay

succNote :: (Partial, Enum a, Eq a, Bounded a) => String -> a -> a
succNote note x = withFrozenCallStack $ fromNote note "succNote, out of range" $ succMay x

succSafe :: (Enum a, Eq a, Bounded a) => a -> a
succSafe = succDef maxBound

predMay :: (Enum a, Eq a, Bounded a) => a -> Maybe a
predMay = liftMay (== minBound) pred

predDef :: (Enum a, Eq a, Bounded a) => a -> a -> a
predDef def = fromMaybe def . predMay

predNote :: (Partial, Enum a, Eq a, Bounded a) => String -> a -> a
predNote note x = withFrozenCallStack $ fromNote note "predNote, out of range" $ predMay x

predSafe :: (Enum a, Eq a, Bounded a) => a -> a
predSafe = predDef minBound

indexMay :: Ix a => (a, a) -> a -> Maybe Int
indexMay b i = if inRange b i then Just (index b i) else Nothing

indexDef :: Ix a => Int -> (a, a) -> a -> Int
indexDef def b = fromMaybe def . indexMay b

indexNote :: (Partial, Ix a) => String -> (a, a) -> a -> Int
indexNote note x y = withFrozenCallStack $ fromNote note "indexNote, out of range" $ indexMay x y


---------------------------------------------------------------------
-- DISCOURAGED

-- | New users are recommended to use 'minimumBound' or 'maximumBound' instead.
minimumDef, maximumDef :: Ord a => a -> [a] -> a
minimumDef def = fromMaybe def . minimumMay
maximumDef def = fromMaybe def . maximumMay

-- | New users are recommended to use 'minimumBoundBy' or 'maximumBoundBy' instead.
minimumByDef, maximumByDef :: a -> (a -> a -> Ordering) -> [a] -> a
minimumByDef def = fromMaybe def .^ minimumByMay
maximumByDef def = fromMaybe def .^ maximumByMay


---------------------------------------------------------------------
-- DEPRECATED

{-# DEPRECATED foldr1Def "Use @foldr1May@ instead." #-}
{-# DEPRECATED foldl1Def "Use @foldl1May@ instead." #-}
{-# DEPRECATED foldl1Def' "Use @foldl1May'@ instead." #-}
foldr1Def, foldl1Def, foldl1Def' :: a -> (a -> a -> a) -> [a] -> a
foldr1Def def = fromMaybe def .^ foldr1May
foldl1Def def = fromMaybe def .^ foldl1May
foldl1Def' def = fromMaybe def .^ foldl1May'
