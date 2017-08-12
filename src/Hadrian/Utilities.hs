module Hadrian.Utilities (

    -- * List manipulation
    fromSingleton, replaceEq, minusOrd, intersectOrd, lookupAll,

    -- * String manipulation
    quote, yesNo,

    -- * FilePath manipulation
    unifyPath, (-/-), matchVersionedFilePath,

    -- * Miscellaneous
    UseColour (..), putColoured
    ) where

import Control.Monad
import Data.Char
import Data.List.Extra
import Development.Shake
import Development.Shake.FilePath
import System.Console.ANSI
import System.Info.Extra
import System.IO

-- | Extract a value from a singleton list, or terminate with an error message
-- if the list does not contain exactly one value.
fromSingleton :: String -> [a] -> a
fromSingleton _   [res] = res
fromSingleton msg _     = error msg

-- | Find and replace all occurrences of a value in a list.
replaceEq :: Eq a => a -> a -> [a] -> [a]
replaceEq from to = map (\cur -> if cur == from then to else cur)

-- Explicit definition to avoid dependency on Data.List.Ordered
-- | Difference of two ordered lists.
minusOrd :: Ord a => [a] -> [a] -> [a]
minusOrd [] _  = []
minusOrd xs [] = xs
minusOrd (x:xs) (y:ys) = case compare x y of
    LT -> x : minusOrd xs (y:ys)
    EQ ->     minusOrd xs ys
    GT ->     minusOrd (x:xs) ys

-- Explicit definition to avoid dependency on Data.List.Ordered. TODO: add tests
-- | Intersection of two ordered lists by a predicate.
intersectOrd :: (a -> b -> Ordering) -> [a] -> [b] -> [a]
intersectOrd cmp = loop
  where
    loop [] _ = []
    loop _ [] = []
    loop (x:xs) (y:ys) = case cmp x y of
        LT ->     loop xs (y:ys)
        EQ -> x : loop xs (y:ys)
        GT ->     loop (x:xs) ys

-- | Lookup all elements of a given sorted list in a given sorted dictionary.
-- @lookupAll list dict@ is equivalent to @map (flip lookup dict) list@ but has
-- linear complexity O(|list| + |dist|) instead of quadratic O(|list| * |dict|).
--
-- > lookupAll ["b", "c"] [("a", 1), ("c", 3), ("d", 4)] == [Nothing, Just 3]
-- > list & dict are sorted: lookupAll list dict == map (flip lookup dict) list
lookupAll :: Ord a => [a] -> [(a, b)] -> [Maybe b]
lookupAll []     _      = []
lookupAll (_:xs) []     = Nothing : lookupAll xs []
lookupAll (x:xs) (y:ys) = case compare x (fst y) of
    LT -> Nothing      : lookupAll xs (y:ys)
    EQ -> Just (snd y) : lookupAll xs (y:ys)
    GT -> lookupAll (x:xs) ys

-- | Add single quotes around a String.
quote :: String -> String
quote s = "'" ++ s ++ "'"

-- | Pretty-print a 'Bool' as a @"YES"@ or @"NO"@ string.
yesNo :: Bool -> String
yesNo True  = "YES"
yesNo False = "NO"

-- | Normalise a path and convert all path separators to @/@, even on Windows.
unifyPath :: FilePath -> FilePath
unifyPath = toStandard . normaliseEx

-- | Combine paths with a forward slash regardless of platform.
(-/-) :: FilePath -> FilePath -> FilePath
"" -/- b = b
a  -/- b
    | last a == '/' = a ++       b
    | otherwise     = a ++ '/' : b

infixr 6 -/-

-- | Given a @prefix@ and a @suffix@ check whether a 'FilePath' matches the
-- template @prefix ++ version ++ suffix@ where @version@ is an arbitrary string
-- comprising digits (@0-9@), dashes (@-@), and dots (@.@). Examples:
--
-- @
-- 'matchVersionedFilePath' "foo/bar"  ".a" "foo/bar.a"     '==' 'True'
-- 'matchVersionedFilePath' "foo/bar"  ".a" "foo\bar.a"     '==' 'False'
-- 'matchVersionedFilePath' "foo/bar"  "a"  "foo/bar.a"     '==' 'True'
-- 'matchVersionedFilePath' "foo/bar"  ""   "foo/bar.a"     '==' 'False'
-- 'matchVersionedFilePath' "foo/bar"  "a"  "foo/bar-0.1.a" '==' 'True'
-- 'matchVersionedFilePath' "foo/bar-" "a"  "foo/bar-0.1.a" '==' 'True'
-- 'matchVersionedFilePath' "foo/bar/" "a"  "foo/bar-0.1.a" '==' 'False'
-- @
matchVersionedFilePath :: String -> String -> FilePath -> Bool
matchVersionedFilePath prefix suffix filePath =
    case stripPrefix prefix filePath >>= stripSuffix suffix of
        Nothing      -> False
        Just version -> all (\c -> isDigit c || c == '-' || c == '.') version

data UseColour = Never | Auto | Always deriving (Eq, Show)

-- | A more colourful version of Shake's 'putNormal'.
putColoured :: UseColour -> ColorIntensity -> Color -> String -> Action ()
putColoured useColour intensity colour msg = do
    supported <- liftIO $ hSupportsANSI stdout
    let c Never  = False
        c Auto   = supported || isWindows -- Colours do work on Windows
        c Always = True
    when (c useColour) . liftIO $ setSGR [SetColor Foreground intensity colour]
    putNormal msg
    when (c useColour) . liftIO $ setSGR [] >> hFlush stdout
