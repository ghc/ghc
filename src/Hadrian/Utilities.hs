module Hadrian.Utilities (
    -- * List manipulation
    fromSingleton, replaceEq, minusOrd, intersectOrd, lookupAll,

    -- * String manipulation
    quote, yesNo,

    -- * FilePath manipulation
    unifyPath, (-/-), matchVersionedFilePath,

    -- * Accessing Shake's type-indexed map
    insertExtra, userSetting,

    -- * Diagnostic info
    UseColour (..), putColoured, BuildProgressColour (..), putBuild,
    SuccessColour (..), putSuccess, ProgressInfo (..),
    putProgressInfo, renderAction, renderProgram, renderLibrary, renderBox,
    renderUnicorn
    ) where

import Control.Monad
import Data.Char
import Data.Dynamic
import Data.HashMap.Strict (HashMap)
import Data.List.Extra
import Data.Maybe
import Development.Shake hiding (Normal)
import Development.Shake.FilePath
import System.Console.ANSI
import System.Info.Extra
import System.IO

import qualified Data.HashMap.Strict as Map

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

-- | Insert a value into Shake's type-indexed map.
insertExtra :: Typeable a => a -> HashMap TypeRep Dynamic -> HashMap TypeRep Dynamic
insertExtra value = Map.insert (typeOf value) (toDyn value)

-- | Lookup a user setting in Shake's type-indexed map 'shakeExtra'. If the
-- setting is not found, return the provided default value instead.
userSetting :: Typeable a => a -> Action a
userSetting defaultValue = do
    extra <- shakeExtra <$> getShakeOptions
    let maybeValue = fromDynamic =<< Map.lookup (typeOf defaultValue) extra
    return $ fromMaybe defaultValue maybeValue

data UseColour = Never | Auto | Always deriving (Eq, Show, Typeable)

-- | A more colourful version of Shake's 'putNormal'.
putColoured :: ColorIntensity -> Color -> String -> Action ()
putColoured intensity colour msg = do
    useColour <- userSetting Never
    supported <- liftIO $ hSupportsANSI stdout
    let c Never  = False
        c Auto   = supported || isWindows -- Colours do work on Windows
        c Always = True
    when (c useColour) . liftIO $ setSGR [SetColor Foreground intensity colour]
    putNormal msg
    when (c useColour) . liftIO $ setSGR [] >> hFlush stdout

newtype BuildProgressColour = BuildProgressColour (ColorIntensity, Color)
    deriving Typeable

-- | Default 'BuildProgressColour'.
magenta :: BuildProgressColour
magenta = BuildProgressColour (Dull, Magenta)

-- | Print a build progress message (e.g. executing a build command).
putBuild :: String -> Action ()
putBuild msg = do
    BuildProgressColour (intensity, colour) <- userSetting magenta
    putColoured intensity colour msg

newtype SuccessColour = SuccessColour (ColorIntensity, Color)
    deriving Typeable

-- | Default 'SuccessColour'.
green :: SuccessColour
green = SuccessColour (Dull, Green)

-- | Print a success message (e.g. a package is built successfully).
putSuccess :: String -> Action ()
putSuccess msg = do
    SuccessColour (intensity, colour) <- userSetting green
    putColoured intensity colour msg

data ProgressInfo = None | Brief | Normal | Unicorn deriving (Eq, Show, Typeable)

-- | Version of 'putBuild' controlled by @--progress-info@ command line flag.
putProgressInfo :: String -> Action ()
putProgressInfo msg = do
    progressInfo <- userSetting None
    when (progressInfo /= None) $ putBuild msg

-- | Render an action.
renderAction :: String -> FilePath -> FilePath -> Action String
renderAction what input output = do
    progressInfo <- userSetting Normal
    return $ case progressInfo of
        None    -> ""
        Brief   -> "| " ++ what ++ ": " ++ i ++ " => " ++ o
        Normal  -> renderBox [ what, "     input: " ++ i, " => output: " ++ o ]
        Unicorn -> renderUnicorn [ what, "     input: " ++ i, " => output: " ++ o ]
  where
    i = unifyPath input
    o = unifyPath output

-- | Render the successful build of a program.
renderProgram :: String -> String -> String -> String
renderProgram name bin synopsis = renderBox [ "Successfully built program " ++ name
                                            , "Executable: " ++ bin
                                            , "Program synopsis: " ++ synopsis ++ "."]

-- | Render the successful build of a library.
renderLibrary :: String -> String -> String -> String
renderLibrary name lib synopsis = renderBox [ "Successfully built library " ++ name
                                            , "Library: " ++ lib
                                            , "Library synopsis: " ++ synopsis ++ "."]

-- | Render the given set of lines in an ASCII box. The minimum width and
-- whether to use Unicode symbols are hardcoded in the function's body.
--
-- >>> renderBox (words "lorem ipsum")
-- /----------\
-- | lorem    |
-- | ipsum    |
-- \----------/
renderBox :: [String] -> String
renderBox ls = tail $ concatMap ('\n' :) (boxTop : map renderLine ls ++ [boxBot])
  where
    -- Minimum total width of the box in characters
    minimumBoxWidth = 32

    -- TODO: Make this setting configurable? Setting to True by default seems
    -- to work poorly with many fonts.
    useUnicode = False

    -- Characters to draw the box
    (dash, pipe, topLeft, topRight, botLeft, botRight, padding)
        | useUnicode = ('─', '│', '╭',  '╮', '╰', '╯', ' ')
        | otherwise  = ('-', '|', '/', '\\', '\\', '/', ' ')

    -- Box width, taking minimum desired length and content into account.
    -- The -4 is for the beginning and end pipe/padding symbols, as
    -- in "| xxx |".
    boxContentWidth = (minimumBoxWidth - 4) `max` maxContentLength
      where
        maxContentLength = maximum (map length ls)

    renderLine l = concat
        [ [pipe, padding]
        , padToLengthWith boxContentWidth padding l
        , [padding, pipe] ]
      where
        padToLengthWith n filler x = x ++ replicate (n - length x) filler

    (boxTop, boxBot) = ( topLeft : dashes ++ [topRight]
                       , botLeft : dashes ++ [botRight] )
      where
        -- +1 for each non-dash (= corner) char
        dashes = replicate (boxContentWidth + 2) dash

-- | Render the given set of lines next to our favorite unicorn Robert.
renderUnicorn :: [String] -> String
renderUnicorn ls =
    unlines $ take (max (length ponyLines) (length boxLines)) $
        zipWith (++) (ponyLines ++ repeat ponyPadding) (boxLines ++ repeat "")
  where
    ponyLines :: [String]
    ponyLines = [ "                   ,;,,;'"
                , "                  ,;;'(    Robert the spitting unicorn"
                , "       __       ,;;' ' \\   wants you to know"
                , "     /'  '\\'~~'~' \\ /'\\.)  that a task      "
                , "  ,;(      )    /  |.  /   just finished!   "
                , " ,;' \\    /-.,,(   ) \\                      "
                , " ^    ) /       ) / )|     Almost there!    "
                , "      ||        ||  \\)                      "
                , "      (_\\       (_\\                         " ]
    ponyPadding :: String
    ponyPadding = "                                            "
    boxLines :: [String]
    boxLines = ["", "", ""] ++ (lines . renderBox $ ls)
