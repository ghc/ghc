{-# LANGUAGE TypeFamilies #-}
module Hadrian.Utilities (
    -- * List manipulation
    fromSingleton, replaceEq, minusOrd, intersectOrd, lookupAll, chunksOfSize,

    -- * String manipulation
    quote, yesNo,

    -- * FilePath manipulation
    unifyPath, (-/-),

    -- * Accessing Shake's type-indexed map
    insertExtra, lookupExtra, userSetting,

    -- * Paths
    BuildRoot (..), buildRoot, isGeneratedSource,

    -- * File system operations
    copyFile, copyFileUntracked, fixFile, makeExecutable, moveFile, removeFile,
    createDirectory, copyDirectory, moveDirectory, removeDirectory,

    -- * Diagnostic info
    UseColour (..), putColoured, BuildProgressColour (..), putBuild,
    SuccessColour (..), putSuccess, ProgressInfo (..),
    putProgressInfo, renderAction, renderProgram, renderLibrary, renderBox,
    renderUnicorn,

    -- * Miscellaneous
    (<&>), (%%>), cmdLineLengthLimit,

    -- * Useful re-exports
    Dynamic, fromDynamic, toDyn, TypeRep, typeOf
    ) where

import Control.Monad.Extra
import Data.Char
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.HashMap.Strict (HashMap)
import Data.List.Extra
import Data.Maybe
import Data.Typeable (TypeRep, typeOf)
import Development.Shake hiding (Normal)
import Development.Shake.Classes
import Development.Shake.FilePath
import System.Console.ANSI
import System.Info.Extra

import qualified Control.Exception.Base as IO
import qualified Data.HashMap.Strict    as Map
import qualified System.Directory.Extra as IO
import qualified System.Info.Extra      as IO
import qualified System.IO              as IO

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

-- | @chunksOfSize size strings@ splits a given list of strings into chunks not
-- exceeding the given @size@. If that is impossible, it uses singleton chunks.
chunksOfSize :: Int -> [String] -> [[String]]
chunksOfSize n = repeatedly f
  where
    f xs = splitAt (max 1 $ length $ takeWhile (<= n) $ scanl1 (+) $ map length xs) xs

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

-- | Like Shake's '%>' but gives higher priority to longer patterns. Useful
-- in situations when a family of build rules, e.g. @"//*.a"@ and @"//*_p.a"@
-- can be matched by the same file, such as @library_p.a@. We break the tie
-- by preferring longer matches, which correpond to longer patterns.
(%%>) :: FilePattern -> (FilePath -> Action ()) -> Rules ()
p %%> a = priority (fromIntegral (length p) + 1) $ p %> a

infix 1 %%>

-- | Build command lines can get very long; for example, when building the Cabal
-- library, they can reach 2MB! Some operating systems do not support command
-- lines of such length, and this function can be used to obtain a reasonable
-- approximation of the limit. On Windows, it is theoretically 32768 characters
-- (since Windows 7). In practice we use 31000 to leave some breathing space for
-- the builder path & name, auxiliary flags, and other overheads. On Mac OS X,
-- ARG_MAX is 262144, yet when using @xargs@ on OSX this is reduced by over
-- 20000. Hence, 200000 seems like a sensible limit. On other operating systems
-- we currently use the 4194304 setting.
cmdLineLengthLimit :: Int
cmdLineLengthLimit | isWindows = 31000
                   | isMac     = 200000
                   | otherwise = 4194304

-- | Insert a value into Shake's type-indexed map.
insertExtra :: Typeable a => a -> HashMap TypeRep Dynamic -> HashMap TypeRep Dynamic
insertExtra value = Map.insert (typeOf value) (toDyn value)

-- | Lookup a value in Shake's type-indexed map.
lookupExtra :: Typeable a => a -> Map.HashMap TypeRep Dynamic -> a
lookupExtra defaultValue extra = fromMaybe defaultValue maybeValue
  where
    maybeValue = fromDynamic =<< Map.lookup (typeOf defaultValue) extra

-- | Lookup a user setting in Shake's type-indexed map 'shakeExtra'. If the
-- setting is not found, return the provided default value instead.
userSetting :: Typeable a => a -> Action a
userSetting defaultValue = do
    extra <- shakeExtra <$> getShakeOptions
    return $ lookupExtra defaultValue extra

newtype BuildRoot = BuildRoot FilePath deriving Typeable

-- | All build results are put into the 'buildRoot' directory.
buildRoot :: Action FilePath
buildRoot = do
    BuildRoot path <- userSetting (BuildRoot "")
    return path

-- | A version of 'fmap' with flipped arguments. Useful for manipulating values
-- in context, e.g. 'buildRoot', as in the example below.
--
-- @
-- buildRoot <&> (-/- "dir") == fmap (-/- "dir") buildRoot
-- @
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

infixl 1 <&>

-- | Given a 'FilePath' to a source file, return 'True' if it is generated.
-- The current implementation simply assumes that a file is generated if it
-- lives in the 'buildRoot' directory. Since most files are not generated the
-- test is usually very fast.
isGeneratedSource :: FilePath -> Action Bool
isGeneratedSource file = buildRoot <&> (`isPrefixOf` file)

-- | Copy a file tracking the source. Create the target directory if missing.
copyFile :: FilePath -> FilePath -> Action ()
copyFile source target = do
    need [source] -- Guarantee the source is built before printing progress info.
    let dir = takeDirectory target
    liftIO $ IO.createDirectoryIfMissing True dir
    putProgressInfo =<< renderAction "Copy file" source target
    quietly $ copyFileChanged source target

-- | Copy a file without tracking the source. Create the target directory if missing.
copyFileUntracked :: FilePath -> FilePath -> Action ()
copyFileUntracked source target = do
    let dir = takeDirectory target
    liftIO $ IO.createDirectoryIfMissing True dir
    putProgressInfo =<< renderAction "Copy file (untracked)" source target
    liftIO $ IO.copyFile source target

-- | Transform a given file by applying a function to its contents.
fixFile :: FilePath -> (String -> String) -> Action ()
fixFile file f = do
    putProgressInfo $ "| Fix " ++ file
    contents <- liftIO $ IO.withFile file IO.ReadMode $ \h -> do
        old <- IO.hGetContents h
        let new = f old
        IO.evaluate $ rnf new
        return new
    liftIO $ writeFile file contents

-- | Make a given file executable by running the @chmod +x@ command.
makeExecutable :: FilePath -> Action ()
makeExecutable file = do
    putProgressInfo $ "| Make " ++ quote file ++ " executable."
    quietly $ cmd "chmod +x " [file]

-- | Move a file. Note that we cannot track the source, because it is moved.
moveFile :: FilePath -> FilePath -> Action ()
moveFile source target = do
    putProgressInfo =<< renderAction "Move file" source target
    quietly $ cmd ["mv", source, target]

-- | Remove a file that doesn't necessarily exist.
removeFile :: FilePath -> Action ()
removeFile file = do
    putProgressInfo $ "| Remove file " ++ file
    liftIO . whenM (IO.doesFileExist file) $ IO.removeFile file

-- | Create a directory if it does not already exist.
createDirectory :: FilePath -> Action ()
createDirectory dir = do
    putProgressInfo $ "| Create directory " ++ dir
    liftIO $ IO.createDirectoryIfMissing True dir

-- | Copy a directory. The contents of the source directory is untracked.
copyDirectory :: FilePath -> FilePath -> Action ()
copyDirectory source target = do
    putProgressInfo =<< renderAction "Copy directory" source target
    quietly $ cmd ["cp", "-r", source, target]

-- | Move a directory. The contents of the source directory is untracked.
moveDirectory :: FilePath -> FilePath -> Action ()
moveDirectory source target = do
    putProgressInfo =<< renderAction "Move directory" source target
    quietly $ cmd ["mv", source, target]

-- | Remove a directory that doesn't necessarily exist.
removeDirectory :: FilePath -> Action ()
removeDirectory dir = do
    putProgressInfo $ "| Remove directory " ++ dir
    liftIO . whenM (IO.doesDirectoryExist dir) $ IO.removeDirectoryRecursive dir

data UseColour = Never | Auto | Always deriving (Eq, Show, Typeable)

-- | A more colourful version of Shake's 'putNormal'.
putColoured :: ColorIntensity -> Color -> String -> Action ()
putColoured intensity colour msg = do
    useColour <- userSetting Never
    supported <- liftIO $ hSupportsANSI IO.stdout
    let c Never  = False
        c Auto   = supported || IO.isWindows -- Colours do work on Windows
        c Always = True
    when (c useColour) . liftIO $ setSGR [SetColor Foreground intensity colour]
    putNormal msg
    when (c useColour) . liftIO $ setSGR [] >> IO.hFlush IO.stdout

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

-- | Version of 'putBuild' controlled by @--progress-info@ command line argument.
putProgressInfo :: String -> Action ()
putProgressInfo msg = do
    progressInfo <- userSetting None
    when (progressInfo /= None) $ putBuild msg

-- | Render an action.
renderAction :: String -> FilePath -> FilePath -> Action String
renderAction what input output = do
    progressInfo <- userSetting Brief
    return $ case progressInfo of
        None    -> ""
        Brief   -> "| " ++ what ++ ": " ++ i ++ " => " ++ o
        Normal  -> renderBox [ what, "     input: " ++ i, " => output: " ++ o ]
        Unicorn -> renderUnicorn [ what, "     input: " ++ i, " => output: " ++ o ]
  where
    i = unifyPath input
    o = unifyPath output

-- | Render the successful build of a program.
renderProgram :: String -> String -> Maybe String -> String
renderProgram name bin synopsis = renderBox $
    [ "Successfully built program " ++ name
    , "Executable: " ++ bin ] ++
    [ "Program synopsis: " ++ prettySynopsis synopsis | isJust synopsis ]

-- | Render the successful build of a library.
renderLibrary :: String -> String -> Maybe String -> String
renderLibrary name lib synopsis = renderBox $
    [ "Successfully built library " ++ name
    , "Library: " ++ lib ] ++
    [ "Library synopsis: " ++ prettySynopsis synopsis | isJust synopsis ]

prettySynopsis :: Maybe String -> String
prettySynopsis Nothing  = ""
prettySynopsis (Just s) = dropWhileEnd isPunctuation s ++ "."

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
