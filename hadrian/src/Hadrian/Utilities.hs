{-# LANGUAGE TypeFamilies #-}
module Hadrian.Utilities (
    -- * List manipulation
    fromSingleton, replaceEq, minusOrd, intersectOrd, lookupAll, chunksOfSize,

    -- * String manipulation
    quote, yesNo, parseYesNo, zeroOne,

    -- * FilePath manipulation
    unifyPath, (-/-), makeRelativeNoSysLink,

    -- * Accessing Shake's type-indexed map
    insertExtra, lookupExtra, userSetting,

    -- * Paths
    BuildRoot (..), buildRoot, buildRootRules, isGeneratedSource,

    -- * File system operations
    copyFile, copyFileUntracked, createFileLink, fixFile,
    makeExecutable, moveFile, removeFile, createDirectory, copyDirectory,
    moveDirectory, removeDirectory, removeFile_,

    -- * Diagnostic info
    Colour (..), ANSIColour (..), putColoured, shouldUseColor,
    BuildProgressColour, mkBuildProgressColour, putBuild,
    SuccessColour, mkSuccessColour, putSuccess,
    FailureColour(..), red, mkFailureColour, putFailure,
    ProgressInfo (..), putProgressInfo,
    renderAction, renderActionNoOutput, renderProgram, renderLibrary, renderBox, renderUnicorn,

    -- * Miscellaneous
    (<&>), (%%>), cmdLineLengthLimit, windowsHost, osxHost, iosHost,

    -- * Useful re-exports
    Dynamic, fromDynamic, toDyn, TypeRep, typeOf
    ) where

import Control.Applicative
import Control.Monad.Extra
import Data.Char
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Functor
import Data.HashMap.Strict (HashMap)
import Data.List.Extra
import Data.Maybe
import Data.Typeable (TypeRep, typeOf)
import Development.Shake hiding (Normal)
import Development.Shake.Classes
import Development.Shake.FilePath
import System.Environment (lookupEnv)

import qualified Control.Exception.Base as IO
import qualified Data.HashMap.Strict    as Map
import qualified System.Directory.Extra as IO
import qualified System.Info.Extra      as IO
import qualified System.IO              as IO
import System.IO.Error (isPermissionError)

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

-- | Add single quotes around a string.
quote :: String -> String
quote s = "'" ++ s ++ "'"

-- | Pretty-print a 'Bool' as a @"YES"@ or @"NO"@ string.
yesNo :: Bool -> String
yesNo True  = "YES"
yesNo False = "NO"

-- | Parse a 'Bool' from a @"YES"@ or @"NO"@ string. Returns @Nothing@ in case
-- of a parse failure.
parseYesNo :: String -> Maybe Bool
parseYesNo "YES" = Just True
parseYesNo "NO"  = Just False
parseYesNo _     = Nothing

-- | Pretty-print a 'Bool' as a @"0"@ or @"1"@ string
zeroOne :: Bool -> String
zeroOne False = "0"
zeroOne True  = "1"

-- | Normalise a path and convert all path separators to @/@, even on Windows.
unifyPath :: FilePath -> FilePath
unifyPath = toStandard . normaliseEx

-- | Combine paths with a forward slash regardless of platform.
(-/-) :: FilePath -> FilePath -> FilePath
_  -/- b | isAbsolute b && not (isAbsolute $ tail b) = b
"" -/- b = b
a  -/- b
    | last a == '/' = a ++       b
    | otherwise     = a ++ '/' : b

infixr 6 -/-

-- | This is like Posix makeRelative, but assumes no sys links in the input
-- paths. This allows the result to start with possibly many "../"s. Input
-- paths must both be relative, or be on the same drive
makeRelativeNoSysLink :: FilePath -> FilePath  -> FilePath
makeRelativeNoSysLink a b
    | aDrive == bDrive
        = if aToB == []
            then "."
            else joinPath aToB
    | otherwise
        = error $ if isRelative a /= isRelative b
            then "Paths must both be relative or both be absolute, but got"
                    ++ " \"" ++ a      ++ "\" and \"" ++ b      ++ "\""
            else "Paths are on different drives "
                    ++ " \"" ++ aDrive ++ "\" and \"" ++ bDrive ++ "\""
    where
        (aDrive, aRelPath) = splitDrive a
        (bDrive, bRelPath) = splitDrive b

        aRelSplit = removeIndirections (splitPath aRelPath)
        bRelSplit = removeIndirections (splitPath bRelPath)

        -- Use removePrefix to get the relative paths relative to a new
        -- base directory as high in the directory tree as possible.
        (baseToA, baseToB) = removePrefix aRelSplit bRelSplit
        aToBase = case baseToA of
                   (p: _) | isDirUp p ->
                      -- if baseToA contains any '..' then there is no way to get
                      -- a path from a to the base directory.
                      -- E.g. if   baseToA == "../u/v"
                      --      then aToBase == "../../<UnknownDir>"
                      error $ "Impossible to find relatieve path from "
                                    ++ a ++ " to " ++ b
                   _ -> ".." <$ baseToA
        aToB = aToBase ++ baseToB

        -- removePrefix "pre123" "prefix456" == ("123", "fix456")
        removePrefix :: Eq a => [a] -> [a] -> ([a], [a])
        removePrefix as [] = (as, [])
        removePrefix [] bs = ([], bs)
        removePrefix (a:as) (b:bs)
            | a == b    = removePrefix as bs
            | otherwise = (a:as, b:bs)

        -- Removes all '.', and tries to remove all '..'. In some cases '..'s
        -- cannot be removes, but will all appear to the left.
        -- e.g. removeIndirections "../a/./b/../../../c" == "../../c"
        removeIndirections :: [String] -> [String]
        removeIndirections [] = []
        removeIndirections (x:xs)
            -- Remove all '.'
            | isDot   x = removeIndirections xs
            -- Bubble all '..' to the left
            | otherwise = case removeIndirections xs of
                        []     -> [x]
                        -- Only when x /= '..' and y == '..' do we need to
                        -- bubble to the left. In that case they cancel out
                        (y:ys) -> if not (isDirUp x) && isDirUp y
                                    then ys
                                    else x : y : ys

        isDirUp ".." = True
        isDirUp "../" = True
        isDirUp _ = False

        isDot "." = True
        isDot "./" = True
        isDot _ = False

-- | Like Shake's '%>' but gives higher priority to longer patterns. Useful
-- in situations when a family of build rules, e.g. @"**/*.a"@ and @"**/*_p.a"@
-- can be matched by the same file, such as @library_p.a@. We break the tie
-- by preferring longer matches, which correspond to longer patterns.
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
cmdLineLengthLimit | IO.isWindows = 31000
                   | IO.isMac     = 200000
                   | otherwise    = 4194304

-- | Check if the host OS is Windows.
windowsHost :: Bool
windowsHost = IO.isWindows

-- | Check if the host OS is Mac OS.
osxHost :: Bool
osxHost = IO.isMac

-- | Check if the host OS is iOS.
iosHost :: Bool
iosHost = IO.os == "ios"

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

-- | Lookup a user setting in Shake's type-indexed map 'shakeExtra'. If the
-- setting is not found, return the provided default value instead.
userSettingRules :: Typeable a => a -> Rules a
userSettingRules defaultValue = do
    extra <- shakeExtra <$> getShakeOptionsRules
    return $ lookupExtra defaultValue extra

newtype BuildRoot = BuildRoot FilePath deriving (Typeable, Eq, Show)

-- | All build results are put into the 'buildRoot' directory.
buildRoot :: Action FilePath
buildRoot = do
    BuildRoot path <- userSetting (BuildRoot "")
    return path

buildRootRules :: Rules FilePath
buildRootRules = do
    BuildRoot path <- userSettingRules (BuildRoot "")
    return path

-- | Given a 'FilePath' to a source file, return 'True' if it is generated.
-- The current implementation simply assumes that a file is generated if it
-- lives in the 'buildRoot' directory. Since most files are not generated the
-- test is usually very fast.
isGeneratedSource :: FilePath -> Action Bool
isGeneratedSource file = buildRoot <&> (`isPrefixOf` file)

-- | Link a file tracking the link target. Create the target directory if
-- missing.
createFileLink :: FilePath -> FilePath -> Action ()
createFileLink linkTarget link
  | windowsHost = copyFile' source link
  | otherwise   = do
    -- TODO `disableHistory` is a temporary fix (see issue #16866). Remove
    -- `disableHistory` when shake issue is fixed: https://github.com/ndmitchell/shake/issues/683.
    historyDisable

    need [source]

    liftIO $ IO.createDirectoryIfMissing True dir
    putProgressInfo =<< renderCreateFileLink linkTarget link
    quietly . liftIO $ do
        IO.removeFile link <|> return ()
        IO.createFileLink linkTarget link

  where dir = takeDirectory link
        source | isAbsolute linkTarget = linkTarget
               | otherwise             = takeDirectory link -/- linkTarget

-- | Copy a file tracking the source. Create the target directory if missing.
copyFile :: FilePath -> FilePath -> Action ()
copyFile source target = do
    need [source] -- Guarantee the source is built before printing progress info.
    let dir = takeDirectory target
    liftIO $ IO.createDirectoryIfMissing True dir
    putProgressInfo =<< renderAction "Copy file" source target
    quietly $ copyFileChanged source target

-- | Remove a file or a link, but don't worry if it fails
removeFile_ :: FilePath -> IO ()
removeFile_ x =
    (IO.removeFile x >> IO.removeDirectoryLink x) `IO.catch` \e ->
        when (isPermissionError e) $ IO.handle (\(_ :: IO.IOException) -> pure ()) $ do
            perms <- IO.getPermissions x
            IO.setPermissions x perms{IO.readable = True, IO.searchable = True, IO.writable = True}
            IO.removeFile x

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

-- | Terminal output colours
data Colour
    = Dull ANSIColour   -- ^ 8-bit ANSI colours
    | Vivid ANSIColour  -- ^ 16-bit vivid ANSI colours
    | Extended String   -- ^ Extended 256-bit colours, manual code stored

-- | ANSI terminal colours
data ANSIColour
    = Black     -- ^ ANSI code: 30
    | Red       -- ^ 31
    | Green     -- ^ 32
    | Yellow    -- ^ 33
    | Blue      -- ^ 34
    | Magenta   -- ^ 35
    | Cyan      -- ^ 36
    | White     -- ^ 37
    | Reset     -- ^ 0

-- | Convert ANSI colour names into their associated codes
colourCode :: ANSIColour -> String
colourCode Black = "30"
colourCode Red = "31"
colourCode Green = "32"
colourCode Yellow = "33"
colourCode Blue = "34"
colourCode Magenta = "35"
colourCode Cyan = "36"
colourCode White = "37"
colourCode Reset = "0"

-- | Create the final ANSI code.
mkColour :: Colour -> String
mkColour (Dull c) = colourCode c
mkColour (Vivid c) = colourCode c ++ ";1"
mkColour (Extended code) = "38;5;" ++ code

-- | A more colourful version of Shake's 'putInfo'.
putColoured :: String -> String -> Action ()
putColoured code msg = do
    useColour <- shakeColor <$> getShakeOptions
    if useColour
        then putInfo $ "\ESC[" ++ code ++ "m" ++ msg ++ "\ESC[0m"
        else putInfo msg

newtype BuildProgressColour = BuildProgressColour String
    deriving Typeable

-- | By default, Hadrian tries to figure out if the current terminal
--   supports colors using this function. The default can be overridden
--   by suppling @--[no-]color@.
shouldUseColor :: IO Bool
shouldUseColor =
  (&&) <$> IO.hIsTerminalDevice IO.stdout
       <*> (not <$> isDumb)
  where
    isDumb = maybe False (== "dumb") <$> lookupEnv "TERM"

-- | Generate an encoded colour for progress output from names.
mkBuildProgressColour :: Colour -> BuildProgressColour
mkBuildProgressColour c = BuildProgressColour $ mkColour c

-- | Default 'BuildProgressColour'.
magenta :: BuildProgressColour
magenta = mkBuildProgressColour (Dull Magenta)

-- | Print a build progress message (e.g. executing a build command).
putBuild :: String -> Action ()
putBuild msg = do
    BuildProgressColour code <- userSetting magenta
    putColoured code msg

newtype SuccessColour = SuccessColour String
    deriving Typeable

-- | Generate an encoded colour for successful output from names
mkSuccessColour :: Colour -> SuccessColour
mkSuccessColour c = SuccessColour $ mkColour c

-- | Default 'SuccessColour'.
green :: SuccessColour
green = mkSuccessColour (Dull Green)

-- | Print a success message (e.g. a package is built successfully).
putSuccess :: String -> Action ()
putSuccess msg = do
    SuccessColour code <- userSetting green
    putColoured code msg

newtype FailureColour = FailureColour String
    deriving Typeable

-- | Generate an encoded colour for failure output messages
mkFailureColour :: Colour -> FailureColour
mkFailureColour c = FailureColour $ mkColour c

-- | Default 'FailureColour'.
red :: FailureColour
red = mkFailureColour (Dull Red)

-- | Print a failure message (e.g. a precondition was not met).
putFailure :: String -> Action ()
putFailure msg = do
  FailureColour code <- userSetting red
  putColoured code msg

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
        Normal  -> renderBox [ what
                             , "     input: " ++ i
                             , " => output: " ++ o ]
        Unicorn -> renderUnicorn [ what
                                 , "     input: " ++ i
                                 , " => output: " ++ o ]
  where
    i = unifyPath input
    o = unifyPath output

-- | Render an action.
renderActionNoOutput :: String -> FilePath -> Action String
renderActionNoOutput what input = do
    progressInfo <- userSetting Brief
    return $ case progressInfo of
        None    -> ""
        Brief   -> "| " ++ what ++ ": " ++ i
        Normal  -> renderBox [ what, "     input: " ++ i ]
        Unicorn -> renderUnicorn [ what, "     input: " ++ i ]
  where
    i = unifyPath input

-- | Render creating a file link.
renderCreateFileLink :: String -> FilePath -> Action String
renderCreateFileLink linkTarget link' = do
    progressInfo <- userSetting Brief
    let what = "Creating file link"
        linkString = link ++ " -> " ++ linkTarget
    return $ case progressInfo of
        None    -> ""
        Brief   -> "| " ++ what ++ ": " ++ linkString
        Normal  -> renderBox [ what
                             , "      link name: " ++ link
                             , " -> link target: " ++ linkTarget ]
        Unicorn -> renderUnicorn [ what
                                 , "      link name: " ++ link
                                 , " -> link target: " ++ linkTarget ]
    where
        link = unifyPath link'

-- | Render the successful build of a program.
renderProgram :: String -> String -> String -> String
renderProgram name bin synopsis = renderBox $
    [ "Successfully built program " ++ name
    , "Executable: " ++ bin ] ++
    [ "Program synopsis: " ++ endWithADot synopsis | not (null synopsis) ]

-- | Render the successful build of a library.
renderLibrary :: String -> String -> String -> String
renderLibrary name lib synopsis = renderBox $
    [ "Successfully built library " ++ name
    , "Library: " ++ lib ] ++
    [ "Library synopsis: " ++ endWithADot synopsis | not (null synopsis) ]

endWithADot :: String -> String
endWithADot s = dropWhileEnd isPunctuation s ++ "."

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
