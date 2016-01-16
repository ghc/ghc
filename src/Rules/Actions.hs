{-# LANGUAGE RecordWildCards #-}
module Rules.Actions (
    build, buildWithResources, copyFile, createDirectory, removeDirectory, moveDirectory,
    fixFile, runConfigure, runMake, applyPatch, renderLibrary, renderProgram,
    runBuilder, makeExecutable,
    ) where

import qualified System.Directory as IO
import System.Console.ANSI

import Base
import Expression
import Oracles
import Oracles.ArgsHash
import Oracles.Config.CmdLineFlag (buildInfo, BuildInfoFlag(..))
import Settings
import Settings.Args
import Settings.Builders.Ar
import qualified Target

-- Build a given target using an appropriate builder and acquiring necessary
-- resources. Force a rebuilt if the argument list has changed since the last
-- built (that is, track changes in the build system).
buildWithResources :: [(Resource, Int)] -> Target -> Action ()
buildWithResources rs target = do
    let builder = Target.builder target
    needBuilder laxDependencies builder
    path    <- builderPath builder
    argList <- interpret target getArgs
    verbose <- interpret target verboseCommands
    let quietlyUnlessVerbose = if verbose then withVerbosity Loud else quietly
    -- The line below forces the rule to be rerun if the args hash has changed
    checkArgsHash target
    withResources rs $ do
        unless verbose $ putInfo target
        quietlyUnlessVerbose $ case builder of
            Ar -> do
                output <- interpret target getOutput
                if "//*.a" ?== output
                then arCmd path argList
                else do
                    input <- interpret target getInput
                    top   <- topDirectory
                    cmd [path] [Cwd output] "x" (top -/- input)

            HsCpp    -> captureStdout target path argList
            GenApply -> captureStdout target path argList

            GenPrimopCode -> do
                src  <- interpret target getInput
                file <- interpret target getOutput
                input <- readFile' src
                Stdout output <- cmd (Stdin input) [path] argList
                writeFileChanged file output

            _  -> cmd [path] argList

-- Most targets are built without explicitly acquiring resources
build :: Target -> Action ()
build = buildWithResources []

captureStdout :: Target -> FilePath -> [String] -> Action ()
captureStdout target path argList = do
    file <- interpret target getOutput
    Stdout output <- cmd [path] argList
    writeFileChanged file output

copyFile :: FilePath -> FilePath -> Action ()
copyFile source target = do
    putProgressInfo $ renderAction "Copy file" source target
    copyFileChanged source target

createDirectory :: FilePath -> Action ()
createDirectory dir = do
    putBuild $ "| Create directory " ++ dir
    liftIO $ IO.createDirectoryIfMissing True dir

removeDirectory :: FilePath -> Action ()
removeDirectory dir = do
    putBuild $ "| Remove directory " ++ dir
    removeDirectoryIfExists dir

-- Note, the source directory is untracked
moveDirectory :: FilePath -> FilePath -> Action ()
moveDirectory source target = do
    putProgressInfo $ renderAction "Move directory" source target
    liftIO $ IO.renameDirectory source target

-- Transform a given file by applying a function to its contents
fixFile :: FilePath -> (String -> String) -> Action ()
fixFile file f = do
    putBuild $ "| Fix " ++ file
    old <- liftIO $ readFile file
    let new = f old
    length new `seq` liftIO $ writeFile file new

runConfigure :: FilePath -> [CmdOption] -> [String] -> Action ()
runConfigure dir opts args = do
    need [dir -/- "configure"]
    putBuild $ "| Run configure in " ++ dir ++ "..."
    quietly $ cmd Shell (EchoStdout False) [Cwd dir] "bash configure" opts' args
    where
        -- Always configure with bash.
        -- This also injects /bin/bash into `libtool`, instead of /bin/sh
        opts' = opts ++ [AddEnv "CONFIG_SHELL" "/bin/bash"]

runMake :: FilePath -> [String] -> Action ()
runMake dir args = do
    need [dir -/- "Makefile"]
    path <- builderPath Make

    -- FIXME: temporary safety net for those who are not on GHC HEAD, see #167
    -- TODO: add need [path] once lookupInPath is enabled on Windows
    fixPath <- if path == "@MakeCmd@" <.> exe
               then do
                   putColoured Red $ "You are behind GHC HEAD, make autodetection is disabled."
                   return "make"
               else return path

    let note = if null args then "" else " (" ++ intercalate ", " args ++ ")"
    putBuild $ "| Run " ++ fixPath ++ note ++ " in " ++ dir ++ "..."
    quietly $ cmd Shell (EchoStdout False) fixPath ["-C", dir] args

applyPatch :: FilePath -> FilePath -> Action ()
applyPatch dir patch = do
    let file = dir -/- patch
    need [file]
    needBuilder False Patch -- TODO: add a specialised version ~needBuilderFalse?
    path <- builderPath Patch
    putBuild $ "| Apply patch " ++ file
    quietly $ cmd Shell (EchoStdout False) [Cwd dir] [path, "-p0 <", patch]

runBuilder :: Builder -> [String] -> Action ()
runBuilder builder args = do
    needBuilder laxDependencies builder
    path <- builderPath builder
    let note = if null args then "" else " (" ++ intercalate ", " args ++ ")"
    putBuild $ "| Run " ++ show builder ++ note
    quietly $ cmd [path] args

makeExecutable :: FilePath -> Action ()
makeExecutable file = do
    putBuild $ "| Make '" ++ file ++ "' executable."
    quietly $ cmd "chmod +x " [file]

-- Print out key information about the command being executed
putInfo :: Target.Target -> Action ()
putInfo Target.Target {..} = putProgressInfo $ renderAction
    ("Run " ++ show builder ++ " (" ++ stageInfo
    ++ "package = " ++ pkgNameString package ++ wayInfo ++ ")")
    (digest inputs)
    (digest outputs)
  where
    stageInfo = if isStaged builder then "" else "stage = " ++ show stage ++ ", "
    wayInfo   = if way == vanilla   then "" else ", way = " ++ show way
    digest [] = "none"
    digest [x] = x
    digest (x:xs) = x ++ " (and " ++ show (length xs) ++ " more)"

-- | Switch for @putBuild@ filtered through @buildInfo@
putProgressInfo :: String -> Action ()
putProgressInfo s | buildInfo /= None = putBuild s
putProgressInfo _                     = pure ()

-- | Render an action.
renderAction :: String -> String -> String -> String
renderAction what input output = case buildInfo of
    Normal  -> renderBox [ what
                         , "     input: " ++ input
                         , " => output: " ++ output ]
    Brief   -> "> " ++ what ++ ": " ++ input ++ " => " ++ output
    Unicorn -> renderUnicorn [ what
                             , "     input: " ++ input
                             , " => output: " ++ output ]
    None    -> ""

-- | Render the successful build of a program
renderProgram :: String -> String -> String -> String
renderProgram name bin synopsis = renderBox [ "Successfully built program " ++ name
                                            , "Executable: " ++ bin
                                            , "Program synopsis: " ++ synopsis ++ "."]

-- | Render the successful built of a library
renderLibrary :: String -> String -> String -> String
renderLibrary name lib synopsis = renderBox [ "Successfully built library " ++ name
                                            , "Library: " ++ lib
                                            , "Library synopsis: " ++ synopsis ++ "."]

-- | Render the given set of lines next to our favorit unicorn Robert.
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

-- | Render the given set of lines in a nice box of ASCII.
--
-- The minimum width and whether to use Unicode symbols are hardcoded in the
-- function's body.
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

    -- FIXME: See Shake #364.
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
