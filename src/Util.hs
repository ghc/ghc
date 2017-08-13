module Util (
    build, buildWithCmdOptions, buildWithResources, copyFile, fixFile, moveFile,
    removeFile, copyDirectory, copyDirectoryContents, createDirectory,
    moveDirectory, removeDirectory, applyPatch, runBuilder, runBuilderWith,
    makeExecutable, renderProgram, renderLibrary, builderEnvironment,
    needBuilder, copyFileUntracked, installDirectory, installData, installScript,
    installProgram, linkSymbolic, bashPath
    ) where

import qualified System.Directory.Extra as IO
import qualified System.IO              as IO
import qualified Control.Exception.Base as IO

import Hadrian.Oracles.ArgsHash
import Hadrian.Oracles.DirectoryContents
import Hadrian.Oracles.Path

import Base
import CmdLineFlag
import Context
import Expression hiding (builder, inputs, outputs, way, stage, package)
import GHC
import Oracles.Setting
import Settings
import Settings.Builders.Ar
import Target
import UserSettings

-- | Build a 'Target' with the right 'Builder' and command line arguments.
-- Force a rebuild if the argument list has changed since the last build.
build :: Target -> Action ()
build = customBuild [] []

-- | Build a 'Target' with the right 'Builder' and command line arguments,
-- acquiring necessary resources. Force a rebuild if the argument list has
-- changed since the last build.
buildWithResources :: [(Resource, Int)] -> Target -> Action ()
buildWithResources rs = customBuild rs []

-- | Build a 'Target' with the right 'Builder' and command line arguments,
-- using given options when executing the build command. Force a rebuild if
-- the argument list has changed since the last build.
buildWithCmdOptions :: [CmdOption] -> Target -> Action ()
buildWithCmdOptions = customBuild []

customBuild :: [(Resource, Int)] -> [CmdOption] -> Target -> Action ()
customBuild rs opts target = do
    let targetBuilder = builder target
    needBuilder targetBuilder
    path    <- builderPath targetBuilder
    argList <- interpret target getArgs
    verbose <- interpret target verboseCommands
    let quietlyUnlessVerbose = if verbose then withVerbosity Loud else quietly
    trackArgsHash target -- Rerun the rule if the hash of argList has changed.
    withResources rs $ do
        putInfo target
        quietlyUnlessVerbose $ case targetBuilder of
            Ar _ -> do
                output <- interpret target getOutput
                if "//*.a" ?== output
                then arCmd path argList
                else do
                    input <- interpret target getInput
                    top   <- topDirectory
                    cmd cmdEcho [Cwd output] [path] "x" (top -/- input)

            Configure dir -> do
                -- Inject /bin/bash into `libtool`, instead of /bin/sh, otherwise Windows breaks.
                -- TODO: Figure out why.
                bash <- bashPath
                let env = AddEnv "CONFIG_SHELL" bash
                cmd Shell cmdEcho env [Cwd dir] [path] opts argList

            HsCpp    -> captureStdout target path argList
            GenApply -> captureStdout target path argList

            GenPrimopCode -> do
                src  <- interpret target getInput
                file <- interpret target getOutput
                input <- readFile' src
                Stdout output <- cmd (Stdin input) [path] argList
                writeFileChanged file output

            Make dir -> cmd Shell cmdEcho path ["-C", dir] argList

            _  -> cmd cmdEcho [path] argList

-- | Suppress build output depending on the @--progress-info@ flag.
cmdEcho :: CmdOption
cmdEcho = EchoStdout $ cmdProgressInfo `elem` [Normal, Unicorn]

-- | Run a builder, capture the standard output, and write it to a given file.
captureStdout :: Target -> FilePath -> [String] -> Action ()
captureStdout target path argList = do
    file <- interpret target getOutput
    Stdout output <- cmd [path] argList
    writeFileChanged file output

-- | Copy a file tracking the source, create the target directory if missing.
copyFile :: FilePath -> FilePath -> Action ()
copyFile source target = do
    need [source] -- Guarantee source is built before printing progress info.
    let dir = takeDirectory target
    liftIO $ IO.createDirectoryIfMissing True dir
    putProgressInfo $ renderAction "Copy file" source target
    copyFileChanged source target

-- | Copy a file without tracking the source, create the target directory if missing.
copyFileUntracked :: FilePath -> FilePath -> Action ()
copyFileUntracked source target = do
    let dir = takeDirectory target
    liftIO $ IO.createDirectoryIfMissing True dir
    putProgressInfo $ renderAction "Copy file (Untracked)" source target
    liftIO $ IO.copyFile source target

-- | Move a file; we cannot track the source, because it is moved.
moveFile :: FilePath -> FilePath -> Action ()
moveFile source target = do
    putProgressInfo $ renderAction "Move file" source target
    quietly $ cmd ["mv", source, target]

-- | Remove a file that doesn't necessarily exist.
removeFile :: FilePath -> Action ()
removeFile file = do
    putBuild $ "| Remove file " ++ file
    liftIO . whenM (IO.doesFileExist file) $ IO.removeFile file

-- | Create a directory if it does not already exist.
createDirectory :: FilePath -> Action ()
createDirectory dir = do
    putBuild $ "| Create directory " ++ dir
    liftIO $ IO.createDirectoryIfMissing True dir

-- | Remove a directory that doesn't necessarily exist.
removeDirectory :: FilePath -> Action ()
removeDirectory dir = do
    putBuild $ "| Remove directory " ++ dir
    liftIO . whenM (IO.doesDirectoryExist dir) $ IO.removeDirectoryRecursive dir

-- | Copy a directory. The contents of the source directory is untracked.
copyDirectory :: FilePath -> FilePath -> Action ()
copyDirectory source target = do
    putProgressInfo $ renderAction "Copy directory" source target
    quietly $ cmd ["cp", "-r", source, target]

-- | Copy the contents of the source directory that matches a given 'Match'
-- expression into the target directory. The copied contents is tracked.
copyDirectoryContents :: Match -> FilePath -> FilePath -> Action ()
copyDirectoryContents expr source target = do
    putProgressInfo $ renderAction "Copy directory contents" source target
    let cp file = copyFile file $ target -/- makeRelative source file
    mapM_ cp =<< directoryContents expr source

-- | Move a directory. The contents of the source directory is untracked.
moveDirectory :: FilePath -> FilePath -> Action ()
moveDirectory source target = do
    putProgressInfo $ renderAction "Move directory" source target
    quietly $ cmd ["mv", source, target]

-- | Transform a given file by applying a function to its contents.
fixFile :: FilePath -> (String -> String) -> Action ()
fixFile file f = do
    putBuild $ "| Fix " ++ file
    contents <- liftIO $ IO.withFile file IO.ReadMode $ \h -> do
        old <- IO.hGetContents h
        let new = f old
        IO.evaluate $ rnf new
        return new
    liftIO $ writeFile file contents

-- | Apply a patch by executing the 'Patch' builder in a given directory.
applyPatch :: FilePath -> FilePath -> Action ()
applyPatch dir patch = do
    let file = dir -/- patch
    needBuilder Patch
    path <- builderPath Patch
    putBuild $ "| Apply patch " ++ file
    quietly $ cmd Shell [Cwd dir] [path, "-p0 <", patch]

-- | Install a directory.
installDirectory :: FilePath -> Action ()
installDirectory dir = do
    path <- fixAbsolutePathOnWindows =<< setting InstallDir
    putBuild $ "| Install directory " ++ dir
    quietly $ cmd path dir

-- | Install data files to a directory and track them.
installData :: [FilePath] -> FilePath -> Action ()
installData fs dir = do
    path <- fixAbsolutePathOnWindows =<< setting InstallData
    need fs
    forM_ fs $ \f -> putBuild $ "| Install data " ++ f ++ " to " ++ dir
    quietly $ cmd path fs dir

-- | Install an executable file to a directory and track it.
installProgram :: FilePath -> FilePath -> Action ()
installProgram f dir = do
    path <- fixAbsolutePathOnWindows =<< setting InstallProgram
    need [f]
    putBuild $ "| Install program " ++ f ++ " to " ++ dir
    quietly $ cmd path f dir

-- | Install an executable script to a directory and track it.
installScript :: FilePath -> FilePath -> Action ()
installScript f dir = do
    path <- fixAbsolutePathOnWindows =<< setting InstallScript
    need [f]
    putBuild $ "| Install script " ++ f ++ " to " ++ dir
    quietly $ cmd path f dir

-- | Create a symbolic link from source file to target file (when symbolic links
-- are supported) and track the source file.
linkSymbolic :: FilePath -> FilePath -> Action ()
linkSymbolic source target = do
    lns <- setting LnS
    unless (null lns) $ do
        need [source] -- Guarantee source is built before printing progress info.
        let dir = takeDirectory target
        liftIO $ IO.createDirectoryIfMissing True dir
        putProgressInfo $ renderAction "Create symbolic link" source target
        quietly $ cmd lns source target

isInternal :: Builder -> Bool
isInternal = isJust . builderProvenance

-- | Make sure a 'Builder' exists and rebuild it if out of date.
needBuilder :: Builder -> Action ()
needBuilder (Configure dir) = need [dir -/- "configure"]
needBuilder (Make      dir) = need [dir -/- "Makefile"]
needBuilder builder         = when (isInternal builder) $ do
    path <- builderPath builder
    need [path]

-- | Write a Builder's path into a given environment variable.
builderEnvironment :: String -> Builder -> Action CmdOption
builderEnvironment variable builder = do
    needBuilder builder
    path <- builderPath builder
    return $ AddEnv variable path

runBuilder :: Builder -> [String] -> Action ()
runBuilder = runBuilderWith []

-- | Run a builder with given list of arguments using custom 'cmd' options.
runBuilderWith :: [CmdOption] -> Builder -> [String] -> Action ()
runBuilderWith options builder args = do
    needBuilder builder
    path <- builderPath builder
    let note = if null args then "" else " (" ++ intercalate ", " args ++ ")"
    putBuild $ "| Run " ++ show builder ++ note
    quietly $ cmd options [path] args

-- | Make a given file executable by running the @chmod@ command.
makeExecutable :: FilePath -> Action ()
makeExecutable file = do
    putBuild $ "| Make " ++ quote file ++ " executable."
    quietly $ cmd "chmod +x " [file]

-- | Lookup the path to the @bash@ interpreter.
bashPath :: Action FilePath
bashPath = lookupInPath "bash"

-- | Print out information about the command being executed.
putInfo :: Target -> Action ()
putInfo t = putProgressInfo $ renderAction
    ("Run " ++ show (builder t) ++ contextInfo)
    (digest $ inputs  t)
    (digest $ outputs t)
  where
    contextInfo = concat $ [ " (" ]
        ++ [ "stage = "     ++ show (stage $ context t) ]
        ++ [ ", package = " ++ pkgNameString (package $ context t) ]
        ++ [ ", way = "     ++ show (way $ context t) | (way $ context t) /= vanilla ]
        ++ [ ")" ]
    digest [] = "none"
    digest [x] = x
    digest (x:xs) = x ++ " (and " ++ show (length xs) ++ " more)"

-- | Version of @putBuild@ controlled by @progressInfo@ command line flag.
putProgressInfo :: String -> Action ()
putProgressInfo msg = when (cmdProgressInfo /= None) $ putBuild msg

-- | Render an action.
renderAction :: String -> FilePath -> FilePath -> String
renderAction what input output = case cmdProgressInfo of
    Normal  -> renderBox [ what, "     input: " ++ i, " => output: " ++ o ]
    Brief   -> "| " ++ what ++ ": " ++ i ++ " => " ++ o
    Unicorn -> renderUnicorn [ what, "     input: " ++ i, " => output: " ++ o ]
    None    -> ""
  where
    i = unifyPath input
    o = unifyPath output

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
