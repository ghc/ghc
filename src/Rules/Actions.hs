module Rules.Actions (
    build, buildWithCmdOptions, buildWithResources, copyFile, fixFile, moveFile,
    removeFile, copyDirectory, copyDirectoryContent, createDirectory,
    moveDirectory, removeDirectory, applyPatch, runBuilder, runBuilderWith,
    makeExecutable, renderProgram, renderLibrary
    ) where

import qualified System.Directory.Extra as IO
import qualified System.IO              as IO
import qualified Control.Exception.Base as IO

import Base
import CmdLineFlag
import Context
import Expression
import Oracles.ArgsHash
import Oracles.WindowsPath
import Settings
import Settings.Args
import Settings.Builders.Ar
import Target

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
customBuild rs opts target@Target {..} = do
    needBuilder builder
    path    <- builderPath builder
    argList <- interpret target getArgs
    verbose <- interpret target verboseCommands
    let quietlyUnlessVerbose = if verbose then withVerbosity Loud else quietly
    -- The line below forces the rule to be rerun if the args hash has changed.
    checkArgsHash target
    withResources rs $ do
        putInfo target
        quietlyUnlessVerbose $ case builder of
            Ar -> do
                output <- interpret target getOutput
                if "//*.a" ?== output
                then arCmd path argList
                else do
                    input <- interpret target getInput
                    top   <- topDirectory
                    cmd [Cwd output] [path] "x" (top -/- input)

            Configure dir -> do
                need [dir -/- "configure"]
                -- Inject /bin/bash into `libtool`, instead of /bin/sh
                let env = AddEnv "CONFIG_SHELL" "/bin/bash"
                cmd Shell cmdEcho env [Cwd dir] [path] opts argList

            HsCpp    -> captureStdout target path argList
            GenApply -> captureStdout target path argList

            GenPrimopCode -> do
                src  <- interpret target getInput
                file <- interpret target getOutput
                input <- readFile' src
                Stdout output <- cmd (Stdin input) [path] argList
                writeFileChanged file output

            Make dir -> do
                need [dir -/- "Makefile"]
                cmd Shell cmdEcho path ["-C", dir] argList

            _  -> cmd [path] argList

cmdEcho :: CmdOption
cmdEcho = EchoStdout $ cmdProgressInfo `elem` [Normal, Unicorn]

-- | Run a builder, capture the standard output, and write it to a given file.
captureStdout :: Target -> FilePath -> [String] -> Action ()
captureStdout target path argList = do
    file <- interpret target getOutput
    Stdout output <- cmd [path] argList
    writeFileChanged file output

-- | Copy a file tracking the source.
copyFile :: FilePath -> FilePath -> Action ()
copyFile source target = do
    need [source] -- Guarantee source is built before printing progress info.
    putProgressInfo $ renderAction "Copy file" source target
    copyFileChanged source target

-- | Move a file; we cannot track the source, because it is moved.
moveFile :: FilePath -> FilePath -> Action ()
moveFile source target = do
    putProgressInfo $ renderAction "Move file" source target
    liftIO $ IO.renameFile source target

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
    quietly $ cmd cmdEcho ["cp", "-r", source, target]

-- | Copy the content of the source directory into the target directory. Only
-- the files and directories for which the predicate returns True are copied.
copyDirectoryContent :: (FilePath -> IO Bool) -> FilePath -> FilePath -> Action ()
copyDirectoryContent test source target = do
    putProgressInfo $ renderAction "Copy directory" source target
    liftIO $ IO.listFilesInside test' source >>= mapM_ cp
  where
    target' a = target -/- fromJust (stripPrefix source a)
    test' a = ifM (test a) (mkdir a >> return True) (return False)
    mkdir a = IO.createDirectoryIfMissing True $ target' a
    cp a = whenM (test a) $ IO.copyFile a $ target' a

-- | Move a directory. The contents of the source directory is untracked.
moveDirectory :: FilePath -> FilePath -> Action ()
moveDirectory source target = do
    putProgressInfo $ renderAction "Move directory" source target
    quietly $ cmd cmdEcho ["mv", source, target]

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

applyPatch :: FilePath -> FilePath -> Action ()
applyPatch dir patch = do
    let file = dir -/- patch
    needBuilder Patch
    path <- builderPath Patch
    putBuild $ "| Apply patch " ++ file
    quietly $ cmd Shell cmdEcho [Cwd dir] [path, "-p0 <", patch]

runBuilder :: Builder -> [String] -> Action ()
runBuilder = runBuilderWith []

runBuilderWith :: [CmdOption] -> Builder -> [String] -> Action ()
runBuilderWith options builder args = do
    needBuilder builder
    path <- builderPath builder
    let note = if null args then "" else " (" ++ intercalate ", " args ++ ")"
    putBuild $ "| Run " ++ show builder ++ note
    quietly $ cmd options [path] args

makeExecutable :: FilePath -> Action ()
makeExecutable file = do
    putBuild $ "| Make " ++ quote file ++ " executable."
    quietly $ cmd "chmod +x " [file]

-- | Print out information about the command being executed.
putInfo :: Target -> Action ()
putInfo Target {..} = putProgressInfo $ renderAction
    ("Run " ++ show builder ++ contextInfo) (digest inputs) (digest outputs)
  where
    contextInfo = concat $ [ " (" ]
        ++ [ "stage = "     ++ show (stage context) ]
        ++ [ ", package = " ++ pkgNameString (package context) ]
        ++ [ ", way = "     ++ show (way context) | way context /= vanilla ]
        ++ [ ")" ]
    digest [] = "none"
    digest [x] = x
    digest (x:xs) = x ++ " (and " ++ show (length xs) ++ " more)"

-- | Version of @putBuild@ controlled by @progressInfo@ command line flag.
putProgressInfo :: String -> Action ()
putProgressInfo msg = when (cmdProgressInfo /= None) $ putBuild msg

-- | Render an action.
renderAction :: String -> String -> String -> String
renderAction what input output = case cmdProgressInfo of
    Normal  -> renderBox [ what
                         , "     input: " ++ input
                         , " => output: " ++ output ]
    Brief   -> "| " ++ what ++ ": " ++ input ++ " => " ++ output
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
