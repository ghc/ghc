{-# LANGUAGE CPP #-}
#include <ghcplatform.h>
-----------------------------------------------------------------------------
--
-- (c) The University of Glasgow, 2004
--
-- runghc program, for invoking from a #! line in a script.  For example:
--
--   script.hs:
--      #!/usr/bin/env runghc
--      main = putStrLn "hello!"
--
-- runghc accepts one flag:
--
--      -f <path>    specify the path
--
-- -----------------------------------------------------------------------------

module Main (main) where

import Control.Exception
import Data.Semigroup as Semi
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO

#if defined(mingw32_HOST_OS)
import System.Process (rawSystem)
import Foreign
import Foreign.C.String
#else
import System.Posix.Process (executeFile)
#endif

main :: IO ()
main = do
    args <- getArgs
    case parseRunGhcFlags args of
        (Help, _) -> printUsage
        (ShowVersion, _) -> printVersion
        (RunGhcFlags (Just ghc), args') -> uncurry (doIt ghc) $ getGhcArgs args'
        (RunGhcFlags Nothing, args') -> do
            mbPath <- getExecPath
            case mbPath of
                Nothing  -> dieProg ("cannot find ghc")
                Just path -> do
                    ghc <- findGhc path
                    uncurry (doIt ghc) $ getGhcArgs args'

-- In some cases, runghc isn't given a path to ghc explicitly. This can occur
-- if $1_$2_SHELL_WRAPPER = NO (which is always the case on Windows). In such
-- a scenario, we must guess where ghc lives. Given a path where ghc might
-- live, we check for the existence of ghc. If we can't find it, we assume that
-- we're building ghc from source, in which case we fall back on ghc-stage2.
-- (See #1185.)
--
-- In-tree Hadrian builds of GHC also happen to give us a wrapper-script-less
-- runghc. In those cases, 'getExecPath' returns the directory where runghc
-- lives, which is also where the 'ghc' executable lives, so the guessing logic
-- covers this scenario just as nicely.
findGhc :: FilePath -> IO FilePath
findGhc path = do
    let ghcDir = takeDirectory (normalise path)
        ghc    = ghcDir </> "ghc" <.> exeExtension
    ghcExists <- doesFileExist ghc
    return $ if ghcExists
             then ghc
             else ghcDir </> "ghc-stage2" <.> exeExtension

data RunGhcFlags = RunGhcFlags (Maybe FilePath) -- GHC location
                 | Help -- Print help text
                 | ShowVersion -- Print version info

instance Semi.Semigroup RunGhcFlags where
    Help <> _ = Help
    _ <> Help = Help
    ShowVersion <> _ = ShowVersion
    _ <> ShowVersion = ShowVersion
    RunGhcFlags _ <> right@(RunGhcFlags (Just _)) = right
    left@(RunGhcFlags _) <> RunGhcFlags Nothing = left

instance Monoid RunGhcFlags where
    mempty = RunGhcFlags Nothing
    mappend = (<>)

parseRunGhcFlags :: [String] -> (RunGhcFlags, [String])
parseRunGhcFlags = f mempty
    where f flags ("-f" : ghc : args)
              = f (flags `mappend` RunGhcFlags (Just ghc)) args
          f flags (('-' : 'f' : ghc) : args)
              = f (flags `mappend` RunGhcFlags (Just ghc)) args
          f flags ("--help" : args) = f (flags `mappend` Help) args
          f flags ("--version" : args) = f (flags `mappend` ShowVersion) args
          -- If you need the first GHC flag to be a -f flag then
          -- you can pass -- first
          f flags ("--" : args) = (flags, args)
          f flags         args  = (flags, args)

printVersion :: IO ()
printVersion = do
    putStrLn ("runghc " ++ CURRENT_PACKAGE_VERSION )

printUsage :: IO ()
printUsage = do
    putStrLn "Usage: runghc [runghc flags] [GHC flags] module [program args]"
    putStrLn ""
    putStrLn "The runghc flags are"
    putStrLn "    -f /path/to/ghc       Tell runghc where GHC is"
    putStrLn "    --ghc-arg=<arg>       Pass an option or argument to GHC"
    putStrLn "    --help                Print this usage information"
    putStrLn "    --version             Print version number"

doIt :: String -- ^ path to GHC
     -> [String] -- ^ GHC args
     -> [String] -- ^ rest of the args
     -> IO ()
doIt ghc ghc_args rest = do
    case rest of
        [] -> do
           -- behave like typical perl, python, ruby interpreters:
           -- read from stdin
           tmpdir <- getTemporaryDirectory
           bracket
             (openTempFile tmpdir "runghcXXXX.hs")
             (\(filename,h) -> do hClose h; removeFile filename)
             $ \(filename,h) -> do
                 getContents >>= hPutStr h
                 hClose h
                 doIt ghc ghc_args [filename]
        filename : prog_args -> do
            -- If the file exists, and is not a .lhs file, then we
            -- want to treat it as a .hs file.
            --
            -- If the file doesn't exist then GHC is going to look for
            -- filename.hs and filename.lhs, and use the appropriate
            -- type.
            exists <- doesFileExist filename
            let xflag = if exists && (takeExtension filename /= ".lhs")
                        then ["-x", "hs"]
                        else []
                c1 = ":set prog " ++ show filename
                c2 = ":main " ++ show prog_args

            let cmd = ghc
                args = ["-ignore-dot-ghci"] ++
                       xflag ++
                       ghc_args ++
                       [ "-e", c1, "-e", c2, filename]


#if defined(mingw32_HOST_OS)
            rawSystem cmd args >>= exitWith
#else
            -- Passing False to avoid searching the PATH, since the cmd should
            -- always be an absolute path to the ghc executable.
            executeFile cmd False args Nothing
#endif

getGhcArgs :: [String] -> ([String], [String])
getGhcArgs args
 = let (ghcArgs, otherArgs) = case break pastArgs args of
                              (xs, "--":ys) -> (xs, ys)
                              (xs, ys)      -> (xs, ys)
   in (map unescape ghcArgs, otherArgs)
    where unescape ('-':'-':'g':'h':'c':'-':'a':'r':'g':'=':arg) =
                case arg of
                    -- Bug #8601: allow --ghc-arg=--ghc-arg= as a prefix as well for backwards compatibility
                    ('-':'-':'g':'h':'c':'-':'a':'r':'g':'=':arg') -> arg'
                    _ -> arg
          unescape arg = arg

pastArgs :: String -> Bool
-- You can use -- to mark the end of the flags, in case you need to use
-- a file called -foo.hs for some reason. You almost certainly shouldn't,
-- though.
pastArgs "--" = True
pastArgs ('-':_) = False
pastArgs _       = True

dieProg :: String -> IO a
dieProg msg = do
    p <- getProgName
    hPutStrLn stderr (p ++ ": " ++ msg)
    exitWith (ExitFailure 1)

-- usage :: String
-- usage = "syntax: runghc [-f GHC-PATH | --] [GHC-ARGS] [--] FILE ARG..."

getExecPath :: IO (Maybe String)
#if defined(mingw32_HOST_OS)
getExecPath = try_size 2048 -- plenty, PATH_MAX is 512 under Win32.
  where
    try_size size = allocaArray (fromIntegral size) $ \buf -> do
        ret <- c_GetModuleFileName nullPtr buf size
        case ret of
          0 -> return Nothing
          _ | ret < size -> fmap Just $ peekCWString buf
            | otherwise  -> try_size (size * 2)

foreign import ccall unsafe "windows.h GetModuleFileNameW"
  c_GetModuleFileName :: Ptr () -> CWString -> Word32 -> IO Word32
#else
getExecPath = Just <$> getExecutablePath
#endif
