{-# LANGUAGE CPP #-}
#include "ghcconfig.h"
-----------------------------------------------------------------------------
--
-- (c) The University of Glasgow, 2004
--
-- runghc program, for invoking from a #! line in a script.  For example:
--
--   script.lhs:
--      #!/usr/bin/env /usr/bin/runghc
--      > main = putStrLn "hello!"
--
-- runghc accepts one flag:
--
--      -f <path>    specify the path
--
-- -----------------------------------------------------------------------------

module Main (main) where

import Control.Exception
import Data.Monoid
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process

#if defined(mingw32_HOST_OS)
import Foreign
import Foreign.C.String
#endif

#if defined(mingw32_HOST_OS)
# if defined(i386_HOST_ARCH)
#  define WINDOWS_CCONV stdcall
# elif defined(x86_64_HOST_ARCH)
#  define WINDOWS_CCONV ccall
# else
#  error Unknown mingw32 arch
# endif
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
                Just path ->
                    let ghc = takeDirectory (normalise path) </> "ghc"
                    in uncurry (doIt ghc) $ getGhcArgs args'

data RunGhcFlags = RunGhcFlags (Maybe FilePath) -- GHC location
                 | Help -- Print help text
                 | ShowVersion -- Print version info

instance Monoid RunGhcFlags where
    mempty = RunGhcFlags Nothing
    Help `mappend` _ = Help
    _ `mappend` Help = Help
    ShowVersion `mappend` _ = ShowVersion
    _ `mappend` ShowVersion = ShowVersion
    RunGhcFlags _ `mappend` right@(RunGhcFlags (Just _)) = right
    left@(RunGhcFlags _) `mappend` RunGhcFlags Nothing = left

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
    putStrLn ("runghc " ++ VERSION)

printUsage :: IO ()
printUsage = do
    putStrLn "Usage: runghc [runghc flags] [GHC flags] module [program args]"
    putStrLn ""
    putStrLn "The runghc flags are"
    putStrLn "    -f /path/to/ghc       Tell runghc where GHC is"
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
            res <- rawSystem ghc (["-ignore-dot-ghci"] ++
                                  xflag ++
                                  ghc_args ++
                                  [ "-e", c1, "-e", c2, filename])
            exitWith res

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

foreign import WINDOWS_CCONV unsafe "windows.h GetModuleFileNameW"
  c_GetModuleFileName :: Ptr () -> CWString -> Word32 -> IO Word32
#else
getExecPath = return Nothing
#endif

