{-# OPTIONS -cpp -fffi #-}
#if __GLASGOW_HASKELL__ < 603
#include "config.h"
#else
#include "ghcconfig.h"
#endif
-----------------------------------------------------------------------------
--
-- (c) The University of Glasgow, 2004
--
-- runghc program, for invoking from a #! line in a script.  For example:
--
--   script.lhs:
--      #! /usr/bin/runghc
--      > main = putStrLn "hello!"
--
-- runghc accepts one flag:
--
--      -f <path>    specify the path
--
-- -----------------------------------------------------------------------------

module Main (main) where

import System.Environment
import System.IO
import Data.List
import System.Exit
import Data.Char
import System.Directory ( removeFile )
import Control.Exception  ( bracket )
import System.Directory ( findExecutable, getTemporaryDirectory )

#ifdef USING_COMPAT
import Compat.RawSystem ( rawSystem )
#else
import System.Cmd       ( rawSystem )
#endif

main :: IO ()
main = do
    args <- getArgs
    case getGhcLoc args of
        (Just ghc, args') -> doIt ghc args'
        (Nothing, args') -> do
            mb_ghc <- findExecutable "ghc"
            case mb_ghc of
                Nothing  -> dieProg ("cannot find ghc")
                Just ghc -> doIt ghc args'

getGhcLoc :: [String] -> (Maybe FilePath, [String])
getGhcLoc ("-f" : ghc : args) = (Just ghc, args)
getGhcLoc (('-' : 'f' : ghc) : args) = (Just ghc, args)
-- If you need the first GHC flag to be a -f flag then you can pass --
-- first
getGhcLoc ("--" : args) = (Nothing, args)
getGhcLoc args = (Nothing, args)

doIt :: String -> [String] -> IO ()
doIt ghc args = do
    let (ghc_args, rest) = getGhcArgs args
    case rest of
        [] -> do
           -- behave like typical perl, python, ruby interpreters:      
           -- read from stdin
           tmpdir <- getTemporaryDirectory
           bracket
             (openTempFile tmpdir "runghcXXXX.hs")
             (\(filename,_) -> removeFile filename)
             $ \(filename,h) -> do
                 getContents >>= hPutStr h
                 hClose h
                 doIt ghc (ghc_args ++ [filename])
        filename : prog_args -> do
            let c1 = ":set prog " ++ show filename
                c2 = ":main " ++ show prog_args
            res <- rawSystem ghc (["-ignore-dot-ghci"] ++ ghc_args ++
                                  [ "-e", c1, "-e", c2, filename])
            exitWith res

getGhcArgs :: [String] -> ([String], [String])
getGhcArgs args
 = let (ghcArgs, otherArgs) = case break pastArgs args of
                              (xs, "--":ys) -> (xs, ys)
                              (xs, ys)      -> (xs, ys)
   in (map unescape ghcArgs, otherArgs)
    where unescape ('-':'-':'g':'h':'c':'-':'a':'r':'g':'=':arg) = arg
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

