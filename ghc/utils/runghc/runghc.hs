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
--	#! /usr/bin/runghc
--	> main = putStrLn "hello!"
--
-- runghc accepts one flag:
--
--	-f <path>    specify the path
--
-- -----------------------------------------------------------------------------

module Main where

import System.Environment
import System.IO
import Data.List
import System.Exit
import Data.Char

import Compat.RawSystem 	( rawSystem )
import Compat.Directory 	( findExecutable )

main = do 
  args <- getArgs
  case args of
    ('-':'f' : ghc) : args -> do
	doIt (dropWhile isSpace ghc) args
    args -> do
	mb_ghc <- findExecutable "ghc"
	case mb_ghc of
	  Nothing  -> dieProg ("cannot find ghc")
	  Just ghc -> doIt ghc args

doIt ghc args = do
  let
    (ghc_args, rest) = break notArg args
  --
  case rest of
     [] -> dieProg "syntax: runghc [-f GHCPATH] [GHC-ARGS] FILE ARG..."
     filename : prog_args -> do
	  res <- rawSystem ghc (
			"-ignore-dot-ghci" : ghc_args ++ 
			[ "-e","System.Environment.withProgName "++show filename++" (System.Environment.withArgs ["
			  ++ concat (intersperse "," (map show prog_args))
			  ++ "] Main.main)", filename])
  	  exitWith res

notArg ('-':_) = False
notArg _       = True

dieProg :: String -> IO a
dieProg msg = do
  p <- getProgName
  hPutStr stderr (p ++ ": " ++ msg)
  exitWith (ExitFailure 1)
