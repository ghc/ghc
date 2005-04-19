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
    ('-':'f' : ghc) : filename : args -> do
	doIt (dropWhile isSpace ghc) filename args
    filename : args -> do
	mb_ghc <- findExecutable "ghc"
	case mb_ghc of
	  Nothing  -> dieProg ("cannot find ghc")
	  Just ghc -> doIt ghc filename args
    _other -> do
	dieProg "syntax: runghc [-f GHCPATH] FILE ARG..."

doIt ghc filename args = do
  res <- rawSystem ghc ["-ignore-dot-ghci", 
			"-e","System.Environment.withProgName "++show filename++" (System.Environment.withArgs ["
			++ concat (intersperse "," (map show args))
			++ "] Main.main)", filename]
  exitWith res

dieProg :: String -> IO a
dieProg msg = do
  p <- getProgName
  hPutStr stderr (p ++ ": " ++ msg)
  exitWith (ExitFailure 1)
