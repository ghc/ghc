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
import System.Directory
import System.Exit
import Data.Char

import Compat.RawSystem 	( rawSystem )

main = do 
  args <- getArgs
  case args of
    ('-':'f' : ghc) : filename : args -> do
	doIt (dropWhile isSpace ghc) filename args
    filename : args -> do
	path <- getEnv "PATH" `catch` \e -> return "."
	ghc <- findBinary "ghc"
	doIt ghc filename args
    _other -> do
	dieProg "syntax: runghc [-f GHCPATH] FILE ARG..."

doIt ghc filename args = do
  res <- rawSystem ghc ["-e","System.Environment.withArgs ["
			++ concat (intersperse "," (map show args))
			++ "] Main.main", filename]
  exitWith res

findBinary :: String -> IO FilePath
findBinary binary = do
  path <- getEnv "PATH"
  search (parsePath path)
  where
    search :: [FilePath] -> IO FilePath
    search [] = dieProg ("cannot find " ++ binary)
    search (d:ds) = do
	let path = d ++ '/':binary
	b <- doesFileExist path
	if b  then return path else search ds

parsePath :: String -> [FilePath]
parsePath path = split pathSep path
  where
#ifdef mingw32_TARGET_OS
	pathSep = ';'
#else
	pathSep = ':'
#endif

split :: Char -> String -> [String]
split c s = case rest of
		[]     -> [chunk] 
		_:rest -> chunk : split c rest
  where (chunk, rest) = break (==c) s

die :: String -> IO a
die msg = do hPutStr stderr msg; exitWith (ExitFailure 1)

dieProg :: String -> IO a
dieProg msg = do p <- getProgName; die (p ++ ": " ++ msg)
