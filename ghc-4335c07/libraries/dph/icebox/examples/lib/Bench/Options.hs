{-# LANGUAGE CPP #-}

#include "MachDeps.h"

module Bench.Options (
  Options(..),
  ndpMain, failWith
) where

import System.Console.GetOpt
import System.IO
import System.Exit
import System.Environment

import Control.Monad ( when )
import Data.Char ( toUpper )
import GHC.IOArray ( newIOArray )

import Data.Array.Parallel.Unlifted.Distributed

data Options = Options { optRuns       :: Int
                       , optAlloc      :: Int
                       , optVerbosity  :: Int
                       , optHelp       :: Bool
                       }

defaultVerbosity :: Int
defaultVerbosity = 1

defaultOptions :: Options
defaultOptions = Options { optRuns       = 1
                         , optAlloc      = 0
                         , optVerbosity  = defaultVerbosity
                         , optHelp       = False
                         }

options = [Option ['r'] ["runs"]
            (ReqArg (\s o -> o { optRuns = read s }) "N")
            "repeat each benchmark N times"
         ,Option ['A'] ["alloc"]
            (ReqArg (\s o -> o { optAlloc = nbytes s }) "N")
            "preallocate memory"
         ,Option ['v'] ["verbose"]
            (OptArg (\r o -> o { optVerbosity = maybe defaultVerbosity read r })
                    "N")
            "verbosity level"
         ,Option ['h'] ["help"]
                     (NoArg (\o -> o { optHelp = True }))
            "show help screen"
         ]
  where
    nbytes s = case reads s of
                 [(n,"")] -> n
                 [(n,[c])] -> case toUpper c of
                                'K' -> n * 1024
                                'M' -> n * 1024 * 1024
                                'G' -> n * 1024 * 1024 * 1024

instance Functor OptDescr where
  fmap f (Option c s d h) = Option c s (fmap f d) h

instance Functor ArgDescr where
  fmap f (NoArg x) = NoArg (f x)
  fmap f (ReqArg g s) = ReqArg (f . g) s
  fmap f (OptArg g s) = OptArg (f . g) s

ndpMain :: String -> String
        -> (Options -> a -> [String] -> IO ())
        -> [OptDescr (a -> a)] -> a
        -> IO ()
ndpMain descr hdr run options' dft =
  do
    args <- getArgs
    case getOpt Permute opts args of
      (fs, files, []) ->
        let (os, os') = foldr ($) (defaultOptions, dft) fs
        in
        if optHelp os
          then do
                 s <- getProgName
                 putStrLn $ usageInfo ("Usage: " ++ s ++ " " ++ hdr ++ "\n"
                                       ++ descr ++ "\n") opts
          else do
                 when (optAlloc os /= 0)
                   $ do
                       _ <- newIOArray (0, optAlloc os `div` SIZEOF_HSWORD) undefined
                       return ()
                 run os os' files
      (_, _, errs) -> failWith errs
  where
    opts = [fmap (\f (r,s) -> (f r, s)) d | d <- options]
           ++ [fmap (\f (r,s) -> (r, f s)) d | d <- options']

failWith :: [String] -> IO a
failWith errs = do
                  mapM_ (hPutStrLn stderr) errs
                  exitFailure

