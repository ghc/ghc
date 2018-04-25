#!/usr/bin/env runhaskell

{-# LANGUAGE ScopedTypeVariables, PatternGuards #-}

-- DPH benchmark driver
-- 
-- It runs all available benchmarks sequential and in parallel.  Parallel
-- execution starts with a single thread and then steps through powers of two
-- up to the number of hardware threads supported by the benchmark hardware.
-- (Hardware threads are the number of cores times the number of hardware
-- thread contexts per core.)
--
-- The driver needs to query the host for simple hardware specs.  These
-- queries are OS-dependent and currently only implemented for Mac OS X and
-- Solaris.  Please send patches adding support for other architectures to
--
--   glasgow-haskell-users@haskell.org
--
-- Hardware specifics should be restricted to the function 'getHardwareSpec'.


import Prelude hiding (catch, seq)

import Control.Exception  (IOException, catch)
import Control.Monad      (liftM)
import Data.Char          (toLower)
import Data.List          (intercalate)
import System.Environment (getProgName, getArgs)
import System.Exit
import System.FilePath
import System.Process     (readProcess, system)
import Text.Printf


-- Constants
-- ---------

noOfRuns :: Int
noOfRuns = 3    -- average over that many runs of a single implementation


-- Hardware
-- --------

data HardwareSpec = HW { uname    :: String  -- machine identification
                       , ncores   :: Int     -- number of cores
                       , nthreads :: Int     -- numbers of hardware threads/core
                       }

getHardwareSpec :: IO HardwareSpec
getHardwareSpec
  = do
      uname <- liftM (filter (/= '\n')) $ readProcess "uname" ["-npsr"] ""
      case uname of
        'D':'a':'r':'w':'i':'n':_ -> do
          ncpu <- do
                    ncpu <- readProcess "sysctl" ["hw.ncpu"] ""
                    case ncpu of
                      'h':'w':'.':'n':'c':'p':'u':':':' ':n -> 
                        return (read n :: Int)
                          `catch` \(e :: IOException) -> fatal (show e)
                      _ -> fatal $ "sysctl hw.ncpu" ++ ncpu
          return $ HW { uname = uname, ncores = ncpu, nthreads = 1 }
        'S':'u':'n':'O':'S':_ -> do
          fatal "not implemented yet"
        _ -> fatal $ "uname:" ++ uname


-- Benchmarks
-- ----------

-- Specification of a single benchmark (that consists of multiple
-- implementations)
--
data BenchmarkSpec = BM { name :: String        -- description
                        , dir  :: FilePath      -- benchmark directory
                        , dph  :: [ImpSpec]     -- DPH benchmarks
                        , seq  :: [ImpSpec]     -- sequential non-DPH benchmarks
                        , par  :: [ImpSpec]     -- parallel non-DPH benchmarks
                        }

-- A single implementation of a benchmark
--
-- This may be a DPH implementation that we run with both dph-seq and dph-par,
-- or it may be a sequential or parallel non-DPH program (in Haskell or a
-- reference language, typically C).  The arguments of a parallel non-DPH
-- program must contain '%d' twice as placeholder, first for the number of OS
-- threads and second for the number of runs to average over.  The arguments of
-- a sequential non-DPH program must contain '%d' for the number of runs.
--
-- Executables are assumed to be in the benchmark directory in subdirectories
-- 'seq/', 'par/', and 'other/' for sequential versions of DPH benchmarks,
-- parallel versions of DPH benchmarks, and non-DPH benchmarks, respectively.
--
data ImpSpec = Imp { impName :: String          -- implementation description
                   , impCmd  :: String          -- executable (in benchmark dir)
                   , impArgs :: [String]        -- arguments
                   }

selectBenchmarks :: [BenchmarkSpec] -> [String] -> IO [BenchmarkSpec]
selectBenchmarks bspecs []   = return bspecs
selectBenchmarks bspecs reqs = mapM selectBenchmark reqs
  where
    selectBenchmark req | bspec:_ <- filter (match req) bspecs = return bspec
                        | otherwise
                        = fatal ("unknown benchmark '" ++ req ++ "' " ++
                                 "(available: " ++ avail ++ ")")
                        where
                          avail = intercalate " " (map name bspecs)

                          match req bspec 
                            = map toLower req == map toLower (name bspec)

runBenchmarks :: HardwareSpec -> [BenchmarkSpec] -> IO ()
runBenchmarks hw = mapM_ (runBenchmark hw)

runBenchmark :: HardwareSpec -> BenchmarkSpec -> IO ()
runBenchmark hw bm
  = do
      printf "\n"
      putStrLn (dash ("-- Benchmark: " ++ name bm ++ " "))
      mapM_ runDphSeq (dph bm)
      mapM_ runDphPar (dph bm)
      mapM_ runSeq (seq bm)
      mapM_ runPar (par bm)
      putStrLn dashAll
  where
    baseDir     = dir bm
    threads     = takeWhile (<= (ncores hw * nthreads hw)) powersOfTwo
    powersOfTwo = 1 : map (*2) powersOfTwo

    runDphSeq (Imp { impName = name, impCmd = cmd, impArgs = args })
      = runSequential ("DPH " ++ name) (seqDir </> cmd) (seqDphExtraArg:args)

    runDphPar (Imp { impName = name, impCmd = cmd, impArgs = args })
      = runParallel threads ("DPH " ++ name) (parDir </> cmd) 
                    (parDphExtraArg:args)

    runSeq (Imp { impName = name, impCmd = cmd, impArgs = args })
      = runSequential name (otherDir </> cmd) args

    runPar (Imp { impName = name, impCmd = cmd, impArgs = args })
      = runParallel threads name (otherDir </> cmd) args

    seqDir   = baseDir </> "seq"
    parDir   = baseDir </> "par"
    otherDir = baseDir </> "other"

    seqDphExtraArg = "-r %d"
    parDphExtraArg = "-r %d +RTS -N%d -RTS"

-- Run a sequential implementation.
--
-- The arguments must contain '%d' once as a placeholder for the number of runs.
--
runSequential :: String -> FilePath -> [String] -> IO ()
runSequential name cmd args
  = do
      printf ">> %s [sequential]\n" name
      systemWithCheck $ printf ("%s " ++ intercalate " " args) cmd noOfRuns

-- Run a parallel implementation on a sequence of thread configurations.
--
runParallel :: [Int] -> String -> FilePath -> [String] -> IO ()
runParallel threads name cmd args = mapM_ (runParallelN name cmd args) threads

-- Run a parallel implementation with the specified number of OS threads.
--
-- The arguments must contain '%d' twice, first as a placeholder for the
-- number of threads and second for the number of runs.
--
runParallelN name cmd args n
  = do
      printf ">> %s [P = %d]\n" name n
      systemWithCheck $ printf ("%s " ++ intercalate " " args) cmd noOfRuns n


-- Utilities
-- ---------

fatal :: String -> IO a
fatal msg
  = do
      name <- getProgName
      putStrLn $ name ++ ": fatal error: " ++ msg
      exitFailure

dash :: String -> String
dash s = s ++ take (79 - length s) (repeat '-')

dashAll :: String
dashAll = dash ""

systemWithCheck :: String -> IO ()
systemWithCheck cmd 
  = do
--      printf "Invoking '%s'\n" cmd
      ec <- system cmd
      case ec of
        ExitSuccess   -> return ()
        ExitFailure c -> printf "execution failed (exit %d)\n" c


-- Main script
-- -----------

main 
  = do
      args        <- getArgs
      benchsToRun <- selectBenchmarks benchmarks args
      hw          <- getHardwareSpec

      putStrLn (dash "-- Data Parallel Haskell benchmarks ")
      printf "** Host               : %s\n" (uname hw)
      printf "** Cores              : %d\n" (ncores hw)
      printf "** Threads/core       : %d\n" (nthreads hw)
      printf "** Runs/implementation: %d\n" noOfRuns
      putStrLn dashAll

      runBenchmarks hw benchsToRun
  where
    benchmarks = [ sumsq, dotp, smvm, quickhull ]

    sumsq = BM { name = "SumSq"
               , dir  = "sumsq"
               , dph  = [ Imp { impName = "primitives"
                              , impCmd  = "prim"
                              , impArgs = [ tenMillion ] 
                              } 
                        , Imp { impName = "vectorised"
                              , impCmd  = "sumsq"
                              , impArgs = [ tenMillion ] 
                              } 
                        ]
               , seq  = [ Imp { impName = "ref C"
                              , impCmd  = "sumsq-c"
                              , impArgs = [ "%d", tenMillion ]
                              } 
                        ]
               , par  = [ {- no parallel reference implementation -} ]
               }

    dotp = BM { name = "DotP"
              , dir  = "dotp"
              , dph  = [ Imp { impName = "primitives"
                             , impCmd  = "prim"
                             , impArgs = [ hundredMillion ] 
                             } 
                       , Imp { impName = "vectorised"
                             , impCmd  = "dotp"
                             , impArgs = [ hundredMillion ] 
                             } 
                       ]
              , seq  = [ {- no sequential reference implementation -} ]
              , par  = [ Imp { impName = "ref Haskell"
                             , impCmd  = "DotP"
                             , impArgs = [ "%d +RTS -N%d -RTS", hundredMillion ]
                             }
                       , Imp { impName = "ref C"
                             , impCmd  = "dotp-c"
                             , impArgs = [ "%d %d", hundredMillion ]
                             } 
                       ]
              }

    smvm = BM { name = "SMVM"
              , dir  = "smvm"
              , dph  = [ Imp { impName = "primitives"
                             , impCmd  = "prim"
                             , impArgs = [ testmat ] 
                             } 
                       , Imp { impName = "vectorised"
                             , impCmd  = "smvm"
                             , impArgs = [ testmat ] 
                             } 
                       ]
              , seq  = [ Imp { impName = "ref C"
                             , impCmd  = "smvm-c"
                             , impArgs = [ "%d", testmat ]
                             } 
                       ]
              , par  = [ {- no parallel reference implementation -} ]
              }
           where
             testmat = "smvm" </> "test.mat" 

    quickhull = BM { name = "QuickHull"
                   , dir  = "quickhull"
                   , dph  = [ Imp { impName = "vectorised"
                                  , impCmd  = "quickhull"
                                  , impArgs = [ oneMillion ] 
                                  } 
                            ]
                   , seq  = [ Imp { impName = "ref Haskell"
                                  , impCmd  = "QuickHull"
                                  , impArgs = [ "%d", oneMillion ]
                                  } 
                            ]
                   , par  = [ {- no parallel reference implementation -} ]
                   }

    oneMillion     = "1000000"
    tenMillion     = "10000000"
    hundredMillion = "100000000"
