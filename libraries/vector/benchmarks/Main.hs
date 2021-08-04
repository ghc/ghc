module Main where

import Criterion.Main
import Criterion.Main.Options
import Options.Applicative

import Algo.ListRank  (listRank)
import Algo.Rootfix   (rootfix)
import Algo.Leaffix   (leaffix)
import Algo.AwShCC    (awshcc)
import Algo.HybCC     (hybcc)
import Algo.Quickhull (quickhull)
import Algo.Spectral  ( spectral )
import Algo.Tridiag   ( tridiag )

import TestData.ParenTree ( parenTree )
import TestData.Graph     ( randomGraph )
import TestData.Random    ( randomVector )

import Data.Vector.Unboxed ( Vector )

import System.Environment
import Data.Word

import Data.Word

data BenchArgs = BenchArgs
  { seed      :: Word32
  , size      :: Int
  , otherArgs :: Mode
  }

defaultSize :: Int
defaultSize = 2000000

defaultSeed :: Word32
defaultSeed = 42

parseBenchArgs :: Parser BenchArgs
parseBenchArgs = BenchArgs
  <$> option auto
      (  long "seed"
      <> metavar "NUM"
      <> value defaultSeed
      <> help "A value with which to initialize the PRNG" )
  <*> option auto
      (  long "size"
      <> metavar "NUM"
      <> value defaultSize
      <> help "A value to use as the default entries in data structures. Benchmarks are broken for very small numbers." )
  <*> parseWith defaultConfig

main :: IO ()
main = do
  args <- execParser $ describeWith parseBenchArgs

  let useSeed = seed args
  let useSize = size args

  let (lparens, rparens) = parenTree useSize
  let (nodes, edges1, edges2) = randomGraph useSeed useSize
  lparens `seq` rparens `seq`
    nodes `seq` edges1 `seq` edges2 `seq` return ()

  as <- randomVector useSeed useSize :: IO (Vector Double)
  bs <- randomVector useSeed useSize :: IO (Vector Double)
  cs <- randomVector useSeed useSize :: IO (Vector Double)
  ds <- randomVector useSeed useSize :: IO (Vector Double)
  sp <- randomVector useSeed (floor $ sqrt $ fromIntegral useSize)
                          :: IO (Vector Double)
  as `seq` bs `seq` cs `seq` ds `seq` sp `seq` return ()
  putStrLn "foo"
  runMode (otherArgs args)
                [ bench "listRank"  $ whnf listRank useSize
                , bench "rootfix"   $ whnf rootfix (lparens, rparens)
                , bench "leaffix"   $ whnf leaffix (lparens, rparens)
                , bench "awshcc"    $ whnf awshcc (nodes, edges1, edges2)
                , bench "hybcc"     $ whnf hybcc  (nodes, edges1, edges2)
                , bench "quickhull" $ whnf quickhull (as,bs)
                , bench "spectral"  $ whnf spectral sp
                , bench "tridiag"   $ whnf tridiag (as,bs,cs,ds)
                ]
