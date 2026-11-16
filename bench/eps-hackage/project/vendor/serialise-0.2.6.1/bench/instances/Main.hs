module Main
  ( main -- :: IO ()
  ) where

import           Criterion.Main (bgroup, defaultMain)

import qualified Instances.Float   as Float
import qualified Instances.Integer as Integer
import qualified Instances.Time    as Time
import qualified Instances.Vector  as Vector

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain
  [ bgroup "float"   Float.benchmarks
  , bgroup "integer" Integer.benchmarks
  , bgroup "time"    Time.benchmarks
  , bgroup "vector"  Vector.benchmarks
  ]
