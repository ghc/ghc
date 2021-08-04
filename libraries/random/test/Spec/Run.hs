module Spec.Run (runsEqual) where

import Data.Word (Word64)
import System.Random.Stateful

runsEqual :: RandomGen g => g -> IO Bool
runsEqual g = do
  let pureResult = runStateGen_ g uniformM :: Word64
      stResult = runSTGen_ g uniformM :: Word64
  ioGenM <- newIOGenM g
  ioResult <- uniformM ioGenM
  atomicGenM <- newAtomicGenM g
  atomicResult <- uniformM atomicGenM
  return $ all (pureResult ==) [stResult, ioResult, atomicResult]
