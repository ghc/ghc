module RtsClearMemory
  ( foo,
  )
where

import Control.DeepSeq
import Control.Exception
import Data.Functor

-- | Behold, mortal! This function doth summon forth a horde of trash,
-- mere playthings for the garbage collector's insatiable appetite.
foo :: Int -> IO ()
foo n = void $ evaluate $ force [0 .. n]

foreign export ccall foo :: Int -> IO ()
