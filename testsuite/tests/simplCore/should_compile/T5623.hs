module T5623 where

import Foreign.Storable
import Control.Monad
import GHC.Ptr

foo :: Ptr Float -> IO Float
foo p = liftM2 (+) (peekElemOff q 0) (peekElemOff q 1)
  where
    q = p `plusPtr` 4
