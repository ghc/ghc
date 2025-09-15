module T10709 where

import GHC.IO
import Control.Monad

x1 = replicateM 2 . mask
x2 = (replicateM 2 . mask) undefined
x3 = (replicateM 2 . mask) $ undefined
