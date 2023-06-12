{-# LANGUAGE MagicHash #-}
module T20204B where

import GHC.Base (Int#)

data Test = Test {-# UNPACK #-} Int#

