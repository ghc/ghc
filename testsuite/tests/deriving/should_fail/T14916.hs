{-# LANGUAGE DeriveAnyClass #-}
module T14916 where

import Data.Coerce
import Data.Typeable

data A = MkA deriving ((~) A)
data B = MkB deriving (Coercible B)
