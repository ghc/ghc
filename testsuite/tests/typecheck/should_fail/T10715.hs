{-# LANGUAGE FlexibleContexts #-}
module T10715 where

import Data.Coerce (coerce, Coercible)
import Data.Ord ( Down )  -- convenient newtype

data X a

doCoerce :: Coercible a (X a) => a -> X a
doCoerce = coerce
