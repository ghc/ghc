{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module T10177 where

import Data.Typeable

newtype V n a = V [a]

class    Typeable a                   => C a
instance (Typeable (V n), Typeable a) => C (V n a)
