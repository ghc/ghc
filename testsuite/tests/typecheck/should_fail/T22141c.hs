{-# LANGUAGE NoDataKinds #-}
{-# LANGUAGE UnboxedTuples #-}
module T22141c where

import Data.Kind (Type)
import Data.Proxy (Proxy)

type T = (# Type, Type #)

type D :: Proxy T -> Type
data D a
