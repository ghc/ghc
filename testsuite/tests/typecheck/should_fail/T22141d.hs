{-# LANGUAGE NoDataKinds #-}
{-# LANGUAGE UnboxedSums #-}
module T22141d where

import Data.Kind (Type)
import Data.Proxy (Proxy)

type T = (# Type | Type #)

type D :: Proxy T -> Type
data D a
