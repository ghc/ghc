{-# LANGUAGE NoDataKinds #-}
module T22141e where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import T22141e_Aux

type D :: Proxy T -> Type
data D a
