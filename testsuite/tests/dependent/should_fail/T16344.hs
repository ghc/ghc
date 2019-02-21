{-# LANGUAGE TypeInType, KindSignatures #-}

module T16344 where

import Data.Kind

data T ka (a::ka) b  = MkT (T Type           Int   Bool)
                           (T (Type -> Type) Maybe Bool)
