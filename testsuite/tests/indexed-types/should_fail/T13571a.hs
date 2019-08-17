{-# LANGUAGE TypeFamilies #-}

module T13571a where

import Data.Kind

type family G a = (r :: Type)
