{-# LANGUAGE TypeFamilies #-}

module T10534a where

import Data.Coerce

data family DF a

silly :: Coercible (DF a) (DF b) => a -> b
silly = coerce
