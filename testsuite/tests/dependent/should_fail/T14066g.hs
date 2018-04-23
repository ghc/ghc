{-# LANGUAGE TypeInType #-}

module T14066g where

import Data.Kind

data SameKind :: k -> k -> Type

data Q a (b :: a) (d :: SameKind c b)
