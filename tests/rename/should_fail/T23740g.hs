{-# LANGUAGE RequiredTypeArguments #-}

module T23740g where

import Data.Kind

data T k (id::head) (b::k) :: k2 -> head -> Type