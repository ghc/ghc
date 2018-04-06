{-# LANGUAGE TypeFamilies, PolyKinds, TypeInType #-}

module T14175 where

import Data.Kind

type family PComp (k :: j -> Type) (x :: k) :: ()
