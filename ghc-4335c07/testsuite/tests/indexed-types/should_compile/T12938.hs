{-# LANGUAGE TypeInType, TypeFamilies #-}

module Bug where

import GHC.Exts

class HasRep a where
  type Rep a :: TYPE r
