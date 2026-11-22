{-# LANGUAGE TypeOperators #-}

-- | List length laws and their equality witness.
module HaskLib (module HaskLib.List, plusZero, plusSucc, (:~:) (Refl)) where

import Data.Type.Equality ((:~:) (Refl))
import HaskLib.List
import HaskLib.Size (plusSucc, plusZero)
