{-# LANGUAGE TypeOperators #-}

-- | List length laws and their equality witness.
module HaskLib (module HaskLib.List, (:~:) (Refl)) where

import Data.Type.Equality ((:~:) (Refl))
import HaskLib.List
