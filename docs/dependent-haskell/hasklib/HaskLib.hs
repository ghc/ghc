{-# LANGUAGE TypeOperators #-}

-- | List length laws and their equality witness.
module HaskLib
    ( module HaskLib.List
    , plusZero, plusSucc, congSucc, congFirst
    , (:~:) (Refl), trans
    ) where

import Data.Type.Equality ((:~:) (Refl), trans)
import HaskLib.List
import HaskLib.Size (congFirst, congSucc, plusSucc, plusZero)
