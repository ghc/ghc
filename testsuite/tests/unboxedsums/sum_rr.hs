{-# LANGUAGE DataKinds, PolyKinds #-}

module Example where

import Data.Typeable
import GHC.Exts

data Wat (a :: TYPE (SumRep '[LiftedRep, IntRep])) = Wat a
