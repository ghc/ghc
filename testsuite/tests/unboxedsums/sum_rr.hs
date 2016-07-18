{-# LANGUAGE DataKinds, KindSignatures #-}

module Example where

import Data.Typeable
import GHC.Exts

data Wat (a :: TYPE 'UnboxedSumRep) = Wat a
