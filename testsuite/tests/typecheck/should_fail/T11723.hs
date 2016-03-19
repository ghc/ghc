{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Example where

import Data.Typeable
import GHC.Exts

data Wat (a :: TYPE 'UnboxedTupleRep) = Wat a
