{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
module T12512 where

import GHC.Exts

class Wat1 (a :: TYPE 'UnboxedTupleRep)
deriving instance Wat1 (# a, b #)

class Wat2 (a :: TYPE 'UnboxedSumRep)
deriving instance Wat2 (# a | b #)
