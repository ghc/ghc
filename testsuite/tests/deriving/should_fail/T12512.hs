{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE TypeInType #-}
module T12512 where

import GHC.Exts

class Wat1 (a :: TYPEvis ('TupleRep ['LiftedRep, 'LiftedRep]))
deriving instance Wat1 (# a, b #)

class Wat2 (a :: TYPEvis ('SumRep ['LiftedRep, 'LiftedRep]))
deriving instance Wat2 (# a | b #)
