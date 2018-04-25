{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Instances for the Scalar class.
--   These let us coerce scalar U.Arrays to PData arrays.
module Data.Array.Parallel.PArray.ScalarInstances where
import Data.Array.Parallel.Lifted.TH.Repr
import Data.Array.Parallel.PArray.Scalar        
import Data.Array.Parallel.PArray.PData
import GHC.Word  ( Word8 )


$(scalarInstances [''Int, ''Float, ''Double, ''Word8])

{- Generated code:
    newtype instance PData Int  = PInt (U.Array Int)

    instance Scalar Int where
      fromScalarPData (PInt xs) = xs
      toScalarPData             = PInt

    instance PR Int where
       <forward to *PRScalar methods>
-}
