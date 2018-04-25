{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Defines the `PRepr` family and `PA` class that converts between the user
--   level element types and our generic representation.
--   Apart from `unpackPA`, the `PA` wrapper functions defined here all have
--   equivalent `PR` versions in "Data.Array.Parallel.PArray.PData",
--   so see there for documentation.
module Data.Array.Parallel.PArray.PRepr
        ( module Data.Array.Parallel.PArray.PRepr.Base
        , module Data.Array.Parallel.PArray.PRepr.Instances

        -- * Nested Arrays
        , module Data.Array.Parallel.PArray.PRepr.Nested
        , unpackPA

        -- * Tuple Arrays
        , module Data.Array.Parallel.PArray.PRepr.Tuple)
where
import Data.Array.Parallel.PArray.PRepr.Base
import Data.Array.Parallel.PArray.PRepr.Instances
import Data.Array.Parallel.PArray.PRepr.Nested
import Data.Array.Parallel.PArray.PRepr.Tuple
import Data.Array.Parallel.PArray.PData
import Data.Array.Parallel.Pretty
import qualified Data.Vector                    as V


-- Pretty -------------------------------------------------------------------
instance (Show a, PA a)
        => Show (PArray a) where
 show (PArray _ pdata)
        = render 
        $ brackets 
        $ text "|"
                <> (hcat $ punctuate comma 
                         $ map (text . show) $ V.toList $ toVectorPA pdata)
                <> text "|"


instance  (PprVirtual a, PA a)
        => PprVirtual (PArray a) where
 pprv (PArray _ pdata)
        = brackets 
        $ text "|"
                <> (hcat $ punctuate comma 
                         $ map pprv $ V.toList $ toVectorPA pdata)
                <> text "|"


-- Unpack ----------------------------------------------------------------------
-- | Unpack an array to reveal its representation.
{-# INLINE_PA unpackPA #-}
unpackPA :: PA a => PArray a -> PData (PRepr a)
unpackPA (PArray _ pdata)
        = toArrPRepr pdata
