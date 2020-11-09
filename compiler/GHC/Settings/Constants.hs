-- | Compile-time settings
module GHC.Settings.Constants where

import GHC.Prelude

import GHC.Settings.Config

hiVersion :: Integer
hiVersion = read (cProjectVersionInt ++ cProjectPatchLevel) :: Integer

-- All pretty arbitrary:

mAX_TUPLE_SIZE :: Int
mAX_TUPLE_SIZE = 64 -- Should really match the number
                    -- of decls in GHC.Tuple

mAX_CTUPLE_SIZE :: Int   -- Constraint tuples
mAX_CTUPLE_SIZE = 64     -- Should match the number of decls in GHC.Classes

mAX_SUM_SIZE :: Int
mAX_SUM_SIZE = 64

-- | Default maximum depth for both class instance search and type family
-- reduction. See also #5395.
mAX_REDUCTION_DEPTH :: Int
mAX_REDUCTION_DEPTH = 200

-- | Default maximum constraint-solver iterations
-- Typically there should be very few
mAX_SOLVER_ITERATIONS :: Int
mAX_SOLVER_ITERATIONS = 4

wORD64_SIZE :: Int
wORD64_SIZE = 8

-- Size of float in bytes.
fLOAT_SIZE :: Int
fLOAT_SIZE = 4

-- Size of double in bytes.
dOUBLE_SIZE :: Int
dOUBLE_SIZE = 8

tARGET_MAX_CHAR :: Int
tARGET_MAX_CHAR = 0x10ffff
