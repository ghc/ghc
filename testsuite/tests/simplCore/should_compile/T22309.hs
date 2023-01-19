{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module ShouldCompile where

import GHC.Int
import GHC.Exts

-- These should unbox into another constructor
data UA = Mk_A !Int
data UB = Mk_B !Int64
data UC = Mk_C !Int32
data UD = Mk_D !Int32 !Int32
data UE = Mk_E !(# Int# #)
data UF = Mk_F !(# Double #)

-- These should not be unpacked into another constructor.
data NU_A = NU_MkA (# Int64, Int64 #)
data NU_B = NU_MkB !Int64 !Int64

-- The types we unbox into

-- These should unpack their fields.
data WU_A = MkW_A !UA
data WU_B = MkW_B !UB
data WU_C = MkW_C !UC
data WU_D = MkW_D !UD
data WU_E = MkW_E !UE
data WU_F = MkW_F !UF

-- These should not unpack their fields, as they are multiple words large.
data WNU_A = MkW_NA !NU_A
data WNU_B = MkW_NB !NU_B


