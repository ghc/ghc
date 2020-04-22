{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE EmptyCase #-}

-- | Primitive panics.
module GHC.Prim.Panic
   ( absentSumFieldError
   , panicError
   )
where

import GHC.Prim
import GHC.Magic

default () -- Double and Integer aren't available yet

-- `stg_panic#` never returns but it can't just return `State# RealWorld` so we
-- indicate that it returns `Void#` too to make the compiler happy.
foreign import prim "stg_paniczh" panic# :: Addr# -> State# RealWorld -> (# State# RealWorld, Void# #)

panicError :: Addr# -> a
panicError errmsg =
  runRW# (\s ->
    case panic# errmsg s of
      (# _, _ #) -> -- This bottom is unreachable but we can't
                    -- use an empty case lest the pattern match
                    -- checker squawks.
                    let x = x in x)

absentSumFieldError :: a
absentSumFieldError = panicError "entered absent sum field!"#
