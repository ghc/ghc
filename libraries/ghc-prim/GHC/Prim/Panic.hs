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

-- | Display the CString whose address is given as an argument and exit.
panicError :: Addr# -> a
panicError errmsg =
  runRW# (\s ->
    case panic# errmsg s of
      (# _, _ #) -> -- This bottom is unreachable but we can't
                    -- use an empty case lest the pattern match
                    -- checker squawks.
                    let x = x in x)

-- | Closure introduced by GHC.Stg.Unarise for unused unboxed sum fields.
--
-- See Note [aBSENT_SUM_FIELD_ERROR_ID] in GHC.Core.Make
absentSumFieldError :: a
absentSumFieldError = panicError "entered absent sum field!"#

-- GHC.Core.Make.aBSENT_SUM_FIELD_ERROR_ID gives absentSumFieldError a bottoming
-- demand signature. But if we ever inlined it (to a call to panicError) we'd
-- lose that information.  Should not happen because absentSumFieldError is only
-- introduced in Stg.Unarise, long after inlining has stopped, but it seems
-- more direct simply to give it a NOINLINE pragma
{-# NOINLINE absentSumFieldError #-}
