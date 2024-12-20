{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE RankNTypes, KindSignatures #-}

-- | Primitive panics.
--
-- Users should not import this module.  It is GHC internal only.
module GHC.Internal.Prim.Panic
   ( absentSumFieldError
   , panicError
   , absentError, absentConstraintError
   )
where

import GHC.Internal.Prim
import GHC.Internal.Magic
import GHC.Internal.Types( Type )

default () -- Double and Integer aren't available yet

{-
Note [Compiler error functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note: this was written before the base/ghc-internal split and before ghc-prim
was merged into ghc-internal. We could probably revisit this. See also
W2 of Note [Tracking dependencies on primitives] in GHC.Internal.Base.

Most error functions (such as pattern match failure) are defined
in base:Control.Exception.Base.  But absentError# and absentSumFieldError#
are defined here in the ghc-prim package for two reasons:

* GHC itself generates calls to these functions as a result of
  strictness analysis, over which the programmer has no control. So
  it is hard to ensure that no such calls exist in the modules
  compiled "before" Control.Base.Exception.  (E.g. when compiling
  with -fdicts-strict.)

* A consequence of defining them in ghc-prim is that the libraries
  defining exceptions have not yet been built, so we can't make them
  into proper Haskell exceptions.

  However, if these functions are ever called, it's a /compiler/ error,
  not a user error, so it seems acceptable that they cannot be caught.

One might wonder why absentError doesn't just call panic#.
For absent error we want to combine two parts, one static, one call site
dependent into one error message. While for absentSumFieldError it's a
static string.

The easiest way to combine the two parts for absentError is to use a
format string with `barf` in the RTS passing the *dynamic* part of the
error as argument. There is no need to do any of this for
absentSumFieldError as it's a static string there.

The alternatives would be to:
* Drop the call site specific information from absentError.
  The call site specific information is at times very helpful for debugging
  so I don't consider this an option.
* Remove the common prefix. We would then need to include the prefix
  in the call site specific string we pass to absentError. Increasing
  code size for no good reason.

Both of which seem worse than having an stg_absentError function specific to
absentError to me.
-}

-- `stg_panic#` never returns but it can't just return `State# RealWorld` so we
-- indicate that it returns `(# #)` too to make the compiler happy.
-- See Note [Compiler error functions]
foreign import prim "stg_paniczh" panic# :: Addr# -> State# RealWorld -> (# State# RealWorld, (# #) #)

-- See Note [Compiler error functions]
foreign import prim "stg_absentErrorzh" stg_absentError# :: Addr# -> State# RealWorld -> (# State# RealWorld, (# #) #)

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

-- | Displays "Oops!  Entered absent arg" ++ errormsg and exits the program.
{-# NOINLINE absentError #-}
absentError :: forall (a :: Type). Addr# -> a
absentError errmsg =
  runRW# (\s ->
    case stg_absentError# errmsg s of
      (# _, _ #) -> -- This bottom is unreachable but we can't
                    -- use an empty case lest the pattern match
                    -- checker squawks.
                    let x = x in x)

{-# NOINLINE absentConstraintError #-}
absentConstraintError :: forall (a :: Type). Addr# -> a
-- We want to give this the type
--    forall (a :: Constraint). Addr# -> a
-- but Haskell source code doesn't allow functions that return Constraint
-- So in this module we lie about the type.  This is fine because
-- absentConstraintError is a wired-in Id with the desired Constraint-kinded
-- type; the type in the interface file is never looked at.
-- The only purpose of this definition is to give a function to call,
-- and for that purpose, delegating to absentError is fine.
absentConstraintError errmsg = absentError errmsg
