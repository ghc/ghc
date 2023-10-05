{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

Check for recursive type constructors.

-}



module GHC.Core.TyCon.RecWalk (

        -- * Recursion breaking
        RecTcChecker, initRecTc, defaultRecTcMaxBound,
        setRecTcMaxBound, checkRecTc

    ) where

import GHC.Prelude

import GHC.Core.TyCon
import GHC.Core.TyCon.Env
import GHC.Utils.Outputable

{-
************************************************************************
*                                                                      *
           Walking over recursive TyCons
*                                                                      *
************************************************************************

Note [Expanding newtypes and products]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When expanding a type to expose a data-type constructor, we need to be
careful about newtypes, lest we fall into an infinite loop. Here are
the key examples:

  newtype Id  x = MkId x
  newtype Fix f = MkFix (f (Fix f))
  newtype T     = MkT (T -> T)

  Type           Expansion
 --------------------------
  T              T -> T
  Fix Maybe      Maybe (Fix Maybe)
  Id (Id Int)    Int
  Fix Id         NO NO NO

Notice that
 * We can expand T, even though it's recursive.
 * We can expand Id (Id Int), even though the Id shows up
   twice at the outer level, because Id is non-recursive

So, when expanding, we keep track of when we've seen a recursive
newtype at outermost level; and bail out if we see it again.

We sometimes want to do the same for product types, so that the
strictness analyser doesn't unbox infinitely deeply.

More precisely, we keep a *count* of how many times we've seen it.
This is to account for
   data instance T (a,b) = MkT (T a) (T b)
Then (#10482) if we have a type like
        T (Int,(Int,(Int,(Int,Int))))
we can still unbox deeply enough during strictness analysis.
We have to treat T as potentially recursive, but it's still
good to be able to unwrap multiple layers.

The function that manages all this is checkRecTc.
-}

data RecTcChecker = RC !Int (TyConEnv Int)
  -- The upper bound, and the number of times
  -- we have encountered each TyCon

instance Outputable RecTcChecker where
  ppr (RC n env) = text "RC:" <> int n <+> ppr env

-- | Initialise a 'RecTcChecker' with 'defaultRecTcMaxBound'.
initRecTc :: RecTcChecker
initRecTc = RC defaultRecTcMaxBound emptyTyConEnv

-- | The default upper bound (100) for the number of times a 'RecTcChecker' is
-- allowed to encounter each 'TyCon'.
defaultRecTcMaxBound :: Int
defaultRecTcMaxBound = 100
-- Should we have a flag for this?

-- | Change the upper bound for the number of times a 'RecTcChecker' is allowed
-- to encounter each 'TyCon'.
setRecTcMaxBound :: Int -> RecTcChecker -> RecTcChecker
setRecTcMaxBound new_bound (RC _old_bound rec_nts) = RC new_bound rec_nts

checkRecTc :: RecTcChecker -> TyCon -> Maybe RecTcChecker
-- Nothing      => Recursion detected
-- Just rec_tcs => Keep going
checkRecTc (RC bound rec_nts) tc
  = case lookupTyConEnv rec_nts tc of
      Just n | n >= bound -> Nothing
             | otherwise  -> Just (RC bound (extendTyConEnv rec_nts tc (n+1)))
      Nothing             -> Just (RC bound (extendTyConEnv rec_nts tc 1))
