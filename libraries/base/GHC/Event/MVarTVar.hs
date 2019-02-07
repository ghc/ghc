{-# language MagicHash #-}

module GHC.Event.CVar
  ( CVar(..)
  , mvar
  , tvar
  , match
  ) where

import GHC.MVar (MVar(..))
import GHC.TVar (TVar(..))

-- Behold. One of the crowl jewels of the February 2019 event manager
-- optimizations. This type is morally equivalent to:
--   Either (MVar Bool) (TVar Status)
-- Representationally, it is very close to:
--   (# MVar# RealWorld Bool | TVar# Status #)
-- However, it is able to be stored without using a tag to distinguish
-- between the two. Instead, it uses the info table when you pattern
-- match on it.

data Status = NotReady | Ready | Closed

-- | Mnemonic: A CVar is a concurrency variable. It is either
-- an MVar or a TVar
data CVar = CVar (Any :: TYPE 'UnliftedRep)

mvar :: MVar Bool -> CVar
mvar (MVar m) = CVar (unsafeCoerceUnlifted m)

tvar :: TVar Status -> CVar
tvar (TVar m) = CVar (unsafeCoerceUnlifted m)

match :: (MVar Bool -> a) -> (TVar Status -> a) -> CVar -> a
{-# INLINE match #-}
match f g (CVar m) = case getClosureType# m of
  41## -> g (TVar (unsafeCoerceUnlifted m))
  _ -> f (MVar (unsafeCoerceUnlifted m))

unsafeCoerceUnlifted ::
  forall (a :: TYPE 'UnliftedRep) (b :: TYPE 'UnliftedRep). a -> b
unsafeCoerceUnlifted = unsafeCoerce#

