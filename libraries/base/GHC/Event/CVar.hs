{-# language MagicHash #-}
{-# language NoImplicitPrelude #-}
{-# language ExplicitForAll #-}
{-# language UnboxedSums #-}
{-# language DataKinds #-}
{-# language KindSignatures #-}
{-# language TypeApplications #-}

module GHC.Event.CVar
  ( CVar
  , Status(..)
  , mvar
  , tvar
  , match
  , close
  ) where

import GHC.Prim (MVar#,TVar#,RealWorld)
import GHC.Types (TYPE,RuntimeRep(UnliftedRep),Any,Bool(False),Type,IO)
import GHC.MVar (MVar(..),putMVar)
import GHC.Conc.Sync (TVar(..),atomically,writeTVar)

-- Behold. One of the crowl jewels of the February 2019 event manager
-- optimizations. This type is morally equivalent to:
--   Either (MVar Bool) (TVar Status)
-- Representationally, it is very close to:
--   (# MVar# RealWorld Bool | TVar# Status #)
-- However, it is able to be stored without using a tag to distinguish
-- between the two. Instead, it uses the info table when you pattern
-- match on it.

data Status = NotReady | Ready | Closed

-- -- | Mnemonic: A CVar is a concurrency variable. It is either
-- -- an MVar or a TVar
-- data CVar = CVar (Any :: TYPE 'UnliftedRep)
-- 
-- mvar :: MVar Bool -> CVar
-- mvar (MVar m) = CVar (unsafeCoerceUnlifted m)
-- 
-- tvar :: TVar Status -> CVar
-- tvar (TVar m) = CVar (unsafeCoerceUnlifted m)
-- 
-- match :: (MVar Bool -> a) -> (TVar Status -> a) -> CVar -> a
-- {-# INLINE match #-}
-- match f g (CVar m) = case getClosureType# (jankUnsafeCoerce m) of
--   41## -> g (TVar (unsafeCoerceUnlifted m))
--   _ -> f (MVar (unsafeCoerceUnlifted m))
-- 
-- -- TODO: Get rid of this once I make getClosureType# accept
-- -- unlifted boxed types instead of lifted boxed types.
-- jankUnsafeCoerce ::
--   forall (a :: TYPE 'UnliftedRep) (b :: Type). a -> b
-- jankUnsafeCoerce x = unsafeCoerce# @'UnliftedRep x
-- 
-- unsafeCoerceUnlifted ::
--   forall (a :: TYPE 'UnliftedRep) (b :: TYPE 'UnliftedRep). a -> b
-- unsafeCoerceUnlifted = unsafeCoerce#


data CVar = CVar (# MVar# RealWorld Bool | TVar# RealWorld Status #)

mvar :: MVar Bool -> CVar
mvar (MVar m) = CVar (# m | #)

tvar :: TVar Status -> CVar
tvar (TVar t) = CVar (# | t #)

match :: (MVar Bool -> a) -> (TVar Status -> a) -> CVar -> a
{-# INLINE match #-}
match f _ (CVar (# m | #)) = f (MVar m)
match _ g (CVar (# | t #)) = g (TVar t)

-- Notify the reading end of the CVar that the file descriptor they were
-- waiting on has been closed. Usually, the user handles this by throwing
-- an exception since it is an indicator of incorrect concurrent access
-- to a file descriptor.
close :: CVar -> IO ()
close = match
  (\mv -> putMVar mv False)
  (\tv -> atomically (writeTVar tv Closed))

