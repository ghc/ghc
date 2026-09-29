{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- |
-- Module      :  GHC.Internal.Unboxed.Do
-- Copyright   :  (c) The University of Glasgow 2026
-- License     :  see libraries/ghc-internal/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Do-notation for @State#@-passing code that binds unboxed values.
--
-- Import this module qualified and use it with @QualifiedDo@:
--
-- > {-# LANGUAGE QualifiedDo, MagicHash, UnboxedTuples #-}
-- > import qualified GHC.Internal.Unboxed.Do as Ubx
-- >
-- > sumTwo :: MutableByteArray# s -> State# s -> (# State# s, Word# #)
-- > sumTwo mba = Ubx.do
-- >    a <- readWordArray# mba 0#          -- a :: Word#
-- >    b <- readWordArray# mba 1#
-- >    writeWordArray# mba 2# (plusWord# a b) -- a unit action is a statement too
-- >    Ubx.pure (plusWord# a b)
--
-- A block has type @State# s -> o@ for any @o@: it can end with a unit
-- action (@State# s -> State# s@) or with an action that returns a value
-- (@State# s -> (# State# s, a #)@). Bound variables can have any
-- representation for which there is a 'Bind#' instance.
--
-- See #27944.

module GHC.Internal.Unboxed.Do
   ( ST#
   , Bind# (..)
   , Then# (..)
   , nop
   , void
   )
where

import GHC.Internal.Prim
import GHC.Internal.Types

-- | A @State#@-passing action that returns a value of any representation.
type ST# :: forall (r :: RuntimeRep). Type -> TYPE r -> Type
type ST# s a = State# s -> (# State# s, a #)

-- | Operations of 'ST#' actions whose result has representation @r@.
-- One instance per representation, so that the bound variable has a fixed one.
--
-- The continuation result @o@ is arbitrary, so that a block can end with a
-- unit action (@o = State# s@) or with an 'ST#' action.
class Bind# (r :: RuntimeRep) where
   (>>=) :: forall s (a :: TYPE r) {r'} (o :: TYPE r').
            ST# s a -> (a -> State# s -> o) -> State# s -> o

   -- | Sequence two actions, discard the result of the first one
   then# :: forall s (a :: TYPE r) {r'} (o :: TYPE r').
            ST# s a -> (State# s -> o) -> State# s -> o

   pure  :: forall s (a :: TYPE r). a -> ST# s a

-- | Sequencing. The discarded action @m@ is a unit action
-- (@State# s -> State# s@) or an 'ST#' action.
class Then# (m :: Type) (n :: Type) where
   (>>) :: m -> n -> n

infixl 1 >>=, >>

-- The `s ~ s'` contexts give instance improvement: the state type of the
-- continuation is inferred from the discarded action.
instance forall s s' r' (o :: TYPE r'). s ~ s'
      => Then# (State# s -> State# s) (State# s' -> o) where
   {-# INLINE (>>) #-}
   m >> k = \s -> case m s of s2 -> k s2

instance forall s s' r (a :: TYPE r) r' (o :: TYPE r'). (s ~ s', Bind# r)
      => Then# (State# s -> (# State# s, a #)) (State# s' -> o) where
   {-# INLINE (>>) #-}
   m >> k = m `then#` k

-- | Unit action that does nothing.
nop :: State# s -> State# s
nop s = s
{-# INLINE nop #-}

-- | Run an action and discard its result.
void :: forall s r (a :: TYPE r). Bind# r => ST# s a -> State# s -> State# s
void m = m `then#` nop
{-# INLINE void #-}

#define BIND_INSTANCE(rep) \
instance Bind# (rep) where { \
   {-# INLINE (>>=) #-} ; \
   m >>= k = \s -> case m s of { (# s2, a #) -> k a s2 } ; \
   {-# INLINE then# #-} ; \
   m `then#` k = \s -> case m s of { (# s2, _ #) -> k s2 } ; \
   {-# INLINE pure #-} ; \
   pure a = \s -> (# s, a #) }

-- Traditional CPP: no commas in macro arguments, hence the type synonyms.
type TupleRep_WW = TupleRep '[WordRep, WordRep]         -- (# Word#, Word# #)
type TupleRep_WI = TupleRep '[WordRep, IntRep]          -- (# Word#, Int# #)
type TupleRep_IW = TupleRep '[IntRep, WordRep]          -- (# Int#, Word# #)
type TupleRep_II = TupleRep '[IntRep, IntRep]           -- (# Int#, Int# #)
type TupleRep_UU = TupleRep '[UnliftedRep, UnliftedRep] -- (# ByteArray#, ByteArray# #)
type TupleRep_UW = TupleRep '[UnliftedRep, WordRep]     -- (# ByteArray#, Word# #)
type TupleRep_WU = TupleRep '[WordRep, UnliftedRep]     -- (# Word#, ByteArray# #)
type TupleRep_LL = TupleRep '[LiftedRep, LiftedRep]     -- (# a, b #)
type TupleRep_LLL = TupleRep '[LiftedRep, LiftedRep, LiftedRep] -- (# a, b, c #)
type SumRep_0U   = SumRep '[ZeroBitRep, UnliftedRep]    -- (# (# #) | ByteArray# #)

-- Only the representations used in ghc-internal. Extend as needed (#27944).
BIND_INSTANCE(IntRep)
BIND_INSTANCE(Int8Rep)
BIND_INSTANCE(Int16Rep)
BIND_INSTANCE(Int32Rep)
BIND_INSTANCE(Int64Rep)
BIND_INSTANCE(WordRep)
BIND_INSTANCE(Word8Rep)
BIND_INSTANCE(Word16Rep)
BIND_INSTANCE(Word32Rep)
BIND_INSTANCE(Word64Rep)
BIND_INSTANCE(AddrRep)
BIND_INSTANCE(FloatRep)
BIND_INSTANCE(DoubleRep)
BIND_INSTANCE(LiftedRep)
BIND_INSTANCE(UnliftedRep)
BIND_INSTANCE(ZeroBitRep)
BIND_INSTANCE(TupleRep_WW)
BIND_INSTANCE(TupleRep_WI)
BIND_INSTANCE(TupleRep_IW)
BIND_INSTANCE(TupleRep_II)
BIND_INSTANCE(TupleRep_UU)
BIND_INSTANCE(TupleRep_UW)
BIND_INSTANCE(TupleRep_WU)
BIND_INSTANCE(TupleRep_LL)
BIND_INSTANCE(TupleRep_LLL)
BIND_INSTANCE(SumRep_0U)
