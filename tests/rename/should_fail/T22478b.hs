{-# LANGUAGE TypeAbstractions #-}

module T22478b where

import Data.Kind (Type)

data P (a :: k) where
  MkP :: forall {k} (a :: k). P a

data T (a :: Type) = MkT

data V a b = MkV


fOutOfOrder :: P (V (a :: k) k) -> V k a
fOutOfOrder (MkP @(V (a :: k) k)) = MkV @k @a

fBangTy (MkP @(!Int)) = ()

fForAllTyScope :: T (forall a. a -> b) -> P b
fForAllTyScope (MkT @(forall a. a -> b)) = const @_ @a (MkP @b) undefined

fRecordTy (MkP @{fld :: Int}) = ()

fConflict1 (MkP @(V a a)) = ()
fConflict2 (x :: a) (MkP @a) = ()
fConflict3 (MkP @a) (MkP @a) = ()
fConflict4 (MkV @a @a) = ()
fConflict5 (MkV @_a @_a) = ()
