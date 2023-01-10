{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TemplateHaskell #-}

module T22478 where

import Data.Kind (Type, Constraint)
import GHC.Exts (Multiplicity(Many))
import qualified Language.Haskell.TH as TH

data P (a :: k) where
  MkP :: forall {k} (a :: k). P a

data T (a :: Type) = MkT

fAppTy :: T (Maybe Int) -> (P Maybe, P Int)
fAppTy (MkT @(f a)) = (MkP @f, MkP @a)

fAppKindTy :: T (P @(Type -> Constraint) Num) -> (P Type, P Constraint, P Num)
fAppKindTy (MkT @(P @(k1 -> k2) c)) = (MkP @k1, MkP @k2, MkP @c)

fOpTy :: T (Either Int Bool) -> (P Either, P Int, P Bool)
fOpTy (MkT @(a `op` b)) = (MkP @op, MkP @a, MkP @b)

fQualTy :: T ((Ord a, Num a) => a) -> (P (Ord a), P (Num a), P a)
fQualTy (MkT @((c1, c2) => a)) = (MkP @c1, MkP @c2, MkP @a)

fFunTy :: T (Int -> Bool) -> (P Int, P Bool, P Many)
fFunTy (MkT @(a %m -> b)) = (MkP @a, MkP @b, MkP @m)

fListTy :: T [Int] -> P Int
fListTy (MkT @[a]) = MkP @a

fTupleTy :: T (Int, Bool) -> (P Int, P Bool)
fTupleTy (MkT @(a, b)) = (MkP @a, MkP @b)

fSumTy :: P (# Int | Bool #) -> (P Int, P Bool)
fSumTy (MkP @(# a | b #)) = (MkP @a, MkP @b)

fExplicitListTy :: P '[Int, Bool] -> (P Int, P Bool)
fExplicitListTy (MkP @'[a, b]) = (MkP @a, MkP @b)

fExplicitTupleTy :: P '(Maybe, Functor) -> (P Maybe, P Functor)
fExplicitTupleTy (MkP @'(m, f)) = (MkP @m, MkP @f)

fKindSig :: P Maybe -> (P Maybe, P (Type -> Type))
fKindSig (MkP @(t :: k)) = (MkP @t, MkP @k)

-- TODO (int-index):
--
-- fForallTy :: T (forall a. a -> b) -> P b
-- fForallTy (MkT @(forall a. a -> b)) = MkP @b

fIParamTy :: T ((?t :: Type) => Int) -> P Type
fIParamTy (MkT @((?t :: k) => Int)) = MkP @k

fTyLit :: P "hello" -> ()
fTyLit (MkP @"hello") = ()

-- TODO (int-index):
--
-- fSpliceTy :: T Int -> P Int
-- fSpliceTy (MkT @($(TH.varT (TH.mkName "t")))) = MkP @t
--
--   Error:
--      • Not in scope: type variable ‘t’
--      • In the untyped splice: $(TH.varT (TH.mkName "t"))
--
--   But should it be an error? I don't think so.