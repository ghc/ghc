{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}

module T22478a where

import Data.Kind (Type, Constraint)
import GHC.Exts (Multiplicity(Many))
import qualified Language.Haskell.TH as TH

data P (a :: k) where
  MkP :: forall {k} (a :: k). P a

data T (a :: Type) = MkT

data V a b = MkV

data E = E :+ E | E :* E

infixl 6 :+
infixl 7 :*

fAppTy :: T (Maybe Int) -> (P Maybe, P Int)
fAppTy (MkT @(f a)) = (MkP @f, MkP @a)

fAppKindTy :: T (P @(Type -> Constraint) Num) -> (P Type, P Constraint, P Num)
fAppKindTy (MkT @(P @(k1 -> k2) c)) = (MkP @k1, MkP @k2, MkP @c)

fOpTy :: T (Either Int Bool) -> (P Either, P Int, P Bool)
fOpTy (MkT @(a `op` b)) = (MkP @op, MkP @a, MkP @b)

fQualTy :: T ((Ord a, Num a) => a) -> (P (Ord a), P (Num a), P a)
fQualTy (MkT @((c1, c2) => a)) = (MkP @c1, MkP @c2, MkP @a)

fFunTy :: T (Int -> Bool) -> (P Int, P Bool, P Many)
fFunTy (MkT @(a %(m :: Multiplicity) -> b)) = (MkP @a, MkP @b, MkP @m)

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

fForallTy :: T (forall a. a -> b) -> P b
fForallTy (MkT @(forall a. a -> b)) = MkP @b

fIParamTy :: T ((?t :: Type) => Int) -> P Type
fIParamTy (MkT @((?t :: k) => Int)) = MkP @k

fTyLit :: P "hello" -> ()
fTyLit (MkP @"hello") = ()

fDependentBind :: P (V k (a :: k)) -> V k a
fDependentBind (MkP @(V k (a :: k))) = MkV @k @a

fArityCheck :: P (a :* b :+ c :* d) -> ()
fArityCheck (MkP @(a :* b :+ c :* d)) = ()

fShadowing :: T a -> T b -> T b
fShadowing (MkT @a) = \(MkT @a) -> MkT @a

fSpliceTy :: T Int -> P Int
fSpliceTy (MkT @($(TH.varT (TH.mkName "t")))) = MkP @t

fKindSigTwoX :: P (Nothing @(a, a)) -> ()
fKindSigTwoX (MkP @(Nothing :: Maybe (t, t))) = ()  -- Accepted (multiple occurrences of ‘t’ notwithstanding)

fTypeSigTwoX (MkP :: P (a,a)) = ()                  -- Accepted (multiple occurrences of ‘a’ notwithstanding)
