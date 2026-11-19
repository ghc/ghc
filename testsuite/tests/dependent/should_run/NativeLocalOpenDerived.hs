{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DerivingStrategies, ExperimentalDependentTypes, StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures, TypeOperators #-}
module NativeLocalOpenDerived where

import Data.Kind (Type)
import Data.Type.Equality
import GHC.Generics (Generic)

type Result :: Type -> Type -> Type
type Result a b = NativeLocalOpenDerived.choose a b

type Later :: Type -> Type -> Type
type Later a b = NativeLocalOpenDerived.again a b
again :: a -> b -> Result a b
again _ y = y

choose x y = const y (earlyLabel x ++ show (Ticket Dusk))
earlyLabel _ = show 1
lateLabel = show 2

type ReportNumber = Double
default (ReportNumber)
data Phase = Noon | Dusk deriving (Eq, Ord, Show, Generic)
data Ticket = Ticket Phase
deriving instance Show Ticket

class Label a where
  label :: a -> String
  label _ = show Dusk
deriving anyclass instance Label Ticket

selected :: Result Char Bool :~: Bool
selected = Refl
