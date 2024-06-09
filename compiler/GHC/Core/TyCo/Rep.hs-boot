{-# LANGUAGE NoPolyKinds #-}
module GHC.Core.TyCo.Rep where

import GHC.Utils.Outputable ( Outputable )
import Data.Data  ( Data )
import {-# SOURCE #-} GHC.Types.Var( Var, VarBndr, FunTyFlag )
import {-# SOURCE #-} GHC.Core.TyCon ( TyCon )
import Language.Haskell.Syntax.Specificity (ForAllTyFlag)

data Type
data Coercion
data FunSel
data CoSel
data UnivCoProvenance
data TyLit
data MCoercion

data Scaled a
scaledThing :: Scaled a -> a

type Mult = Type

type PredType = Type
type RuntimeRepType = Type
type Kind = Type
type ThetaType = [PredType]
type CoercionN = Coercion
type MCoercionN = MCoercion

mkForAllTy       :: VarBndr Var ForAllTyFlag -> Type -> Type
mkNakedTyConTy   :: TyCon -> Type
mkNakedFunTy     :: FunTyFlag -> Type -> Type -> Type


-- To support Data instances in GHC.Core.Coercion.Axiom
instance Data Type

-- To support instances PiTyBinder in Var
instance Data a => Data (Scaled a)

-- To support debug pretty-printing
instance Outputable Type
instance Outputable a => Outputable (Scaled a)
