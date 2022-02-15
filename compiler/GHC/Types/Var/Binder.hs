{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
module GHC.Types.Var.Binder
  ( VarBndr (..)
  , binderVar
  , binderVars
  , binderArgFlag
  , tyVarSpecToBinder
  , tyVarSpecToBinders
  , tyVarReqToBinder
  , tyVarReqToBinders
  , mapVarBndr
  , mapVarBndrs
  , lookupVarBndr
  ) where

import GHC.Prelude

import GHC.Types.Var.ArgFlag
import GHC.Utils.Outputable

import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Data
import qualified Data.Semigroup as S

{- Note [The VarBndr type and its uses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

See Haddocks below
-}

{- | Variable Binder

VarBndr is polymorphic in both var and visibility fields.
Currently there are nine different uses of 'VarBndr':

* Var.TyCoVarBinder = VarBndr TyCoVar ArgFlag
  Binder of a forall-type; see ForAllTy in GHC.Core.TyCo.Rep

* Var.TyVarBinder = VarBndr TyVar ArgFlag
  Subset of TyCoVarBinder when we are sure the binder is a TyVar

* Var.InvisTVBinder = VarBndr TyVar Specificity
  Specialised form of TyVarBinder, when ArgFlag = Invisible s
  See GHC.Core.Type.splitForAllInvisTVBinders

* Var.ReqTVBinder = VarBndr TyVar ()
  Specialised form of TyVarBinder, when ArgFlag = Required
  See GHC.Core.Type.splitForAllReqTVBinders
  This one is barely used

* TyCon.TyConBinder = VarBndr TyVar TyConBndrVis
  Binders of a TyCon; see TyCon in GHC.Core.TyCon

* TyCon.TyConTyCoBinder = VarBndr TyCoVar TyConBndrVis
  Binders of a PromotedDataCon
  See Note [Promoted GADT data constructors] in GHC.Core.TyCon

* IfaceType.IfaceForAllBndr     = VarBndr IfaceBndr ArgFlag
* IfaceType.IfaceForAllSpecBndr = VarBndr IfaceBndr Specificity
* IfaceType.IfaceTyConBinder    = VarBndr IfaceBndr TyConBndrVis
-}
data VarBndr var argf = Bndr var argf
  deriving ( Eq, Data
           , Functor, Foldable, Traversable
           )

binderVar :: VarBndr tv argf -> tv
binderVar (Bndr v _) = v

binderVars :: [VarBndr tv argf] -> [tv]
binderVars tvbs = map binderVar tvbs

binderArgFlag :: VarBndr tv argf -> argf
binderArgFlag (Bndr _ argf) = argf

tyVarSpecToBinders :: [VarBndr a Specificity] -> [VarBndr a ArgFlag]
tyVarSpecToBinders = map tyVarSpecToBinder

tyVarSpecToBinder :: VarBndr a Specificity -> VarBndr a ArgFlag
tyVarSpecToBinder (Bndr tv vis) = Bndr tv (Invisible vis)

tyVarReqToBinders :: [VarBndr a ()] -> [VarBndr a ArgFlag]
tyVarReqToBinders = map tyVarReqToBinder

tyVarReqToBinder :: VarBndr a () -> VarBndr a ArgFlag
tyVarReqToBinder (Bndr tv _) = Bndr tv Required

instance Bifunctor VarBndr where
  bimap f g (Bndr v a) = Bndr (f v) (g a)

instance Bifoldable VarBndr where
  bifoldMap f g (Bndr v a) = f v S.<> g a

instance Bitraversable VarBndr where
  bitraverse f g (Bndr v a) = Bndr <$> f v <*> g a

mapVarBndr :: (var -> var') -> (VarBndr var flag) -> (VarBndr var' flag)
mapVarBndr f (Bndr v fl) = Bndr (f v) fl

mapVarBndrs :: (var -> var') -> [VarBndr var flag] -> [VarBndr var' flag]
mapVarBndrs f = map (mapVarBndr f)

lookupVarBndr :: Eq var => var -> [VarBndr var flag] -> Maybe flag
lookupVarBndr var bndrs = lookup var zipped_bndrs
  where
    zipped_bndrs = map (\(Bndr v f) -> (v,f)) bndrs

instance Outputable tv => Outputable (VarBndr tv ArgFlag) where
  ppr (Bndr v Required)  = ppr v
  ppr (Bndr v Specified) = char '@' <> ppr v
  ppr (Bndr v Inferred)  = braces (ppr v)

instance Outputable tv => Outputable (VarBndr tv Specificity) where
  ppr = ppr . tyVarSpecToBinder
