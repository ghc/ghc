{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1999
-}

-- | This module is separate from "TcTypeable" because the functions in this
-- module are used in "ClsInst", and importing "TcTypeable" from "ClsInst"
-- would lead to an import cycle.
module TcTypeableValidity (tyConIsTypeable, typeIsTypeable) where

import GhcPrelude

import TyCoRep
import TyCon
import Type

import Data.Maybe (isJust)

-- | Is a particular 'TyCon' representable by @Typeable@?. These exclude type
-- families and polytypes.
tyConIsTypeable :: TyCon -> Bool
tyConIsTypeable tc =
       isJust (tyConRepName_maybe tc)
    && typeIsTypeable (dropForAlls $ tyConKind tc)

-- | Is a particular 'Type' representable by @Typeable@? Here we look for
-- polytypes and types containing casts (which may be, for instance, a type
-- family).
typeIsTypeable :: Type -> Bool
-- We handle types of the form (TYPE LiftedRep) specifically to avoid
-- looping on (tyConIsTypeable RuntimeRep). We used to consider (TYPE rr)
-- to be typeable without inspecting rr, but this exhibits bad behavior
-- when rr is a type family.
typeIsTypeable ty
  | Just ty' <- coreView ty         = typeIsTypeable ty'
typeIsTypeable ty
  | isLiftedTypeKind ty             = True
typeIsTypeable (TyVarTy _)          = True
typeIsTypeable (AppTy a b)          = typeIsTypeable a && typeIsTypeable b
typeIsTypeable (FunTy a b)          = typeIsTypeable a && typeIsTypeable b
typeIsTypeable (TyConApp tc args)   = tyConIsTypeable tc
                                   && all typeIsTypeable args
typeIsTypeable (ForAllTy{})         = False
typeIsTypeable (LitTy _)            = True
typeIsTypeable (CastTy{})           = False
typeIsTypeable (CoercionTy{})       = False
