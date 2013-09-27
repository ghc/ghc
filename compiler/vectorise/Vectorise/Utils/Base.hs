module Vectorise.Utils.Base 
  ( voidType
  , newLocalVVar

  , mkDataConTag, dataConTagZ
  , mkWrapType
  , mkClosureTypes
  , mkPReprType
  , mkPDataType, mkPDatasType
  , splitPrimTyCon
  , mkBuiltinCo

  , wrapNewTypeBodyOfWrap
  , unwrapNewTypeBodyOfWrap
  , wrapNewTypeBodyOfPDataWrap
  , unwrapNewTypeBodyOfPDataWrap
  , wrapNewTypeBodyOfPDatasWrap
  , unwrapNewTypeBodyOfPDatasWrap
  
  , pdataReprTyCon
  , pdataReprTyConExact
  , pdatasReprTyConExact
  , pdataUnwrapScrut
  
  , preprSynTyCon
) where

import Vectorise.Monad
import Vectorise.Vect
import Vectorise.Builtins

import CoreSyn
import CoreUtils
import FamInstEnv
import Coercion
import Type
import TyCon
import DataCon
import MkId
import DynFlags
import FastString

#include "HsVersions.h"

-- Simple Types ---------------------------------------------------------------

voidType :: VM Type
voidType = mkBuiltinTyConApp voidTyCon []


-- Name Generation ------------------------------------------------------------

newLocalVVar :: FastString -> Type -> VM VVar
newLocalVVar fs vty
  = do
      lty <- mkPDataType vty
      vv  <- newLocalVar fs vty
      lv  <- newLocalVar fs lty
      return (vv,lv)


-- Constructors ---------------------------------------------------------------

mkDataConTag :: DynFlags -> DataCon -> CoreExpr
mkDataConTag dflags = mkIntLitInt dflags . dataConTagZ

dataConTagZ :: DataCon -> Int
dataConTagZ con = dataConTag con - fIRST_TAG


-- Type Construction ----------------------------------------------------------

-- |Make an application of the 'Wrap' type constructor.
--
mkWrapType :: Type -> VM Type
mkWrapType ty = mkBuiltinTyConApp wrapTyCon [ty]

-- |Make an application of the closure type constructor.
--
mkClosureTypes :: [Type] -> Type -> VM Type
mkClosureTypes = mkBuiltinTyConApps closureTyCon

-- |Make an application of the 'PRepr' type constructor.
--
mkPReprType :: Type -> VM Type
mkPReprType ty = mkBuiltinTyConApp preprTyCon [ty]

-- | Make an appliction of the 'PData' tycon to some argument.
--
mkPDataType :: Type -> VM Type
mkPDataType ty = mkBuiltinTyConApp pdataTyCon [ty]

-- | Make an application of the 'PDatas' tycon to some argument.
--
mkPDatasType :: Type -> VM Type
mkPDatasType ty = mkBuiltinTyConApp pdatasTyCon [ty]

-- Make an application of a builtin type constructor to some arguments.
--
mkBuiltinTyConApp :: (Builtins -> TyCon) -> [Type] -> VM Type
mkBuiltinTyConApp get_tc tys
  = do { tc <- builtin get_tc
       ; return $ mkTyConApp tc tys
       }

-- Make a cascading application of a builtin type constructor.
--
mkBuiltinTyConApps :: (Builtins -> TyCon) -> [Type] -> Type -> VM Type
mkBuiltinTyConApps get_tc tys ty
 = do { tc <- builtin get_tc
      ; return $ foldr (mk tc) ty tys
      }
  where
    mk tc ty1 ty2 = mkTyConApp tc [ty1,ty2]


-- Type decomposition ---------------------------------------------------------

-- |Checks if a type constructor is defined in 'GHC.Prim' (e.g., 'Int#'); if so, returns it.
--
splitPrimTyCon :: Type -> Maybe TyCon
splitPrimTyCon ty
  | Just (tycon, []) <- splitTyConApp_maybe ty
  , isPrimTyCon tycon
  = Just tycon
  | otherwise = Nothing


-- Coercion Construction -----------------------------------------------------

-- |Make a representational coersion to some builtin type.
--
mkBuiltinCo :: (Builtins -> TyCon) -> VM Coercion
mkBuiltinCo get_tc
  = do { tc <- builtin get_tc
       ; return $ mkTyConAppCo Representational tc []
       }


-- Wrapping and unwrapping the 'Wrap' newtype ---------------------------------

-- |Apply the constructor wrapper of the 'Wrap' /newtype/.
--
wrapNewTypeBodyOfWrap :: CoreExpr -> Type -> VM CoreExpr
wrapNewTypeBodyOfWrap e ty
  = do { wrap_tc <- builtin wrapTyCon
       ; return $ wrapNewTypeBody wrap_tc [ty] e
       }

-- |Strip the constructor wrapper of the 'Wrap' /newtype/.
--
unwrapNewTypeBodyOfWrap :: CoreExpr -> Type -> VM CoreExpr
unwrapNewTypeBodyOfWrap e ty
  = do { wrap_tc <- builtin wrapTyCon
       ; return $ unwrapNewTypeBody wrap_tc [ty] e
       }

-- |Apply the constructor wrapper of the 'PData' /newtype/ instance of 'Wrap'.
--
wrapNewTypeBodyOfPDataWrap :: CoreExpr -> Type -> VM CoreExpr
wrapNewTypeBodyOfPDataWrap e ty
  = do { wrap_tc  <- builtin wrapTyCon
       ; pwrap_tc <- pdataReprTyConExact wrap_tc
       ; return $ wrapNewTypeBody pwrap_tc [ty] e
       }

-- |Strip the constructor wrapper of the 'PData' /newtype/ instance of 'Wrap'.
--
unwrapNewTypeBodyOfPDataWrap :: CoreExpr -> Type -> VM CoreExpr
unwrapNewTypeBodyOfPDataWrap e ty
  = do { wrap_tc  <- builtin wrapTyCon
       ; pwrap_tc <- pdataReprTyConExact wrap_tc
       ; return $ unwrapNewTypeBody pwrap_tc [ty] (unwrapFamInstScrut pwrap_tc [ty] e)
       }

-- |Apply the constructor wrapper of the 'PDatas' /newtype/ instance of 'Wrap'.
--
wrapNewTypeBodyOfPDatasWrap :: CoreExpr -> Type -> VM CoreExpr
wrapNewTypeBodyOfPDatasWrap e ty
  = do { wrap_tc  <- builtin wrapTyCon
       ; pwrap_tc <- pdatasReprTyConExact wrap_tc
       ; return $ wrapNewTypeBody pwrap_tc [ty] e
       }

-- |Strip the constructor wrapper of the 'PDatas' /newtype/ instance of 'Wrap'.
--
unwrapNewTypeBodyOfPDatasWrap :: CoreExpr -> Type -> VM CoreExpr
unwrapNewTypeBodyOfPDatasWrap e ty
  = do { wrap_tc  <- builtin wrapTyCon
       ; pwrap_tc <- pdatasReprTyConExact wrap_tc
       ; return $ unwrapNewTypeBody pwrap_tc [ty] (unwrapFamInstScrut pwrap_tc [ty] e)
       }


-- 'PData' representation types ----------------------------------------------

-- |Get the representation tycon of the 'PData' data family for a given type.
--
-- This tycon does not appear explicitly in the source program — see Note [PData TyCons] in
-- 'Vectorise.Generic.Description':
--
--   @pdataReprTyCon {Sum2} = {PDataSum2}@
--
-- The type for which we look up a 'PData' instance may be more specific than the type in the
-- instance declaration.  In that case the second component of the result will be more specific than
-- a set of distinct type variables.
-- 
pdataReprTyCon :: Type -> VM (TyCon, [Type])
pdataReprTyCon ty 
  = do 
    { FamInstMatch { fim_instance = famInst
                   , fim_tys      = tys } <- builtin pdataTyCon >>= (`lookupFamInst` [ty])
    ; return (dataFamInstRepTyCon famInst, tys)
    }

-- |Get the representation tycon of the 'PData' data family for a given type constructor.
--
-- For example, for a binary type constructor 'T', we determine the representation type constructor
-- for 'PData (T a b)'.
--
pdataReprTyConExact :: TyCon -> VM TyCon
pdataReprTyConExact tycon
  = do {   -- look up the representation tycon; if there is a match at all, it will be be exact
       ;   -- (i.e.,' _tys' will be distinct type variables)
       ; (ptycon, _tys) <- pdataReprTyCon (tycon `mkTyConApp` mkTyVarTys (tyConTyVars tycon))
       ; return ptycon
       }

-- |Get the representation tycon of the 'PDatas' data family for a given type constructor.
--
-- For example, for a binary type constructor 'T', we determine the representation type constructor
-- for 'PDatas (T a b)'.
--
pdatasReprTyConExact :: TyCon -> VM TyCon
pdatasReprTyConExact tycon
  = do {   -- look up the representation tycon; if there is a match at all, it will be be exact
       ; (FamInstMatch { fim_instance = ptycon }) <- pdatasReprTyCon (tycon `mkTyConApp` mkTyVarTys (tyConTyVars tycon))
       ; return $ dataFamInstRepTyCon ptycon
       }
  where
    pdatasReprTyCon ty = builtin pdatasTyCon >>= (`lookupFamInst` [ty])

-- |Unwrap a 'PData' representation scrutinee.
--
pdataUnwrapScrut :: VExpr -> VM (CoreExpr, CoreExpr, DataCon)
pdataUnwrapScrut (ve, le)
  = do { (tc, arg_tys) <- pdataReprTyCon ty
       ; let [dc] = tyConDataCons tc
       ; return (ve, unwrapFamInstScrut tc arg_tys le, dc)
       }
  where
    ty = exprType ve


-- 'PRepr' representation types ----------------------------------------------

-- |Get the representation tycon of the 'PRepr' type family for a given type.
--
preprSynTyCon :: Type -> VM FamInstMatch
preprSynTyCon ty = builtin preprTyCon >>= (`lookupFamInst` [ty])
