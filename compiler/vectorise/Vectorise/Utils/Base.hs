
module Vectorise.Utils.Base (
	voidType,
	newLocalVVar,

	mkDataConTagLit,
	mkDataConTag, dataConTagZ,
	mkBuiltinTyConApp,
	mkBuiltinTyConApps,
	mkWrapType,
	mkClosureTypes,
	mkPReprType,
	mkPArrayType, splitPrimTyCon,
	mkPArray,
	mkPDataType,
	mkBuiltinCo,
	mkVScrut,
	
	pdataReprTyCon,
	pdataReprDataCon,
)
where
import Vectorise.Monad
import Vectorise.Vect
import Vectorise.Builtins

import CoreSyn
import CoreUtils
import Coercion
import Type
import TyCon
import DataCon
import MkId
import Literal
import Outputable
import FastString


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
mkDataConTagLit :: DataCon -> Literal
mkDataConTagLit = mkMachInt . toInteger . dataConTagZ


mkDataConTag :: DataCon -> CoreExpr
mkDataConTag = mkIntLitInt . dataConTagZ


dataConTagZ :: DataCon -> Int
dataConTagZ con = dataConTag con - fIRST_TAG


mkBuiltinTyConApp :: (Builtins -> TyCon) -> [Type] -> VM Type
mkBuiltinTyConApp get_tc tys
  = do
      tc <- builtin get_tc
      return $ mkTyConApp tc tys


mkBuiltinTyConApps :: (Builtins -> TyCon) -> [Type] -> Type -> VM Type
mkBuiltinTyConApps get_tc tys ty
  = do
      tc <- builtin get_tc
      return $ foldr (mk tc) ty tys
  where
    mk tc ty1 ty2 = mkTyConApp tc [ty1,ty2]


mkWrapType :: Type -> VM Type
mkWrapType ty = mkBuiltinTyConApp wrapTyCon [ty]


mkClosureTypes :: [Type] -> Type -> VM Type
mkClosureTypes = mkBuiltinTyConApps closureTyCon


mkPReprType :: Type -> VM Type
mkPReprType ty = mkBuiltinTyConApp preprTyCon [ty]


-----
mkPArrayType :: Type -> VM Type
mkPArrayType ty
  | Just tycon <- splitPrimTyCon ty
  = do
      r <- lookupPrimPArray tycon
      case r of
        Just arr -> return $ mkTyConApp arr []
        Nothing  -> cantVectorise "Primitive tycon not vectorised" (ppr tycon)

mkPArrayType ty = mkBuiltinTyConApp parrayTyCon [ty]

splitPrimTyCon :: Type -> Maybe TyCon
splitPrimTyCon ty
  | Just (tycon, []) <- splitTyConApp_maybe ty
  , isPrimTyCon tycon
  = Just tycon

  | otherwise = Nothing


------
mkPArray :: Type -> CoreExpr -> CoreExpr -> VM CoreExpr
mkPArray ty len dat = do
                        tc <- builtin parrayTyCon
                        let [dc] = tyConDataCons tc
                        return $ mkConApp dc [Type ty, len, dat]


mkPDataType :: Type -> VM Type
mkPDataType ty = mkBuiltinTyConApp pdataTyCon [ty]


mkBuiltinCo :: (Builtins -> TyCon) -> VM Coercion
mkBuiltinCo get_tc
  = do
      tc <- builtin get_tc
      return $ mkTyConApp tc []


mkVScrut :: VExpr -> VM (CoreExpr, CoreExpr, TyCon, [Type])
mkVScrut (ve, le)
  = do
      (tc, arg_tys) <- pdataReprTyCon ty
      return (ve, unwrapFamInstScrut tc arg_tys le, tc, arg_tys)
  where
    ty = exprType ve

pdataReprTyCon :: Type -> VM (TyCon, [Type])
pdataReprTyCon ty = builtin pdataTyCon >>= (`lookupFamInst` [ty])


pdataReprDataCon :: Type -> VM (DataCon, [Type])
pdataReprDataCon ty
  = do
      (tc, arg_tys) <- pdataReprTyCon ty
      let [dc] = tyConDataCons tc
      return (dc, arg_tys)


