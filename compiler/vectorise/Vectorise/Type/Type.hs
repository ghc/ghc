
module Vectorise.Type.Type
	( vectTyCon
	, vectAndLiftType
	, vectType)
where
import Vectorise.Utils
import Vectorise.Monad
import Vectorise.Builtins
import TypeRep
import Type
import TyCon
import Var
import Outputable
import Control.Monad
import Data.List
import Data.Maybe


-- | Vectorise a type constructor.
vectTyCon :: TyCon -> VM TyCon
vectTyCon tc
  | isFunTyCon tc        = builtin closureTyCon
  | isBoxedTupleTyCon tc = return tc
  | isUnLiftedTyCon tc   = return tc
  | otherwise            
  = maybeCantVectoriseM "Tycon not vectorised: " (ppr tc)
	$ lookupTyCon tc


-- | Produce the vectorised and lifted versions of a type.
vectAndLiftType :: Type -> VM (Type, Type)
vectAndLiftType ty | Just ty' <- coreView ty = vectAndLiftType ty'
vectAndLiftType ty
  = do
      mdicts   <- mapM paDictArgType (reverse tyvars)
      let dicts = [dict | Just dict <- mdicts]
      vmono_ty <- vectType mono_ty
      lmono_ty <- mkPDataType vmono_ty
      return (abstractType tyvars dicts vmono_ty,
              abstractType tyvars dicts lmono_ty)
  where
    (tyvars, mono_ty) = splitForAllTys ty


-- | Vectorise a type.
vectType :: Type -> VM Type
vectType ty
	| Just ty'	<- coreView ty
	= vectType ty'
	
vectType (TyVarTy tv) 		= return $ TyVarTy tv
vectType (AppTy ty1 ty2) 	= liftM2 AppTy    (vectType ty1) (vectType ty2)
vectType (TyConApp tc tys) 	= liftM2 TyConApp (vectTyCon tc) (mapM vectType tys)
vectType (FunTy ty1 ty2)   	= liftM2 TyConApp (builtin closureTyCon)
                                             	  (mapM vectAndBoxType [ty1,ty2])

-- For each quantified var we need to add a PA dictionary out the front of the type.
-- So          forall a. C  a => a -> a   
-- turns into  forall a. Cv a => PA a => a :-> a
vectType ty@(ForAllTy _ _)
 = do
      -- split the type into the quantified vars, its dictionaries and the body.
      let (tyvars, tyBody)   = splitForAllTys ty
      let (tyArgs, tyResult) = splitFunTys    tyBody

      let (tyArgs_dict, tyArgs_regular) 
                  = partition isDictType tyArgs

      -- vectorise the body.
      let tyBody' = mkFunTys tyArgs_regular tyResult
      tyBody''    <- vectType tyBody'

      -- vectorise the dictionary parameters.
      dictsVect   <- mapM vectType tyArgs_dict

      -- make a PA dictionary for each of the type variables.
      dictsPA     <- liftM catMaybes $ mapM paDictArgType tyvars

      -- pack it all back together.
      traceVt "vect ForAllTy: " $ ppr (abstractType tyvars (dictsPA ++ dictsVect) tyBody'')
      return $ abstractType tyvars (dictsPA ++ dictsVect) tyBody''

vectType ty = cantVectorise "Can't vectorise type" (ppr ty)


-- | Add quantified vars and dictionary parameters to the front of a type.
abstractType :: [TyVar] -> [Type] -> Type -> Type
abstractType tyvars dicts = mkForAllTys tyvars . mkFunTys dicts


-- | Check if some type is a type class dictionary.
isDictType :: Type -> Bool
isDictType ty
 = case splitTyConApp_maybe ty of
	Just (tyCon, _)		-> isClassTyCon tyCon
	_			-> False


-- | Create the boxed version of a vectorised type.
vectAndBoxType :: Type -> VM Type
vectAndBoxType ty = vectType ty >>= boxType


-- | Create the boxed version of a type.
boxType :: Type -> VM Type
boxType ty
  | Just (tycon, []) <- splitTyConApp_maybe ty
  , isUnLiftedTyCon tycon
  = do
      r <- lookupBoxedTyCon tycon
      case r of
        Just tycon' -> return $ mkTyConApp tycon' []
        Nothing     -> return ty

  | otherwise	= return ty


