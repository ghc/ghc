-- Apply the vectorisation transformation to types. This is the \mathcal{L}_t scheme in HtM.

module Vectorise.Type.Type
  ( vectTyCon
  , vectAndLiftType
  , vectType
  ) 
where

import Vectorise.Utils
import Vectorise.Monad
import Vectorise.Builtins
import TcType
import Type
import TypeRep
import TyCon
import Control.Monad
import Control.Applicative
import Data.Maybe


-- |Vectorise a type constructor. Unless there is a vectorised version (stripped of embedded
-- parallel arrays), the vectorised version is the same as the original.
--
vectTyCon :: TyCon -> VM TyCon
vectTyCon tc = maybe tc id <$> lookupTyCon tc

-- |Produce the vectorised and lifted versions of a type.
--
-- NB: Here we are limited to properly handle predicates at the toplevel only.  Anything embedded
--     in what is called the 'body_ty' below will end up as an argument to the type family 'PData'.
--
vectAndLiftType :: Type -> VM (Type, Type)
vectAndLiftType ty | Just ty' <- coreView ty = vectAndLiftType ty'
vectAndLiftType ty
  = do { padicts  <- liftM catMaybes $ mapM paDictArgType tyvars
       ; vmono_ty <- vectType mono_ty
       ; lmono_ty <- mkPDataType vmono_ty
       ; return (abstractType tyvars (padicts ++ theta) vmono_ty,
                 abstractType tyvars (padicts ++ theta) lmono_ty)
       }
  where
    (tyvars, phiTy)  = splitForAllTys ty
    (theta, mono_ty) = tcSplitPhiTy phiTy 

-- |Vectorise a type.
--
-- For each quantified var we need to add a PA dictionary out the front of the type.
-- So          forall a.         C  a => a -> a   
-- turns into  forall a. PA a => Cv a => a :-> a
--
vectType :: Type -> VM Type
vectType ty
  | Just ty'  <- coreView ty
  = vectType ty'
vectType (TyVarTy tv)      = return $ TyVarTy tv
vectType (LitTy l)         = return $ LitTy l
vectType (AppTy ty1 ty2)   = AppTy <$> vectType ty1 <*> vectType ty2
vectType (TyConApp tc tys) = TyConApp <$> vectTyCon tc <*> mapM vectType tys
vectType (FunTy ty1 ty2)   
  | isPredTy ty1
  = FunTy <$> vectType ty1 <*> vectType ty2   -- don't build a closure for dictionary abstraction
  | otherwise
  = TyConApp <$> builtin closureTyCon <*> mapM vectType [ty1, ty2]
vectType ty@(ForAllTy _ _)
 = do {   -- strip off consecutive foralls
      ; let (tyvars, tyBody) = splitForAllTys ty

          -- vectorise the body
      ; vtyBody <- vectType tyBody

          -- make a PA dictionary for each of the type variables
      ; dictsPA <- liftM catMaybes $ mapM paDictArgType tyvars

          -- add the PA dictionaries after the foralls
      ; return $ abstractType tyvars dictsPA vtyBody
      }

-- |Add quantified vars and dictionary parameters to the front of a type.
--
abstractType :: [TyVar] -> [Type] -> Type -> Type
abstractType tyvars dicts = mkForAllTys tyvars . mkFunTys dicts
