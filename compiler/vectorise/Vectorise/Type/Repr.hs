
-- | Representation of Algebraic Data Types.
module Vectorise.Type.Repr
	( CompRepr	(..)
	, ProdRepr	(..)
	, ConRepr	(..)
	, SumRepr	(..)
	, tyConRepr
	, sumReprType
	, conReprType
	, prodReprType
	, compReprType
	, compOrigType)
where
import Vectorise.Utils
import Vectorise.Monad
import Vectorise.Builtins

import CoreSyn
import DataCon
import TyCon
import Type
import Control.Monad


data CompRepr = Keep Type
                     CoreExpr     -- PR dictionary for the type
              | Wrap Type

data ProdRepr = EmptyProd
              | UnaryProd CompRepr
              | Prod { repr_tup_tc   :: TyCon  -- representation tuple tycon
                     , repr_ptup_tc  :: TyCon  -- PData representation tycon
                     , repr_comp_tys :: [Type] -- representation types of
                     , repr_comps    :: [CompRepr]          -- components
                     }
data ConRepr  = ConRepr DataCon ProdRepr

data SumRepr  = EmptySum
              | UnarySum ConRepr
              | Sum  { repr_sum_tc   :: TyCon  -- representation sum tycon
                     , repr_psum_tc  :: TyCon  -- PData representation tycon
                     , repr_sel_ty   :: Type   -- type of selector
                     , repr_con_tys :: [Type]  -- representation types of
                     , repr_cons     :: [ConRepr]           -- components
                     }

tyConRepr :: TyCon -> VM SumRepr
tyConRepr tc = sum_repr (tyConDataCons tc)
  where
    sum_repr []    = return EmptySum
    sum_repr [con] = liftM UnarySum (con_repr con)
    sum_repr cons  = do
                       rs     <- mapM con_repr cons
                       sum_tc <- builtin (sumTyCon arity)
                       tys    <- mapM conReprType rs
                       (psum_tc, _) <- pdataReprTyCon (mkTyConApp sum_tc tys)
                       sel_ty <- builtin (selTy arity)
                       return $ Sum { repr_sum_tc  = sum_tc
                                    , repr_psum_tc = psum_tc
                                    , repr_sel_ty  = sel_ty
                                    , repr_con_tys = tys
                                    , repr_cons    = rs
                                    }
      where
        arity = length cons

    con_repr con = liftM (ConRepr con) (prod_repr (dataConRepArgTys con))

    prod_repr []   = return EmptyProd
    prod_repr [ty] = liftM UnaryProd (comp_repr ty)
    prod_repr tys  = do
                       rs <- mapM comp_repr tys
                       tup_tc <- builtin (prodTyCon arity)
                       tys'    <- mapM compReprType rs
                       (ptup_tc, _) <- pdataReprTyCon (mkTyConApp tup_tc tys')
                       return $ Prod { repr_tup_tc   = tup_tc
                                     , repr_ptup_tc  = ptup_tc
                                     , repr_comp_tys = tys'
                                     , repr_comps    = rs
                                     }
      where
        arity = length tys
    
    comp_repr ty = liftM (Keep ty) (prDictOfReprType ty)
                   `orElseV` return (Wrap ty)

sumReprType :: SumRepr -> VM Type
sumReprType EmptySum = voidType
sumReprType (UnarySum r) = conReprType r
sumReprType (Sum { repr_sum_tc  = sum_tc, repr_con_tys = tys })
  = return $ mkTyConApp sum_tc tys

conReprType :: ConRepr -> VM Type
conReprType (ConRepr _ r) = prodReprType r

prodReprType :: ProdRepr -> VM Type
prodReprType EmptyProd = voidType
prodReprType (UnaryProd r) = compReprType r
prodReprType (Prod { repr_tup_tc = tup_tc, repr_comp_tys = tys })
  = return $ mkTyConApp tup_tc tys

compReprType :: CompRepr -> VM Type
compReprType (Keep ty _) = return ty
compReprType (Wrap ty) = do
                             wrap_tc <- builtin wrapTyCon
                             return $ mkTyConApp wrap_tc [ty]

compOrigType :: CompRepr -> Type
compOrigType (Keep ty _) = ty
compOrigType (Wrap ty) = ty
