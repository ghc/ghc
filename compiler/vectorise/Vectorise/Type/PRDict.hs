
module Vectorise.Type.PRDict 
	(buildPRDict)
where
import Vectorise.Utils
import Vectorise.Monad
import Vectorise.Builtins
import Vectorise.Type.Repr
import CoreSyn
import CoreUtils
import TyCon
import Type
import Coercion



buildPRDict :: TyCon -> TyCon -> TyCon -> SumRepr -> VM CoreExpr
buildPRDict vect_tc prepr_tc _ r
  = do
      dict <- sum_dict r
      pr_co <- mkBuiltinCo prTyCon
      let co = mkAppCoercion pr_co
             . mkSymCoercion
             $ mkTyConApp arg_co ty_args
      return (mkCoerce co dict)
  where
    ty_args = mkTyVarTys (tyConTyVars vect_tc)
    Just arg_co = tyConFamilyCoercion_maybe prepr_tc

    sum_dict EmptySum = prDFunOfTyCon =<< builtin voidTyCon
    sum_dict (UnarySum r) = con_dict r
    sum_dict (Sum { repr_sum_tc  = sum_tc
                  , repr_con_tys = tys
                  , repr_cons    = cons
                  })
      = do
          dicts <- mapM con_dict cons
          dfun  <- prDFunOfTyCon sum_tc
          return $ dfun `mkTyApps` tys `mkApps` dicts

    con_dict (ConRepr _ r) = prod_dict r

    prod_dict EmptyProd = prDFunOfTyCon =<< builtin voidTyCon
    prod_dict (UnaryProd r) = comp_dict r
    prod_dict (Prod { repr_tup_tc   = tup_tc
                    , repr_comp_tys = tys
                    , repr_comps    = comps })
      = do
          dicts <- mapM comp_dict comps
          dfun <- prDFunOfTyCon tup_tc
          return $ dfun `mkTyApps` tys `mkApps` dicts

    comp_dict (Keep _ pr) = return pr
    comp_dict (Wrap ty)   = wrapPR ty


