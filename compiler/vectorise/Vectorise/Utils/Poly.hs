
module Vectorise.Utils.Poly (
	polyAbstract, 
	polyApply,
	polyVApply,
	polyArity
)
where
import Vectorise.Vect
import Vectorise.Monad
import Vectorise.Utils.PADict
import CoreSyn
import Type
import FastString
import Control.Monad


-- Poly Functions -------------------------------------------------------------
polyAbstract :: [TyVar] -> ([Var] -> VM a) -> VM a
polyAbstract tvs p
  = localV
  $ do
      mdicts <- mapM mk_dict_var tvs
      zipWithM_ (\tv -> maybe (defLocalTyVar tv)
                              (defLocalTyVarWithPA tv . Var)) tvs mdicts
      p (mk_args mdicts)
  where
    mk_dict_var tv = do
                       r <- paDictArgType tv
                       case r of
                         Just ty -> liftM Just (newLocalVar (fsLit "dPA") ty)
                         Nothing -> return Nothing

    mk_args mdicts = [dict | Just dict <- mdicts]


polyArity :: [TyVar] -> VM Int
polyArity tvs = do
                  tys <- mapM paDictArgType tvs
                  return $ length [() | Just _ <- tys]


polyApply :: CoreExpr -> [Type] -> VM CoreExpr
polyApply expr tys
 = do dicts <- mapM paDictOfType tys
      return $ expr `mkTyApps` tys `mkApps` dicts


polyVApply :: VExpr -> [Type] -> VM VExpr
polyVApply expr tys
 = do dicts <- mapM paDictOfType tys
      return     $ mapVect (\e -> e `mkTyApps` tys `mkApps` dicts) expr
