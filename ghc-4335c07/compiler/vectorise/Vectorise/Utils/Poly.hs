-- |Auxiliary functions to vectorise type abstractions.

module Vectorise.Utils.Poly
  ( polyAbstract
  , polyApply
  , polyVApply
  , polyArity
  )
where

import GhcPrelude

import Vectorise.Vect
import Vectorise.Monad
import Vectorise.Utils.PADict
import CoreSyn
import Type
import FastString
import Control.Monad


-- Vectorisation of type arguments -------------------------------------------------------------

-- |Vectorise under the 'PA' dictionary variables corresponding to a set of type arguments.
--
-- The dictionary variables are new local variables that are entered into the local vectorisation
-- map.
--
-- The purpose of this function is to introduce the additional 'PA' dictionary arguments that are
-- needed when vectorising type abstractions.
--
polyAbstract :: [TyVar] -> ([Var] -> VM a) -> VM a
polyAbstract tvs p
  = localV
  $ do { mdicts <- mapM mk_dict_var tvs
       ; zipWithM_ (\tv -> maybe (defLocalTyVar tv)
                                 (defLocalTyVarWithPA tv . Var)) tvs mdicts
       ; p (mk_args mdicts)
       }
  where
    mk_dict_var tv
      = do { r <- paDictArgType tv
           ; case r of
               Just ty -> liftM Just (newLocalVar (fsLit "dPA") ty)
               Nothing -> return Nothing
           }

    mk_args mdicts = [dict | Just dict <- mdicts]

-- |Determine the number of 'PA' dictionary arguments required for a set of type variables (depends
-- on their kinds).
--
polyArity :: [TyVar] -> VM Int
polyArity tvs
  = do { tys <- mapM paDictArgType tvs
       ; return $ length [() | Just _ <- tys]
       }

-- |Apply a expression to its type arguments as well as 'PA' dictionaries for these type arguments.
--
polyApply :: CoreExpr -> [Type] -> VM CoreExpr
polyApply expr tys
 = do { dicts <- mapM paDictOfType tys
      ; return $ expr `mkTyApps` tys `mkApps` dicts
      }

-- |Apply a vectorised expression to a set of type arguments together with 'PA' dictionaries for
-- these type arguments.
--
polyVApply :: VExpr -> [Type] -> VM VExpr
polyVApply expr tys
 = do { dicts <- mapM paDictOfType tys
      ; return $ mapVect (\e -> e `mkTyApps` tys `mkApps` dicts) expr
      }
