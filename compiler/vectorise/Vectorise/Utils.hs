module Vectorise.Utils (
  module Vectorise.Utils.Base,
  module Vectorise.Utils.Closure,
  module Vectorise.Utils.Hoisting,
  module Vectorise.Utils.PADict,
  module Vectorise.Utils.Poly,

  -- * Annotated Exprs
  collectAnnTypeArgs,
  collectAnnDictArgs,
  collectAnnTypeBinders,
  collectAnnValBinders,
  isAnnTypeArg,

  -- * PD Functions
  replicatePD, emptyPD, packByTagPD,
  combinePD, liftPD,

  -- * Scalars
  isScalar, zipScalars, scalarClosure,

  -- * Naming
  newLocalVar
) where

import Vectorise.Utils.Base
import Vectorise.Utils.Closure
import Vectorise.Utils.Hoisting
import Vectorise.Utils.PADict
import Vectorise.Utils.Poly
import Vectorise.Monad
import Vectorise.Builtins
import CoreSyn
import CoreUtils
import Id
import Type
import Control.Monad


-- Annotated Exprs ------------------------------------------------------------

collectAnnTypeArgs :: AnnExpr b ann -> (AnnExpr b ann, [Type])
collectAnnTypeArgs expr = go expr []
  where
    go (_, AnnApp f (_, AnnType ty)) tys = go f (ty : tys)
    go e                             tys = (e, tys)

collectAnnDictArgs :: AnnExpr Var ann -> (AnnExpr Var ann, [AnnExpr Var ann])
collectAnnDictArgs expr = go expr []
  where
    go e@(_, AnnApp f arg) dicts 
      | isPredTy . exprType . deAnnotate $ arg = go f (arg : dicts)
      | otherwise                              = (e, dicts)
    go e                        dicts          = (e, dicts)

collectAnnTypeBinders :: AnnExpr Var ann -> ([Var], AnnExpr Var ann)
collectAnnTypeBinders expr = go [] expr
  where
    go bs (_, AnnLam b e) | isTyVar b = go (b : bs) e
    go bs e                           = (reverse bs, e)

-- |Collect all consecutive value binders that are not dictionaries.
--
collectAnnValBinders :: AnnExpr Var ann -> ([Var], AnnExpr Var ann)
collectAnnValBinders expr = go [] expr
  where
    go bs (_, AnnLam b e) | isId b 
                          && (not . isPredTy . idType $ b) = go (b : bs) e
    go bs e                                                = (reverse bs, e)

isAnnTypeArg :: AnnExpr b ann -> Bool
isAnnTypeArg (_, AnnType _) = True
isAnnTypeArg _              = False


-- PD "Parallel Data" Functions -----------------------------------------------
--
--   Given some data that has a PA dictionary, we can convert it to its 
--   representation type, perform some operation on the data, then convert it back.
--
--   In the DPH backend, the types of these functions are defined
--   in dph-common/D.A.P.Lifted/PArray.hs
--

-- |An empty array of the given type.
--
emptyPD :: Type -> VM CoreExpr
emptyPD = paMethod emptyPDVar emptyPD_PrimVar

-- |Produce an array containing copies of a given element.
--
replicatePD :: CoreExpr     -- ^ Number of copies in the resulting array.
            -> CoreExpr     -- ^ Value to replicate.
            -> VM CoreExpr
replicatePD len x 
  = liftM (`mkApps` [len,x])
        $ paMethod replicatePDVar replicatePD_PrimVar (exprType x)

-- |Select some elements from an array that correspond to a particular tag value and pack them into a new
-- array.
--
-- > packByTagPD Int# [:23, 42, 95, 50, 27, 49:]  3 [:1, 2, 1, 2, 3, 2:] 2 
-- >   ==> [:42, 50, 49:]
--
packByTagPD :: Type       -- ^ Element type.
            -> CoreExpr   -- ^ Source array.
            -> CoreExpr   -- ^ Length of resulting array.
            -> CoreExpr   -- ^ Tag values of elements in source array.
            -> CoreExpr   -- ^ The tag value for the elements to select.
            -> VM CoreExpr
packByTagPD ty xs len tags t
  = liftM (`mkApps` [xs, len, tags, t])
          (paMethod packByTagPDVar packByTagPD_PrimVar ty)

-- |Combine some arrays based on a selector.  The selector says which source array to choose for each
-- element of the resulting array.
--
combinePD :: Type         -- ^ Element type
          -> CoreExpr     -- ^ Length of resulting array
          -> CoreExpr     -- ^ Selector.
          -> [CoreExpr]   -- ^ Arrays to combine.
          -> VM CoreExpr
combinePD ty len sel xs
  = liftM (`mkApps` (len : sel : xs))
          (paMethod (combinePDVar n) (combinePD_PrimVar n) ty)
  where
    n = length xs

-- |Like `replicatePD` but use the lifting context in the vectoriser state.
--
liftPD :: CoreExpr -> VM CoreExpr
liftPD x
  = do
      lc <- builtin liftingContext
      replicatePD (Var lc) x


-- Scalars --------------------------------------------------------------------

isScalar :: Type -> VM Bool
isScalar ty
  = do
    { scalar <- builtin scalarClass
    ; existsInst scalar [ty]
    }

zipScalars :: [Type] -> Type -> VM CoreExpr
zipScalars arg_tys res_ty
  = do 
    { scalar <- builtin scalarClass
    ; (dfuns, _) <- mapAndUnzipM (\ty -> lookupInst scalar [ty]) ty_args
    ; zipf <- builtin (scalarZip $ length arg_tys)
    ; return $ Var zipf `mkTyApps` ty_args `mkApps` map Var dfuns
    }
    where
      ty_args = arg_tys ++ [res_ty]

scalarClosure :: [Type] -> Type -> CoreExpr -> CoreExpr -> VM CoreExpr
scalarClosure arg_tys res_ty scalar_fun array_fun
  = do
    { ctr <- builtin (closureCtrFun $ length arg_tys)
    ; pas <- mapM paDictOfType (init arg_tys)
    ; return $ Var ctr `mkTyApps` (arg_tys ++ [res_ty])
                       `mkApps`   (pas ++ [scalar_fun, array_fun])
    }
