
module Vectorise.Generic.PADict
  ( buildPADict
  ) where

import Vectorise.Monad
import Vectorise.Builtins
import Vectorise.Generic.Description
import Vectorise.Generic.PAMethods ( buildPAScAndMethods )
import Vectorise.Utils

import BasicTypes
import CoreSyn
import CoreUtils
import CoreUnfold
import Module
import TyCon
import CoAxiom
import Type
import Id
import Var
import Name
import FastString


-- |Build the PA dictionary function for some type and hoist it to top level.
--
-- The PA dictionary holds fns that convert values to and from their vectorised representations.
--
-- @Recall the definition:
--    class PR (PRepr a) => PA a where
--      toPRepr      :: a -> PRepr a
--      fromPRepr    :: PRepr a -> a
--      toArrPRepr   :: PData a -> PData (PRepr a)
--      fromArrPRepr :: PData (PRepr a) -> PData a
--      toArrPReprs   :: PDatas a         -> PDatas (PRepr a)
--      fromArrPReprs :: PDatas (PRepr a) -> PDatas a
--
-- Example:
--    df :: forall a. PR (PRepr a) -> PA a -> PA (T a)
--    df = /\a. \(c:PR (PRepr a)) (d:PA a). MkPA c ($PR_df a d) ($toPRepr a d) ... 
--    $dPR_df :: forall a. PA a -> PR (PRepr (T a))
--    $dPR_df = ....   
--    $toRepr :: forall a. PA a -> T a -> PRepr (T a)
--    $toPRepr = ...
-- The "..." stuff is filled in by buildPAScAndMethods
-- @
--
buildPADict
        :: TyCon        -- ^ tycon of the type being vectorised.
        -> CoAxiom Unbranched
                        -- ^ Coercion between the type and 
                        --     its vectorised representation.
        -> TyCon        -- ^ PData  instance tycon
        -> TyCon        -- ^ PDatas instance tycon
        -> SumRepr      -- ^ representation used for the type being vectorised.
        -> VM Var       -- ^ name of the top-level dictionary function.

buildPADict vect_tc prepr_ax pdata_tc pdatas_tc repr
 = polyAbstract tvs $ \args ->    -- The args are the dictionaries we lambda abstract over; and they
                                  -- are put in the envt, so when we need a (PA a) we can find it in
                                  -- the envt; they don't include the silent superclass args yet
   do { mod <- liftDs getModule
      ; let dfun_name = mkLocalisedOccName mod mkPADFunOcc vect_tc_name
   
          -- The superclass dictionary is a (silent) argument if the tycon is polymorphic...
      ; let mk_super_ty = do { r <- mkPReprType inst_ty
                             ; pr_cls <- builtin prClass
                             ; return $ mkClassPred pr_cls [r]
                             }
      ; super_tys  <- sequence [mk_super_ty | not (null tvs)]
      ; super_args <- mapM (newLocalVar (fsLit "pr")) super_tys
      ; let val_args = super_args ++ args
            all_args = tvs ++ val_args
     
          -- ...it is constant otherwise
      ; super_consts <- sequence [prDictOfPReprInstTyCon inst_ty prepr_ax [] | null tvs]

          -- Get ids for each of the methods in the dictionary, including superclass
      ; paMethodBuilders <- buildPAScAndMethods
      ; method_ids       <- mapM (method val_args dfun_name) paMethodBuilders

          -- Expression to build the dictionary.
      ; pa_dc  <- builtin paDataCon
      ; let dict = mkLams all_args (mkConApp pa_dc con_args)
            con_args = Type inst_ty
                     : map Var super_args  -- the superclass dictionary is either 
                    ++ super_consts        -- lambda-bound or constant
                    ++ map (method_call val_args) method_ids

          -- Build the type of the dictionary function.
      ; pa_cls <- builtin paClass
      ; let dfun_ty = mkForAllTys tvs
                    $ mkFunTys (map varType val_args)
                               (mkClassPred pa_cls [inst_ty])

          -- Set the unfolding for the inliner.
      ; raw_dfun <- newExportedVar dfun_name dfun_ty
      ; let dfun_unf = mkDFunUnfolding all_args pa_dc con_args
            dfun = raw_dfun `setIdUnfolding`  dfun_unf
                            `setInlinePragma` dfunInlinePragma

          -- Add the new binding to the top-level environment.
      ; hoistBinding dfun dict
      ; return dfun
      }
  where
    tvs          = tyConTyVars vect_tc
    arg_tys      = mkTyVarTys tvs
    inst_ty      = mkTyConApp vect_tc arg_tys
    vect_tc_name = getName vect_tc

    method args dfun_name (name, build)
     = localV
     $ do  expr     <- build vect_tc prepr_ax pdata_tc pdatas_tc repr
           let body = mkLams (tvs ++ args) expr
           raw_var  <- newExportedVar (method_name dfun_name name) (exprType body)
           let var  = raw_var
                      `setIdUnfolding` mkInlineUnfolding (Just (length args)) body
                      `setInlinePragma` alwaysInlinePragma
           hoistBinding var body
           return var

    method_call args id        = mkApps (Var id) (map Type arg_tys ++ map Var args)
    method_name dfun_name name = mkVarOcc $ occNameString dfun_name ++ ('$' : name)
