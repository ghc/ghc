{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
#if __GLASGOW_HASKELL__ >= 611
{-# OPTIONS_GHC -XNoMonoLocalBinds #-}
#endif
-- Roman likes local bindings
-- If this module lives on I'd like to get rid of this flag in due course

module Vectorise.Type.Env ( 
	vectTypeEnv,
)
where
import Vectorise.Env
import Vectorise.Vect
import Vectorise.Monad
import Vectorise.Builtins
import Vectorise.Type.TyConDecl
import Vectorise.Type.Classify
import Vectorise.Type.PADict
import Vectorise.Type.PData
import Vectorise.Type.PRepr
import Vectorise.Type.Repr
import Vectorise.Utils

import HscTypes
import CoreSyn
import CoreUtils
import CoreUnfold
import DataCon
import TyCon
import Type
import FamInstEnv
import OccName
import Id
import MkId
import Var
import NameEnv

import Unique
import UniqFM
import Util
import Outputable
import FastString
import MonadUtils
import Control.Monad
import Data.List

debug		= False
dtrace s x	= if debug then pprTrace "VectType" s x else x


-- | Vectorise a type environment.
--   The type environment contains all the type things defined in a module.
vectTypeEnv 
	:: TypeEnv
	-> VM ( TypeEnv			-- Vectorised type environment.
	      , [FamInst]		-- New type family instances.
	      , [(Var, CoreExpr)])	-- New top level bindings.
	
vectTypeEnv env
 = dtrace (ppr env)
 $ do
      cs <- readGEnv $ mk_map . global_tycons

      -- Split the list of TyCons into the ones we have to vectorise vs the
      -- ones we can pass through unchanged. We also pass through algebraic 
      -- types that use non Haskell98 features, as we don't handle those.
      let (conv_tcs, keep_tcs) = classifyTyCons cs groups
          keep_dcs             = concatMap tyConDataCons keep_tcs

      zipWithM_ defTyCon   keep_tcs keep_tcs
      zipWithM_ defDataCon keep_dcs keep_dcs

      new_tcs <- vectTyConDecls conv_tcs

      let orig_tcs = keep_tcs ++ conv_tcs

      -- We don't need to make new representation types for dictionary
      -- constructors. The constructors are always fully applied, and we don't 
      -- need to lift them to arrays as a dictionary of a particular type
      -- always has the same value.
      let vect_tcs = filter (not . isClassTyCon) 
                   $ keep_tcs ++ new_tcs

      (_, binds, inst_tcs) <- fixV $ \ ~(dfuns', _, _) ->
        do
          defTyConPAs (zipLazy vect_tcs dfuns')
          reprs     <- mapM tyConRepr vect_tcs
          repr_tcs  <- zipWith3M buildPReprTyCon orig_tcs vect_tcs reprs
          pdata_tcs <- zipWith3M buildPDataTyCon orig_tcs vect_tcs reprs

          dfuns     <- sequence 
                    $  zipWith5 buildTyConBindings
                               orig_tcs
                               vect_tcs
                               repr_tcs
                               pdata_tcs
                               reprs

          binds     <- takeHoisted
          return (dfuns, binds, repr_tcs ++ pdata_tcs)

      let all_new_tcs = new_tcs ++ inst_tcs

      let new_env = extendTypeEnvList env
                       (map ATyCon all_new_tcs
                        ++ [ADataCon dc | tc <- all_new_tcs
                                        , dc <- tyConDataCons tc])

      return (new_env, map mkLocalFamInst inst_tcs, binds)
  where
    tycons = typeEnvTyCons env
    groups = tyConGroups tycons

    mk_map env = listToUFM_Directly [(u, getUnique n /= u) | (u,n) <- nameEnvUniqueElts env]



buildTyConBindings :: TyCon -> TyCon -> TyCon -> TyCon -> SumRepr -> VM Var
buildTyConBindings orig_tc vect_tc prepr_tc pdata_tc repr
 = do vectDataConWorkers orig_tc vect_tc pdata_tc
      buildPADict vect_tc prepr_tc pdata_tc repr


vectDataConWorkers :: TyCon -> TyCon -> TyCon -> VM ()
vectDataConWorkers orig_tc vect_tc arr_tc
 = do bs <- sequence
          . zipWith3 def_worker  (tyConDataCons orig_tc) rep_tys
          $ zipWith4 mk_data_con (tyConDataCons vect_tc)
                                 rep_tys
                                 (inits rep_tys)
                                 (tail $ tails rep_tys)
      mapM_ (uncurry hoistBinding) bs
 where
    tyvars   = tyConTyVars vect_tc
    var_tys  = mkTyVarTys tyvars
    ty_args  = map Type var_tys
    res_ty   = mkTyConApp vect_tc var_tys

    cons     = tyConDataCons vect_tc
    arity    = length cons
    [arr_dc] = tyConDataCons arr_tc

    rep_tys  = map dataConRepArgTys $ tyConDataCons vect_tc


    mk_data_con con tys pre post
      = liftM2 (,) (vect_data_con con)
                   (lift_data_con tys pre post (mkDataConTag con))

    sel_replicate len tag
      | arity > 1 = do
                      rep <- builtin (selReplicate arity)
                      return [rep `mkApps` [len, tag]]

      | otherwise = return []

    vect_data_con con = return $ mkConApp con ty_args
    lift_data_con tys pre_tys post_tys tag
      = do
          len  <- builtin liftingContext
          args <- mapM (newLocalVar (fsLit "xs"))
                  =<< mapM mkPDataType tys

          sel  <- sel_replicate (Var len) tag

          pre   <- mapM emptyPD (concat pre_tys)
          post  <- mapM emptyPD (concat post_tys)

          return . mkLams (len : args)
                 . wrapFamInstBody arr_tc var_tys
                 . mkConApp arr_dc
                 $ ty_args ++ sel ++ pre ++ map Var args ++ post

    def_worker data_con arg_tys mk_body
      = do
          arity <- polyArity tyvars
          body <- closedV
                . inBind orig_worker
                . polyAbstract tyvars $ \args ->
                  liftM (mkLams (tyvars ++ args) . vectorised)
                $ buildClosures tyvars [] arg_tys res_ty mk_body

          raw_worker <- cloneId mkVectOcc orig_worker (exprType body)
          let vect_worker = raw_worker `setIdUnfolding`
                              mkInlineUnfolding (Just arity) body
          defGlobalVar orig_worker vect_worker
          return (vect_worker, body)
      where
        orig_worker = dataConWorkId data_con

