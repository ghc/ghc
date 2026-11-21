module PairCorePlugin (plugin) where

import Control.Monad (unless)
import GHC.Core.Class (mkClass)
import GHC.Core.Lint
import GHC.Core.Predicate (mkNomEqPred, mkReprEqPred)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Data.Bag (bagToList)
import GHC.Data.BooleanFormula (mkAnd)
import GHC.Driver.Config.Diagnostic (initDiagOpts)
import GHC.Plugins
import GHC.Tc.Utils.TcType (noConcreteTyVars)
import GHC.Types.Id.Make (mkDataConWorkId, vanillaDataConBoxer)
import GHC.Types.Var (mkCoVar)

plugin :: Plugin
plugin = defaultPlugin
  { installCoreToDos = \_ todos -> pure (CoreDoPluginPass "R11 scoped HEq prototype" check : todos)
  , pluginRecompile = purePlugin
  }

check :: ModGuts -> CoreM ModGuts
check guts = do
  dflags <- getDynFlags
  index <- case [tc | tc <- mg_tcs guts, getOccString tc == "Index"] of
    [tc] -> pure tc
    _ -> panic "The Transport fixture must define Index"
  let lintConfig = LintConfig
        { l_diagOpts = initDiagOpts dflags
        , l_platform = targetPlatform dflags
        , l_flags = LF
            { lf_check_global_ids = True
            , lf_check_inline_loop_breakers = True
            , lf_check_static_ptrs = RejectEverywhere
            , lf_report_unsat_syns = True
            , lf_check_linearity = True
            , lf_check_fixed_rep = True
            , lf_check_rubbish_lits = True
            , lf_allow_weak_joins = False
            , lf_allow_beta_joins = False
            , lf_allow_dead_occs = False
            , lf_tc_coercion = False
            }
        , l_vars = []
        }
  let modl = mg_module guts
  pairName <- freshExternal modl (mkTcOcc "HEqPair")
  dataName <- freshExternal modl (mkDataOcc "HEqPair#")
  workerName <- freshExternal modl (mkVarOcc "$WHEqPair")
  repName <- freshExternal modl (mkVarOcc "$tcHEqPair")
  promotedName <- freshExternal modl (mkVarOcc "$tc'HEqPair")
  let binders = tyConBinders heqTyCon
      tvs = binderVars binders
      [kTy, lTy, aTy, bTy] = mkTyVarTys tvs
      kindProp = mkNomEqPred kTy lTy
      valueProp ck = mkNomEqPred (mkCastTy aTy (mkCoVarCo ck)) bTy
      leftTy = mkTyConApp index [kTy, aTy]
      rightTy = mkTyConApp index [lTy, bTy]
      binder lbl ty = do
        unique <- getUniqueM
        pure (mkSysLocal (fsLit lbl) unique manyDataConTy ty)
  ckTemplate <- freshCoVar "ckTemplate" kindProp
  let dictTy = mkTyConApp pairTc (mkTyVarTys tvs)
      pairTc = mkClassTyCon pairName (mkTyConKind binders constraintKind) binders
                           [Nominal, Nominal, Nominal, Nominal] (mkDataTyConRhs [pairCon]) cls repName
      cls = mkClass pairName tvs [] [] [] [] [] (mkAnd []) pairTc
      pairCon = mkDataCon dataName False promotedName [] [] [NotMarkedStrict] []
                         tvs [ckTemplate] noConcreteTyVars (mkTyVarBinders Specified tvs)
                         [] [valueProp ckTemplate] [] dictTy NoPromInfo pairTc 1 []
                         workerId representation
      workerId = mkDataConWorkId workerName pairCon
      -- This model uses only the worker and its DataAlt. DCR builds the
      -- worker telescope, which NoDataConRep obtains from the wrapper.
      representation = DCR workerId vanillaDataConBoxer [mkScaled manyDataConTy (valueProp ckTemplate)]
  ckIn <- freshCoVar "ckIn" kindProp
  cvIn <- freshCoVar "cvIn" (valueProp ckIn)
  ck <- freshCoVar "ck" kindProp
  cv <- freshCoVar "cv" (valueProp ck)
  old <- freshCoVar "legacy" (mkNomEqPred aTy bTy)
  wrongRole <- freshCoVar "wrongRole" (mkReprEqPred kTy lTy)
  unrelated <- freshCoVar "unrelated" kindProp
  cvEscapes <- freshCoVar "cvEscapes" (valueProp unrelated)
  dictionary <- binder "dictionary" dictTy
  wild <- binder "wild" dictTy
  input <- binder "input" leftTy
  let worker = mkTyApps (Var (dataConWorkId pairCon)) (mkTyVarTys tvs)
      pack = mkApps worker [Coercion (mkCoVarCo ckIn), Coercion (mkCoVarCo cvIn)]
      castWith kindCo valueCo =
        let combined = mkTransCo (mkGReflCo Nominal aTy (MCo kindCo)) valueCo
        in Cast (Var input) (mkSubCo (mkTyConAppCo Nominal index [kindCo, combined]))
      work = castWith (mkCoVarCo ck) (mkCoVarCo cv)
      unpack scrut = Case scrut wild rightTy [Alt (DataAlt pairCon) [ck, cv] work]
      producer = mkLams (tvs ++ [ckIn, cvIn]) pack
      transport = mkLams (tvs ++ [dictionary, input]) (unpack (Var dictionary))
      roundTrip = mkLams (tvs ++ [ckIn, cvIn, input]) (unpack pack)
      oldPack = mkLams (tvs ++ [old]) (App worker (Coercion (mkCoVarCo old)))
      wrongRolePack = mkLams (tvs ++ [wrongRole]) (App worker (Coercion (mkCoVarCo wrongRole)))
      oldUnpack = mkLams (tvs ++ [dictionary, input]) $
        Case (Var dictionary) wild leftTy [Alt (DataAlt pairCon) [old] (Var input)]
      badScope = mkLams (tvs ++ [dictionary, input]) $
        Case (Var dictionary) wild rightTy
          [Alt (DataAlt pairCon) [ck, cvEscapes] (castWith (mkCoVarCo ck) (mkCoVarCo cvEscapes))]
  putMsg (text "Worker" <+> ppr (dataConRepType pairCon))
  putMsg (text "Transport" $$ ppr transport)
  unless (all isHomogeneousNominal [ckIn, cvIn, ck, cv]) $
    pprPanic "model created a heterogeneous coercion binder" (ppr [ckIn, cvIn, ck, cv])
  mapM_ (accepted lintConfig)
    [ ("produce pair", producer)
    , ("consume pair", transport)
    , ("round trip", roundTrip)
    ]
  mapM_ (rejected lintConfig)
    [ ("old single-field producer", oldPack)
    , ("old single-field consumer", oldUnpack)
    , ("escaped kind proof", badScope)
    , ("representational kind proof", wrongRolePack)
    ]
  pure guts

isHomogeneousNominal :: CoVar -> Bool
isHomogeneousNominal co =
  let (lhs, rhs, role) = coVarTypesRole co
  in role == Nominal && typeKind lhs `eqType` typeKind rhs

freshExternal :: Module -> OccName -> CoreM Name
freshExternal mdl occ = do
  unique <- getUniqueM
  pure (mkExternalName unique mdl occ noSrcSpan)

freshCoVar :: String -> Type -> CoreM CoVar
freshCoVar label typ = do
  unique <- getUniqueM
  pure (mkCoVar (mkSystemVarName unique (fsLit label)) typ)

accepted :: LintConfig -> (String, CoreExpr) -> CoreM ()
accepted config (label, expr) = case lintExpr config expr of
  Nothing -> putMsgS ("R11 accepted " ++ label)
  Just messages -> pprPanic ("R11 rejected " ++ label) (vcat (bagToList messages))

rejected :: LintConfig -> (String, CoreExpr) -> CoreM ()
rejected config (label, expr) = case lintExpr config expr of
  Nothing -> pprPanic ("R11 accepted " ++ label) (ppr expr)
  Just messages -> putMsg (text ("R11 rejected " ++ label) $$ vcat (bagToList messages))
