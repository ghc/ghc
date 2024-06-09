{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- -----------------------------------------------------------------------------
--
-- (c) The University of Glasgow, 2005-2007
--
-- Running statements interactively
--
-- -----------------------------------------------------------------------------

module GHC.Runtime.Eval (
        Resume(..), History(..),
        execStmt, execStmt', ExecOptions(..), execOptions, ExecResult(..), resumeExec,
        runDecls, runDeclsWithLocation, runParsedDecls,
        parseImportDecl, SingleStep(..),
        abandon, abandonAll,
        getResumeContext,
        getHistorySpan,
        getModBreaks,
        getHistoryModule,
        setupBreakpoint,
        back, forward,
        setContext, getContext,
        mkTopLevEnv,
        getNamesInScope,
        getRdrNamesInScope,
        moduleIsInterpreted,
        getInfo,
        exprType,
        typeKind,
        parseName,
        parseInstanceHead,
        getInstancesForType,
        getDocs,
        GetDocsFailure(..),
        showModule,
        moduleIsBootOrNotObjectLinkable,
        parseExpr, compileParsedExpr,
        compileExpr, dynCompileExpr,
        compileExprRemote, compileParsedExprRemote,
        Term(..), obtainTermFromId, obtainTermFromVal, reconstructType
        ) where

import GHC.Prelude

import GHC.Driver.Monad
import GHC.Driver.Main
import GHC.Driver.Errors.Types ( hoistTcRnMessage )
import GHC.Driver.Env
import GHC.Driver.DynFlags
import GHC.Driver.Ppr
import GHC.Driver.Config

import GHC.Rename.Names (importsFromIface)

import GHC.Runtime.Eval.Types
import GHC.Runtime.Interpreter as GHCi
import GHC.Runtime.Heap.Inspect
import GHC.Runtime.Context
import GHCi.Message
import GHCi.RemoteTypes
import GHC.ByteCode.Types

import GHC.Linker.Loader as Loader

import GHC.Hs

import GHC.Core.Class (classTyCon)
import GHC.Core.FamInstEnv ( FamInst, orphNamesOfFamInst )
import GHC.Core.InstEnv
import GHC.Core.Predicate
import GHC.Core.TyCo.Ppr
import GHC.Core.TyCon
import GHC.Core.Type       hiding( typeKind )
import qualified GHC.Core.Type as Type

import GHC.Iface.Env       ( newInteractiveBinder )
import GHC.Iface.Load      ( loadSrcInterface )
import GHC.Tc.Utils.TcType
import GHC.Tc.Types.Constraint
import GHC.Tc.Types.Origin

import GHC.Builtin.Names ( toDynName )
import GHC.Builtin.Types ( pretendNameIsInScope )

import GHC.Data.Maybe
import GHC.Data.FastString
import GHC.Data.Bag

import GHC.Utils.Error
import GHC.Utils.Exception
import GHC.Utils.Logger
import GHC.Utils.Misc
import GHC.Utils.Monad
import GHC.Utils.Outputable
import GHC.Utils.Panic

import GHC.Types.RepType
import GHC.Types.Fixity.Env
import GHC.Types.Var
import GHC.Types.Id as Id
import GHC.Types.Name      hiding ( varName )
import GHC.Types.Name.Set
import GHC.Types.Name.Reader
import GHC.Types.Var.Env
import GHC.Types.SrcLoc
import GHC.Types.Unique
import GHC.Types.Unique.Supply
import GHC.Types.Unique.DSet
import GHC.Types.TyThing
import GHC.Types.Breakpoint
import GHC.Types.Unique.Map

import GHC.Unit
import GHC.Unit.Module.Graph
import GHC.Unit.Module.ModIface
import GHC.Unit.Module.ModSummary
import GHC.Unit.Home.ModInfo

import GHC.Tc.Module ( runTcInteractive, tcRnType, loadUnqualIfaces )
import GHC.Tc.Solver (simplifyWantedsTcM)
import GHC.Tc.Utils.Env (tcGetInstEnvs, lookupGlobal)
import GHC.Tc.Utils.Instantiate (instDFunType)
import GHC.Tc.Utils.Monad
import GHC.Tc.Zonk.Env ( ZonkFlexi (SkolemiseFlexi) )

import GHC.Unit.Env
import GHC.IfaceToCore

import Control.Monad
import Control.Monad.Catch as MC
import Data.Array
import Data.Dynamic
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (find,intercalate)
import Data.List.NonEmpty (NonEmpty)
import System.Directory
import Unsafe.Coerce ( unsafeCoerce )

-- -----------------------------------------------------------------------------
-- running a statement interactively

getResumeContext :: GhcMonad m => m [Resume]
getResumeContext = withSession (return . ic_resume . hsc_IC)

mkHistory :: HscEnv -> ForeignHValue -> InternalBreakpointId -> History
mkHistory hsc_env hval ibi = History hval ibi (findEnclosingDecls hsc_env ibi)

getHistoryModule :: History -> Module
getHistoryModule = ibi_tick_mod . historyBreakpointId

getHistorySpan :: HscEnv -> History -> SrcSpan
getHistorySpan hsc_env hist =
  let ibi = historyBreakpointId hist in
  case lookupHugByModule (ibi_tick_mod ibi) (hsc_HUG hsc_env) of
    Just hmi -> modBreaks_locs (getModBreaks hmi) ! ibi_tick_index ibi
    _ -> panic "getHistorySpan"

{- | Finds the enclosing top level function name -}
-- ToDo: a better way to do this would be to keep hold of the decl_path computed
-- by the coverage pass, which gives the list of lexically-enclosing bindings
-- for each tick.
findEnclosingDecls :: HscEnv -> InternalBreakpointId -> [String]
findEnclosingDecls hsc_env ibi =
   let hmi = expectJust "findEnclosingDecls" $ lookupHugByModule (ibi_tick_mod ibi) (hsc_HUG hsc_env)
   in modBreaks_decls (getModBreaks hmi) ! ibi_tick_index ibi

-- | Update fixity environment in the current interactive context.
updateFixityEnv :: GhcMonad m => FixityEnv -> m ()
updateFixityEnv fix_env = do
  hsc_env <- getSession
  let ic = hsc_IC hsc_env
  setSession $ hsc_env { hsc_IC = ic { ic_fix_env = fix_env } }

-- -----------------------------------------------------------------------------
-- execStmt

-- | default ExecOptions
execOptions :: ExecOptions
execOptions = ExecOptions
  { execSingleStep = RunToCompletion
  , execSourceFile = "<interactive>"
  , execLineNumber = 1
  , execWrap = EvalThis -- just run the statement, don't wrap it in anything
  }

-- | Run a statement in the current interactive context.
execStmt
  :: GhcMonad m
  => String             -- ^ a statement (bind or expression)
  -> ExecOptions
  -> m ExecResult
execStmt input exec_opts@ExecOptions{..} = do
    hsc_env <- getSession

    mb_stmt <-
      liftIO $
      runInteractiveHsc hsc_env $
      hscParseStmtWithLocation execSourceFile execLineNumber input

    case mb_stmt of
      -- empty statement / comment
      Nothing -> return (ExecComplete (Right []) 0)
      Just stmt -> execStmt' stmt input exec_opts

-- | Like `execStmt`, but takes a parsed statement as argument. Useful when
-- doing preprocessing on the AST before execution, e.g. in GHCi (see
-- GHCi.UI.runStmt).
execStmt' :: GhcMonad m => GhciLStmt GhcPs -> String -> ExecOptions -> m ExecResult
execStmt' stmt stmt_text ExecOptions{..} = do
    hsc_env <- getSession
    let interp = hscInterp hsc_env

    -- Turn off -fwarn-unused-local-binds when running a statement, to hide
    -- warnings about the implicit bindings we introduce.
    let ic       = hsc_IC hsc_env -- use the interactive dflags
        idflags' = ic_dflags ic `wopt_unset` Opt_WarnUnusedLocalBinds
        hsc_env' = mkInteractiveHscEnv (hsc_env{ hsc_IC = ic{ ic_dflags = idflags' }})

    r <- liftIO $ hscParsedStmt hsc_env' stmt

    case r of
      Nothing ->
        -- empty statement / comment
        return (ExecComplete (Right []) 0)
      Just (ids, hval, fix_env) -> do
        updateFixityEnv fix_env

        status <-
          withVirtualCWD $
            liftIO $ do
              let eval_opts = initEvalOpts idflags' (isStep execSingleStep)
              evalStmt interp eval_opts (execWrap hval)

        let ic = hsc_IC hsc_env
            bindings = (ic_tythings ic, ic_gre_cache ic)

            size = ghciHistSize idflags'

        handleRunStatus execSingleStep stmt_text bindings ids
                        status (emptyHistory size)

runDecls :: GhcMonad m => String -> m [Name]
runDecls = runDeclsWithLocation "<interactive>" 1

-- | Run some declarations and return any user-visible names that were brought
-- into scope.
runDeclsWithLocation :: GhcMonad m => String -> Int -> String -> m [Name]
runDeclsWithLocation source line_num input = do
    hsc_env <- getSession
    decls <- liftIO (hscParseDeclsWithLocation hsc_env source line_num input)
    runParsedDecls decls

-- | Like `runDeclsWithLocation`, but takes parsed declarations as argument.
-- Useful when doing preprocessing on the AST before execution, e.g. in GHCi
-- (see GHCi.UI.runStmt).
runParsedDecls :: GhcMonad m => [LHsDecl GhcPs] -> m [Name]
runParsedDecls decls = do
    hsc_env <- getSession
    (tyThings, ic) <- liftIO (hscParsedDecls hsc_env decls)

    setSession $ hsc_env { hsc_IC = ic }
    hsc_env <- getSession
    hsc_env' <- liftIO $ rttiEnvironment hsc_env
    setSession hsc_env'
    return $ filter (not . isDerivedOccName . nameOccName)
             -- For this filter, see Note [What to show to users]
           $ map getName tyThings

{- Note [What to show to users]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We don't want to display internally-generated bindings to users.
Things like the coercion axiom for newtypes. These bindings all get
OccNames that users can't write, to avoid the possibility of name
clashes (in linker symbols).  That gives a convenient way to suppress
them. The relevant predicate is OccName.isDerivedOccName.
See #11051 for more background and examples.
-}

withVirtualCWD :: GhcMonad m => m a -> m a
withVirtualCWD m = do
  hsc_env <- getSession

    -- a virtual CWD is only necessary when we're running interpreted code in
    -- the same process as the compiler.
  case interpInstance <$> hsc_interp hsc_env of
    Just (ExternalInterp {}) -> m
    _ -> do
      let ic = hsc_IC hsc_env
      let set_cwd = do
            dir <- liftIO $ getCurrentDirectory
            case ic_cwd ic of
               Just dir -> liftIO $ setCurrentDirectory dir
               Nothing  -> return ()
            return dir

          reset_cwd orig_dir = do
            virt_dir <- liftIO $ getCurrentDirectory
            hsc_env <- getSession
            let old_IC = hsc_IC hsc_env
            setSession hsc_env{  hsc_IC = old_IC{ ic_cwd = Just virt_dir } }
            liftIO $ setCurrentDirectory orig_dir

      MC.bracket set_cwd reset_cwd $ \_ -> m

parseImportDecl :: GhcMonad m => String -> m (ImportDecl GhcPs)
parseImportDecl expr = withSession $ \hsc_env -> liftIO $ hscImport hsc_env expr

emptyHistory :: Int -> BoundedList History
emptyHistory size = nilBL size

handleRunStatus :: GhcMonad m
                => SingleStep -> String
                -> ResumeBindings
                -> [Id]
                -> EvalStatus_ [ForeignHValue] [HValueRef]
                -> BoundedList History
                -> m ExecResult

handleRunStatus step expr bindings final_ids status history
  | RunAndLogSteps <- step = tracing
  | otherwise              = not_tracing
 where
  tracing
    | EvalBreak apStack_ref (Just eval_break) resume_ctxt _ccs <- status
    = do
       hsc_env <- getSession
       let interp = hscInterp hsc_env
       let dflags = hsc_dflags hsc_env
       let ibi = evalBreakpointToId (hsc_HPT hsc_env) eval_break
       let hmi = expectJust "handleRunStatus" $ lookupHpt (hsc_HPT hsc_env) (moduleName (ibi_tick_mod ibi))
           breaks = getModBreaks hmi

       b <- liftIO $
              breakpointStatus interp (modBreaks_flags breaks) (ibi_tick_index ibi)
       if b
         then not_tracing
           -- This breakpoint is explicitly enabled; we want to stop
           -- instead of just logging it.
         else do
           apStack_fhv <- liftIO $ mkFinalizedHValue interp apStack_ref
           let !history' = mkHistory hsc_env apStack_fhv ibi `consBL` history
                 -- history is strict, otherwise our BoundedList is pointless.
           fhv <- liftIO $ mkFinalizedHValue interp resume_ctxt
           let eval_opts = initEvalOpts dflags True
           status <- liftIO $ GHCi.resumeStmt interp eval_opts fhv
           handleRunStatus RunAndLogSteps expr bindings final_ids
                           status history'
    | otherwise
    = not_tracing

  not_tracing
    -- Hit a breakpoint
    | EvalBreak apStack_ref maybe_break resume_ctxt ccs <- status
    = do
         hsc_env <- getSession
         let interp = hscInterp hsc_env
         resume_ctxt_fhv <- liftIO $ mkFinalizedHValue interp resume_ctxt
         apStack_fhv <- liftIO $ mkFinalizedHValue interp apStack_ref
         let ibi = evalBreakpointToId (hsc_HPT hsc_env) <$> maybe_break
         (hsc_env1, names, span, decl) <- liftIO $
           bindLocalsAtBreakpoint hsc_env apStack_fhv ibi
         let
           resume = Resume
             { resumeStmt = expr
             , resumeContext = resume_ctxt_fhv
             , resumeBindings = bindings
             , resumeFinalIds = final_ids
             , resumeApStack = apStack_fhv
             , resumeBreakpointId = ibi
             , resumeSpan = span
             , resumeHistory = toListBL history
             , resumeDecl = decl
             , resumeCCS = ccs
             , resumeHistoryIx = 0
             }
           hsc_env2 = pushResume hsc_env1 resume

         setSession hsc_env2
         return (ExecBreak names ibi)

    -- Completed successfully
    | EvalComplete allocs (EvalSuccess hvals) <- status
    = do hsc_env <- getSession
         let final_ic = extendInteractiveContextWithIds (hsc_IC hsc_env) final_ids
             final_names = map getName final_ids
             interp = hscInterp hsc_env
         liftIO $ Loader.extendLoadedEnv interp (zip final_names hvals)
         hsc_env' <- liftIO $ rttiEnvironment hsc_env{hsc_IC=final_ic}
         setSession hsc_env'
         return (ExecComplete (Right final_names) allocs)

    -- Completed with an exception
    | EvalComplete alloc (EvalException e) <- status
    = return (ExecComplete (Left (fromSerializableException e)) alloc)


resumeExec :: GhcMonad m => (SrcSpan->Bool) -> SingleStep -> Maybe Int
           -> m ExecResult
resumeExec canLogSpan step mbCnt
 = do
   hsc_env <- getSession
   let ic = hsc_IC hsc_env
       resume = ic_resume ic

   case resume of
     [] -> liftIO $
           throwGhcExceptionIO (ProgramError "not stopped at a breakpoint")
     (r:rs) -> do
        -- unbind the temporary locals by restoring the TypeEnv from
        -- before the breakpoint, and drop this Resume from the
        -- InteractiveContext.
        let (resume_tmp_te,resume_gre_cache) = resumeBindings r
            ic' = ic { ic_tythings = resume_tmp_te,
                       ic_gre_cache = resume_gre_cache,
                       ic_resume   = rs }
        setSession hsc_env{ hsc_IC = ic' }

        -- remove any bindings created since the breakpoint from the
        -- linker's environment
        let old_names = map getName resume_tmp_te
            new_names = [ n | thing <- ic_tythings ic
                            , let n = getName thing
                            , not (n `elem` old_names) ]
            interp    = hscInterp hsc_env
            dflags    = hsc_dflags hsc_env
        liftIO $ Loader.deleteFromLoadedEnv interp new_names

        case r of
          Resume { resumeStmt = expr
                 , resumeContext = fhv
                 , resumeBindings = bindings
                 , resumeFinalIds = final_ids
                 , resumeApStack = apStack
                 , resumeBreakpointId = mb_brkpt
                 , resumeSpan = span
                 , resumeHistory = hist } ->
               withVirtualCWD $ do
                -- When the user specified a break ignore count, set it
                -- in the interpreter
                case (mb_brkpt, mbCnt) of
                  (Just brkpt, Just cnt) -> setupBreakpoint hsc_env (toBreakpointId brkpt) cnt
                  _ -> return ()

                let eval_opts = initEvalOpts dflags (isStep step)
                status <- liftIO $ GHCi.resumeStmt interp eval_opts fhv
                let prevHistoryLst = fromListBL 50 hist
                    hist' = case mb_brkpt of
                       Nothing -> prevHistoryLst
                       Just bi
                         | not $ canLogSpan span -> prevHistoryLst
                         | otherwise -> mkHistory hsc_env apStack bi `consBL`
                                                        fromListBL 50 hist
                handleRunStatus step expr bindings final_ids status hist'

setupBreakpoint :: GhcMonad m => HscEnv -> BreakpointId -> Int -> m ()   -- #19157
setupBreakpoint hsc_env bi cnt = do
  let modl = bi_tick_mod bi
      breaks hsc_env modl = getModBreaks $ expectJust "setupBreakpoint" $
         lookupHpt (hsc_HPT hsc_env) (moduleName modl)
      modBreaks  = breaks hsc_env modl
      breakarray = modBreaks_flags modBreaks
      interp = hscInterp hsc_env
  _ <- liftIO $ GHCi.storeBreakpoint interp breakarray (bi_tick_index bi) cnt
  pure ()

back :: GhcMonad m => Int -> m ([Name], Int, SrcSpan, String)
back n = moveHist (+n)

forward :: GhcMonad m => Int -> m ([Name], Int, SrcSpan, String)
forward n = moveHist (subtract n)

moveHist :: GhcMonad m => (Int -> Int) -> m ([Name], Int, SrcSpan, String)
moveHist fn = do
  hsc_env <- getSession
  case ic_resume (hsc_IC hsc_env) of
     [] -> liftIO $
           throwGhcExceptionIO (ProgramError "not stopped at a breakpoint")
     (r:rs) -> do
        let ix = resumeHistoryIx r
            history = resumeHistory r
            new_ix = fn ix
        --
        when (history `lengthLessThan` new_ix) $ liftIO $
           throwGhcExceptionIO (ProgramError "no more logged breakpoints")
        when (new_ix < 0) $ liftIO $
           throwGhcExceptionIO (ProgramError "already at the beginning of the history")

        let
          update_ic apStack mb_info = do
            (hsc_env1, names, span, decl) <-
              liftIO $ bindLocalsAtBreakpoint hsc_env apStack mb_info
            let ic = hsc_IC hsc_env1
                r' = r { resumeHistoryIx = new_ix }
                ic' = ic { ic_resume = r':rs }

            setSession hsc_env1{ hsc_IC = ic' }

            return (names, new_ix, span, decl)

        -- careful: we want apStack to be the AP_STACK itself, not a thunk
        -- around it, hence the cases are carefully constructed below to
        -- make this the case.  ToDo: this is v. fragile, do something better.
        if new_ix == 0
           then case r of
                   Resume { resumeApStack = apStack,
                            resumeBreakpointId = mb_brkpt } ->
                          update_ic apStack mb_brkpt
           else case history !! (new_ix - 1) of
                   History{..} ->
                     update_ic historyApStack (Just historyBreakpointId)


-- -----------------------------------------------------------------------------
-- After stopping at a breakpoint, add free variables to the environment

result_fs :: FastString
result_fs = fsLit "_result"

bindLocalsAtBreakpoint
        :: HscEnv
        -> ForeignHValue
        -> Maybe InternalBreakpointId
        -> IO (HscEnv, [Name], SrcSpan, String)

-- Nothing case: we stopped when an exception was raised, not at a
-- breakpoint.  We have no location information or local variables to
-- bind, all we can do is bind a local variable to the exception
-- value.
bindLocalsAtBreakpoint hsc_env apStack Nothing = do
   let exn_occ = mkVarOccFS (fsLit "_exception")
       span    = mkGeneralSrcSpan (fsLit "<unknown>")
   exn_name <- newInteractiveBinder hsc_env exn_occ span

   let e_fs    = fsLit "e"
       e_name  = mkInternalName (getUnique e_fs) (mkTyVarOccFS e_fs) span
       e_tyvar = mkRuntimeUnkTyVar e_name liftedTypeKind
       exn_id  = Id.mkVanillaGlobal exn_name (mkTyVarTy e_tyvar)

       ictxt0 = hsc_IC hsc_env
       ictxt1 = extendInteractiveContextWithIds ictxt0 [exn_id]
       interp = hscInterp hsc_env
   --
   Loader.extendLoadedEnv interp [(exn_name, apStack)]
   return (hsc_env{ hsc_IC = ictxt1 }, [exn_name], span, "<exception thrown>")

-- Just case: we stopped at a breakpoint, we have information about the location
-- of the breakpoint and the free variables of the expression.
bindLocalsAtBreakpoint hsc_env apStack_fhv (Just ibi) = do
   let
       interp    = hscInterp hsc_env

       info_mod  = ibi_info_mod ibi
       info_hmi  = expectJust "bindLocalsAtBreakpoint" $ lookupHpt (hsc_HPT hsc_env) (moduleName info_mod)
       info_brks = getModBreaks info_hmi
       info      = expectJust "bindLocalsAtBreakpoint2" $ IntMap.lookup (ibi_info_index ibi) (modBreaks_breakInfo info_brks)

       tick_mod  = ibi_tick_mod ibi
       tick_hmi  = expectJust "bindLocalsAtBreakpoint" $ lookupHpt (hsc_HPT hsc_env) (moduleName tick_mod)
       tick_brks = getModBreaks tick_hmi
       occs      = modBreaks_vars tick_brks ! ibi_tick_index ibi
       span      = modBreaks_locs tick_brks ! ibi_tick_index ibi
       decl      = intercalate "." $ modBreaks_decls tick_brks ! ibi_tick_index ibi

  -- Rehydrate to understand the breakpoint info relative to the current environment.
  -- This design is critical to preventing leaks (#22530)
   (mbVars, result_ty) <- initIfaceLoad hsc_env
                            $ initIfaceLcl info_mod (text "debugger") NotBoot
                            $ hydrateCgBreakInfo info

   let

           -- Filter out any unboxed ids by changing them to Nothings;
           -- we can't bind these at the prompt
       mbPointers = nullUnboxed <$> mbVars

       (ids, offsets, occs') = syncOccs mbPointers occs

       free_tvs = tyCoVarsOfTypesWellScoped (result_ty:map idType ids)

   -- It might be that getIdValFromApStack fails, because the AP_STACK
   -- has been accidentally evaluated, or something else has gone wrong.
   -- So that we don't fall over in a heap when this happens, just don't
   -- bind any free variables instead, and we emit a warning.
   mb_hValues <-
      mapM (getBreakpointVar interp apStack_fhv . fromIntegral) offsets
   when (any isNothing mb_hValues) $
      debugTraceMsg (hsc_logger hsc_env) 1 $
          text "Warning: _result has been evaluated, some bindings have been lost"

   us <- mkSplitUniqSupply 'I'   -- Dodgy; will give the same uniques every time
   let tv_subst     = newTyVars us free_tvs
       (filtered_ids, occs'') = unzip         -- again, sync the occ-names
          [ (id, occ) | (id, Just _hv, occ) <- zip3 ids mb_hValues occs' ]
       (_,tidy_tys) = tidyOpenTypes emptyTidyEnv $
                      map (substTy tv_subst . idType) filtered_ids

   new_ids     <- zipWith3M mkNewId occs'' tidy_tys filtered_ids
   result_name <- newInteractiveBinder hsc_env (mkVarOccFS result_fs) span

   let result_id = Id.mkVanillaGlobal result_name
                     (substTy tv_subst result_ty)
       result_ok = isPointer result_id

       final_ids | result_ok = result_id : new_ids
                 | otherwise = new_ids
       ictxt0 = hsc_IC hsc_env
       ictxt1 = extendInteractiveContextWithIds ictxt0 final_ids
       names  = map idName new_ids

   let fhvs = catMaybes mb_hValues
   Loader.extendLoadedEnv interp (zip names fhvs)
   when result_ok $ Loader.extendLoadedEnv interp [(result_name, apStack_fhv)]
   hsc_env1 <- rttiEnvironment hsc_env{ hsc_IC = ictxt1 }
   return (hsc_env1, if result_ok then result_name:names else names, span, decl)
  where
        -- We need a fresh Unique for each Id we bind, because the linker
        -- state is single-threaded and otherwise we'd spam old bindings
        -- whenever we stop at a breakpoint.  The InteractveContext is properly
        -- saved/restored, but not the linker state.  See #1743, test break026.
   mkNewId :: OccName -> Type -> Id -> IO Id
   mkNewId occ ty old_id
     = do { name <- newInteractiveBinder hsc_env occ (getSrcSpan old_id)
          ; return (Id.mkVanillaGlobalWithInfo name ty (idInfo old_id)) }

   newTyVars :: UniqSupply -> [TcTyVar] -> Subst
     -- Similarly, clone the type variables mentioned in the types
     -- we have here, *and* make them all RuntimeUnk tyvars
   newTyVars us tvs = foldl' new_tv emptySubst (tvs `zip` uniqsFromSupply us)
   new_tv subst (tv,uniq) = extendTCvSubstWithClone subst tv new_tv
    where
     new_tv = mkRuntimeUnkTyVar (setNameUnique (tyVarName tv) uniq)
                                (substTy subst (tyVarKind tv))

   isPointer id | [rep] <- typePrimRep (idType id)
                , isGcPtrRep rep                   = True
                | otherwise                        = False

   -- Convert unboxed Id's to Nothings
   nullUnboxed (Just (fv@(id, _)))
     | isPointer id          = Just fv
     | otherwise             = Nothing
   nullUnboxed Nothing       = Nothing

   -- See Note [Syncing breakpoint info]
   syncOccs :: [Maybe (a,b)] -> [c] -> ([a], [b], [c])
   syncOccs mbVs ocs = unzip3 $ catMaybes $ joinOccs mbVs ocs
     where
       joinOccs :: [Maybe (a,b)] -> [c] -> [Maybe (a,b,c)]
       joinOccs = zipWithEqual "bindLocalsAtBreakpoint" joinOcc
       joinOcc mbV oc = (\(a,b) c -> (a,b,c)) <$> mbV <*> pure oc

rttiEnvironment :: HscEnv -> IO HscEnv
rttiEnvironment hsc_env@HscEnv{hsc_IC=ic} = do
   let tmp_ids = [id | AnId id <- ic_tythings ic]
       incompletelyTypedIds =
           [id | id <- tmp_ids
               , not $ noSkolems id
               , (occNameFS.nameOccName.idName) id /= result_fs]
   foldM improveTypes hsc_env (map idName incompletelyTypedIds)
    where
     noSkolems = noFreeVarsOfType . idType
     improveTypes hsc_env@HscEnv{hsc_IC=ic} name = do
      let tmp_ids = [id | AnId id <- ic_tythings ic]
          Just id = find (\i -> idName i == name) tmp_ids
      if noSkolems id
         then return hsc_env
         else do
           mb_new_ty <- reconstructType hsc_env 10 id
           let old_ty = idType id
           case mb_new_ty of
             Nothing -> return hsc_env
             Just new_ty -> do
              case improveRTTIType hsc_env old_ty new_ty of
               Nothing -> warnPprTrace True (":print failed to calculate the "
                                             ++ "improvement for a type")
                              (vcat [ text "id" <+> ppr id
                                    , text "old_ty" <+> debugPprType old_ty
                                    , text "new_ty" <+> debugPprType new_ty ]) $
                          return hsc_env
               Just subst -> do
                 let logger = hsc_logger hsc_env
                 putDumpFileMaybe logger Opt_D_dump_rtti "RTTI"
                   FormatText
                   (fsep [text "RTTI Improvement for", ppr id, equals,
                          ppr subst])

                 let ic' = substInteractiveContext ic subst
                 return hsc_env{hsc_IC=ic'}

pushResume :: HscEnv -> Resume -> HscEnv
pushResume hsc_env resume = hsc_env { hsc_IC = ictxt1 }
  where
        ictxt0 = hsc_IC hsc_env
        ictxt1 = ictxt0 { ic_resume = resume : ic_resume ictxt0 }


  {-
  Note [Syncing breakpoint info]
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  To display the values of the free variables for a single breakpoint, the
  function `GHC.Runtime.Eval.bindLocalsAtBreakpoint` pulls
  out the information from the fields `modBreaks_breakInfo` and
  `modBreaks_vars` of the `ModBreaks` data structure.
  For a specific breakpoint this gives 2 lists of type `Id` (or `Var`)
  and `OccName`.
  They are used to create the Id's for the free variables and must be kept
  in sync!

  There are 3 situations where items are removed from the Id list
  (or replaced with `Nothing`):
  1.) If function `GHC.StgToByteCode.schemeER_wrk` (which creates
      the Id list) doesn't find an Id in the ByteCode environment.
  2.) If function `GHC.Runtime.Eval.bindLocalsAtBreakpoint`
      filters out unboxed elements from the Id list, because GHCi cannot
      yet handle them.
  3.) If the GHCi interpreter doesn't find the reference to a free variable
      of our breakpoint. This also happens in the function
      bindLocalsAtBreakpoint.

  If an element is removed from the Id list, then the corresponding element
  must also be removed from the Occ list. Otherwise GHCi will confuse
  variable names as in #8487.
  -}

-- -----------------------------------------------------------------------------
-- Abandoning a resume context

abandon :: GhcMonad m => m Bool
abandon = do
   hsc_env <- getSession
   let ic = hsc_IC hsc_env
       resume = ic_resume ic
       interp = hscInterp hsc_env
   case resume of
      []    -> return False
      r:rs  -> do
         setSession hsc_env{ hsc_IC = ic { ic_resume = rs } }
         liftIO $ abandonStmt interp (resumeContext r)
         return True

abandonAll :: GhcMonad m => m Bool
abandonAll = do
   hsc_env <- getSession
   let ic = hsc_IC hsc_env
       resume = ic_resume ic
       interp = hscInterp hsc_env
   case resume of
      []  -> return False
      rs  -> do
         setSession hsc_env{ hsc_IC = ic { ic_resume = [] } }
         liftIO $ mapM_ (abandonStmt interp. resumeContext) rs
         return True

-- -----------------------------------------------------------------------------
-- Bounded list, optimised for repeated cons

data BoundedList a = BL
                        {-# UNPACK #-} !Int  -- length
                        {-# UNPACK #-} !Int  -- bound
                        [a] -- left
                        [a] -- right,  list is (left ++ reverse right)

nilBL :: Int -> BoundedList a
nilBL bound = BL 0 bound [] []

consBL :: a -> BoundedList a -> BoundedList a
consBL a (BL len bound left right)
  | len < bound = BL (len+1) bound (a:left) right
  | null right  = BL len     bound [a]      $! tail (reverse left)
  | otherwise   = BL len     bound (a:left) $! tail right

toListBL :: BoundedList a -> [a]
toListBL (BL _ _ left right) = left ++ reverse right

fromListBL :: Int -> [a] -> BoundedList a
fromListBL bound l = BL (length l) bound l []

-- lenBL (BL len _ _ _) = len

-- -----------------------------------------------------------------------------
-- | Set the interactive evaluation context.
--
-- (setContext imports) sets the ic_imports field (which in turn
-- determines what is in scope at the prompt) to 'imports', and
-- updates the icReaderEnv environment to reflect it.
--
-- We retain in scope all the things defined at the prompt, and kept
-- in ic_tythings.  (Indeed, they shadow stuff from ic_imports.)

setContext :: GhcMonad m => [InteractiveImport] -> m ()
setContext imports
  = do { hsc_env <- getSession
       ; let dflags = hsc_dflags hsc_env
       ; all_env_err <- liftIO $ findGlobalRdrEnv hsc_env imports
       ; case all_env_err of
           Left (mod, err) ->
               liftIO $ throwGhcExceptionIO (formatError dflags mod err)
           Right all_env -> do {
       ; let old_ic         = hsc_IC hsc_env
             !final_gre_cache = ic_gre_cache old_ic `replaceImportEnv` all_env
       ; setSession
         hsc_env{ hsc_IC = old_ic { ic_imports   = imports
                                  , ic_gre_cache = final_gre_cache }}}}
  where
    formatError dflags mod err = ProgramError . showSDoc dflags $
      text "Cannot add module" <+> ppr mod <+>
      text "to context:" <+> text err

findGlobalRdrEnv :: HscEnv -> [InteractiveImport]
                 -> IO (Either (ModuleName, String) GlobalRdrEnv)
-- Compute the GlobalRdrEnv for the interactive context
findGlobalRdrEnv hsc_env imports
  = do { idecls_env <- hscRnImportDecls hsc_env idecls
                    -- This call also loads any orphan modules
       ; partitionWithM mkEnv imods >>= \case
           (err : _, _)     -> return $ Left err
           ([], imods_env)  -> return $ Right (foldr plusGlobalRdrEnv idecls_env imods_env)
       }
  where
    idecls :: [LImportDecl GhcPs]
    idecls = [noLocA d | IIDecl d <- imports]

    imods :: [ModuleName]
    imods = [m | IIModule m <- imports]

    mkEnv mod = mkTopLevEnv hsc_env mod >>= \case
      Left err -> pure $ Left (mod, err)
      Right env -> pure $ Right env

mkTopLevEnv :: HscEnv -> ModuleName -> IO (Either String GlobalRdrEnv)
mkTopLevEnv hsc_env modl
  = case lookupHpt hpt modl of
      Nothing -> pure $ Left "not a home module"
      Just details ->
         case mi_top_env (hm_iface details) of
                Nothing  -> pure $ Left "not interpreted"
                Just (IfaceTopEnv exports imports) -> do
                  imports_env <-
                        runInteractiveHsc hsc_env
                      $ ioMsgMaybe $ hoistTcRnMessage $ runTcInteractive hsc_env
                      $ fmap (foldr plusGlobalRdrEnv emptyGlobalRdrEnv)
                      $ forM imports $ \iface_import -> do
                        let ImpUserSpec spec details = tcIfaceImport hsc_env iface_import
                        iface <- loadSrcInterface (text "imported by GHCi") (moduleName $ is_mod spec) (is_isboot spec) (is_pkg_qual spec)
                        pure $ case details of
                          ImpUserAll -> importsFromIface hsc_env iface spec Nothing
                          ImpUserEverythingBut ns -> importsFromIface hsc_env iface spec (Just ns)
                          ImpUserExplicit x -> x
                  let get_GRE_info nm = tyThingGREInfo <$> lookupGlobal hsc_env nm
                  let exports_env = hydrateGlobalRdrEnv get_GRE_info exports
                  pure $ Right $ plusGlobalRdrEnv imports_env exports_env
  where
    hpt = hsc_HPT hsc_env

-- | Get the interactive evaluation context, consisting of a pair of the
-- set of modules from which we take the full top-level scope, and the set
-- of modules from which we take just the exports respectively.
getContext :: GhcMonad m => m [InteractiveImport]
getContext = withSession $ \HscEnv{ hsc_IC=ic } ->
             return (ic_imports ic)

-- | Returns @True@ if the specified module is interpreted, and hence has
-- its full top-level scope available.
moduleIsInterpreted :: GhcMonad m => Module -> m Bool
moduleIsInterpreted modl = withSession $ \h ->
 if notHomeModule (hsc_home_unit h) modl
        then return False
        else case lookupHpt (hsc_HPT h) (moduleName modl) of
                Just details       -> return (isJust (mi_top_env (hm_iface details)))
                _not_a_home_module -> return False

-- | Looks up an identifier in the current interactive context (for :info)
-- Filter the instances by the ones whose tycons (or classes resp)
-- are in scope (qualified or otherwise).  Otherwise we list a whole lot too many!
-- The exact choice of which ones to show, and which to hide, is a judgement call.
--      (see #1581)
getInfo :: GhcMonad m => Bool -> Name
        -> m (Maybe (TyThing,Fixity,[ClsInst],[FamInst], SDoc))
getInfo allInfo name
  = withSession $ \hsc_env ->
    do mb_stuff <- liftIO $ hscTcRnGetInfo hsc_env name
       case mb_stuff of
         Nothing -> return Nothing
         Just (thing, fixity, cls_insts, fam_insts, docs) -> do
           let rdr_env = icReaderEnv (hsc_IC hsc_env)

           -- Filter the instances based on whether the constituent names of their
           -- instance heads are all in scope.
           let cls_insts' = filter (plausible rdr_env . orphNamesOfClsInst) cls_insts
               fam_insts' = filter (plausible rdr_env . orphNamesOfFamInst) fam_insts
           return (Just (thing, fixity, cls_insts', fam_insts', docs))
  where
    plausible rdr_env names
          -- Dfun involving only names that are in icReaderEnv
        = allInfo
       || nameSetAll ok names
        where   -- A name is ok if it's in the rdr_env,
                -- whether qualified or not
          ok n | n == name              = True
                       -- The one we looked for in the first place!
               | pretendNameIsInScope n = True
                   -- See Note [pretendNameIsInScope] in GHC.Builtin.Names
               | isExternalName n       = isJust (lookupGRE_Name rdr_env n)
               | otherwise              = True

-- | Returns all names in scope in the current interactive context
getNamesInScope :: GhcMonad m => m [Name]
getNamesInScope = withSession $ \hsc_env ->
  return $ map greName $ globalRdrEnvElts (icReaderEnv (hsc_IC hsc_env))

-- | Returns all 'RdrName's in scope in the current interactive
-- context, excluding any that are internally-generated.
getRdrNamesInScope :: GhcMonad m => m [RdrName]
getRdrNamesInScope = withSession $ \hsc_env -> do
  let
      ic = hsc_IC hsc_env
      gbl_rdrenv = icReaderEnv ic
      gbl_names = concatMap greRdrNames $ globalRdrEnvElts gbl_rdrenv
  -- Exclude internally generated names; see e.g. #11328
  return (filter (not . isDerivedOccName . rdrNameOcc) gbl_names)


-- | Parses a string as an identifier, and returns the list of 'Name's that
-- the identifier can refer to in the current interactive context.
parseName :: GhcMonad m => String -> m (NonEmpty Name)
parseName str = withSession $ \hsc_env -> liftIO $
   do { lrdr_name <- hscParseIdentifier hsc_env str
      ; hscTcRnLookupRdrName hsc_env lrdr_name }


getDocs :: GhcMonad m
        => Name
        -> m (Either GetDocsFailure (Maybe [HsDoc GhcRn], IntMap (HsDoc GhcRn)))
           -- TODO: What about docs for constructors etc.?
getDocs name =
  withSession $ \hsc_env -> do
     case nameModule_maybe name of
       Nothing -> pure (Left (NameHasNoModule name))
       Just mod -> do
         if isInteractiveModule mod
           then pure (Left InteractiveName)
           else do
             iface <- liftIO $ hscGetModuleInterface hsc_env mod
             case mi_docs iface of
               Nothing -> pure (Left (NoDocsInIface mod compiled))
               Just Docs { docs_decls = decls
                         , docs_args = args
                         } ->
                 pure (Right ( lookupUniqMap decls name
                             , fromMaybe mempty $ lookupUniqMap args name))
  where
    compiled =
      -- TODO: Find a more direct indicator.
      case nameSrcLoc name of
        RealSrcLoc {} -> False
        UnhelpfulLoc {} -> True

-- | Failure modes for 'getDocs'.
data GetDocsFailure

    -- | 'nameModule_maybe' returned 'Nothing'.
  = NameHasNoModule Name

    -- | The module was loaded without @-haddock@,
  | NoDocsInIface
      Module
      Bool -- ^ 'True': The module was compiled.
           -- 'False': The module was :loaded.

    -- | The 'Name' was defined interactively.
  | InteractiveName

instance Outputable GetDocsFailure where
  ppr (NameHasNoModule name) =
    quotes (ppr name) <+> text "has no module where we could look for docs."
  ppr (NoDocsInIface mod compiled) = vcat
    [ text "Can't find any documentation for" <+> ppr mod <> char '.'
    , if compiled
        then text "Try re-compiling with '-haddock'."
        else text "Try running ':set -haddock' and :load the file again."
        -- TODO: Figure out why :reload doesn't load the docs and maybe fix it.
    ]
  ppr InteractiveName =
    text "Docs are unavailable for interactive declarations."

-- -----------------------------------------------------------------------------
-- Getting the type of an expression

-- | Get the type of an expression
-- Returns the type as described by 'TcRnExprMode'
exprType :: GhcMonad m => TcRnExprMode -> String -> m Type
exprType mode expr = withSession $ \hsc_env -> do
   ty <- liftIO $ hscTcExpr hsc_env mode expr
   return $ tidyType emptyTidyEnv ty

-- -----------------------------------------------------------------------------
-- Getting the kind of a type

-- | Get the kind of a  type
typeKind  :: GhcMonad m => Bool -> String -> m (Type, Kind)
typeKind normalise str = withSession $ \hsc_env ->
   liftIO $ hscKcType hsc_env normalise str

-- ----------------------------------------------------------------------------
-- Getting the class instances for a type

{-
  Note [Querying instances for a type]
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Here is the implementation of GHC proposal 41.
  (https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0041-ghci-instances.rst)

  The objective is to take a query string representing a (partial) type, and
  report all the class single-parameter class instances available to that type.
  Extending this feature to multi-parameter typeclasses is left as future work.

  The general outline of how we solve this is:

  1. Parse the type, leaving skolems in the place of type-holes.
  2. For every class, get a list of all instances that match with the query type.
  3. For every matching instance, ask GHC for the context the instance dictionary needs.
  4. Format and present the results, substituting our query into the instance
     and simplifying the context.

  For example, given the query "Maybe Int", we want to return:

  instance Show (Maybe Int)
  instance Read (Maybe Int)
  instance Eq   (Maybe Int)
  ....

  [Holes in queries]

  Often times we want to know what instances are available for a polymorphic type,
  like `Maybe a`, and we'd like to return instances such as:

  instance Show a => Show (Maybe a)
  ....

  These queries are expressed using type holes, so instead of `Maybe a` the user writes
  `Maybe _`, we parse the type and during zonking, we skolemise it, replacing the holes
  with (un-named) type variables.

  When zonking the type holes we have two real choices: replace them with Any or replace
  them with skolem typevars. Using skolem type variables ensures that the output is more
  intuitive to end users, and there is no difference in the results between Any and skolems.

-}

-- Find all instances that match a provided type
getInstancesForType :: GhcMonad m => Type -> m [ClsInst]
getInstancesForType ty = withSession $ \hsc_env ->
  liftIO $ runInteractiveHsc hsc_env $
    ioMsgMaybe $ hoistTcRnMessage $ runTcInteractive hsc_env $ do
      -- Bring class and instances from unqualified modules into scope, this fixes #16793.
      loadUnqualIfaces hsc_env (hsc_IC hsc_env)
      matches <- findMatchingInstances ty

      fmap catMaybes . forM matches $ uncurry checkForExistence

-- Parse a type string and turn any holes into skolems
parseInstanceHead :: GhcMonad m => String -> m Type
parseInstanceHead str = withSession $ \hsc_env0 -> do
  (ty, _) <- liftIO $ runInteractiveHsc hsc_env0 $ do
    hsc_env <- getHscEnv
    ty <- hscParseType str
    ioMsgMaybe $ hoistTcRnMessage $ tcRnType hsc_env SkolemiseFlexi True ty

  return ty

-- Get all the constraints required of a dictionary binding
getDictionaryBindings :: PredType -> TcM CtEvidence
getDictionaryBindings theta = do
  dictName <- newName (mkDictOcc (mkVarOccFS (fsLit "magic")))
  let dict_var = mkVanillaGlobal dictName theta
  loc <- getCtLocM (GivenOrigin (getSkolemInfo unkSkol)) Nothing

  return CtWanted {
    ctev_pred = varType dict_var,
    ctev_dest = EvVarDest dict_var,
    ctev_loc = loc,
    ctev_rewriters = emptyRewriterSet
  }

-- Find instances where the head unifies with the provided type
findMatchingInstances :: Type -> TcM [(ClsInst, [DFunInstType])]
findMatchingInstances ty = do
  ies@(InstEnvs {ie_global = ie_global, ie_local = ie_local}) <- tcGetInstEnvs
  let allClasses = uniqDSetToList $ instEnvClasses ie_global `unionUniqDSets` instEnvClasses ie_local
  return $ concatMap (try_cls ies) allClasses
  where
  {- Check that a class instance is well-kinded.
    Since `:instances` only works for unary classes, we're looking for instances of kind
    k -> Constraint where k is the type of the queried type.
  -}
  try_cls ies cls
    | Just (_, _, arg_kind, res_kind) <- splitFunTy_maybe (tyConKind $ classTyCon cls)
    , isConstraintKind res_kind
    , Type.typeKind ty `eqType` arg_kind
    , (matches, _, _) <- lookupInstEnv True ies cls [ty]
    = matches
    | otherwise
    = []


{-
  When we've found an instance that a query matches against, we still need to
  check that all the instance's constraints are satisfiable. checkForExistence
  creates an instance dictionary and verifies that any unsolved constraints
  mention a type-hole, meaning it is blocked on an unknown.

  If the instance satisfies this condition, then we return it with the query
  substituted into the instance and all constraints simplified, for example given:

  instance D a => C (MyType a b) where

  and the query `MyType _ String`

  the unsolved constraints will be [D _] so we apply the substitution:

  { a -> _; b -> String}

  and return the instance:

  instance D _ => C (MyType _ String)

-}

checkForExistence :: ClsInst -> [DFunInstType] -> TcM (Maybe ClsInst)
checkForExistence clsInst mb_inst_tys = do
  -- We want to force the solver to attempt to solve the constraints for clsInst.
  -- Usually, this isn't a problem since there should only be a single instance
  -- for a type. However, when we have overlapping instances, the solver will give up
  -- since it can't decide which instance to use. To get around this restriction, instead
  -- of asking the solver to solve a constraint for clsInst, we ask it to solve the
  -- thetas of clsInst.
  (tys, thetas) <- instDFunType (is_dfun clsInst) mb_inst_tys
  wanteds <- mapM getDictionaryBindings thetas
  -- It's important to zonk constraints after solving in order to expose things like TypeErrors
  -- which otherwise appear as opaque type variables. (See #18262).
  WC { wc_simple = simples, wc_impl = impls } <- simplifyWantedsTcM wanteds

  -- The simples might contain superclasses. This clutters up the output
  -- (we want e.g. instance Ord a => Ord (Maybe a), not
  -- instance (Ord a, Eq a) => Ord (Maybe a)). So we use mkMinimalBySCs
  let simple_preds = map ctPred (bagToList simples)
  let minimal_simples = mkMinimalBySCs id simple_preds

  if all allowedSimple minimal_simples && solvedImplics impls
  then return . Just $ substInstArgs tys minimal_simples clsInst
  else return Nothing

  where
  allowedSimple :: PredType -> Bool
  allowedSimple pred = isSatisfiablePred pred

  solvedImplics :: Bag Implication -> Bool
  solvedImplics impls = allBag (isSolvedStatus . ic_status) impls

  -- Stricter version of isTyVarClassPred that requires all TyConApps to have at least
  -- one argument or for the head to be a TyVar. The reason is that we want to ensure
  -- that all residual constraints mention a type-hole somewhere in the constraint,
  -- meaning that with the correct choice of a concrete type it could be possible for
  -- the constraint to be discharged.
  isSatisfiablePred :: PredType -> Bool
  isSatisfiablePred ty = case getClassPredTys_maybe ty of
      Just (_, tys@(_:_)) -> all isTyVarTy tys
      _                   -> isTyVarTy ty

  empty_subst = mkEmptySubst (mkInScopeSet (tyCoVarsOfType (idType $ is_dfun clsInst)))

  {- Create a ClsInst with instantiated arguments and constraints.

     The thetas are the list of constraints that couldn't be solved because
     they mention a type-hole.
  -}
  substInstArgs ::  [Type] -> [PredType] -> ClsInst -> ClsInst
  substInstArgs tys thetas inst = let
      subst = foldl' (\a b -> uncurry (extendTvSubstAndInScope a) b) empty_subst (zip dfun_tvs tys)
      -- Build instance head with arguments substituted in
      tau   = mkClassPred cls (substTheta subst args)
      -- Constrain the instance with any residual constraints
      phi   = mkPhiTy thetas tau
      sigma = mkForAllTys (map (\v -> Bndr v Inferred) dfun_tvs) phi

    in inst { is_dfun = (is_dfun inst) { varType = sigma }}
    where
    (dfun_tvs, _, cls, args) = instanceSig inst

-----------------------------------------------------------------------------
-- Compile an expression, run it, and deliver the result

-- | Parse an expression, the parsed expression can be further processed and
-- passed to compileParsedExpr.
parseExpr :: GhcMonad m => String -> m (LHsExpr GhcPs)
parseExpr expr = withSession $ \hsc_env ->
  liftIO $ runInteractiveHsc hsc_env $ hscParseExpr expr

-- | Compile an expression, run it, and deliver the resulting HValue.
compileExpr :: GhcMonad m => String -> m HValue
compileExpr expr = do
  parsed_expr <- parseExpr expr
  compileParsedExpr parsed_expr

-- | Compile an expression, run it, and deliver the resulting HValue.
compileExprRemote :: GhcMonad m => String -> m ForeignHValue
compileExprRemote expr = do
  parsed_expr <- parseExpr expr
  compileParsedExprRemote parsed_expr

-- | Compile a parsed expression (before renaming), run it, and deliver
-- the resulting HValue.
compileParsedExprRemote :: GhcMonad m => LHsExpr GhcPs -> m ForeignHValue
compileParsedExprRemote expr@(L loc _) = withSession $ \hsc_env -> do
  let dflags = hsc_dflags hsc_env
  let interp = hscInterp hsc_env

  -- > let _compileParsedExpr = expr
  -- Create let stmt from expr to make hscParsedStmt happy.
  -- We will ignore the returned [Id], namely [expr_id], and not really
  -- create a new binding.
  let expr_fs = fsLit "_compileParsedExpr"
      loc' = locA loc
      expr_name = mkInternalName (getUnique expr_fs) (mkTyVarOccFS expr_fs) loc'
      let_stmt = L loc . LetStmt noAnn . (HsValBinds noAnn) $
        ValBinds NoAnnSortKey
                     [mkHsVarBind loc' (getRdrName expr_name) expr] []

  pstmt <- liftIO $ hscParsedStmt hsc_env let_stmt
  let (hvals_io, fix_env) = case pstmt of
        Just ([_id], hvals_io', fix_env') -> (hvals_io', fix_env')
        _ -> panic "compileParsedExprRemote"

  updateFixityEnv fix_env
  let eval_opts = initEvalOpts dflags False
  status <- liftIO $ evalStmt interp eval_opts (EvalThis hvals_io)
  case status of
    EvalComplete _ (EvalSuccess [hval]) -> return hval
    EvalComplete _ (EvalException e) ->
      liftIO $ throwIO (fromSerializableException e)
    _ -> panic "compileParsedExpr"

compileParsedExpr :: GhcMonad m => LHsExpr GhcPs -> m HValue
compileParsedExpr expr = do
   fhv <- compileParsedExprRemote expr
   interp <- hscInterp <$> getSession
   liftIO $ wormhole interp fhv

-- | Compile an expression, run it and return the result as a Dynamic.
dynCompileExpr :: GhcMonad m => String -> m Dynamic
dynCompileExpr expr = do
  parsed_expr <- parseExpr expr
  -- > Data.Dynamic.toDyn expr
  let loc = getLoc parsed_expr
      to_dyn_expr = mkHsApp (L loc . HsVar noExtField . L (l2l loc) $ getRdrName toDynName)
                            parsed_expr
  hval <- compileParsedExpr to_dyn_expr
  return (unsafeCoerce hval :: Dynamic)

-----------------------------------------------------------------------------
-- show a module and it's source/object filenames

showModule :: GhcMonad m => ModSummary -> m String
showModule mod_summary =
    withSession $ \hsc_env -> do
        let dflags = hsc_dflags hsc_env
        let interpreted =
              case lookupHug (hsc_HUG hsc_env) (ms_unitid mod_summary) (ms_mod_name mod_summary) of
               Nothing       -> panic "missing linkable"
               Just mod_info -> isJust (homeModInfoByteCode mod_info)  && isNothing (homeModInfoObject mod_info)
        return (showSDoc dflags $ showModMsg dflags interpreted (ModuleNode [] mod_summary))

moduleIsBootOrNotObjectLinkable :: GhcMonad m => ModSummary -> m Bool
moduleIsBootOrNotObjectLinkable mod_summary = withSession $ \hsc_env ->
  case lookupHug (hsc_HUG hsc_env) (ms_unitid mod_summary) (ms_mod_name mod_summary) of
        Nothing       -> panic "missing linkable"
        Just mod_info -> return . isNothing $ homeModInfoByteCode mod_info

----------------------------------------------------------------------------
-- RTTI primitives

obtainTermFromVal :: HscEnv -> Int -> Bool -> Type -> a -> IO Term
#if defined(HAVE_INTERNAL_INTERPRETER)
obtainTermFromVal hsc_env bound force ty x = case interpInstance interp of
  InternalInterp    -> cvObtainTerm hsc_env bound force ty (unsafeCoerce x)
#else
obtainTermFromVal hsc_env _bound _force _ty _x = case interpInstance interp of
#endif
  ExternalInterp {} -> throwIO (InstallationError
                        "this operation requires -fno-external-interpreter")
  where
    interp = hscInterp hsc_env

obtainTermFromId :: HscEnv -> Int -> Bool -> Id -> IO Term
obtainTermFromId hsc_env bound force id =  do
  (hv, _, _) <- Loader.loadName (hscInterp hsc_env) hsc_env (varName id)
  cvObtainTerm hsc_env bound force (idType id) hv

-- Uses RTTI to reconstruct the type of an Id, making it less polymorphic
reconstructType :: HscEnv -> Int -> Id -> IO (Maybe Type)
reconstructType hsc_env bound id = do
  (hv, _, _) <- Loader.loadName (hscInterp hsc_env) hsc_env (varName id)
  cvReconstructType hsc_env bound (idType id) hv

mkRuntimeUnkTyVar :: Name -> Kind -> TyVar
mkRuntimeUnkTyVar name kind = mkTcTyVar name kind RuntimeUnk
