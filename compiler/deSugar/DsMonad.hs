{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


@DsMonad@: monadery used in desugaring
-}

{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}  -- instance MonadThings is necessarily an orphan

module DsMonad (
        DsM, mapM, mapAndUnzipM,
        initDs, initDsTc, initTcDsForSolver, initDsWithModGuts, fixDs,
        foldlM, foldrM, whenGOptM, unsetGOptM, unsetWOptM, xoptM,
        Applicative(..),(<$>),

        duplicateLocalDs, newSysLocalDsNoLP, newSysLocalDs,
        newSysLocalsDsNoLP, newSysLocalsDs, newUniqueId,
        newFailLocalDs, newPredVarDs,
        getSrcSpanDs, putSrcSpanDs,
        mkPrintUnqualifiedDs,
        newUnique,
        UniqSupply, newUniqueSupply,
        getGhcModeDs, dsGetFamInstEnvs,
        dsLookupGlobal, dsLookupGlobalId, dsDPHBuiltin, dsLookupTyCon,
        dsLookupDataCon, dsLookupConLike,

        PArrBuiltin(..),
        dsLookupDPHRdrEnv, dsLookupDPHRdrEnv_maybe,
        dsInitPArrBuiltin,

        DsMetaEnv, DsMetaVal(..), dsGetMetaEnv, dsLookupMetaEnv, dsExtendMetaEnv,

        -- Getting and setting EvVars and term constraints in local environment
        getDictsDs, addDictsDs, getTmCsDs, addTmCsDs,

        -- Iterations for pm checking
        incrCheckPmIterDs, resetPmIterDs, dsGetCompleteMatches,

        -- Warnings and errors
        DsWarning, warnDs, warnIfSetDs, errDs, errDsCoreExpr,
        failWithDs, failDs, discardWarningsDs,
        askNoErrsDs,

        -- Data types
        DsMatchContext(..),
        EquationInfo(..), MatchResult(..), DsWrapper, idDsWrapper,
        CanItFail(..), orFail,

        -- Levity polymorphism
        dsNoLevPoly, dsNoLevPolyExpr, dsWhenNoErrs
    ) where

import TcRnMonad
import FamInstEnv
import CoreSyn
import MkCore    ( unitExpr )
import CoreUtils ( exprType, isExprLevPoly )
import HsSyn
import TcIface
import TcMType ( checkForLevPolyX, formatLevPolyErr )
import LoadIface
import Finder
import PrelNames
import RdrName
import HscTypes
import Bag
import DataCon
import ConLike
import TyCon
import PmExpr
import Id
import Module
import Outputable
import SrcLoc
import Type
import UniqSupply
import Name
import NameEnv
import DynFlags
import ErrUtils
import FastString
import Maybes
import Var (EvVar)
import qualified GHC.LanguageExtensions as LangExt
import UniqFM ( lookupWithDefaultUFM )

import Data.IORef
import Control.Monad

{-
************************************************************************
*                                                                      *
                Data types for the desugarer
*                                                                      *
************************************************************************
-}

data DsMatchContext
  = DsMatchContext (HsMatchContext Name) SrcSpan
  deriving ()

instance Outputable DsMatchContext where
  ppr (DsMatchContext hs_match ss) = ppr ss <+> pprMatchContext hs_match

data EquationInfo
  = EqnInfo { eqn_pats :: [Pat GhcTc],  -- The patterns for an eqn
              eqn_rhs  :: MatchResult } -- What to do after match

instance Outputable EquationInfo where
    ppr (EqnInfo pats _) = ppr pats

type DsWrapper = CoreExpr -> CoreExpr
idDsWrapper :: DsWrapper
idDsWrapper e = e

-- The semantics of (match vs (EqnInfo wrap pats rhs)) is the MatchResult
--      \fail. wrap (case vs of { pats -> rhs fail })
-- where vs are not bound by wrap


-- A MatchResult is an expression with a hole in it
data MatchResult
  = MatchResult
        CanItFail       -- Tells whether the failure expression is used
        (CoreExpr -> DsM CoreExpr)
                        -- Takes a expression to plug in at the
                        -- failure point(s). The expression should
                        -- be duplicatable!

data CanItFail = CanFail | CantFail

orFail :: CanItFail -> CanItFail -> CanItFail
orFail CantFail CantFail = CantFail
orFail _        _        = CanFail

{-
************************************************************************
*                                                                      *
                Monad functions
*                                                                      *
************************************************************************
-}

-- Compatibility functions
fixDs :: (a -> DsM a) -> DsM a
fixDs    = fixM

type DsWarning = (SrcSpan, SDoc)
        -- Not quite the same as a WarnMsg, we have an SDoc here
        -- and we'll do the print_unqual stuff later on to turn it
        -- into a Doc.

-- | Run a 'DsM' action inside the 'TcM' monad.
initDsTc :: DsM a -> TcM a
initDsTc thing_inside
  = do { tcg_env  <- getGblEnv
       ; msg_var  <- getErrsVar
       ; hsc_env  <- getTopEnv
       ; envs     <- mkDsEnvsFromTcGbl hsc_env msg_var tcg_env
       ; setEnvs envs $ initDPH thing_inside
       }

-- | Run a 'DsM' action inside the 'IO' monad.
initDs :: HscEnv -> TcGblEnv -> DsM a -> IO (Messages, Maybe a)
initDs hsc_env tcg_env thing_inside
  = do { msg_var <- newIORef emptyMessages
       ; envs <- mkDsEnvsFromTcGbl hsc_env msg_var tcg_env
       ; runDs hsc_env envs thing_inside
       }

-- | Build a set of desugarer environments derived from a 'TcGblEnv'.
mkDsEnvsFromTcGbl :: MonadIO m
                  => HscEnv -> IORef Messages -> TcGblEnv
                  -> m (DsGblEnv, DsLclEnv)
mkDsEnvsFromTcGbl hsc_env msg_var tcg_env
  = do { pm_iter_var <- liftIO $ newIORef 0
       ; let dflags   = hsc_dflags hsc_env
             this_mod = tcg_mod tcg_env
             type_env = tcg_type_env tcg_env
             rdr_env  = tcg_rdr_env tcg_env
             fam_inst_env = tcg_fam_inst_env tcg_env
             complete_matches = hptCompleteSigs hsc_env
                                ++ tcg_complete_matches tcg_env
       ; return $ mkDsEnvs dflags this_mod rdr_env type_env fam_inst_env
                           msg_var pm_iter_var complete_matches
       }

runDs :: HscEnv -> (DsGblEnv, DsLclEnv) -> DsM a -> IO (Messages, Maybe a)
runDs hsc_env (ds_gbl, ds_lcl) thing_inside
  = do { res    <- initTcRnIf 'd' hsc_env ds_gbl ds_lcl
                              (initDPH $ tryM thing_inside)
       ; msgs   <- readIORef (ds_msgs ds_gbl)
       ; let final_res
               | errorsFound dflags msgs = Nothing
               | Right r <- res          = Just r
               | otherwise               = panic "initDs"
       ; return (msgs, final_res)
       }
  where dflags = hsc_dflags hsc_env

-- | Run a 'DsM' action in the context of an existing 'ModGuts'
initDsWithModGuts :: HscEnv -> ModGuts -> DsM a -> IO (Messages, Maybe a)
initDsWithModGuts hsc_env guts thing_inside
  = do { pm_iter_var <- newIORef 0
       ; msg_var <- newIORef emptyMessages
       ; let dflags   = hsc_dflags hsc_env
             type_env = typeEnvFromEntities ids (mg_tcs guts) (mg_fam_insts guts)
             rdr_env  = mg_rdr_env guts
             fam_inst_env = mg_fam_inst_env guts
             this_mod = mg_module guts
             complete_matches = hptCompleteSigs hsc_env
                                ++ mg_complete_sigs guts

             bindsToIds (NonRec v _)   = [v]
             bindsToIds (Rec    binds) = map fst binds
             ids = concatMap bindsToIds (mg_binds guts)

             envs  = mkDsEnvs dflags this_mod rdr_env type_env
                              fam_inst_env msg_var pm_iter_var
                              complete_matches
       ; runDs hsc_env envs thing_inside
       }

initTcDsForSolver :: TcM a -> DsM (Messages, Maybe a)
-- Spin up a TcM context so that we can run the constraint solver
-- Returns any error messages generated by the constraint solver
-- and (Just res) if no error happened; Nothing if an error happened
--
-- Simon says: I'm not very happy about this.  We spin up a complete TcM monad
--             only to immediately refine it to a TcS monad.
-- Better perhaps to make TcS into its own monad, rather than building on TcS
-- But that may in turn interact with plugins

initTcDsForSolver thing_inside
  = do { (gbl, lcl) <- getEnvs
       ; hsc_env    <- getTopEnv

       ; let DsGblEnv { ds_mod = mod
                      , ds_fam_inst_env = fam_inst_env } = gbl

             DsLclEnv { dsl_loc = loc }                  = lcl

       ; liftIO $ initTc hsc_env HsSrcFile False mod loc $
         updGblEnv (\tc_gbl -> tc_gbl { tcg_fam_inst_env = fam_inst_env }) $
         thing_inside }

mkDsEnvs :: DynFlags -> Module -> GlobalRdrEnv -> TypeEnv -> FamInstEnv
         -> IORef Messages -> IORef Int -> [CompleteMatch]
         -> (DsGblEnv, DsLclEnv)
mkDsEnvs dflags mod rdr_env type_env fam_inst_env msg_var pmvar
         complete_matches
  = let if_genv = IfGblEnv { if_doc       = text "mkDsEnvs",
                             if_rec_types = Just (mod, return type_env) }
        if_lenv = mkIfLclEnv mod (text "GHC error in desugarer lookup in" <+> ppr mod)
                             False -- not boot!
        real_span = realSrcLocSpan (mkRealSrcLoc (moduleNameFS (moduleName mod)) 1 1)
        completeMatchMap = mkCompleteMatchMap complete_matches
        gbl_env = DsGblEnv { ds_mod     = mod
                           , ds_fam_inst_env = fam_inst_env
                           , ds_if_env  = (if_genv, if_lenv)
                           , ds_unqual  = mkPrintUnqualified dflags rdr_env
                           , ds_msgs    = msg_var
                           , ds_dph_env = emptyGlobalRdrEnv
                           , ds_parr_bi = panic "DsMonad: uninitialised ds_parr_bi"
                           , ds_complete_matches = completeMatchMap
                           }
        lcl_env = DsLclEnv { dsl_meta    = emptyNameEnv
                           , dsl_loc     = real_span
                           , dsl_dicts   = emptyBag
                           , dsl_tm_cs   = emptyBag
                           , dsl_pm_iter = pmvar
                           }
    in (gbl_env, lcl_env)


{-
************************************************************************
*                                                                      *
                Operations in the monad
*                                                                      *
************************************************************************

And all this mysterious stuff is so we can occasionally reach out and
grab one or more names.  @newLocalDs@ isn't exported---exported
functions are defined with it.  The difference in name-strings makes
it easier to read debugging output.

Note [Levity polymorphism checking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
According to the "Levity Polymorphism" paper (PLDI '17), levity
polymorphism is forbidden in precisely two places: in the type of a bound
term-level argument and in the type of an argument to a function. The paper
explains it more fully, but briefly: expressions in these contexts need to be
stored in registers, and it's hard (read, impossible) to store something
that's levity polymorphic.

We cannot check for bad levity polymorphism conveniently in the type checker,
because we can't tell, a priori, which levity metavariables will be solved.
At one point, I (Richard) thought we could check in the zonker, but it's hard
to know where precisely are the abstracted variables and the arguments. So
we check in the desugarer, the only place where we can see the Core code and
still report respectable syntax to the user. This covers the vast majority
of cases; see calls to DsMonad.dsNoLevPoly and friends.

Levity polymorphism is also prohibited in the types of binders, and the
desugarer checks for this in GHC-generated Ids. (The zonker handles
the user-writted ids in zonkIdBndr.) This is done in newSysLocalDsNoLP.
The newSysLocalDs variant is used in the vast majority of cases where
the binder is obviously not levity polymorphic, omitting the check.
It would be nice to ASSERT that there is no levity polymorphism here,
but we can't, because of the fixM in DsArrows. It's all OK, though:
Core Lint will catch an error here.

However, the desugarer is the wrong place for certain checks. In particular,
the desugarer can't report a sensible error message if an HsWrapper is malformed.
After all, GHC itself produced the HsWrapper. So we store some message text
in the appropriate HsWrappers (e.g. WpFun) that we can print out in the
desugarer.

There are a few more checks in places where Core is generated outside the
desugarer. For example, in datatype and class declarations, where levity
polymorphism is checked for during validity checking. It would be nice to
have one central place for all this, but that doesn't seem possible while
still reporting nice error messages.

-}

-- Make a new Id with the same print name, but different type, and new unique
newUniqueId :: Id -> Type -> DsM Id
newUniqueId id = mk_local (occNameFS (nameOccName (idName id)))

duplicateLocalDs :: Id -> DsM Id
duplicateLocalDs old_local
  = do  { uniq <- newUnique
        ; return (setIdUnique old_local uniq) }

newPredVarDs :: PredType -> DsM Var
newPredVarDs pred
 = newSysLocalDs pred

newSysLocalDsNoLP, newSysLocalDs, newFailLocalDs :: Type -> DsM Id
newSysLocalDsNoLP  = mk_local (fsLit "ds")

-- this variant should be used when the caller can be sure that the variable type
-- is not levity-polymorphic. It is necessary when the type is knot-tied because
-- of the fixM used in DsArrows. See Note [Levity polymorphism checking]
newSysLocalDs = mkSysLocalOrCoVarM (fsLit "ds")
newFailLocalDs = mkSysLocalOrCoVarM (fsLit "fail")
  -- the fail variable is used only in a situation where we can tell that
  -- levity-polymorphism is impossible.

newSysLocalsDsNoLP, newSysLocalsDs :: [Type] -> DsM [Id]
newSysLocalsDsNoLP = mapM newSysLocalDsNoLP
newSysLocalsDs = mapM newSysLocalDs

mk_local :: FastString -> Type -> DsM Id
mk_local fs ty = do { dsNoLevPoly ty (text "When trying to create a variable of type:" <+>
                                      ppr ty)  -- could improve the msg with another
                                               -- parameter indicating context
                    ; mkSysLocalOrCoVarM fs ty }

{-
We can also reach out and either set/grab location information from
the @SrcSpan@ being carried around.
-}

getGhcModeDs :: DsM GhcMode
getGhcModeDs =  getDynFlags >>= return . ghcMode

-- | Get in-scope type constraints (pm check)
getDictsDs :: DsM (Bag EvVar)
getDictsDs = do { env <- getLclEnv; return (dsl_dicts env) }

-- | Add in-scope type constraints (pm check)
addDictsDs :: Bag EvVar -> DsM a -> DsM a
addDictsDs ev_vars
  = updLclEnv (\env -> env { dsl_dicts = unionBags ev_vars (dsl_dicts env) })

-- | Get in-scope term constraints (pm check)
getTmCsDs :: DsM (Bag SimpleEq)
getTmCsDs = do { env <- getLclEnv; return (dsl_tm_cs env) }

-- | Add in-scope term constraints (pm check)
addTmCsDs :: Bag SimpleEq -> DsM a -> DsM a
addTmCsDs tm_cs
  = updLclEnv (\env -> env { dsl_tm_cs = unionBags tm_cs (dsl_tm_cs env) })

-- | Increase the counter for elapsed pattern match check iterations.
-- If the current counter is already over the limit, fail
incrCheckPmIterDs :: DsM Int
incrCheckPmIterDs = do
  env <- getLclEnv
  cnt <- readTcRef (dsl_pm_iter env)
  max_iters <- maxPmCheckIterations <$> getDynFlags
  if cnt >= max_iters
    then failM
    else updTcRef (dsl_pm_iter env) (+1)
  return cnt

-- | Reset the counter for pattern match check iterations to zero
resetPmIterDs :: DsM ()
resetPmIterDs = do { env <- getLclEnv; writeTcRef (dsl_pm_iter env) 0 }

getSrcSpanDs :: DsM SrcSpan
getSrcSpanDs = do { env <- getLclEnv
                  ; return (RealSrcSpan (dsl_loc env)) }

putSrcSpanDs :: SrcSpan -> DsM a -> DsM a
putSrcSpanDs (UnhelpfulSpan {}) thing_inside
  = thing_inside
putSrcSpanDs (RealSrcSpan real_span) thing_inside
  = updLclEnv (\ env -> env {dsl_loc = real_span}) thing_inside

-- | Emit a warning for the current source location
-- NB: Warns whether or not -Wxyz is set
warnDs :: WarnReason -> SDoc -> DsM ()
warnDs reason warn
  = do { env <- getGblEnv
       ; loc <- getSrcSpanDs
       ; dflags <- getDynFlags
       ; let msg = makeIntoWarning reason $
                   mkWarnMsg dflags loc (ds_unqual env) warn
       ; updMutVar (ds_msgs env) (\ (w,e) -> (w `snocBag` msg, e)) }

-- | Emit a warning only if the correct WarnReason is set in the DynFlags
warnIfSetDs :: WarningFlag -> SDoc -> DsM ()
warnIfSetDs flag warn
  = whenWOptM flag $
    warnDs (Reason flag) warn

errDs :: SDoc -> DsM ()
errDs err
  = do  { env <- getGblEnv
        ; loc <- getSrcSpanDs
        ; dflags <- getDynFlags
        ; let msg = mkErrMsg dflags loc (ds_unqual env) err
        ; updMutVar (ds_msgs env) (\ (w,e) -> (w, e `snocBag` msg)) }

-- | Issue an error, but return the expression for (), so that we can continue
-- reporting errors.
errDsCoreExpr :: SDoc -> DsM CoreExpr
errDsCoreExpr err
  = do { errDs err
       ; return unitExpr }

failWithDs :: SDoc -> DsM a
failWithDs err
  = do  { errDs err
        ; failM }

failDs :: DsM a
failDs = failM

-- (askNoErrsDs m) runs m
-- If m fails,
--    then (askNoErrsDs m) fails
-- If m succeeds with result r,
--    then (askNoErrsDs m) succeeds with result (r, b),
--         where b is True iff m generated no errors
-- Regardless of success or failure,
--   propagate any errors/warnings generated by m
--
-- c.f. TcRnMonad.askNoErrs
askNoErrsDs :: DsM a -> DsM (a, Bool)
askNoErrsDs thing_inside
 = do { errs_var <- newMutVar emptyMessages
      ; env <- getGblEnv
      ; mb_res <- tryM $  -- Be careful to catch exceptions
                          -- so that we propagate errors correctly
                          -- (Trac #13642)
                  setGblEnv (env { ds_msgs = errs_var }) $
                  thing_inside

      -- Propagate errors
      ; msgs@(warns, errs) <- readMutVar errs_var
      ; updMutVar (ds_msgs env) (\ (w,e) -> (w `unionBags` warns, e `unionBags` errs))

      -- And return
      ; case mb_res of
           Left _    -> failM
           Right res -> do { dflags <- getDynFlags
                           ; let errs_found = errorsFound dflags msgs
                           ; return (res, not errs_found) } }

mkPrintUnqualifiedDs :: DsM PrintUnqualified
mkPrintUnqualifiedDs = ds_unqual <$> getGblEnv

instance MonadThings (IOEnv (Env DsGblEnv DsLclEnv)) where
    lookupThing = dsLookupGlobal

-- | Attempt to load the given module and return its exported entities if
-- successful.
dsLoadModule :: SDoc -> Module -> DsM GlobalRdrEnv
dsLoadModule doc mod
  = do { env    <- getGblEnv
       ; setEnvs (ds_if_env env) $ do
       { iface <- loadInterface doc mod ImportBySystem
       ; case iface of
           Failed err      -> pprPanic "DsMonad.dsLoadModule: failed to load" (err $$ doc)
           Succeeded iface -> return $ mkGlobalRdrEnv . gresFromAvails prov . mi_exports $ iface
       } }
  where
    prov     = Just (ImpSpec { is_decl = imp_spec, is_item = ImpAll })
    imp_spec = ImpDeclSpec { is_mod = name, is_qual = True,
                             is_dloc = wiredInSrcSpan, is_as = name }
    name = moduleName mod

dsLookupGlobal :: Name -> DsM TyThing
-- Very like TcEnv.tcLookupGlobal
dsLookupGlobal name
  = do  { env <- getGblEnv
        ; setEnvs (ds_if_env env)
                  (tcIfaceGlobal name) }

dsLookupGlobalId :: Name -> DsM Id
dsLookupGlobalId name
  = tyThingId <$> dsLookupGlobal name

dsLookupTyCon :: Name -> DsM TyCon
dsLookupTyCon name
  = tyThingTyCon <$> dsLookupGlobal name

dsLookupDataCon :: Name -> DsM DataCon
dsLookupDataCon name
  = tyThingDataCon <$> dsLookupGlobal name

dsLookupConLike :: Name -> DsM ConLike
dsLookupConLike name
  = tyThingConLike <$> dsLookupGlobal name


dsGetFamInstEnvs :: DsM FamInstEnvs
-- Gets both the external-package inst-env
-- and the home-pkg inst env (includes module being compiled)
dsGetFamInstEnvs
  = do { eps <- getEps; env <- getGblEnv
       ; return (eps_fam_inst_env eps, ds_fam_inst_env env) }

dsGetMetaEnv :: DsM (NameEnv DsMetaVal)
dsGetMetaEnv = do { env <- getLclEnv; return (dsl_meta env) }

-- | The @COMPLETE@ pragams provided by the user for a given `TyCon`.
dsGetCompleteMatches :: TyCon -> DsM [CompleteMatch]
dsGetCompleteMatches tc = do
  eps <- getEps
  env <- getGblEnv
  let lookup_completes ufm = lookupWithDefaultUFM ufm [] tc
      eps_matches_list = lookup_completes $ eps_complete_matches eps
      env_matches_list = lookup_completes $ ds_complete_matches env
  return $ eps_matches_list ++ env_matches_list

dsLookupMetaEnv :: Name -> DsM (Maybe DsMetaVal)
dsLookupMetaEnv name = do { env <- getLclEnv; return (lookupNameEnv (dsl_meta env) name) }

dsExtendMetaEnv :: DsMetaEnv -> DsM a -> DsM a
dsExtendMetaEnv menv thing_inside
  = updLclEnv (\env -> env { dsl_meta = dsl_meta env `plusNameEnv` menv }) thing_inside

discardWarningsDs :: DsM a -> DsM a
-- Ignore warnings inside the thing inside;
-- used to ignore inaccessable cases etc. inside generated code
discardWarningsDs thing_inside
  = do  { env <- getGblEnv
        ; old_msgs <- readTcRef (ds_msgs env)

        ; result <- thing_inside

        -- Revert messages to old_msgs
        ; writeTcRef (ds_msgs env) old_msgs

        ; return result }

-- | Fail with an error message if the type is levity polymorphic.
dsNoLevPoly :: Type -> SDoc -> DsM ()
-- See Note [Levity polymorphism checking]
dsNoLevPoly ty doc = checkForLevPolyX errDs doc ty

-- | Check an expression for levity polymorphism, failing if it is
-- levity polymorphic.
dsNoLevPolyExpr :: CoreExpr -> SDoc -> DsM ()
-- See Note [Levity polymorphism checking]
dsNoLevPolyExpr e doc
  | isExprLevPoly e = errDs (formatLevPolyErr (exprType e) $$ doc)
  | otherwise       = return ()

-- | Runs the thing_inside. If there are no errors, then returns the expr
-- given. Otherwise, returns unitExpr. This is useful for doing a bunch
-- of levity polymorphism checks and then avoiding making a core App.
-- (If we make a core App on a levity polymorphic argument, detecting how
-- to handle the let/app invariant might call isUnliftedType, which panics
-- on a levity polymorphic type.)
-- See #12709 for an example of why this machinery is necessary.
dsWhenNoErrs :: DsM a -> (a -> CoreExpr) -> DsM CoreExpr
dsWhenNoErrs thing_inside mk_expr
  = do { (result, no_errs) <- askNoErrsDs thing_inside
       ; return $ if no_errs
                  then mk_expr result
                  else unitExpr }

--------------------------------------------------------------------------
--                  Data Parallel Haskell
--------------------------------------------------------------------------

-- | Run a 'DsM' with DPH things in scope if necessary.
initDPH :: DsM a -> DsM a
initDPH = loadDAP . initDPHBuiltins

-- | Extend the global environment with a 'GlobalRdrEnv' containing the exported
-- entities of,
--
--   * 'Data.Array.Parallel'      iff '-XParallelArrays' specified (see also 'checkLoadDAP').
--   * 'Data.Array.Parallel.Prim' iff '-fvectorise' specified.
loadDAP :: DsM a -> DsM a
loadDAP thing_inside
  = do { dapEnv  <- loadOneModule dATA_ARRAY_PARALLEL_NAME      checkLoadDAP          paErr
       ; dappEnv <- loadOneModule dATA_ARRAY_PARALLEL_PRIM_NAME (goptM Opt_Vectorise) veErr
       ; updGblEnv (\env -> env {ds_dph_env = dapEnv `plusOccEnv` dappEnv }) thing_inside
       }
  where
    loadOneModule :: ModuleName           -- the module to load
                  -> DsM Bool             -- under which condition
                  -> MsgDoc               -- error message if module not found
                  -> DsM GlobalRdrEnv     -- empty if condition 'False'
    loadOneModule modname check err
      = do { doLoad <- check
           ; if not doLoad
             then return emptyGlobalRdrEnv
             else do {
           ; hsc_env <- getTopEnv
           ; result <- liftIO $ findImportedModule hsc_env modname Nothing
           ; case result of
               Found _ mod -> dsLoadModule err mod
               _           -> pprPgmError "Unable to use Data Parallel Haskell (DPH):" err
           } }

    paErr       = text "To use ParallelArrays," <+> specBackend $$ hint1 $$ hint2
    veErr       = text "To use -fvectorise," <+> specBackend $$ hint1 $$ hint2
    specBackend = text "you must specify a DPH backend package"
    hint1       = text "Look for packages named 'dph-lifted-*' with 'ghc-pkg'"
    hint2       = text "You may need to install them with 'cabal install dph-examples'"

-- | If '-XParallelArrays' given, we populate the builtin table for desugaring
-- those.
initDPHBuiltins :: DsM a -> DsM a
initDPHBuiltins thing_inside
  = do { doInitBuiltins <- checkLoadDAP
       ; if doInitBuiltins
         then dsInitPArrBuiltin thing_inside
         else thing_inside
       }

checkLoadDAP :: DsM Bool
checkLoadDAP
  = do { paEnabled <- xoptM LangExt.ParallelArrays
       ; mod <- getModule
         -- do not load 'Data.Array.Parallel' iff compiling 'base:GHC.PArr' or a
         -- module called 'dATA_ARRAY_PARALLEL_NAME'; see also the comments at the top
         -- of 'base:GHC.PArr' and 'Data.Array.Parallel' in the DPH libraries
       ; return $ paEnabled &&
                  mod /= gHC_PARR' &&
                  moduleName mod /= dATA_ARRAY_PARALLEL_NAME
       }

-- | Populate 'ds_parr_bi' from 'ds_dph_env'.
--
dsInitPArrBuiltin :: DsM a -> DsM a
dsInitPArrBuiltin thing_inside
  = do { lengthPVar         <- externalVar (fsLit "lengthP")
       ; replicatePVar      <- externalVar (fsLit "replicateP")
       ; singletonPVar      <- externalVar (fsLit "singletonP")
       ; mapPVar            <- externalVar (fsLit "mapP")
       ; filterPVar         <- externalVar (fsLit "filterP")
       ; zipPVar            <- externalVar (fsLit "zipP")
       ; crossMapPVar       <- externalVar (fsLit "crossMapP")
       ; indexPVar          <- externalVar (fsLit "!:")
       ; emptyPVar          <- externalVar (fsLit "emptyP")
       ; appPVar            <- externalVar (fsLit "+:+")
       -- ; enumFromToPVar     <- externalVar (fsLit "enumFromToP")
       -- ; enumFromThenToPVar <- externalVar (fsLit "enumFromThenToP")
       ; enumFromToPVar     <- return arithErr
       ; enumFromThenToPVar <- return arithErr

       ; updGblEnv (\env -> env {ds_parr_bi = PArrBuiltin
                                              { lengthPVar         = lengthPVar
                                              , replicatePVar      = replicatePVar
                                              , singletonPVar      = singletonPVar
                                              , mapPVar            = mapPVar
                                              , filterPVar         = filterPVar
                                              , zipPVar            = zipPVar
                                              , crossMapPVar       = crossMapPVar
                                              , indexPVar          = indexPVar
                                              , emptyPVar          = emptyPVar
                                              , appPVar            = appPVar
                                              , enumFromToPVar     = enumFromToPVar
                                              , enumFromThenToPVar = enumFromThenToPVar
                                              } })
                   thing_inside
       }
  where
    externalVar :: FastString -> DsM Var
    externalVar fs = dsLookupDPHRdrEnv (mkVarOccFS fs) >>= dsLookupGlobalId

    arithErr = panic "Arithmetic sequences have to wait until we support type classes"

-- |Get a name from "Data.Array.Parallel" for the desugarer, from the
-- 'ds_parr_bi' component of the global desugerar environment.
--
dsDPHBuiltin :: (PArrBuiltin -> a) -> DsM a
dsDPHBuiltin sel = (sel . ds_parr_bi) <$> getGblEnv

-- |Lookup a name exported by 'Data.Array.Parallel.Prim' or 'Data.Array.Parallel.Prim'.
--  Panic if there isn't one, or if it is defined multiple times.
dsLookupDPHRdrEnv :: OccName -> DsM Name
dsLookupDPHRdrEnv occ
  = liftM (fromMaybe (pprPanic nameNotFound (ppr occ)))
  $ dsLookupDPHRdrEnv_maybe occ
  where nameNotFound  = "Name not found in 'Data.Array.Parallel' or 'Data.Array.Parallel.Prim':"

-- |Lookup a name exported by 'Data.Array.Parallel.Prim' or 'Data.Array.Parallel.Prim',
--  returning `Nothing` if it's not defined. Panic if it's defined multiple times.
dsLookupDPHRdrEnv_maybe :: OccName -> DsM (Maybe Name)
dsLookupDPHRdrEnv_maybe occ
  = do { env <- ds_dph_env <$> getGblEnv
       ; let gres = lookupGlobalRdrEnv env occ
       ; case gres of
           []    -> return $ Nothing
           [gre] -> return $ Just $ gre_name gre
           _     -> pprPanic multipleNames (ppr occ)
       }
  where multipleNames = "Multiple definitions in 'Data.Array.Parallel' and 'Data.Array.Parallel.Prim':"
