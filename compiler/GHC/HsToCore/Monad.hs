{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}  -- instance MonadThings is necessarily an orphan

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


Monadery used in desugaring
-}

module GHC.HsToCore.Monad (
        DsM, mapM, mapAndUnzipM,
        initDs, initDsTc, initTcDsForSolver, initDsWithModGuts, fixDs,
        foldlM, foldrM, whenGOptM, unsetGOptM, unsetWOptM, xoptM,
        Applicative(..),(<$>),

        duplicateLocalDs, newSysLocalDs, newSysLocalsDs,
        newSysLocalMDs, newSysLocalsMDs, newFailLocalMDs,
        newUniqueId, newPredVarDs,
        getSrcSpanDs, putSrcSpanDs, putSrcSpanDsA,
        mkNamePprCtxDs,
        newUnique,
        UniqSupply, newUniqueSupply,
        getGhcModeDs, dsGetFamInstEnvs,
        dsLookupGlobal, dsLookupGlobalId, dsLookupTyCon,
        dsLookupDataCon, dsLookupConLike,
        getCCIndexDsM,

        DsMetaEnv, DsMetaVal(..), dsGetMetaEnv, dsLookupMetaEnv, dsExtendMetaEnv,

        -- Getting and setting pattern match oracle states
        getPmNablas, updPmNablas,

        -- Tracking evidence variable coherence
        addUnspecables, getUnspecables,

        -- Get COMPLETE sets of a TyCon
        dsGetCompleteMatches,

        -- Warnings and errors
        DsWarning, diagnosticDs, errDsCoreExpr,
        failWithDs, failDs, discardWarningsDs,
        addMessagesDs, captureMessagesDs,

        -- Data types
        DsMatchContext(..),
        EquationInfo(..), EquationInfoNE, prependPats, mkEqnInfo, eqnMatchResult,
        MatchResult (..), runMatchResult, DsWrapper, idDsWrapper,

        -- Trace injection
        pprRuntimeTrace
    ) where

import GHC.Prelude

import GHC.Driver.Env
import GHC.Driver.DynFlags
import GHC.Driver.Ppr
import GHC.Driver.Config.Diagnostic

import GHC.Hs

import GHC.HsToCore.Types
import GHC.HsToCore.Errors.Types
import GHC.HsToCore.Pmc.Solver.Types (Nablas, initNablas)

import GHC.Core.FamInstEnv
import GHC.Core
import GHC.Core.Make  ( unitExpr )
import GHC.Core.Utils ( exprType )
import GHC.Core.DataCon
import GHC.Core.ConLike
import GHC.Core.TyCon
import GHC.Core.Type
import GHC.Core.Multiplicity

import GHC.IfaceToCore

import GHC.Tc.Utils.Monad

import GHC.Builtin.Names

import GHC.Data.FastString

import GHC.Unit.Env
import GHC.Unit.External
import GHC.Unit.Module
import GHC.Unit.Module.ModGuts

import GHC.Types.Name.Reader
import GHC.Types.SourceFile
import GHC.Types.Id
import GHC.Types.Var (EvId)
import GHC.Types.SrcLoc
import GHC.Types.TypeEnv
import GHC.Types.Unique.Supply
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.Name.Ppr
import GHC.Types.Literal ( mkLitString )
import GHC.Types.CostCentre.State
import GHC.Types.TyThing
import GHC.Types.Error
import GHC.Types.CompleteMatch
import GHC.Types.Unique.DSet

import GHC.Tc.Utils.Env (lookupGlobal)

import GHC.Utils.Error
import GHC.Utils.Outputable
import GHC.Utils.Panic
import qualified GHC.Data.Strict as Strict

import Data.IORef
import GHC.Driver.Env.KnotVars
import qualified Data.Set as S
import GHC.IO.Unsafe (unsafeInterleaveIO)

{-
************************************************************************
*                                                                      *
                Data types for the desugarer
*                                                                      *
************************************************************************
-}

data DsMatchContext
  = DsMatchContext HsMatchContextRn SrcSpan
  deriving ()

instance Outputable DsMatchContext where
  ppr (DsMatchContext hs_match ss) = ppr ss <+> pprMatchContext hs_match

data EquationInfo
  = EqnMatch  { eqn_pat :: LPat GhcTc
                -- ^ The first pattern of the equation
                --
                -- NB: The location info is used to determine whether the
                -- pattern is generated or not.
                -- This helps us avoid warnings on patterns that GHC elaborated.
                --
                -- NB: We have /already/ applied 'decideBangHood' to this
                -- pattern. See Note [decideBangHood] in "GHC.HsToCore.Utils"

              , eqn_rest :: EquationInfo }
                -- ^ The rest of the equation after its first pattern

  | EqnDone
  -- The empty tail of an equation having no more patterns
            (MatchResult CoreExpr)
            -- ^ What to do after match

type EquationInfoNE = EquationInfo
-- An EquationInfo which has at least one pattern

prependPats :: [LPat GhcTc] -> EquationInfo -> EquationInfo
prependPats [] eqn = eqn
prependPats (pat:pats) eqn = EqnMatch { eqn_pat = pat, eqn_rest = prependPats pats eqn }

mkEqnInfo :: [LPat GhcTc] -> MatchResult CoreExpr -> EquationInfo
mkEqnInfo pats = prependPats pats . EqnDone

eqnMatchResult :: EquationInfo -> MatchResult CoreExpr
eqnMatchResult (EqnDone rhs) = rhs
eqnMatchResult (EqnMatch { eqn_rest = eq }) = eqnMatchResult eq

instance Outputable EquationInfo where
    ppr = ppr . allEqnPats where
      allEqnPats (EqnDone {}) = []
      allEqnPats (EqnMatch { eqn_pat = pat, eqn_rest = eq }) = unLoc pat : allEqnPats eq

type DsWrapper = CoreExpr -> CoreExpr
idDsWrapper :: DsWrapper
idDsWrapper e = e

-- The semantics of (match vs (EqnInfo wrap pats rhs)) is the MatchResult CoreExpr
--      \fail. wrap (case vs of { pats -> rhs fail })
-- where vs are not bound by wrap

-- | This is a value of type a with potentially a CoreExpr-shaped hole in it.
-- This is used to deal with cases where we are potentially handling pattern
-- match failure, and want to later specify how failure is handled.
data MatchResult a
  -- | We represent the case where there is no hole without a function from
  -- 'CoreExpr', like this, because sometimes we have nothing to put in the
  -- hole and so want to be sure there is in fact no hole.
  = MR_Infallible (DsM a)
  | MR_Fallible (CoreExpr -> DsM a)
  deriving (Functor)

-- | Product is an "or" on fallibility---the combined match result is infallible
-- only if the left and right argument match results both were.
--
-- This is useful for combining a bunch of alternatives together and then
-- getting the overall fallibility of the entire group. See 'mkDataConCase' for
-- an example.
instance Applicative MatchResult where
  pure v = MR_Infallible (pure v)
  MR_Infallible f <*> MR_Infallible x = MR_Infallible (f <*> x)
  f <*> x = MR_Fallible $ \fail -> runMatchResult fail f <*> runMatchResult fail x

-- Given a fail expression to use, and a MatchResult CoreExpr, compute the filled CoreExpr whether
-- the MatchResult CoreExpr was failable or not.
runMatchResult :: CoreExpr -> MatchResult a -> DsM a
runMatchResult fail = \case
  MR_Infallible body -> body
  MR_Fallible body_fn -> body_fn fail

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
        -- and we'll do the name_ppr_ctx stuff later on to turn it
        -- into a Doc.

-- | Run a 'DsM' action inside the 'TcM' monad.
initDsTc :: DsM a -> TcM (Messages DsMessage, Maybe a)
initDsTc thing_inside
  = do { tcg_env  <- getGblEnv
       ; msg_var  <- liftIO $ newIORef emptyMessages
       ; hsc_env  <- getTopEnv
       ; envs     <- mkDsEnvsFromTcGbl hsc_env msg_var tcg_env
       ; e_result <- tryM $  -- need to tryM so that we don't discard
                             -- DsMessages
                     setEnvs envs thing_inside
       ; msgs     <- liftIO $ readIORef msg_var
       ; return (msgs, case e_result of Left _  -> Nothing
                                        Right x -> Just x)
       }

-- | Run a 'DsM' action inside the 'IO' monad.
initDs :: HscEnv -> TcGblEnv -> DsM a -> IO (Messages DsMessage, Maybe a)
initDs hsc_env tcg_env thing_inside
  = do { msg_var <- newIORef emptyMessages
       ; envs <- mkDsEnvsFromTcGbl hsc_env msg_var tcg_env
       ; runDs hsc_env envs thing_inside
       }

-- | Build a set of desugarer environments derived from a 'TcGblEnv'.
mkDsEnvsFromTcGbl :: MonadIO m
                  => HscEnv -> IORef (Messages DsMessage) -> TcGblEnv
                  -> m (DsGblEnv, DsLclEnv)
mkDsEnvsFromTcGbl hsc_env msg_var tcg_env
  = do { cc_st_var   <- liftIO $ newIORef newCostCentreState
       ; eps <- liftIO $ hscEPS hsc_env
       ; let unit_env = hsc_unit_env hsc_env
             this_mod = tcg_mod tcg_env
             type_env = tcg_type_env tcg_env
             rdr_env  = tcg_rdr_env tcg_env
             fam_inst_env = tcg_fam_inst_env tcg_env
             ptc = initPromotionTickContext (hsc_dflags hsc_env)
             -- re-use existing next_wrapper_num to ensure uniqueness
             next_wrapper_num_var = tcg_next_wrapper_num tcg_env

       ; ds_complete_matches <-
           liftIO $ unsafeInterleaveIO $
             -- This call to 'unsafeInterleaveIO' ensures we only do this work
             -- when we need to look at the COMPLETE pragmas, avoiding doing work
             -- when we don't need them.
             --
             -- Relevant test case: MultiLayerModulesTH_Make, which regresses
             -- in allocations by ~5% if we don't do this.
           traverse (lookupCompleteMatch type_env hsc_env) $
             localAndImportedCompleteMatches (tcg_complete_matches tcg_env) hsc_env eps
       ; return $ mkDsEnvs unit_env this_mod rdr_env type_env fam_inst_env ptc
                           msg_var cc_st_var next_wrapper_num_var ds_complete_matches
       }

-- | We have in hand the `CompleteMatches` for the module, but when
-- doing pattern-match overlap checking we want the `ConLike` for each
-- data constructor, not just its `Name`.  This function makes the
-- transition.
lookupCompleteMatch :: TypeEnv -> HscEnv -> CompleteMatch -> IO DsCompleteMatch
lookupCompleteMatch type_env hsc_env (CompleteMatch { cmConLikes = nms, cmResultTyCon = mb_tc })
  = do { cons <- mapMUniqDSet lookup_conLike nms
       ; return $ CompleteMatch { cmConLikes = cons, cmResultTyCon = mb_tc } }
  where
    lookup_conLike :: Name -> IO ConLike
    lookup_conLike nm
      | Just ty <- wiredInNameTyThing_maybe nm
      = go ty
      | Just ty <- lookupTypeEnv type_env nm
      = go ty
      | otherwise
      = go =<< lookupGlobal hsc_env nm
      where
        go :: TyThing -> IO ConLike
        go (AConLike cl) = return cl
        go ty = pprPanic "lookup_conLike not a ConLike" (ppr nm <+> ppr ty)

runDs :: HscEnv -> (DsGblEnv, DsLclEnv) -> DsM a -> IO (Messages DsMessage, Maybe a)
runDs hsc_env (ds_gbl, ds_lcl) thing_inside
  = do { res    <- initTcRnIf 'd' hsc_env ds_gbl ds_lcl
                              (tryM thing_inside)
       ; msgs   <- readIORef (ds_msgs ds_gbl)
       ; let final_res
               | errorsFound msgs = Nothing
               | Right r <- res   = Just r
               | otherwise        = panic "initDs"
       ; return (msgs, final_res)
       }

-- | Run a 'DsM' action in the context of an existing 'ModGuts'
initDsWithModGuts :: HscEnv -> ModGuts -> DsM a -> IO (Messages DsMessage, Maybe a)
initDsWithModGuts hsc_env (ModGuts { mg_module = this_mod, mg_binds = binds
                                   , mg_tcs = tycons, mg_fam_insts = fam_insts
                                   , mg_patsyns = patsyns, mg_rdr_env = rdr_env
                                   , mg_fam_inst_env = fam_inst_env
                                   , mg_complete_matches = local_complete_matches
                          }) thing_inside
  = do { cc_st_var   <- newIORef newCostCentreState
       ; next_wrapper_num <- newIORef emptyModuleEnv
       ; msg_var <- newIORef emptyMessages
       ; eps <- liftIO $ hscEPS hsc_env
       ; let unit_env = hsc_unit_env hsc_env
             type_env = typeEnvFromEntities ids tycons patsyns fam_insts
             ptc = initPromotionTickContext (hsc_dflags hsc_env)
             bindsToIds (NonRec v _)   = [v]
             bindsToIds (Rec    binds) = map fst binds
             ids = concatMap bindsToIds binds
       ; ds_complete_matches <- traverse (lookupCompleteMatch type_env hsc_env) $
            localAndImportedCompleteMatches local_complete_matches hsc_env eps
       ; let
             envs  = mkDsEnvs unit_env this_mod rdr_env type_env
                              fam_inst_env ptc msg_var cc_st_var
                              next_wrapper_num ds_complete_matches
       ; runDs hsc_env envs thing_inside
       }

initTcDsForSolver :: TcM a -> DsM a
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

         -- The DsGblEnv is used to inform the typechecker's solver of a few
         -- key pieces of information:
         --
         --  - ds_fam_inst_env tells it how to reduce type families,
         --  - ds_gbl_rdr_env  tells it which newtypes it can unwrap.
       ; let DsGblEnv { ds_mod = mod
                      , ds_fam_inst_env = fam_inst_env
                      , ds_gbl_rdr_env  = rdr_env
                      } = gbl
             DsLclEnv { dsl_loc = loc } = lcl

       ; (msgs, mb_ret) <- liftIO $ initTc hsc_env HsSrcFile False mod loc $
         updGblEnv (\tc_gbl -> tc_gbl { tcg_fam_inst_env = fam_inst_env
                                      , tcg_rdr_env      = rdr_env }) $
         thing_inside
       ; case mb_ret of
           Just ret -> pure ret
           Nothing  -> pprPanic "initTcDsForSolver" (vcat $ pprMsgEnvelopeBagWithLocDefault (getErrorMessages msgs)) }

mkDsEnvs :: UnitEnv -> Module -> GlobalRdrEnv -> TypeEnv -> FamInstEnv
         -> PromotionTickContext
         -> IORef (Messages DsMessage) -> IORef CostCentreState
         -> IORef (ModuleEnv Int) -> DsCompleteMatches
         -> (DsGblEnv, DsLclEnv)
mkDsEnvs unit_env mod rdr_env type_env fam_inst_env ptc msg_var cc_st_var
         next_wrapper_num complete_matches
  = let if_genv = IfGblEnv { if_doc       = text "mkDsEnvs"
  -- Failing tests here are `ghci` and `T11985` if you get this wrong.
  -- this is very very "at a distance" because the reason for this check is that the type_env in interactive
  -- mode is the smushed together of all the interactive modules.
  -- See Note [Why is KnotVars not a ModuleEnv]
                             , if_rec_types = KnotVars [mod] (\that_mod -> if that_mod == mod || isInteractiveModule mod
                                                          then Just (return type_env)
                                                          else Nothing) }
        if_lenv = mkIfLclEnv mod (text "GHC error in desugarer lookup in" <+> ppr mod)
                             NotBoot
        real_span = realSrcLocSpan (mkRealSrcLoc (moduleNameFS (moduleName mod)) 1 1)
        gbl_env = DsGblEnv { ds_mod     = mod
                           , ds_fam_inst_env = fam_inst_env
                           , ds_gbl_rdr_env  = rdr_env
                           , ds_if_env  = (if_genv, if_lenv)
                           , ds_name_ppr_ctx = mkNamePprCtx ptc unit_env rdr_env
                           , ds_msgs    = msg_var
                           , ds_complete_matches = complete_matches
                           , ds_cc_st   = cc_st_var
                           , ds_next_wrapper_num = next_wrapper_num
                           }
        lcl_env = DsLclEnv { dsl_meta        = emptyNameEnv
                           , dsl_loc         = real_span
                           , dsl_nablas      = initNablas
                           , dsl_unspecables = mempty
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

-}

-- Make a new Id with the same print name, but different type, and new unique
newUniqueId :: Id -> Mult -> Type -> DsM Id
newUniqueId id = mkSysLocalOrCoVarM (occNameFS (nameOccName (idName id)))

duplicateLocalDs :: Id -> DsM Id
duplicateLocalDs old_local
  = do  { uniq <- newUnique
        ; return (setIdUnique old_local uniq) }

newPredVarDs :: PredType -> DsM Var
newPredVarDs
 = mkSysLocalOrCoVarM (fsLit "ds") ManyTy  -- like newSysLocalDs, but we allow covars

newSysLocalMDs, newFailLocalMDs :: Type -> DsM Id
-- Implicitly have ManyTy multiplicity, hence the "M"
newSysLocalMDs  = mkSysLocalM (fsLit "ds")    ManyTy
newFailLocalMDs = mkSysLocalM (fsLit "fail") ManyTy

newSysLocalsMDs :: [Type] -> DsM [Id]
newSysLocalsMDs = mapM newSysLocalMDs

newSysLocalDs :: Scaled Type -> DsM Id
newSysLocalDs (Scaled w t) = mkSysLocalM (fsLit "ds") w t

newSysLocalsDs :: [Scaled Type] -> DsM [Id]
newSysLocalsDs = mapM newSysLocalDs

{-
We can also reach out and either set/grab location information from
the @SrcSpan@ being carried around.
-}

getGhcModeDs :: DsM GhcMode
getGhcModeDs =  getDynFlags >>= return . ghcMode

-- | Get the current pattern match oracle state. See 'dsl_nablas'.
getPmNablas :: DsM Nablas
getPmNablas = do { env <- getLclEnv; return (dsl_nablas env) }

-- | Set the pattern match oracle state within the scope of the given action.
-- See 'dsl_nablas'.
updPmNablas :: Nablas -> DsM a -> DsM a
updPmNablas nablas = updLclEnv (\env -> env { dsl_nablas = nablas })

addUnspecables :: S.Set EvId -> DsM a -> DsM a
addUnspecables unspecables = updLclEnv (\env -> env{ dsl_unspecables = unspecables `mappend` dsl_unspecables env })

getUnspecables :: DsM (S.Set EvId)
getUnspecables = dsl_unspecables <$> getLclEnv

getSrcSpanDs :: DsM SrcSpan
getSrcSpanDs = do { env <- getLclEnv
                  ; return (RealSrcSpan (dsl_loc env) Strict.Nothing) }

putSrcSpanDs :: SrcSpan -> DsM a -> DsM a
putSrcSpanDs (UnhelpfulSpan {}) thing_inside
  = thing_inside
putSrcSpanDs (RealSrcSpan real_span _) thing_inside
  = updLclEnv (\ env -> env {dsl_loc = real_span}) thing_inside

putSrcSpanDsA :: EpAnn ann -> DsM a -> DsM a
putSrcSpanDsA loc = putSrcSpanDs (locA loc)

-- | Emit a diagnostic for the current source location. In case the diagnostic is a warning,
-- the latter will be ignored and discarded if the relevant 'WarningFlag' is not set in the DynFlags.
-- See Note [Discarding Messages] in 'GHC.Types.Error'.
diagnosticDs :: DsMessage -> DsM ()
diagnosticDs dsMessage
  = do { env <- getGblEnv
       ; loc <- getSrcSpanDs
       ; !diag_opts <- initDiagOpts <$> getDynFlags
       ; let msg = mkMsgEnvelope diag_opts loc (ds_name_ppr_ctx env) dsMessage
       ; updMutVar (ds_msgs env) (\ msgs -> msg `addMessage` msgs) }

addMessagesDs :: Messages DsMessage -> DsM ()
addMessagesDs msgs1
  = do { msg_var <- ds_msgs <$> getGblEnv
       ; msgs0 <- liftIO $ readIORef msg_var
       ; liftIO $ writeIORef msg_var (msgs0 `unionMessages` msgs1) }

-- | Issue an error, but return the expression for (), so that we can continue
-- reporting errors.
errDsCoreExpr :: DsMessage -> DsM CoreExpr
errDsCoreExpr msg
  = do { diagnosticDs msg
       ; return unitExpr }

failWithDs :: DsMessage -> DsM a
failWithDs msg
  = do  { diagnosticDs msg
        ; failM }

failDs :: DsM a
failDs = failM

captureMessagesDs :: DsM a -> DsM (Messages DsMessage, a)
captureMessagesDs thing_inside
  = do { msg_var <- liftIO $ newIORef emptyMessages
       ; res <- updGblEnv (\gbl -> gbl {ds_msgs = msg_var}) thing_inside
       ; msgs <- liftIO $ readIORef msg_var
       ; return (msgs, res) }

mkNamePprCtxDs :: DsM NamePprCtx
mkNamePprCtxDs = ds_name_ppr_ctx <$> getGblEnv

instance MonadThings (IOEnv (Env DsGblEnv DsLclEnv)) where
    lookupThing = dsLookupGlobal

dsLookupGlobal :: Name -> DsM TyThing
-- Very like GHC.Tc.Utils.Env.tcLookupGlobal
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

-- | The @COMPLETE@ pragmas that are in scope.
dsGetCompleteMatches :: DsM DsCompleteMatches
dsGetCompleteMatches = ds_complete_matches <$> getGblEnv

dsLookupMetaEnv :: Name -> DsM (Maybe DsMetaVal)
dsLookupMetaEnv name = do { env <- getLclEnv; return (lookupNameEnv (dsl_meta env) name) }

dsExtendMetaEnv :: DsMetaEnv -> DsM a -> DsM a
dsExtendMetaEnv menv thing_inside
  = updLclEnv (\env -> env { dsl_meta = dsl_meta env `plusNameEnv` menv }) thing_inside

discardWarningsDs :: DsM a -> DsM a
-- Ignore warnings inside the thing inside;
-- used to ignore inaccessible cases etc. inside generated code
discardWarningsDs thing_inside
  = do  { env <- getGblEnv
        ; old_msgs <- readTcRef (ds_msgs env)

        ; result <- thing_inside

        -- Revert messages to old_msgs
        ; writeTcRef (ds_msgs env) old_msgs

        ; return result }

-- | Inject a trace message into the compiled program. Whereas
-- pprTrace prints out information *while compiling*, pprRuntimeTrace
-- captures that information and causes it to be printed *at runtime*
-- using Debug.Trace.trace.
--
--   pprRuntimeTrace hdr doc expr
--
-- will produce an expression that looks like
--
--   trace (hdr + doc) expr
--
-- When using this to debug a module that Debug.Trace depends on,
-- it is necessary to import {-# SOURCE #-} Debug.Trace () in that
-- module. We could avoid this inconvenience by wiring in Debug.Trace.trace,
-- but that doesn't seem worth the effort and maintenance cost.
pprRuntimeTrace :: String   -- ^ header
                -> SDoc     -- ^ information to output
                -> CoreExpr -- ^ expression
                -> DsM CoreExpr
pprRuntimeTrace str doc expr = do
  traceId <- dsLookupGlobalId traceName
  unpackCStringId <- dsLookupGlobalId unpackCStringName
  dflags <- getDynFlags
  let message :: CoreExpr
      message = App (Var unpackCStringId) $
                Lit $ mkLitString $ showSDoc dflags (hang (text str) 4 doc)
  return $ mkApps (Var traceId) [Type (exprType expr), message, expr]

-- | See 'getCCIndexM'.
getCCIndexDsM :: FastString -> DsM CostCentreIndex
getCCIndexDsM = getCCIndexM ds_cc_st
