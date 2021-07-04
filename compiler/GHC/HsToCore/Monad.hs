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

        duplicateLocalDs, newSysLocalDsNoLP, newSysLocalDs,
        newSysLocalsDsNoLP, newSysLocalsDs, newUniqueId,
        newFailLocalDs, newPredVarDs,
        getSrcSpanDs, putSrcSpanDs, putSrcSpanDsA,
        mkPrintUnqualifiedDs,
        newUnique,
        UniqSupply, newUniqueSupply,
        getGhcModeDs, dsGetFamInstEnvs,
        dsLookupGlobal, dsLookupGlobalId, dsLookupTyCon,
        dsLookupDataCon, dsLookupConLike,
        getCCIndexDsM,

        DsMetaEnv, DsMetaVal(..), dsGetMetaEnv, dsLookupMetaEnv, dsExtendMetaEnv,

        -- Getting and setting pattern match oracle states
        getPmNablas, updPmNablas,

        -- Get COMPLETE sets of a TyCon
        dsGetCompleteMatches,

        -- Warnings and errors
        DsWarning, diagnosticDs, errDsCoreExpr,
        failWithDs, failDs, discardWarningsDs,
        askNoErrsDs,

        -- Data types
        DsMatchContext(..),
        EquationInfo(..), MatchResult (..), runMatchResult, DsWrapper, idDsWrapper,

        -- Representation polymorphism
        dsNoLevPoly, dsNoLevPolyExpr,

        -- Trace injection
        pprRuntimeTrace
    ) where

import GHC.Prelude

import GHC.Driver.Env
import GHC.Driver.Session
import GHC.Driver.Ppr
import GHC.Driver.Config.Diagnostic

import GHC.Hs

import GHC.HsToCore.Types
import GHC.HsToCore.Errors.Types
import GHC.HsToCore.Pmc.Solver.Types (Nablas, initNablas)

import GHC.Core.FamInstEnv
import GHC.Core
import GHC.Core.Make  ( unitExpr )
import GHC.Core.Utils ( exprType, isExprLevPoly )
import GHC.Core.DataCon
import GHC.Core.ConLike
import GHC.Core.TyCon
import GHC.Core.Type
import GHC.Core.Multiplicity

import GHC.IfaceToCore

import GHC.Tc.Errors.Types ( LevityCheckProvenance(..) )
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.TcMType ( checkForLevPolyX )

import GHC.Builtin.Names

import GHC.Data.FastString

import GHC.Unit.Env
import GHC.Unit.External
import GHC.Unit.Module
import GHC.Unit.Module.ModGuts

import GHC.Types.Name.Reader
import GHC.Types.Basic ( Origin )
import GHC.Types.SourceFile
import GHC.Types.Id
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

import GHC.Utils.Error
import GHC.Utils.Outputable
import GHC.Utils.Panic
import qualified GHC.Data.Strict as Strict

import Data.IORef

{-
************************************************************************
*                                                                      *
                Data types for the desugarer
*                                                                      *
************************************************************************
-}

data DsMatchContext
  = DsMatchContext (HsMatchContext GhcRn) SrcSpan
  deriving ()

instance Outputable DsMatchContext where
  ppr (DsMatchContext hs_match ss) = ppr ss <+> pprMatchContext hs_match

data EquationInfo
  = EqnInfo { eqn_pats :: [Pat GhcTc]
              -- ^ The patterns for an equation
              --
              -- NB: We have /already/ applied 'decideBangHood' to
              -- these patterns.  See Note [decideBangHood] in "GHC.HsToCore.Utils"

            , eqn_orig :: Origin
              -- ^ Was this equation present in the user source?
              --
              -- This helps us avoid warnings on patterns that GHC elaborated.
              --
              -- For instance, the pattern @-1 :: Word@ gets desugared into
              -- @W# -1## :: Word@, but we shouldn't warn about an overflowed
              -- literal for /both/ of these cases.

            , eqn_rhs  :: MatchResult CoreExpr
              -- ^ What to do after match
            }

instance Outputable EquationInfo where
    ppr (EqnInfo pats _ _) = ppr pats

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

-- | Product is an "or" on falliblity---the combined match result is infallible
-- only if the left and right argument match results both were.
--
-- This is useful for combining a bunch of alternatives together and then
-- getting the overall falliblity of the entire group. See 'mkDataConCase' for
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
        -- and we'll do the print_unqual stuff later on to turn it
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
             complete_matches = hptCompleteSigs hsc_env         -- from the home package
                                ++ tcg_complete_matches tcg_env -- from the current module
                                ++ eps_complete_matches eps     -- from imports
             -- re-use existing next_wrapper_num to ensure uniqueness
             next_wrapper_num_var = tcg_next_wrapper_num tcg_env
       ; return $ mkDsEnvs unit_env this_mod rdr_env type_env fam_inst_env
                           msg_var cc_st_var next_wrapper_num_var complete_matches
       }

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
             complete_matches = hptCompleteSigs hsc_env     -- from the home package
                                ++ local_complete_matches  -- from the current module
                                ++ eps_complete_matches eps -- from imports

             bindsToIds (NonRec v _)   = [v]
             bindsToIds (Rec    binds) = map fst binds
             ids = concatMap bindsToIds binds

             envs  = mkDsEnvs unit_env this_mod rdr_env type_env
                              fam_inst_env msg_var cc_st_var
                              next_wrapper_num complete_matches
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

       ; let DsGblEnv { ds_mod = mod
                      , ds_fam_inst_env = fam_inst_env
                      , ds_gbl_rdr_env  = rdr_env }      = gbl
       -- This is *the* use of ds_gbl_rdr_env:
       -- Make sure the solver (used by the pattern-match overlap checker) has
       -- access to the GlobalRdrEnv and FamInstEnv for the module, so that it
       -- knows how to reduce type families, and which newtypes it can unwrap.


             DsLclEnv { dsl_loc = loc }                  = lcl

       ; (msgs, mb_ret) <- liftIO $ initTc hsc_env HsSrcFile False mod loc $
         updGblEnv (\tc_gbl -> tc_gbl { tcg_fam_inst_env = fam_inst_env
                                      , tcg_rdr_env      = rdr_env }) $
         thing_inside
       ; case mb_ret of
           Just ret -> pure ret
           Nothing  -> pprPanic "initTcDsForSolver" (vcat $ pprMsgEnvelopeBagWithLoc (getErrorMessages msgs)) }

mkDsEnvs :: UnitEnv -> Module -> GlobalRdrEnv -> TypeEnv -> FamInstEnv
         -> IORef (Messages DsMessage) -> IORef CostCentreState
         -> IORef (ModuleEnv Int) -> CompleteMatches
         -> (DsGblEnv, DsLclEnv)
mkDsEnvs unit_env mod rdr_env type_env fam_inst_env msg_var cc_st_var
         next_wrapper_num complete_matches
  = let if_genv = IfGblEnv { if_doc       = text "mkDsEnvs",
                             if_rec_types = Just (mod, return type_env) }
        if_lenv = mkIfLclEnv mod (text "GHC error in desugarer lookup in" <+> ppr mod)
                             NotBoot
        real_span = realSrcLocSpan (mkRealSrcLoc (moduleNameFS (moduleName mod)) 1 1)
        gbl_env = DsGblEnv { ds_mod     = mod
                           , ds_fam_inst_env = fam_inst_env
                           , ds_gbl_rdr_env  = rdr_env
                           , ds_if_env  = (if_genv, if_lenv)
                           , ds_unqual  = mkPrintUnqualified unit_env rdr_env
                           , ds_msgs    = msg_var
                           , ds_complete_matches = complete_matches
                           , ds_cc_st   = cc_st_var
                           , ds_next_wrapper_num = next_wrapper_num
                           }
        lcl_env = DsLclEnv { dsl_meta    = emptyNameEnv
                           , dsl_loc     = real_span
                           , dsl_nablas  = initNablas
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

Note [Representation polymorphism checking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
According to the "Levity Polymorphism" paper (PLDI '17),
representation polymorphism is forbidden in precisely two places:
in the type of a bound term-level argument, and in the type of an argument
to a function.
Note that the paper doesn't distinguish levity polymorphism, such as
  \(v::Levity). \(a::TYPE (BoxedRep v)). \(x::a). expr
from the more general representation polymorphism, as the BoxedRep
constructor of RuntimeRep didn't exist at the time.

The paper explains the restrictions more fully, but briefly:
expressions in these contexts need to be stored in registers, and it's
hard (read: impossible) to store something that's representation-polymorphic.

We cannot check for bad representation polymorphism conveniently
in the type checker, because we can't tell, a priori, which
representation metavariables will be solved.
At one point, I (Richard) thought we could check in the zonker, but it's hard
to know where precisely are the abstracted variables and the arguments. So
we check in the desugarer, the only place where we can see the Core code and
still report respectable syntax to the user. This covers the vast majority
of cases; see calls to GHC.HsToCore.Monad.dsNoLevPoly and friends.

Representation polymorphism is also prohibited in the types of binders, and the
desugarer checks for this in GHC-generated Ids. (The zonker handles
the user-writted ids in zonkIdBndr.) This is done in newSysLocalDsNoLP.
The newSysLocalDs variant is used in the vast majority of cases where
the binder is obviously not representation-polymorphic, omitting the check.
It would be nice to ASSERT that there is no representation polymorphism here,
but we can't, because of the fixM in GHC.HsToCore.Arrows. It's all OK, though:
Core Lint will catch an error here.

However, the desugarer is the wrong place for certain checks. In particular,
the desugarer can't report a sensible error message if an HsWrapper is malformed.
After all, GHC itself produced the HsWrapper. So we store some message text
in the appropriate HsWrappers (e.g. WpFun) that we can print out in the
desugarer.

There are a few more checks in places where Core is generated outside the
desugarer. For example, in datatype and class declarations, where
representation polymorphism is checked for during validity checking.
It would be nice to have one central place for all this, but that doesn't
seem possible while still reporting nice error messages.

-}

-- Make a new Id with the same print name, but different type, and new unique
newUniqueId :: Id -> Mult -> Type -> DsM Id
newUniqueId id = mk_local (occNameFS (nameOccName (idName id)))

duplicateLocalDs :: Id -> DsM Id
duplicateLocalDs old_local
  = do  { uniq <- newUnique
        ; return (setIdUnique old_local uniq) }

newPredVarDs :: PredType -> DsM Var
newPredVarDs
 = mkSysLocalOrCoVarM (fsLit "ds") Many  -- like newSysLocalDs, but we allow covars

newSysLocalDsNoLP, newSysLocalDs, newFailLocalDs :: Mult -> Type -> DsM Id
newSysLocalDsNoLP  = mk_local (fsLit "ds")

-- this variant should be used when the caller can be sure that the variable type
-- is not representation-polymorphic. It is necessary when the type
-- is knot-tied because of the fixM used in GHC.HsToCore.Arrows.
-- See Note [Representation polymorphism checking]
newSysLocalDs = mkSysLocalM (fsLit "ds")
newFailLocalDs = mkSysLocalM (fsLit "fail")
  -- the fail variable is used only in a situation where we can tell that
  -- representation polymorphism is impossible.

newSysLocalsDsNoLP, newSysLocalsDs :: [Scaled Type] -> DsM [Id]
newSysLocalsDsNoLP = mapM (\(Scaled w t) -> newSysLocalDsNoLP w t)
newSysLocalsDs = mapM (\(Scaled w t) -> newSysLocalDs w t)

mk_local :: FastString -> Mult -> Type -> DsM Id
mk_local fs w ty = do { dsNoLevPoly ty LevityCheckInVarType -- could improve the msg with another
                                                            -- parameter indicating context
                      ; mkSysLocalOrCoVarM fs w ty }

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

getSrcSpanDs :: DsM SrcSpan
getSrcSpanDs = do { env <- getLclEnv
                  ; return (RealSrcSpan (dsl_loc env) Strict.Nothing) }

putSrcSpanDs :: SrcSpan -> DsM a -> DsM a
putSrcSpanDs (UnhelpfulSpan {}) thing_inside
  = thing_inside
putSrcSpanDs (RealSrcSpan real_span _) thing_inside
  = updLclEnv (\ env -> env {dsl_loc = real_span}) thing_inside

putSrcSpanDsA :: SrcSpanAnn' ann -> DsM a -> DsM a
putSrcSpanDsA loc = putSrcSpanDs (locA loc)

-- | Emit a diagnostic for the current source location. In case the diagnostic is a warning,
-- the latter will be ignored and discarded if the relevant 'WarningFlag' is not set in the DynFlags.
-- See Note [Discarding Messages] in 'GHC.Types.Error'.
diagnosticDs :: DsMessage -> DsM ()
diagnosticDs dsMessage
  = do { env <- getGblEnv
       ; loc <- getSrcSpanDs
       ; !diag_opts <- initDiagOpts <$> getDynFlags
       ; let msg = mkMsgEnvelope diag_opts loc (ds_unqual env) dsMessage
       ; updMutVar (ds_msgs env) (\ msgs -> msg `addMessage` msgs) }

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

-- (askNoErrsDs m) runs m
-- If m fails,
--    then (askNoErrsDs m) fails
-- If m succeeds with result r,
--    then (askNoErrsDs m) succeeds with result (r, b),
--         where b is True iff m generated no errors
-- Regardless of success or failure,
--   propagate any errors/warnings generated by m
--
-- c.f. GHC.Tc.Utils.Monad.askNoErrs
askNoErrsDs :: DsM a -> DsM (a, Bool)
askNoErrsDs thing_inside
 = do { errs_var <- newMutVar emptyMessages
      ; env <- getGblEnv
      ; mb_res <- tryM $  -- Be careful to catch exceptions
                          -- so that we propagate errors correctly
                          -- (#13642)
                  setGblEnv (env { ds_msgs = errs_var }) $
                  thing_inside

      -- Propagate errors
      ; msgs <- readMutVar errs_var
      ; updMutVar (ds_msgs env) (unionMessages msgs)

      -- And return
      ; case mb_res of
           Left _    -> failM
           Right res -> do { let errs_found = errorsFound msgs
                           ; return (res, not errs_found) } }

mkPrintUnqualifiedDs :: DsM PrintUnqualified
mkPrintUnqualifiedDs = ds_unqual <$> getGblEnv

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
dsGetCompleteMatches :: DsM CompleteMatches
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

-- | Fail with an error message if the type is representation-polymorphic.
dsNoLevPoly :: Type -> LevityCheckProvenance -> DsM ()
-- See Note [Representation polymorphism checking]
dsNoLevPoly ty provenance =
  checkForLevPolyX (\ty -> failWithDs . DsLevityPolyInType ty) provenance ty

-- | Check an expression for representation polymorphism, failing if it is
-- representation-polymorphic.
dsNoLevPolyExpr :: CoreExpr -> LevityExprProvenance -> DsM ()
-- See Note [Representation polymorphism checking]
dsNoLevPolyExpr e provenance
  | isExprLevPoly e = diagnosticDs (DsLevityPolyInExpr e provenance)
  | otherwise       = return ()

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
