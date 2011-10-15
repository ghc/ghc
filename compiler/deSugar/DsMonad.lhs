%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

@DsMonad@: monadery used in desugaring

\begin{code}
module DsMonad (
        DsM, mapM, mapAndUnzipM,
        initDs, initDsTc, fixDs,
        foldlM, foldrM, ifDOptM, unsetDOptM, unsetWOptM,
        Applicative(..),(<$>),

        newLocalName,
        duplicateLocalDs, newSysLocalDs, newSysLocalsDs, newUniqueId,
        newFailLocalDs, newPredVarDs,
        getSrcSpanDs, putSrcSpanDs,
        getModuleDs,
        mkPrintUnqualifiedDs,
        newUnique, 
        UniqSupply, newUniqueSupply,
        getDOptsDs, getGhcModeDs, doptDs, woptDs,
        dsLookupGlobal, dsLookupGlobalId, dsLookupDPHId, dsLookupTyCon, dsLookupDataCon,

        DsMetaEnv, DsMetaVal(..), dsLookupMetaEnv, dsExtendMetaEnv,

        dsLoadModule,

        -- Warnings
        DsWarning, warnDs, failWithDs,

        -- Data types
        DsMatchContext(..),
        EquationInfo(..), MatchResult(..), DsWrapper, idDsWrapper,
        CanItFail(..), orFail
    ) where

import TcRnMonad
import CoreSyn
import HsSyn
import TcIface
import LoadIface
import RdrName
import HscTypes
import Bag
import DataCon
import TyCon
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

import Data.IORef
\end{code}

%************************************************************************
%*                                                                      *
                Data types for the desugarer
%*                                                                      *
%************************************************************************

\begin{code}
data DsMatchContext
  = DsMatchContext (HsMatchContext Name) SrcSpan
  deriving ()

data EquationInfo
  = EqnInfo { eqn_pats :: [Pat Id],     -- The patterns for an eqn
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
\end{code}


%************************************************************************
%*                                                                      *
                Monad stuff
%*                                                                      *
%************************************************************************

Now the mondo monad magic (yes, @DsM@ is a silly name)---carry around
a @UniqueSupply@ and some annotations, which
presumably include source-file location information:
\begin{code}
type DsM result = TcRnIf DsGblEnv DsLclEnv result

-- Compatibility functions
fixDs :: (a -> DsM a) -> DsM a
fixDs    = fixM

type DsWarning = (SrcSpan, SDoc)
        -- Not quite the same as a WarnMsg, we have an SDoc here 
        -- and we'll do the print_unqual stuff later on to turn it
        -- into a Doc.

data DsGblEnv = DsGblEnv {
        ds_mod     :: Module,                   -- For SCC profiling
        ds_unqual  :: PrintUnqualified,
        ds_msgs    :: IORef Messages,           -- Warning messages
        ds_if_env  :: (IfGblEnv, IfLclEnv)      -- Used for looking up global, 
                                                -- possibly-imported things
    }

data DsLclEnv = DsLclEnv {
        ds_meta    :: DsMetaEnv,        -- Template Haskell bindings
        ds_loc     :: SrcSpan           -- to put in pattern-matching error msgs
     }

-- Inside [| |] brackets, the desugarer looks 
-- up variables in the DsMetaEnv
type DsMetaEnv = NameEnv DsMetaVal

data DsMetaVal
   = Bound Id           -- Bound by a pattern inside the [| |]. 
                        -- Will be dynamically alpha renamed.
                        -- The Id has type THSyntax.Var

   | Splice (HsExpr Id) -- These bindings are introduced by
                        -- the PendingSplices on a HsBracketOut

initDs :: HscEnv
        -> Module -> GlobalRdrEnv -> TypeEnv
        -> DsM a
        -> IO (Messages, Maybe a)
-- Print errors and warnings, if any arise

initDs hsc_env mod rdr_env type_env thing_inside
  = do  { msg_var <- newIORef (emptyBag, emptyBag)
        ; let dflags = hsc_dflags hsc_env
        ; (ds_gbl_env, ds_lcl_env) <- mkDsEnvs dflags mod rdr_env type_env msg_var

        ; either_res <- initTcRnIf 'd' hsc_env ds_gbl_env ds_lcl_env $
                        tryM thing_inside       -- Catch exceptions (= errors during desugaring)

        -- Display any errors and warnings 
        -- Note: if -Werror is used, we don't signal an error here.
        ; msgs <- readIORef msg_var

        ; let final_res | errorsFound dflags msgs = Nothing
                        | otherwise = case either_res of
                                        Right res -> Just res
                                        Left exn -> pprPanic "initDs" (text (show exn))
                -- The (Left exn) case happens when the thing_inside throws
                -- a UserError exception.  Then it should have put an error
                -- message in msg_var, so we just discard the exception

        ; return (msgs, final_res) }

initDsTc :: DsM a -> TcM a
initDsTc thing_inside
  = do  { this_mod <- getModule
        ; tcg_env  <- getGblEnv
        ; msg_var  <- getErrsVar
        ; dflags   <- getDOpts
        ; let type_env = tcg_type_env tcg_env
              rdr_env  = tcg_rdr_env tcg_env
        ; ds_envs <- liftIO $ mkDsEnvs dflags this_mod rdr_env type_env msg_var
        ; setEnvs ds_envs thing_inside }

mkDsEnvs :: DynFlags -> Module -> GlobalRdrEnv -> TypeEnv -> IORef Messages -> IO (DsGblEnv, DsLclEnv)
mkDsEnvs dflags mod rdr_env type_env msg_var
  = do -- TODO: unnecessarily monadic
       let     if_genv = IfGblEnv { if_rec_types = Just (mod, return type_env) }
               if_lenv = mkIfLclEnv mod (ptext (sLit "GHC error in desugarer lookup in") <+> ppr mod)
               gbl_env = DsGblEnv { ds_mod = mod, 
                                    ds_if_env = (if_genv, if_lenv),
                                    ds_unqual = mkPrintUnqualified dflags rdr_env,
                                    ds_msgs = msg_var}
               lcl_env = DsLclEnv { ds_meta = emptyNameEnv, 
                                    ds_loc = noSrcSpan }

       return (gbl_env, lcl_env)
\end{code}

%************************************************************************
%*                                                                      *
                Operations in the monad
%*                                                                      *
%************************************************************************

And all this mysterious stuff is so we can occasionally reach out and
grab one or more names.  @newLocalDs@ isn't exported---exported
functions are defined with it.  The difference in name-strings makes
it easier to read debugging output.

\begin{code}
-- Make a new Id with the same print name, but different type, and new unique
newUniqueId :: Id -> Type -> DsM Id
newUniqueId id = mkSysLocalM (occNameFS (nameOccName (idName id)))

duplicateLocalDs :: Id -> DsM Id
duplicateLocalDs old_local 
  = do  { uniq <- newUnique
        ; return (setIdUnique old_local uniq) }

newPredVarDs :: PredType -> DsM Var
newPredVarDs pred
 = newSysLocalDs pred
 
newSysLocalDs, newFailLocalDs :: Type -> DsM Id
newSysLocalDs  = mkSysLocalM (fsLit "ds")
newFailLocalDs = mkSysLocalM (fsLit "fail")

newSysLocalsDs :: [Type] -> DsM [Id]
newSysLocalsDs tys = mapM newSysLocalDs tys
\end{code}

We can also reach out and either set/grab location information from
the @SrcSpan@ being carried around.

\begin{code}
getDOptsDs :: DsM DynFlags
getDOptsDs = getDOpts

doptDs :: DynFlag -> TcRnIf gbl lcl Bool
doptDs = doptM

woptDs :: WarningFlag -> TcRnIf gbl lcl Bool
woptDs = woptM

getGhcModeDs :: DsM GhcMode
getGhcModeDs =  getDOptsDs >>= return . ghcMode

getModuleDs :: DsM Module
getModuleDs = do { env <- getGblEnv; return (ds_mod env) }

getSrcSpanDs :: DsM SrcSpan
getSrcSpanDs = do { env <- getLclEnv; return (ds_loc env) }

putSrcSpanDs :: SrcSpan -> DsM a -> DsM a
putSrcSpanDs new_loc thing_inside = updLclEnv (\ env -> env {ds_loc = new_loc}) thing_inside

warnDs :: SDoc -> DsM ()
warnDs warn = do { env <- getGblEnv 
                 ; loc <- getSrcSpanDs
                 ; let msg = mkWarnMsg loc (ds_unqual env) 
                                      (ptext (sLit "Warning:") <+> warn)
                 ; updMutVar (ds_msgs env) (\ (w,e) -> (w `snocBag` msg, e)) }

failWithDs :: SDoc -> DsM a
failWithDs err 
  = do  { env <- getGblEnv 
        ; loc <- getSrcSpanDs
        ; let msg = mkErrMsg loc (ds_unqual env) err
        ; updMutVar (ds_msgs env) (\ (w,e) -> (w, e `snocBag` msg))
        ; failM }

mkPrintUnqualifiedDs :: DsM PrintUnqualified
mkPrintUnqualifiedDs = ds_unqual <$> getGblEnv
\end{code}

\begin{code}
instance MonadThings (IOEnv (Env DsGblEnv DsLclEnv)) where
    lookupThing = dsLookupGlobal

dsLookupGlobal :: Name -> DsM TyThing
-- Very like TcEnv.tcLookupGlobal
dsLookupGlobal name 
  = do  { env <- getGblEnv
        ; setEnvs (ds_if_env env)
                  (tcIfaceGlobal name) }

dsLookupGlobalId :: Name -> DsM Id
dsLookupGlobalId name 
  = tyThingId <$> dsLookupGlobal name

-- Looking up a global DPH 'Id' is like 'dsLookupGlobalId', but the package, in which the looked
-- up name is located, varies with the active DPH backend.
--
dsLookupDPHId :: (PackageId -> Name) -> DsM Id
dsLookupDPHId nameInPkg
  = do { dflags <- getDOpts
       ; case dphPackageMaybe dflags of
           Just pkg -> tyThingId <$> dsLookupGlobal (nameInPkg pkg)
           Nothing  -> failWithDs $ ptext err
       }
  where
    err = sLit "To use -XParallelArrays select a DPH backend with -fdph-par or -fdph-seq"

dsLookupTyCon :: Name -> DsM TyCon
dsLookupTyCon name
  = tyThingTyCon <$> dsLookupGlobal name

dsLookupDataCon :: Name -> DsM DataCon
dsLookupDataCon name
  = tyThingDataCon <$> dsLookupGlobal name
\end{code}

\begin{code}
dsLookupMetaEnv :: Name -> DsM (Maybe DsMetaVal)
dsLookupMetaEnv name = do { env <- getLclEnv; return (lookupNameEnv (ds_meta env) name) }

dsExtendMetaEnv :: DsMetaEnv -> DsM a -> DsM a
dsExtendMetaEnv menv thing_inside
  = updLclEnv (\env -> env { ds_meta = ds_meta env `plusNameEnv` menv }) thing_inside
\end{code}

\begin{code}
dsLoadModule :: SDoc -> Module -> DsM ()
dsLoadModule doc mod
  = do { env <- getGblEnv
       ; setEnvs (ds_if_env env)
                 (loadSysInterface doc mod >> return ())
       }
\end{code}

