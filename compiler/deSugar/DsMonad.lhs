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
        
        assertDAPPLoaded, lookupDAPPRdrEnv, dsImportDecl, dsImportId, dsImportTyCon,

        DsMetaEnv, DsMetaVal(..), dsLookupMetaEnv, dsExtendMetaEnv,

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
import PrelNames
import Avail
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
import Maybes
import Control.Monad

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
        ds_if_env  :: (IfGblEnv, IfLclEnv),     -- Used for looking up global, 
                                                -- possibly-imported things
        ds_dph_env :: GlobalRdrEnv              -- exported entities of 'Data.Array.Parallel.Prim' iff
                                                -- '-fdph-*' flag was given (i.e., 'DynFlags.DPHBackend /=
                                                -- DPHNone'); otherwise, empty
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
        ; let dflags                   = hsc_dflags hsc_env
              (ds_gbl_env, ds_lcl_env) = mkDsEnvs dflags mod rdr_env type_env msg_var

        ; either_res <- initTcRnIf 'd' hsc_env ds_gbl_env ds_lcl_env $
                          loadDAPP dflags $
                            tryM thing_inside       -- Catch exceptions (= errors during desugaring)

        -- Display any errors and warnings 
        -- Note: if -Werror is used, we don't signal an error here.
        ; msgs <- readIORef msg_var

        ; let final_res | errorsFound dflags msgs = Nothing
                        | otherwise = case either_res of
                                        Right res -> Just res
                                        Left exn  -> pprPanic "initDs" (text (show exn))
                -- The (Left exn) case happens when the thing_inside throws
                -- a UserError exception.  Then it should have put an error
                -- message in msg_var, so we just discard the exception

        ; return (msgs, final_res) 
        }
  where
    -- Extend the global environment with a 'GlobalRdrEnv' containing the exported entities of
    -- 'Data.Array.Parallel.Prim' if '-fdph-*' specified.
    loadDAPP dflags thing_inside
      | Just pkg <- dphPackageMaybe dflags
      = do { rdr_env <- loadModule sdoc (dATA_ARRAY_PARALLEL_PRIM pkg)
           ; updGblEnv (\env -> env {ds_dph_env = rdr_env}) thing_inside
           }
      | otherwise
      = do { ifXOptM Opt_ParallelArrays (liftIO $ fatalErrorMsg dflags $ ptext selectBackendErrPA)
           ; ifDOptM Opt_Vectorise      (liftIO $ fatalErrorMsg dflags $ ptext selectBackendErrVect)
           ; thing_inside
           }

    sdoc = ptext (sLit "Internal Data Parallel Haskell interface 'Data.Array.Parallel.Prim'")

    selectBackendErrVect = sLit "To use -fvectorise select a DPH backend with -fdph-par or -fdph-seq"
    selectBackendErrPA   = sLit "To use -XParallelArrays select a DPH backend with -fdph-par or -fdph-seq"

initDsTc :: DsM a -> TcM a
initDsTc thing_inside
  = do  { this_mod <- getModule
        ; tcg_env  <- getGblEnv
        ; msg_var  <- getErrsVar
        ; dflags   <- getDOpts
        ; let type_env = tcg_type_env tcg_env
              rdr_env  = tcg_rdr_env tcg_env
              ds_envs  = mkDsEnvs dflags this_mod rdr_env type_env msg_var
        ; setEnvs ds_envs thing_inside
        }

mkDsEnvs :: DynFlags -> Module -> GlobalRdrEnv -> TypeEnv -> IORef Messages -> (DsGblEnv, DsLclEnv)
mkDsEnvs dflags mod rdr_env type_env msg_var
  = let if_genv = IfGblEnv { if_rec_types = Just (mod, return type_env) }
        if_lenv = mkIfLclEnv mod (ptext (sLit "GHC error in desugarer lookup in") <+> ppr mod)
        gbl_env = DsGblEnv { ds_mod     = mod
                           , ds_if_env  = (if_genv, if_lenv)
                           , ds_unqual  = mkPrintUnqualified dflags rdr_env
                           , ds_msgs    = msg_var
                           , ds_dph_env = emptyGlobalRdrEnv
                           }
        lcl_env = DsLclEnv { ds_meta = emptyNameEnv
                           , ds_loc  = noSrcSpan
                           }
    in (gbl_env, lcl_env)

-- Attempt to load the given module and return its exported entities if successful; otherwise, return an
-- empty environment.  See "Note [Loading Data.Array.Parallel.Prim]".
--
loadModule :: SDoc -> Module -> DsM GlobalRdrEnv
loadModule doc mod
  = do { env <- getGblEnv
       ; setEnvs (ds_if_env env) $ do
       { iface <- loadInterface doc mod ImportBySystem
       ;   case iface of
             Failed _err     -> return $ mkGlobalRdrEnv []
             Succeeded iface -> return $ mkGlobalRdrEnv . gresFromAvails prov . mi_exports $ iface
       } }
  where
    prov     = Imported [ImpSpec { is_decl = imp_spec, is_item = ImpAll }]
    imp_spec = ImpDeclSpec { is_mod = name, is_qual = True,
                             is_dloc = wiredInSrcSpan, is_as = name }
    name = moduleName mod
\end{code}

Note [Loading Data.Array.Parallel.Prim]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We generally attempt to load the interface of 'Data.Array.Parallel.Prim' when a DPH backend is selected.
However, while compiling packages containing a DPH backend, we will start out compiling the modules
'Data.Array.Parallel.Prim' depends on — i.e., when compiling these modules, the interface won't exist yet.
This is fine, as these modules do not use the vectoriser, but we need to ensure that GHC doesn't barf when
the interface is missing.  Instead of an error message, we just put an empty 'GlobalRdrEnv' into the
'DsM' state.


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
-- Complain if 'Data.Array.Parallel.Prim' wasn't loaded (and we are about to use it).
--
-- See "Note [Loading Data.Array.Parallel.Prim]".
--
assertDAPPLoaded :: DsM ()
assertDAPPLoaded 
  = do { env <- ds_dph_env <$> getGblEnv
       ; when (null $ occEnvElts env) $
           panic "'Data.Array.Parallel.Prim' not available; probably missing dependencies in DPH package"
       }

-- Look up a name exported by 'Data.Array.Parallel.Prim'.
--
lookupDAPPRdrEnv :: OccName -> DsM Name
lookupDAPPRdrEnv occ
  = do { env <- ds_dph_env <$> getGblEnv
       ; let gres = lookupGlobalRdrEnv env occ
       ; case gres of
           []    -> pprPanic "Name not found in 'Data.Array.Parallel.Prim':" (ppr occ)
           [gre] -> return $ gre_name gre
           _     -> pprPanic "Multiple definitions in 'Data.Array.Parallel.Prim':" (ppr occ)
       }

-- Find the thing repferred to by an imported name.
--
dsImportDecl :: Name -> DsM TyThing
dsImportDecl name
  = do { env <- getGblEnv
       ; setEnvs (ds_if_env env) $ do
       { mb_thing <- importDecl name
       ; case mb_thing of
           Failed err      -> failIfM err
           Succeeded thing -> return thing
       } }

dsImportId :: Name -> DsM Id
dsImportId name
  = tyThingId <$> dsImportDecl name

dsImportTyCon :: Name -> DsM TyCon
dsImportTyCon name
  = tyThingTyCon <$> dsImportDecl name
\end{code}

\begin{code}
dsLookupMetaEnv :: Name -> DsM (Maybe DsMetaVal)
dsLookupMetaEnv name = do { env <- getLclEnv; return (lookupNameEnv (ds_meta env) name) }

dsExtendMetaEnv :: DsMetaEnv -> DsM a -> DsM a
dsExtendMetaEnv menv thing_inside
  = updLclEnv (\env -> env { ds_meta = ds_meta env `plusNameEnv` menv }) thing_inside
\end{code}
