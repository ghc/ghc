%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

@DsMonad@: monadery used in desugaring

\begin{code}
{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module DsMonad (
	DsM, mappM, mapAndUnzipM,
	initDs, initDsTc, returnDs, thenDs, listDs, fixDs, mapAndUnzipDs, 
	foldlDs, foldrDs, ifOptDs,

	newTyVarsDs, newLocalName,
	duplicateLocalDs, newSysLocalDs, newSysLocalsDs, newUniqueId,
	newFailLocalDs,
	getSrcSpanDs, putSrcSpanDs,
	getModuleDs,
	newUnique, 
	UniqSupply, newUniqueSupply,
	getDOptsDs, getGhcModeDs, doptDs,
	dsLookupGlobal, dsLookupGlobalId, dsLookupTyCon, dsLookupDataCon,
        dsLookupClass,

	DsMetaEnv, DsMetaVal(..), dsLookupMetaEnv, dsExtendMetaEnv,

	-- Warnings
	DsWarning, warnDs, failWithDs,

	-- Data types
	DsMatchContext(..),
	EquationInfo(..), MatchResult(..), DsWrapper, idDsWrapper,
	CanItFail(..), orFail
    ) where

#include "HsVersions.h"

import TcRnMonad
import CoreSyn
import HsSyn
import TcIface
import RdrName
import HscTypes
import Bag
import DataCon
import TyCon
import Class
import Id
import Module
import Var
import Outputable
import SrcLoc
import Type
import UniqSupply
import Name
import NameEnv
import OccName
import DynFlags
import ErrUtils

import Data.IORef

infixr 9 `thenDs`
\end{code}

%************************************************************************
%*									*
		Data types for the desugarer
%*									*
%************************************************************************

\begin{code}
data DsMatchContext
  = DsMatchContext (HsMatchContext Name) SrcSpan
  | NoMatchContext
  deriving ()

data EquationInfo
  = EqnInfo { eqn_pats :: [Pat Id],    	-- The patterns for an eqn
	      eqn_rhs  :: MatchResult }	-- What to do after match

type DsWrapper = CoreExpr -> CoreExpr
idDsWrapper e = e

-- The semantics of (match vs (EqnInfo wrap pats rhs)) is the MatchResult
--	\fail. wrap (case vs of { pats -> rhs fail })
-- where vs are not bound by wrap


-- A MatchResult is an expression with a hole in it
data MatchResult
  = MatchResult
	CanItFail	-- Tells whether the failure expression is used
	(CoreExpr -> DsM CoreExpr)
			-- Takes a expression to plug in at the
			-- failure point(s). The expression should
			-- be duplicatable!

data CanItFail = CanFail | CantFail

orFail CantFail CantFail = CantFail
orFail _        _	 = CanFail
\end{code}


%************************************************************************
%*									*
		Monad stuff
%*									*
%************************************************************************

Now the mondo monad magic (yes, @DsM@ is a silly name)---carry around
a @UniqueSupply@ and some annotations, which
presumably include source-file location information:
\begin{code}
type DsM result = TcRnIf DsGblEnv DsLclEnv result

-- Compatibility functions
fixDs    = fixM
thenDs   = thenM
returnDs = returnM
listDs   = sequenceM
foldlDs  = foldlM
foldrDs  = foldrM
mapAndUnzipDs = mapAndUnzipM
ifOptDs   = ifOptM

type DsWarning = (SrcSpan, SDoc)
	-- Not quite the same as a WarnMsg, we have an SDoc here 
	-- and we'll do the print_unqual stuff later on to turn it
	-- into a Doc.

data DsGblEnv = DsGblEnv {
	ds_mod	   :: Module,       		-- For SCC profiling
	ds_unqual  :: PrintUnqualified,
	ds_msgs    :: IORef Messages,		-- Warning messages
	ds_if_env  :: (IfGblEnv, IfLclEnv)	-- Used for looking up global, 
						-- possibly-imported things
    }

data DsLclEnv = DsLclEnv {
	ds_meta	   :: DsMetaEnv,	-- Template Haskell bindings
	ds_loc	   :: SrcSpan		-- to put in pattern-matching error msgs
     }

-- Inside [| |] brackets, the desugarer looks 
-- up variables in the DsMetaEnv
type DsMetaEnv = NameEnv DsMetaVal

data DsMetaVal
   = Bound Id		-- Bound by a pattern inside the [| |]. 
			-- Will be dynamically alpha renamed.
			-- The Id has type THSyntax.Var

   | Splice (HsExpr Id)	-- These bindings are introduced by
			-- the PendingSplices on a HsBracketOut

initDs  :: HscEnv
	-> Module -> GlobalRdrEnv -> TypeEnv
	-> DsM a
	-> IO (Maybe a)
-- Print errors and warnings, if any arise

initDs hsc_env mod rdr_env type_env thing_inside
  = do 	{ msg_var <- newIORef (emptyBag, emptyBag)
	; let dflags = hsc_dflags hsc_env
        ; (ds_gbl_env, ds_lcl_env) <- mkDsEnvs dflags mod rdr_env type_env msg_var

	; either_res <- initTcRnIf 'd' hsc_env ds_gbl_env ds_lcl_env $
		        tryM thing_inside	-- Catch exceptions (= errors during desugaring)

	-- Display any errors and warnings 
	-- Note: if -Werror is used, we don't signal an error here.
	; msgs <- readIORef msg_var
        ; printErrorsAndWarnings dflags msgs 

	; let final_res | errorsFound dflags msgs = Nothing
		        | otherwise = case either_res of
				        Right res -> Just res
				        Left exn -> pprPanic "initDs" (text (show exn))
		-- The (Left exn) case happens when the thing_inside throws
		-- a UserError exception.  Then it should have put an error
		-- message in msg_var, so we just discard the exception

	; return final_res }

initDsTc :: DsM a -> TcM a
initDsTc thing_inside
  = do	{ this_mod <- getModule
	; tcg_env  <- getGblEnv
	; msg_var  <- getErrsVar
        ; dflags   <- getDOpts
	; let type_env = tcg_type_env tcg_env
	      rdr_env  = tcg_rdr_env tcg_env
        ; ds_envs <- ioToIOEnv$ mkDsEnvs dflags this_mod rdr_env type_env msg_var
	; setEnvs ds_envs thing_inside }

mkDsEnvs :: DynFlags -> Module -> GlobalRdrEnv -> TypeEnv -> IORef Messages -> IO (DsGblEnv, DsLclEnv)
mkDsEnvs dflags mod rdr_env type_env msg_var
  = do 
       sites_var <- newIORef []
       let     if_genv = IfGblEnv { if_rec_types = Just (mod, return type_env) }
               if_lenv = mkIfLclEnv mod (ptext SLIT("GHC error in desugarer lookup in") <+> ppr mod)
               gbl_env = DsGblEnv { ds_mod = mod, 
    			            ds_if_env = (if_genv, if_lenv),
    			            ds_unqual = mkPrintUnqualified dflags rdr_env,
    			            ds_msgs = msg_var}
               lcl_env = DsLclEnv { ds_meta = emptyNameEnv, 
			            ds_loc = noSrcSpan }

       return (gbl_env, lcl_env)

\end{code}

%************************************************************************
%*									*
		Operations in the monad
%*									*
%************************************************************************

And all this mysterious stuff is so we can occasionally reach out and
grab one or more names.  @newLocalDs@ isn't exported---exported
functions are defined with it.  The difference in name-strings makes
it easier to read debugging output.

\begin{code}
-- Make a new Id with the same print name, but different type, and new unique
newUniqueId :: Name -> Type -> DsM Id
newUniqueId id ty
  = newUnique 	`thenDs` \ uniq ->
    returnDs (mkSysLocal (occNameFS (nameOccName id)) uniq ty)

duplicateLocalDs :: Id -> DsM Id
duplicateLocalDs old_local 
  = newUnique 	`thenDs` \ uniq ->
    returnDs (setIdUnique old_local uniq)

newSysLocalDs, newFailLocalDs :: Type -> DsM Id
newSysLocalDs ty
  = newUnique 	`thenDs` \ uniq ->
    returnDs (mkSysLocal FSLIT("ds") uniq ty)

newSysLocalsDs tys = mappM newSysLocalDs tys

newFailLocalDs ty 
  = newUnique 	`thenDs` \ uniq ->
    returnDs (mkSysLocal FSLIT("fail") uniq ty)
	-- The UserLocal bit just helps make the code a little clearer
\end{code}

\begin{code}
newTyVarsDs :: [TyVar] -> DsM [TyVar]
newTyVarsDs tyvar_tmpls 
  = newUniqueSupply	`thenDs` \ uniqs ->
    returnDs (zipWith setTyVarUnique tyvar_tmpls (uniqsFromSupply uniqs))
\end{code}

We can also reach out and either set/grab location information from
the @SrcSpan@ being carried around.

\begin{code}
getDOptsDs :: DsM DynFlags
getDOptsDs = getDOpts

doptDs :: DynFlag -> TcRnIf gbl lcl Bool
doptDs = doptM

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
				      (ptext SLIT("Warning:") <+> warn)
		 ; updMutVar (ds_msgs env) (\ (w,e) -> (w `snocBag` msg, e)) }
	    where

failWithDs :: SDoc -> DsM a
failWithDs err 
  = do	{ env <- getGblEnv 
	; loc <- getSrcSpanDs
	; let msg = mkErrMsg loc (ds_unqual env) err
	; updMutVar (ds_msgs env) (\ (w,e) -> (w, e `snocBag` msg))
	; failM }
	where
\end{code}

\begin{code}
dsLookupGlobal :: Name -> DsM TyThing
-- Very like TcEnv.tcLookupGlobal
dsLookupGlobal name 
  = do	{ env <- getGblEnv
	; setEnvs (ds_if_env env)
		  (tcIfaceGlobal name) }

dsLookupGlobalId :: Name -> DsM Id
dsLookupGlobalId name 
  = dsLookupGlobal name		`thenDs` \ thing ->
    returnDs (tyThingId thing)

dsLookupTyCon :: Name -> DsM TyCon
dsLookupTyCon name
  = dsLookupGlobal name		`thenDs` \ thing ->
    returnDs (tyThingTyCon thing)

dsLookupDataCon :: Name -> DsM DataCon
dsLookupDataCon name
  = dsLookupGlobal name		`thenDs` \ thing ->
    returnDs (tyThingDataCon thing)

dsLookupClass :: Name -> DsM Class
dsLookupClass name
  = dsLookupGlobal name         `thenDs` \ thing ->
    returnDs (tyThingClass thing)
\end{code}

\begin{code}
dsLookupMetaEnv :: Name -> DsM (Maybe DsMetaVal)
dsLookupMetaEnv name = do { env <- getLclEnv; return (lookupNameEnv (ds_meta env) name) }

dsExtendMetaEnv :: DsMetaEnv -> DsM a -> DsM a
dsExtendMetaEnv menv thing_inside
  = updLclEnv (\env -> env { ds_meta = ds_meta env `plusNameEnv` menv }) thing_inside
\end{code}
