%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[DsMonad]{@DsMonad@: monadery used in desugaring}

\begin{code}
module DsMonad (
	DsM, mappM, mapAndUnzipM,
	initDs, returnDs, thenDs, listDs, fixDs, mapAndUnzipDs, 
	foldlDs, foldrDs,

	newTyVarsDs, newLocalName,
	duplicateLocalDs, newSysLocalDs, newSysLocalsDs, newUniqueId,
	newFailLocalDs,
	getSrcSpanDs, putSrcSpanDs,
	getModuleDs,
	newUnique, 
	UniqSupply, newUniqueSupply,
	getDOptsDs,
	dsLookupGlobal, dsLookupGlobalId, dsLookupTyCon, dsLookupDataCon,

	DsMetaEnv, DsMetaVal(..), dsLookupMetaEnv, dsExtendMetaEnv,

	-- Warnings
	DsWarning, dsWarn, 

	-- Data types
	DsMatchContext(..),
	EquationInfo(..), MatchResult(..), DsWrapper, idWrapper,
	CanItFail(..), orFail
    ) where

#include "HsVersions.h"

import TcRnMonad
import CoreSyn		( CoreExpr )
import HsSyn		( HsExpr, HsMatchContext, Pat )
import TcIface		( tcIfaceGlobal )
import RdrName		( GlobalRdrEnv )
import HscTypes		( TyThing(..), TypeEnv, HscEnv, 
			  tyThingId, tyThingTyCon, tyThingDataCon, unQualInScope )
import Bag		( emptyBag, snocBag, Bag )
import DataCon		( DataCon )
import TyCon		( TyCon )
import Id		( mkSysLocal, setIdUnique, Id )
import Module		( Module )
import Var		( TyVar, setTyVarUnique )
import Outputable
import SrcLoc		( noSrcSpan, SrcSpan )
import Type             ( Type )
import UniqSupply	( UniqSupply, uniqsFromSupply )
import Name		( Name, nameOccName )
import NameEnv
import OccName          ( occNameFS )
import DynFlags	( DynFlags )
import ErrUtils		( WarnMsg, mkWarnMsg )
import Bag		( mapBag )

import DATA_IOREF	( newIORef, readIORef )

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
  = EqnInfo { eqn_wrap :: DsWrapper,	-- Bindings
	      eqn_pats :: [Pat Id],    	-- The patterns for an eqn
	      eqn_rhs  :: MatchResult }	-- What to do after match

type DsWrapper = CoreExpr -> CoreExpr
idWrapper e = e

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


type DsWarning = (SrcSpan, SDoc)
	-- Not quite the same as a WarnMsg, we have an SDoc here 
	-- and we'll do the print_unqual stuff later on to turn it
	-- into a Doc.

data DsGblEnv = DsGblEnv {
	ds_mod	   :: Module,       		-- For SCC profiling
	ds_warns   :: IORef (Bag DsWarning),	-- Warning messages
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

-- initDs returns the UniqSupply out the end (not just the result)

initDs  :: HscEnv
	-> Module -> GlobalRdrEnv -> TypeEnv
	-> DsM a
	-> IO (a, Bag WarnMsg)

initDs hsc_env mod rdr_env type_env thing_inside
  = do 	{ warn_var <- newIORef emptyBag
	; let { if_genv = IfGblEnv { if_rec_types = Just (mod, return type_env) }
	      ; if_lenv = mkIfLclEnv mod (ptext SLIT("GHC error in desugarer lookup in") <+> ppr mod)
	      ; gbl_env = DsGblEnv { ds_mod = mod, 
				     ds_if_env = (if_genv, if_lenv),
				     ds_warns = warn_var }
	      ; lcl_env = DsLclEnv { ds_meta = emptyNameEnv, 
				     ds_loc = noSrcSpan } }

	; res <- initTcRnIf 'd' hsc_env gbl_env lcl_env thing_inside

	; warns <- readIORef warn_var
	; return (res, mapBag mk_warn warns)
	}
   where
    print_unqual = unQualInScope rdr_env

    mk_warn :: (SrcSpan,SDoc) -> WarnMsg
    mk_warn (loc,sdoc) = mkWarnMsg loc print_unqual sdoc
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

getModuleDs :: DsM Module
getModuleDs = do { env <- getGblEnv; return (ds_mod env) }

getSrcSpanDs :: DsM SrcSpan
getSrcSpanDs = do { env <- getLclEnv; return (ds_loc env) }

putSrcSpanDs :: SrcSpan -> DsM a -> DsM a
putSrcSpanDs new_loc thing_inside = updLclEnv (\ env -> env {ds_loc = new_loc}) thing_inside

dsWarn :: SDoc -> DsM ()
dsWarn warn = do { env <- getGblEnv 
		 ; loc <- getSrcSpanDs
		 ; updMutVar (ds_warns env) (`snocBag` (loc,msg)) }
	    where
	      msg = ptext SLIT("Warning:") <+> warn
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
\end{code}

\begin{code}
dsLookupMetaEnv :: Name -> DsM (Maybe DsMetaVal)
dsLookupMetaEnv name = do { env <- getLclEnv; return (lookupNameEnv (ds_meta env) name) }

dsExtendMetaEnv :: DsMetaEnv -> DsM a -> DsM a
dsExtendMetaEnv menv thing_inside
  = updLclEnv (\env -> env { ds_meta = ds_meta env `plusNameEnv` menv }) thing_inside
\end{code}


