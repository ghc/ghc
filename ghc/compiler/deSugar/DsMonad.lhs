%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[DsMonad]{@DsMonad@: monadery used in desugaring}

\begin{code}
module DsMonad (
	DsM,
	initDs, returnDs, thenDs, mapDs, listDs, fixDs,
	mapAndUnzipDs, zipWithDs, foldlDs,
	uniqSMtoDsM,
	newTyVarsDs, cloneTyVarsDs,
	duplicateLocalDs, newSysLocalDs, newSysLocalsDs, newUniqueId,
	newFailLocalDs,
	getSrcLocDs, putSrcLocDs,
	getModuleDs,
	getUniqueDs, getUniquesDs,
	UniqSupply, getUniqSupplyDs,
	getDOptsDs,
	dsLookupGlobal, dsLookupGlobalId, dsLookupTyCon, dsLookupDataCon,

	DsMetaEnv, DsMetaVal(..), dsLookupMetaEnv, dsExtendMetaEnv,

	dsWarn, 
	DsWarnings,
	DsMatchContext(..)
    ) where

#include "HsVersions.h"

import TcHsSyn		( TypecheckedPat, TypecheckedMatchContext, TypecheckedHsExpr )
import HscTypes		( TyThing(..) )
import Bag		( emptyBag, snocBag, Bag )
import DataCon		( DataCon )
import TyCon		( TyCon )
import DataCon		( DataCon )
import Id		( mkSysLocal, setIdUnique, Id )
import Module		( Module )
import Var		( TyVar, setTyVarUnique )
import Outputable
import SrcLoc		( noSrcLoc, SrcLoc )
import Type             ( Type )
import UniqSupply	( initUs_, getUniqueUs, getUniquesUs, thenUs, returnUs, 
			  fixUs, UniqSM, UniqSupply, getUs )
import Unique		( Unique ) 
import Name		( Name, nameOccName )
import NameEnv
import OccName          ( occNameFS )
import CmdLineOpts	( DynFlags )

infixr 9 `thenDs`
\end{code}

Now the mondo monad magic (yes, @DsM@ is a silly name)---carry around
a @UniqueSupply@ and some annotations, which
presumably include source-file location information:
\begin{code}
newtype DsM result
  = DsM (DsEnv -> DsWarnings -> UniqSM (result, DsWarnings))

unDsM (DsM x) = x	

data DsEnv = DsEnv {
	ds_dflags :: DynFlags,
	ds_globals :: Name -> TyThing,	-- Lookup well-known Ids
	ds_meta	   :: DsMetaEnv,	-- Template Haskell bindings
	ds_loc	   :: SrcLoc,		-- to put in pattern-matching error msgs
	ds_mod	   :: Module       	-- module: for SCC profiling
     }

-- Inside [| |] brackets, the desugarer looks 
-- up variables in the DsMetaEnv
type DsMetaEnv = NameEnv DsMetaVal

data DsMetaVal
   = Bound Id		-- Bound by a pattern inside the [| |]. 
			-- Will be dynamically alpha renamed.
			-- The Id has type String

   | Splice TypecheckedHsExpr	-- These bindings are introduced by
				-- the PendingSplices on a HsBracketOut

instance Monad DsM where
  return = returnDs
  (>>=)  = thenDs

type DsWarnings = Bag DsWarning         -- The desugarer reports matches which are
					-- completely shadowed or incomplete patterns
type DsWarning = (SrcLoc, SDoc)

{-# INLINE thenDs #-}
{-# INLINE returnDs #-}

-- initDs returns the UniqSupply out the end (not just the result)

initDs  :: DynFlags
	-> UniqSupply
	-> (Name -> TyThing)
	-> Module   -- module name: for profiling
	-> DsM a
	-> (a, DsWarnings)

initDs dflags init_us lookup mod (DsM action)
  = initUs_ init_us (action ds_env emptyBag)
  where
    ds_env = DsEnv { ds_dflags = dflags, ds_globals = lookup,
		     ds_loc = noSrcLoc, ds_mod = mod,
		     ds_meta = emptyNameEnv }

thenDs :: DsM a -> (a -> DsM b) -> DsM b

thenDs (DsM m1) m2 = DsM( \ env warns ->
    m1 env warns	`thenUs` \ (result, warns1) ->
    unDsM (m2 result) env warns1)

returnDs :: a -> DsM a
returnDs result = DsM (\ env warns -> returnUs (result, warns))

fixDs :: (a -> DsM a) -> DsM a
fixDs f = DsM (\env warns -> fixUs (\ ~(a, _warns') -> unDsM (f a) env warns))

listDs :: [DsM a] -> DsM [a]
listDs []     = returnDs []
listDs (x:xs)
  = x		`thenDs` \ r  ->
    listDs xs	`thenDs` \ rs ->
    returnDs (r:rs)

mapDs :: (a -> DsM b) -> [a] -> DsM [b]

mapDs f []     = returnDs []
mapDs f (x:xs)
  = f x		`thenDs` \ r  ->
    mapDs f xs	`thenDs` \ rs ->
    returnDs (r:rs)

foldlDs :: (a -> b -> DsM a) -> a -> [b] -> DsM a

foldlDs k z []     = returnDs z
foldlDs k z (x:xs) = k z x `thenDs` \ r ->
		     foldlDs k r xs

mapAndUnzipDs :: (a -> DsM (b, c)) -> [a] -> DsM ([b], [c])

mapAndUnzipDs f []     = returnDs ([], [])
mapAndUnzipDs f (x:xs)
  = f x		    	`thenDs` \ (r1, r2)  ->
    mapAndUnzipDs f xs	`thenDs` \ (rs1, rs2) ->
    returnDs (r1:rs1, r2:rs2)

zipWithDs :: (a -> b -> DsM c) -> [a] -> [b] -> DsM [c]

zipWithDs f []	   ys = returnDs []
zipWithDs f (x:xs) (y:ys)
  = f x y		`thenDs` \ r  ->
    zipWithDs f xs ys	`thenDs` \ rs ->
    returnDs (r:rs)
\end{code}

And all this mysterious stuff is so we can occasionally reach out and
grab one or more names.  @newLocalDs@ isn't exported---exported
functions are defined with it.  The difference in name-strings makes
it easier to read debugging output.

\begin{code}
uniqSMtoDsM :: UniqSM a -> DsM a
uniqSMtoDsM u_action = DsM(\ env warns -> 
	u_action	`thenUs` \ res ->
	returnUs (res, warns))

    
getUniqueDs :: DsM Unique
getUniqueDs = DsM (\ env warns -> 
    getUniqueUs		`thenUs` \ uniq -> 
    returnUs (uniq, warns))

getUniquesDs :: DsM [Unique]
getUniquesDs = DsM(\ env warns -> 
    getUniquesUs		`thenUs` \ uniqs -> 
    returnUs (uniqs, warns))

getUniqSupplyDs :: DsM UniqSupply
getUniqSupplyDs = DsM(\ env warns -> 
    getUs		`thenUs` \ uniqs -> 
    returnUs (uniqs, warns))

-- Make a new Id with the same print name, but different type, and new unique
newUniqueId :: Name -> Type -> DsM Id
newUniqueId id ty
  = getUniqueDs 	`thenDs` \ uniq ->
    returnDs (mkSysLocal (occNameFS (nameOccName id)) uniq ty)

duplicateLocalDs :: Id -> DsM Id
duplicateLocalDs old_local 
  = getUniqueDs 	`thenDs` \ uniq ->
    returnDs (setIdUnique old_local uniq)

newSysLocalDs, newFailLocalDs :: Type -> DsM Id
newSysLocalDs ty
  = getUniqueDs 	`thenDs` \ uniq ->
    returnDs (mkSysLocal FSLIT("ds") uniq ty)

newSysLocalsDs tys = mapDs newSysLocalDs tys

newFailLocalDs ty 
  = getUniqueDs 	`thenDs` \ uniq ->
    returnDs (mkSysLocal FSLIT("fail") uniq ty)
	-- The UserLocal bit just helps make the code a little clearer
\end{code}

\begin{code}
cloneTyVarsDs :: [TyVar] -> DsM [TyVar]
cloneTyVarsDs tyvars 
  = getUniquesDs	`thenDs` \ uniqs ->
    returnDs (zipWith setTyVarUnique tyvars uniqs)

newTyVarsDs :: [TyVar] -> DsM [TyVar]
newTyVarsDs tyvar_tmpls 
  = getUniquesDs	`thenDs` \ uniqs ->
    returnDs (zipWith setTyVarUnique tyvar_tmpls uniqs)
\end{code}

We can also reach out and either set/grab location information from
the @SrcLoc@ being carried around.

\begin{code}
getDOptsDs :: DsM DynFlags
getDOptsDs = DsM(\ env warns -> returnUs (ds_dflags env, warns))

getModuleDs :: DsM Module
getModuleDs = DsM(\ env warns -> returnUs (ds_mod env, warns))

getSrcLocDs :: DsM SrcLoc
getSrcLocDs = DsM(\ env warns -> returnUs (ds_loc env, warns))

putSrcLocDs :: SrcLoc -> DsM a -> DsM a
putSrcLocDs new_loc (DsM expr) = DsM(\ env warns ->
    expr (env { ds_loc = new_loc }) warns)

dsWarn :: DsWarning -> DsM ()
dsWarn warn = DsM(\ env warns -> returnUs ((), warns `snocBag` warn))
\end{code}

\begin{code}
dsLookupGlobal :: Name -> DsM TyThing
dsLookupGlobal name 
  = DsM(\ env warns -> returnUs (ds_globals env name, warns))

dsLookupGlobalId :: Name -> DsM Id
dsLookupGlobalId name 
  = dsLookupGlobal name		`thenDs` \ thing ->
    returnDs $ case thing of
		AnId id -> id
		other   -> pprPanic "dsLookupGlobalId" (ppr name)

dsLookupTyCon :: Name -> DsM TyCon
dsLookupTyCon name
  = dsLookupGlobal name		`thenDs` \ thing ->
    returnDs $ case thing of
		 ATyCon tc -> tc
		 other     -> pprPanic "dsLookupTyCon" (ppr name)

dsLookupDataCon :: Name -> DsM DataCon
dsLookupDataCon name
  = dsLookupGlobal name		`thenDs` \ thing ->
    returnDs $ case thing of
		 ADataCon dc -> dc
		 other       -> pprPanic "dsLookupDataCon" (ppr name)
\end{code}

\begin{code}
dsLookupMetaEnv :: Name -> DsM (Maybe DsMetaVal)
dsLookupMetaEnv name = DsM(\ env warns -> returnUs (lookupNameEnv (ds_meta env) name, warns))

dsExtendMetaEnv :: DsMetaEnv -> DsM a -> DsM a
dsExtendMetaEnv menv (DsM m)
  = DsM (\ env warns -> m (env { ds_meta = ds_meta env `plusNameEnv` menv }) warns)
\end{code}


%************************************************************************
%*									*
\subsection{Type synonym @EquationInfo@ and access functions for its pieces}
%*									*
%************************************************************************

\begin{code}
data DsMatchContext
  = DsMatchContext TypecheckedMatchContext [TypecheckedPat] SrcLoc
  | NoMatchContext
  deriving ()
\end{code}
