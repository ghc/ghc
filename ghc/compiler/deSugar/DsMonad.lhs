%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[DsMonad]{@DsMonad@: monadery used in desugaring}

\begin{code}
module DsMonad (
	DsM,
	initDs, returnDs, thenDs, andDs, mapDs, listDs,
	mapAndUnzipDs, zipWithDs, foldlDs,
	uniqSMtoDsM,
	newTyVarsDs, cloneTyVarsDs,
	duplicateLocalDs, newSysLocalDs, newSysLocalsDs,
	newFailLocalDs,
	getSrcLocDs, putSrcLocDs,
	getModuleAndGroupDs,
	getUniqueDs,
	dsLookupGlobalValue,

	ValueEnv,
	dsWarn, 
	DsWarnings,
	DsMatchContext(..), DsMatchKind(..), pprDsWarnings
    ) where

#include "HsVersions.h"

import Bag		( emptyBag, snocBag, bagToList, Bag )
import ErrUtils 	( WarnMsg )
import HsSyn		( OutPat )
import Id		( mkUserLocal, mkSysLocal, setIdUnique, Id )
import Name		( Module, Name, maybeWiredInIdName )
import Var		( TyVar, setTyVarUnique )
import VarEnv
import Outputable
import SrcLoc		( noSrcLoc, SrcLoc )
import TcHsSyn		( TypecheckedPat )
import TcEnv		( ValueEnv )
import Type             ( Type )
import UniqSupply	( initUs, splitUniqSupply, uniqFromSupply, uniqsFromSupply,
			  UniqSM, UniqSupply )
import Unique		( Unique )
import UniqFM		( lookupWithDefaultUFM )
import Util		( zipWithEqual )

infixr 9 `thenDs`
\end{code}

Now the mondo monad magic (yes, @DsM@ is a silly name)---carry around
a @UniqueSupply@ and some annotations, which
presumably include source-file location information:
\begin{code}
type DsM result =
	UniqSupply
        -> ValueEnv
	-> SrcLoc		 -- to put in pattern-matching error msgs
	-> (Module, Group)       -- module + group name : for SCC profiling
	-> DsWarnings
	-> (result, DsWarnings)

type DsWarnings = Bag WarnMsg           -- The desugarer reports matches which are
					-- completely shadowed or incomplete patterns

type Group = FAST_STRING

{-# INLINE andDs #-}
{-# INLINE thenDs #-}
{-# INLINE returnDs #-}

-- initDs returns the UniqSupply out the end (not just the result)

initDs  :: UniqSupply
	-> ValueEnv
	-> (Module, Group)      -- module name: for profiling; (group name: from switches)
	-> DsM a
	-> (a, DsWarnings)

initDs init_us genv module_and_group action
  = action init_us genv noSrcLoc module_and_group emptyBag

thenDs :: DsM a -> (a -> DsM b) -> DsM b
andDs  :: (a -> a -> a) -> DsM a -> DsM a -> DsM a

thenDs m1 m2 us genv loc mod_and_grp warns
  = case splitUniqSupply us		    of { (s1, s2) ->
    case (m1 s1 genv loc mod_and_grp warns)  of { (result, warns1) ->
    m2 result s2 genv loc mod_and_grp warns1}}

andDs combiner m1 m2 us genv loc mod_and_grp warns
  = case splitUniqSupply us		    of { (s1, s2) ->
    case (m1 s1 genv loc mod_and_grp warns)  of { (result1, warns1) ->
    case (m2 s2 genv loc mod_and_grp warns1) of { (result2, warns2) ->
    (combiner result1 result2, warns2) }}}

returnDs :: a -> DsM a
returnDs result us genv loc mod_and_grp warns = (result, warns)

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
newSysLocalDs, newFailLocalDs :: Type -> DsM Id
newSysLocalDs ty us genv loc mod_and_grp warns
  = case uniqFromSupply us of { assigned_uniq ->
    (mkSysLocal SLIT("ds") assigned_uniq ty, warns) }

newSysLocalsDs tys = mapDs newSysLocalDs tys

newFailLocalDs ty us genv loc mod_and_grp warns
  = case uniqFromSupply us of { assigned_uniq ->
    (mkSysLocal SLIT("fail") assigned_uniq ty, warns) }
	-- The UserLocal bit just helps make the code a little clearer

getUniqueDs :: DsM Unique
getUniqueDs us genv loc mod_and_grp warns
  = case (uniqFromSupply us) of { assigned_uniq ->
    (assigned_uniq, warns) }

duplicateLocalDs :: Id -> DsM Id
duplicateLocalDs old_local us genv loc mod_and_grp warns
  = case uniqFromSupply us of { assigned_uniq ->
    (setIdUnique old_local assigned_uniq, warns) }

cloneTyVarsDs :: [TyVar] -> DsM [TyVar]
cloneTyVarsDs tyvars us genv loc mod_and_grp warns
  = case uniqsFromSupply (length tyvars) us of { uniqs ->
    (zipWithEqual "cloneTyVarsDs" setTyVarUnique tyvars uniqs, warns) }
\end{code}

\begin{code}
newTyVarsDs :: [TyVar] -> DsM [TyVar]

newTyVarsDs tyvar_tmpls us genv loc mod_and_grp warns
  = case uniqsFromSupply (length tyvar_tmpls) us of { uniqs ->
    (zipWithEqual "newTyVarsDs" setTyVarUnique tyvar_tmpls uniqs, warns) }
\end{code}

We can also reach out and either set/grab location information from
the @SrcLoc@ being carried around.
\begin{code}
uniqSMtoDsM :: UniqSM a -> DsM a

uniqSMtoDsM u_action us genv loc mod_and_grp warns
  = (initUs us u_action, warns)

getSrcLocDs :: DsM SrcLoc
getSrcLocDs us genv loc mod_and_grp warns
  = (loc, warns)

putSrcLocDs :: SrcLoc -> DsM a -> DsM a
putSrcLocDs new_loc expr us genv old_loc mod_and_grp warns
  = expr us genv new_loc mod_and_grp warns

dsWarn :: WarnMsg -> DsM ()
dsWarn warn us genv loc mod_and_grp warns = ((), warns `snocBag` warn)

\end{code}

\begin{code}
getModuleAndGroupDs :: DsM (Module, Group)
getModuleAndGroupDs us genv loc mod_and_grp warns
  = (mod_and_grp, warns)
\end{code}

\begin{code}
dsLookupGlobalValue :: Name -> DsM Id
dsLookupGlobalValue name us genv loc mod_and_grp warns
  = case maybeWiredInIdName name of
	Just id -> (id, warns)
	Nothing -> (lookupWithDefaultUFM genv def name, warns)
  where
    def = pprPanic "tcLookupGlobalValue:" (ppr name)
\end{code}


%************************************************************************
%*									*
%* type synonym EquationInfo and access functions for its pieces	*
%*									*
%************************************************************************

\begin{code}
data DsMatchContext
  = DsMatchContext DsMatchKind [TypecheckedPat] SrcLoc
  | NoMatchContext
  deriving ()

data DsMatchKind
  = FunMatch Id
  | CaseMatch
  | LambdaMatch
  | PatBindMatch
  | DoBindMatch
  | ListCompMatch
  | LetMatch
  deriving ()

pprDsWarnings :: DsWarnings -> SDoc
pprDsWarnings warns = vcat (bagToList warns)
\end{code}
