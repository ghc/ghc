%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[DsMonad]{@DsMonad@: monadery used in desugaring}

\begin{code}
#include "HsVersions.h"

module DsMonad (
	SYN_IE(DsM),
	initDs, returnDs, thenDs, andDs, mapDs, listDs,
	mapAndUnzipDs, zipWithDs,
	uniqSMtoDsM,
	newTyVarsDs, cloneTyVarsDs,
	duplicateLocalDs, newSysLocalDs, newSysLocalsDs,
	newFailLocalDs,
	getSrcLocDs, putSrcLocDs,
	getModuleAndGroupDs,
	extendEnvDs, lookupEnvDs, 
	SYN_IE(DsIdEnv),

	dsShadowWarn, dsIncompleteWarn,
	SYN_IE(DsWarnings),
	DsMatchContext(..), DsMatchKind(..), pprDsWarnings,
        DsWarnFlavour -- Nuke with 1.4

    ) where

IMP_Ubiq()

import Bag		( emptyBag, snocBag, bagToList, Bag )
import CmdLineOpts	( opt_SccGroup )
import CoreSyn		( SYN_IE(CoreExpr) )
import CoreUtils	( substCoreExpr )
import HsSyn		( OutPat )
import Id		( mkSysLocal, mkIdWithNewUniq,
			  lookupIdEnv, growIdEnvList, GenId, SYN_IE(IdEnv),
			  SYN_IE(Id)
			)
import PprType		( GenType, GenTyVar )
import PprStyle		( PprStyle(..) )
import Outputable	( pprQuote, Outputable(..) )
import Pretty
import SrcLoc		( noSrcLoc, SrcLoc )
import TcHsSyn		( SYN_IE(TypecheckedPat) )
import Type             ( SYN_IE(Type) )
import TyVar		( nullTyVarEnv, cloneTyVar, GenTyVar{-instance Eq-}, SYN_IE(TyVar) )
import Unique		( Unique{-instances-} )
import UniqSupply	( splitUniqSupply, getUnique, getUniques,
			  mapUs, thenUs, returnUs, SYN_IE(UniqSM),
			  UniqSupply )
import Util		( assoc, mapAccumL, zipWithEqual, panic )

infixr 9 `thenDs`
\end{code}

Now the mondo monad magic (yes, @DsM@ is a silly name)---carry around
a @UniqueSupply@ and some annotations, which
presumably include source-file location information:
\begin{code}
type DsM result =
	UniqSupply
	-> SrcLoc			-- to put in pattern-matching error msgs
	-> (FAST_STRING, FAST_STRING)	-- "module"+"group" : for SCC profiling
	-> DsIdEnv
	-> DsWarnings
	-> (result, DsWarnings)

type DsWarnings = Bag (DsWarnFlavour, DsMatchContext)
					-- The desugarer reports matches which are
					-- completely shadowed or incomplete patterns
{-# INLINE andDs #-}
{-# INLINE thenDs #-}
{-# INLINE returnDs #-}

-- initDs returns the UniqSupply out the end (not just the result)

initDs  :: UniqSupply
	-> DsIdEnv
	-> FAST_STRING -- module name: for profiling; (group name: from switches)
	-> DsM a
	-> (a, DsWarnings)

initDs init_us env mod_name action
  = action init_us noSrcLoc module_and_group env emptyBag
  where
    module_and_group = (mod_name, grp_name)
    grp_name  = case opt_SccGroup of
		    Just xx -> _PK_ xx
		    Nothing -> mod_name	-- default: module name

thenDs :: DsM a -> (a -> DsM b) -> DsM b
andDs  :: (a -> a -> a) -> DsM a -> DsM a -> DsM a

thenDs m1 m2 us loc mod_and_grp env warns
  = case splitUniqSupply us		    of { (s1, s2) ->
    case (m1 s1 loc mod_and_grp env warns)  of { (result, warns1) ->
    m2 result s2 loc mod_and_grp env warns1}}

andDs combiner m1 m2 us loc mod_and_grp env warns
  = case splitUniqSupply us		    of { (s1, s2) ->
    case (m1 s1 loc mod_and_grp env warns)  of { (result1, warns1) ->
    case (m2 s2 loc mod_and_grp env warns1) of { (result2, warns2) ->
    (combiner result1 result2, warns2) }}}

returnDs :: a -> DsM a
returnDs result us loc mod_and_grp env warns = (result, warns)

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
newLocalDs :: FAST_STRING -> Type -> DsM Id
newLocalDs nm ty us loc mod_and_grp env warns
  = case (getUnique us) of { assigned_uniq ->
    (mkSysLocal nm assigned_uniq ty loc, warns) }

newSysLocalDs	    = newLocalDs SLIT("ds")
newSysLocalsDs tys  = mapDs (newLocalDs SLIT("ds")) tys
newFailLocalDs	    = newLocalDs SLIT("fail")

duplicateLocalDs :: Id -> DsM Id
duplicateLocalDs old_local us loc mod_and_grp env warns
  = case (getUnique us) of { assigned_uniq ->
    (mkIdWithNewUniq old_local assigned_uniq, warns) }

cloneTyVarsDs :: [TyVar] -> DsM [TyVar]
cloneTyVarsDs tyvars us loc mod_and_grp env warns
  = case (getUniques (length tyvars) us) of { uniqs ->
    (zipWithEqual "cloneTyVarsDs" cloneTyVar tyvars uniqs, warns) }
\end{code}

\begin{code}
newTyVarsDs :: [TyVar] -> DsM [TyVar]

newTyVarsDs tyvar_tmpls us loc mod_and_grp env warns
  = case (getUniques (length tyvar_tmpls) us) of { uniqs ->
    (zipWithEqual "newTyVarsDs" cloneTyVar tyvar_tmpls uniqs, warns) }
\end{code}

We can also reach out and either set/grab location information from
the @SrcLoc@ being carried around.
\begin{code}
uniqSMtoDsM :: UniqSM a -> DsM a

uniqSMtoDsM u_action us loc mod_and_grp env warns
  = (u_action us, warns)

getSrcLocDs :: DsM SrcLoc
getSrcLocDs us loc mod_and_grp env warns
  = (loc, warns)

putSrcLocDs :: SrcLoc -> DsM a -> DsM a
putSrcLocDs new_loc expr us old_loc mod_and_grp env warns
  = expr us new_loc mod_and_grp env warns

dsShadowWarn :: DsMatchContext -> DsM ()
dsShadowWarn cxt us loc mod_and_grp env warns
  = ((), warns `snocBag` (Shadowed, cxt))

dsIncompleteWarn :: DsMatchContext -> DsM ()
dsIncompleteWarn cxt us loc mod_and_grp env warns
  = ((), warns `snocBag` (Incomplete, cxt))
\end{code}

\begin{code}
getModuleAndGroupDs :: DsM (FAST_STRING, FAST_STRING)
getModuleAndGroupDs us loc mod_and_grp env warns
  = (mod_and_grp, warns)
\end{code}

\begin{code}
type DsIdEnv = IdEnv Id

extendEnvDs :: [(Id, Id)] -> DsM a -> DsM a

extendEnvDs pairs then_do us loc mod_and_grp old_env warns
  = then_do us loc mod_and_grp (growIdEnvList old_env pairs) warns

lookupEnvDs :: Id -> DsM Id
lookupEnvDs id us loc mod_and_grp env warns
  = (case (lookupIdEnv env id) of
      Nothing -> id
      Just xx -> xx,
     warns)
\end{code}

%************************************************************************
%*									*
%* type synonym EquationInfo and access functions for its pieces	*
%*									*
%************************************************************************

\begin{code}
data DsWarnFlavour = Shadowed | Incomplete deriving ()

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
  deriving ()

pprDsWarnings :: PprStyle -> DsWarnings -> Doc
pprDsWarnings sty warns
  = vcat (map pp_warn (bagToList warns))
  where
    pp_warn (flavour, NoMatchContext) = sep [ptext SLIT("Warning: Some match is"), 
					       case flavour of
							Shadowed   -> ptext SLIT("shadowed")
							Incomplete -> ptext SLIT("possibly incomplete")]

    pp_warn (flavour, DsMatchContext kind pats loc)
       = hang (hcat [ppr PprForUser loc, ptext SLIT(": ")])
	     4 (hang msg
		     4 (pp_match kind pats))
       where
	msg = case flavour of
		Shadowed   -> ptext SLIT("Warning: Pattern match(es) completely overlapped")     
		Incomplete -> ptext SLIT("Warning: Possibly incomplete patterns")

    pp_match (FunMatch fun) pats
      = hsep [ptext SLIT("in the definition of function"), ppr sty fun]

    pp_match CaseMatch pats
      = hang (ptext SLIT("in a group of case alternatives beginning:"))
	4 (ppr_pats pats)

    pp_match PatBindMatch pats
      = hang (ptext SLIT("in a pattern binding:"))
	4 (ppr_pats pats)

    pp_match LambdaMatch pats
      = hang (ptext SLIT("in a lambda abstraction:"))
	4 (ppr_pats pats)

    pp_match DoBindMatch pats
      = hang (ptext SLIT("in a `do' pattern binding:"))
	4 (ppr_pats pats)

    ppr_pats pats = pprQuote sty $ \ sty ->
		    sep [sep (map (ppr sty) pats), ptext SLIT("-> ...")]
\end{code}
