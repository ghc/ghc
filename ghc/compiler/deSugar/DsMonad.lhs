%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[DsMonad]{@DsMonad@: monadery used in desugaring}

\begin{code}
#include "HsVersions.h"

module DsMonad (
	DsM(..),
	initDs, returnDs, thenDs, andDs, mapDs, listDs,
	mapAndUnzipDs, zipWithDs,
	uniqSMtoDsM,
	newTyVarsDs, cloneTyVarsDs,
	duplicateLocalDs, newSysLocalDs, newSysLocalsDs,
	newFailLocalDs,
	getSrcLocDs, putSrcLocDs,
	getModuleAndGroupDs,
	extendEnvDs, lookupEnvDs, lookupEnvWithDefaultDs,
	DsIdEnv(..),
	lookupId,

	dsShadowError,
	DsMatchContext(..), DsMatchKind(..), pprDsWarnings
    ) where

IMP_Ubiq()

import Bag		( emptyBag, snocBag, bagToList )
import CmdLineOpts	( opt_SccGroup )
import CoreSyn		( SYN_IE(CoreExpr) )
import CoreUtils	( substCoreExpr )
import HsSyn		( OutPat )
import Id		( mkSysLocal, mkIdWithNewUniq,
			  lookupIdEnv, growIdEnvList, GenId, SYN_IE(IdEnv)
			)
import PprType		( GenType, GenTyVar )
import PprStyle		( PprStyle(..) )
import Pretty
import SrcLoc		( unpackSrcLoc, mkUnknownSrcLoc, SrcLoc )
import TcHsSyn		( TypecheckedPat(..) )
import TyVar		( nullTyVarEnv, cloneTyVar, GenTyVar{-instance Eq-} )
import Unique		( Unique{-instances-} )
import UniqSupply	( splitUniqSupply, getUnique, getUniques,
			  mapUs, thenUs, returnUs, SYN_IE(UniqSM) )
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

type DsWarnings = Bag DsMatchContext	-- The desugarer reports matches which are
					-- completely shadowed
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
  = action init_us mkUnknownSrcLoc module_and_group env emptyBag
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

zipWithDs f []	   [] = returnDs []
zipWithDs f (x:xs) (y:ys)
  = f x y		`thenDs` \ r  ->
    zipWithDs f xs ys	`thenDs` \ rs ->
    returnDs (r:rs)
-- Note: crashes if lists not equal length (like zipWithEqual)
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

getSrcLocDs :: DsM (String, String)
getSrcLocDs us loc mod_and_grp env warns
  = case (unpackSrcLoc loc) of { (x,y) ->
    ((_UNPK_ x, _UNPK_ y), warns) }

putSrcLocDs :: SrcLoc -> DsM a -> DsM a
putSrcLocDs new_loc expr us old_loc mod_and_grp env warns
  = expr us new_loc mod_and_grp env warns

dsShadowError :: DsMatchContext -> DsM ()
dsShadowError cxt us loc mod_and_grp env warns
  = ((), warns `snocBag` cxt)
\end{code}

\begin{code}
getModuleAndGroupDs :: DsM (FAST_STRING, FAST_STRING)
getModuleAndGroupDs us loc mod_and_grp env warns
  = (mod_and_grp, warns)
\end{code}

\begin{code}
type DsIdEnv = IdEnv CoreExpr

extendEnvDs :: [(Id, CoreExpr)] -> DsM a -> DsM a

extendEnvDs pairs then_do us loc mod_and_grp old_env warns
  = case splitUniqSupply us 	    of { (s1, s2) ->
    let
	revised_pairs = subst_all pairs s1
    in
    then_do s2 loc mod_and_grp (growIdEnvList old_env revised_pairs) warns
    }
  where
    subst_all pairs = mapUs subst pairs

    subst (v, expr)
      = substCoreExpr old_env nullTyVarEnv expr `thenUs` \ new_expr ->
	returnUs (v, new_expr)

lookupEnvDs :: Id -> DsM (Maybe CoreExpr)
lookupEnvDs id us loc mod_and_grp env warns
  = (lookupIdEnv env id, warns)
  -- Note: we don't assert anything about the Id
  -- being looked up.  There's not really anything
  -- much to say about it. (WDP 94/06)

lookupEnvWithDefaultDs :: Id -> CoreExpr -> DsM CoreExpr
lookupEnvWithDefaultDs id deflt us loc mod_and_grp env warns
  = (case (lookupIdEnv env id) of
      Nothing -> deflt
      Just xx -> xx,
     warns)

lookupId :: [(Id, a)] -> Id -> a
lookupId env id
  = assoc "lookupId" env id
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

data DsMatchKind
  = FunMatch Id
  | CaseMatch
  | LambdaMatch
  | PatBindMatch
  | DoBindMatch

pprDsWarnings :: PprStyle -> Bag DsMatchContext -> Pretty
pprDsWarnings sty warns
  = ppAboves (map pp_cxt (bagToList warns))
  where
    pp_cxt NoMatchContext = ppPStr SLIT("Some match is shadowed; I don't know what")
    pp_cxt (DsMatchContext kind pats loc)
      = ppHang (ppBesides [ppr PprForUser loc, ppPStr SLIT(": ")])
	     4 (ppHang (ppPStr SLIT("Pattern match(es) completely overlapped:"))
		     4 (pp_match kind pats))

    pp_match (FunMatch fun) pats
      = ppHang (ppr sty fun)
	4 (ppSep [ppSep (map (ppr sty) pats), ppPStr SLIT("= ...")])

    pp_match CaseMatch pats
      = ppHang (ppPStr SLIT("in a case alternative:"))
	4 (ppSep [ppSep (map (ppr sty) pats), pp_arrow_dotdotdot])

    pp_match PatBindMatch pats
      = ppHang (ppPStr SLIT("in a pattern binding:"))
	4 (ppSep [ppSep (map (ppr sty) pats), pp_arrow_dotdotdot])

    pp_match LambdaMatch pats
      = ppHang (ppPStr SLIT("in a lambda abstraction:"))
	4 (ppSep [ppSep (map (ppr sty) pats), pp_arrow_dotdotdot])

    pp_match DoBindMatch pats
      = ppHang (ppPStr SLIT("in a `do' pattern binding:"))
	4 (ppSep [ppSep (map (ppr sty) pats), pp_arrow_dotdotdot])

    pp_arrow_dotdotdot = ppPStr SLIT("-> ...")
\end{code}
