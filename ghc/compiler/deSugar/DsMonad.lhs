%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[DesugarMonad]{@DesugarMonad@: monadery used in desugaring}

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
	getSwitchCheckerDs, ifSwitchSetDs,
	getModuleAndGroupDs,
	extendEnvDs, lookupEnvDs, lookupEnvWithDefaultDs,
	DsIdEnv(..),
	lookupId,

	dsShadowError,
	DsMatchContext(..), DsMatchKind(..), pprDsWarnings,

#ifdef DPH
	listDs,
#endif

	-- and to make the interface self-sufficient...
	Id, DataCon(..), SrcLoc, TyVar, TyVarTemplate, UniType, TauType(..),
	ThetaType(..), SigmaType(..), SplitUniqSupply, UniqSM(..),
	PlainCoreExpr(..), CoreExpr, GlobalSwitch, SwitchResult
	
	IF_ATTACK_PRAGMAS(COMMA lookupUFM COMMA lookupIdEnv)
	IF_ATTACK_PRAGMAS(COMMA mkIdWithNewUniq COMMA mkSysLocal)
	IF_ATTACK_PRAGMAS(COMMA unpackSrcLoc COMMA mkUniqueSupplyGrimily)
	IF_ATTACK_PRAGMAS(COMMA mkUniqueGrimily)
	IF_ATTACK_PRAGMAS(COMMA splitUniqSupply COMMA getSUnique)
    ) where

import AbsSyn
import AbsUniType	( cloneTyVarFromTemplate, cloneTyVar,
			  TyVar, TyVarTemplate, UniType, TauType(..),
			  ThetaType(..), SigmaType(..), Class
			  IF_ATTACK_PRAGMAS(COMMA cmpTyVar)
			)
import Bag
import CmdLineOpts	-- ( GlobalSwitch(..), SwitchResult(..), switchIsOn )
import Id		( mkIdWithNewUniq, mkSysLocal, Id, DataCon(..) )
import IdEnv		-- ( mkIdEnv, IdEnv )
import Maybes		( assocMaybe, Maybe(..) )
import Outputable
import PlainCore
import Pretty
import SrcLoc		( unpackSrcLoc, mkUnknownSrcLoc, SrcLoc )
import TyVarEnv		-- ( nullTyVarEnv, TyVarEnv )
import SplitUniq
import Unique
import Util

infixr 9 `thenDs`
\end{code}

Now the mondo monad magic (yes, @DsM@ is a silly name)---carry around
a @UniqueSupply@ and some annotations, which
presumably include source-file location information:
\begin{code}
type DsM result =
	SplitUniqSupply
	-> SrcLoc			    -- to put in pattern-matching error msgs
	-> (GlobalSwitch -> SwitchResult)   -- so we can consult global switches
	-> (FAST_STRING, FAST_STRING)		    -- "module"+"group" : for SCC profiling
	-> DsIdEnv
	-> DsWarnings
	-> (result, DsWarnings)

type DsWarnings = Bag DsMatchContext	-- The desugarer reports matches which are 
					-- completely shadowed

#ifdef __GLASGOW_HASKELL__
{-# INLINE andDs #-}
{-# INLINE thenDs #-}
{-# INLINE returnDs #-}
#endif

-- initDs returns the UniqSupply out the end (not just the result)

initDs  :: SplitUniqSupply
	-> DsIdEnv
	-> (GlobalSwitch -> SwitchResult)
	-> FAST_STRING -- module name: for profiling; (group name: from switches)
	-> DsM a
	-> (a, DsWarnings)

initDs init_us env sw_chkr mod_name action
  = action init_us mkUnknownSrcLoc sw_chkr module_and_group env emptyBag
  where
    module_and_group = (mod_name, grp_name)
    grp_name  = case (stringSwitchSet sw_chkr SccGroup) of
		    Just xx -> _PK_ xx
		    Nothing -> mod_name	-- default: module name

thenDs :: DsM a -> (a -> DsM b) -> DsM b
andDs  :: (a -> a -> a) -> DsM a -> DsM a -> DsM a

thenDs expr cont us loc sw_chkr mod_and_grp env warns
  = case splitUniqSupply us	    of { (s1, s2) ->
    case (expr s1 loc sw_chkr mod_and_grp env warns)  of { (result, warns1) ->
    cont result s2 loc sw_chkr mod_and_grp env warns1}}

andDs combiner m1 m2 us loc sw_chkr mod_and_grp env warns
  = case splitUniqSupply us	    of { (s1, s2) ->
    case (m1 s1 loc sw_chkr mod_and_grp env warns)    of { (result1, warns1) ->
    case (m2 s2 loc sw_chkr mod_and_grp env warns1)   of { (result2, warns2) ->
    (combiner result1 result2, warns2) }}}

returnDs :: a -> DsM a
returnDs result us loc sw_chkr mod_and_grp env warns = (result, warns)

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
\end{code}

And all this mysterious stuff is so we can occasionally reach out and
grab one or more names.  @newLocalDs@ isn't exported---exported
functions are defined with it.  The difference in name-strings makes
it easier to read debugging output.
\begin{code}
newLocalDs :: FAST_STRING -> UniType -> DsM Id
newLocalDs nm ty us loc sw_chkr mod_and_grp env warns
  = case (getSUnique us) of { assigned_uniq ->
    (mkSysLocal nm assigned_uniq ty loc, warns) }

newSysLocalDs	    = newLocalDs SLIT("ds")
newSysLocalsDs tys  = mapDs (newLocalDs SLIT("ds")) tys
newFailLocalDs	    = newLocalDs SLIT("fail")

duplicateLocalDs :: Id -> DsM Id
duplicateLocalDs old_local us loc sw_chkr mod_and_grp env warns
  = case (getSUnique us) of { assigned_uniq ->
    (mkIdWithNewUniq old_local assigned_uniq, warns) }

cloneTyVarsDs :: [TyVar] -> DsM [TyVar]
cloneTyVarsDs tyvars us loc sw_chkr mod_and_grp env warns
  = case (getSUniques (length tyvars) us) of { uniqs ->
    (zipWith cloneTyVar tyvars uniqs, warns) }
\end{code}

\begin{code}
newTyVarsDs :: [TyVarTemplate] -> DsM [TyVar]

newTyVarsDs tyvar_tmpls us loc sw_chkr mod_and_grp env warns
  = case (getSUniques (length tyvar_tmpls) us) of { uniqs ->
    (zipWith cloneTyVarFromTemplate tyvar_tmpls uniqs, warns) }
\end{code}

We can also reach out and either set/grab location information from
the @SrcLoc@ being carried around.
\begin{code}
uniqSMtoDsM :: UniqSM a -> DsM a

uniqSMtoDsM u_action us loc sw_chkr mod_and_grp env warns
  = let
    	us_to_use = mkUniqueSupplyGrimily us
    in
    (snd (u_action us_to_use), warns)

getSrcLocDs :: DsM (String, String)
getSrcLocDs us loc sw_chkr mod_and_grp env warns
  = case (unpackSrcLoc loc) of { (x,y) ->
    ((_UNPK_ x, _UNPK_ y), warns) }

putSrcLocDs :: SrcLoc -> DsM a -> DsM a
putSrcLocDs new_loc expr us old_loc sw_chkr mod_and_grp env warns
  = expr us new_loc sw_chkr mod_and_grp env warns

dsShadowError :: DsMatchContext -> DsM ()
dsShadowError cxt us loc sw_chkr mod_and_grp env warns
  = ((), warns `snocBag` cxt)
\end{code}

\begin{code}
getSwitchCheckerDs :: DsM (GlobalSwitch -> Bool)
getSwitchCheckerDs us loc sw_chkr mod_and_grp env warns
  = (switchIsOn sw_chkr, warns)

ifSwitchSetDs :: GlobalSwitch -> DsM a -> DsM a -> DsM a
ifSwitchSetDs switch then_ else_ us loc sw_chkr mod_and_grp env warns
  = (if switchIsOn sw_chkr switch then then_ else else_)
	us loc sw_chkr mod_and_grp env warns

getModuleAndGroupDs :: DsM (FAST_STRING, FAST_STRING)
getModuleAndGroupDs us loc sw_chkr mod_and_grp env warns
  = (mod_and_grp, warns)
\end{code}

\begin{code}
type DsIdEnv = IdEnv PlainCoreExpr

extendEnvDs :: [(Id, PlainCoreExpr)] -> DsM a -> DsM a

extendEnvDs pairs expr us loc sw_chkr mod_and_grp old_env warns
  = case splitUniqSupply us 	    of { (s1, s2) ->
    case (mapAccumL subst s1 pairs) of { (_, revised_pairs) ->
    expr s2 loc sw_chkr mod_and_grp (growIdEnvList old_env revised_pairs) warns
    }}
  where
    subst us (v, expr)
      = case splitUniqSupply us	of { (s1, s2) ->
	let
	    us_to_use = mkUniqueSupplyGrimily s1
	in
	case (substCoreExpr us_to_use old_env nullTyVarEnv expr) of { (_, expr2) ->
	(s2, (v, expr2)) }}

lookupEnvDs :: Id -> DsM (Maybe PlainCoreExpr)
lookupEnvDs id us loc sw_chkr mod_and_grp env warns
  = (lookupIdEnv env id, warns)
  -- Note: we don't assert anything about the Id
  -- being looked up.  There's not really anything
  -- much to say about it. (WDP 94/06)

lookupEnvWithDefaultDs :: Id -> PlainCoreExpr -> DsM PlainCoreExpr
lookupEnvWithDefaultDs id deflt us loc sw_chkr mod_and_grp env warns
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

    pp_arrow_dotdotdot = ppPStr SLIT("-> ...")
\end{code}
