%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[SimplCore]{Driver for simplifying @Core@ programs}

\begin{code}
module SimplCore ( core2core ) where

#include "HsVersions.h"

import CmdLineOpts	( CoreToDo(..), SimplifierSwitch(..), 
			  SwitchResult, switchIsOn,
			  opt_D_dump_occur_anal,
			  opt_D_dump_simpl_iterations,
			  opt_D_simplifier_stats,
			  opt_D_dump_simpl,
			  opt_D_verbose_core2core,
			  opt_D_dump_occur_anal
			)
import CoreLint		( beginPass, endPass )
import CoreSyn
import PprCore		( pprCoreBindings )
import OccurAnal	( occurAnalyseBinds )
import CoreUtils	( exprIsTrivial, coreExprType )
import Simplify		( simplBind )
import SimplUtils	( etaCoreExpr, findDefault )
import SimplMonad
import CoreUnfold
import Const		( Con(..), Literal(..), literalType, mkMachInt )
import ErrUtils		( dumpIfSet )
import FloatIn		( floatInwards )
import FloatOut		( floatOutwards )
import Id		( Id, mkSysLocal, mkUserId, isBottomingId,
			  idType, setIdType, idName, idInfo, idDetails
			)
import IdInfo		( InlinePragInfo(..), specInfo, setSpecInfo,
			  inlinePragInfo, setInlinePragInfo,
			  setUnfoldingInfo
			)
import VarEnv
import VarSet
import Name		( mkLocalName, tidyOccName, tidyTopName, initTidyOccEnv, isExported,
			  Module, NamedThing(..), OccName
			)
import TyCon		( TyCon, isDataTyCon )
import PrimOp		( PrimOp(..) )
import PrelInfo		( unpackCStringId, unpackCString2Id,
			  integerZeroId, integerPlusOneId,
			  integerPlusTwoId, integerMinusOneId,
			  int2IntegerId, addr2IntegerId
			)
import Type		( Type, splitAlgTyConApp_maybe, 
			  isUnLiftedType, mkTyVarTy, 
			  tidyType, tidyTypes, tidyTopType, tidyTyVar, tidyTyVars,
			  Type
			)
import Class		( Class, classSelIds )
import TysWiredIn	( isIntegerTy )
import LiberateCase	( liberateCase )
import SAT		( doStaticArgs )
import Specialise	( specProgram)
import SpecEnv		( specEnvToList, specEnvFromList )
import StrictAnal	( saWwTopBinds )
import Var		( TyVar, mkId )
import Unique		( Unique, Uniquable(..),
			  ratioTyConKey, mkUnique, incrUnique, initTidyUniques
		        )
import UniqSupply	( UniqSupply, splitUniqSupply, uniqFromSupply )
import Constants	( tARGET_MIN_INT, tARGET_MAX_INT )
import Util		( mapAccumL )
import Bag
import Maybes
import IO		( hPutStr, stderr )
import Outputable
\end{code}

\begin{code}
core2core :: [CoreToDo]		-- Spec of what core-to-core passes to do
	  -> Module		-- Module name (profiling only)
	  -> [Class]		-- Local classes
	  -> UniqSupply		-- A name supply
	  -> [CoreBind]		-- Input
	  -> IO [CoreBind]	-- Result

core2core core_todos module_name classes us binds
  = do
	let (us1, us2) = splitUniqSupply us

	-- Do the main business
	processed_binds <- doCorePasses us1 binds core_todos

	-- Do the post-simplification business
	post_simpl_binds <- doPostSimplification us2 processed_binds

	-- Do the final tidy-up
	final_binds <- tidyCorePgm module_name classes post_simpl_binds

	-- Return results
	return final_binds

doCorePasses us binds []
  = return binds

doCorePasses us binds (to_do : to_dos) 
  = do
	let (us1, us2) =  splitUniqSupply us
	binds1 	       <- doCorePass us1 binds to_do
	doCorePasses us2 binds1 to_dos

doCorePass us binds (CoreDoSimplify sw_chkr) = _scc_ "Simplify"       simplifyPgm sw_chkr us binds
doCorePass us binds CoreLiberateCase	     = _scc_ "LiberateCase"   liberateCase binds
doCorePass us binds CoreDoFloatInwards	     = _scc_ "FloatInwards"   floatInwards binds
doCorePass us binds CoreDoFullLaziness       = _scc_ "CoreFloating"   floatOutwards us binds
doCorePass us binds CoreDoStaticArgs	     = _scc_ "CoreStaticArgs" doStaticArgs us binds
doCorePass us binds CoreDoStrictness	     = _scc_ "CoreStranal"    saWwTopBinds us binds
doCorePass us binds CoreDoSpecialising	     = _scc_ "Specialise"     specProgram us binds
\end{code}


%************************************************************************
%*									*
\subsection{The driver for the simplifier}
%*									*
%************************************************************************

\begin{code}
simplifyPgm :: (SimplifierSwitch -> SwitchResult)
	    -> UniqSupply
	    -> [CoreBind]		-- Input
	    -> IO [CoreBind]		-- New bindings

simplifyPgm sw_chkr us binds
  = do {
	beginPass "Simplify";

	(termination_msg, it_count, counts, binds') <- iteration us 1 zeroSimplCount binds;

	dumpIfSet opt_D_simplifier_stats "Simplifier statistics"
		  (vcat [text termination_msg <+> text "after" <+> ppr it_count <+> text "iterations",
			 text "",
			 pprSimplCount counts]);

	endPass "Simplify" 
		(opt_D_verbose_core2core && not opt_D_dump_simpl_iterations)
		binds'
    }
  where
    max_iterations      = getSimplIntSwitch sw_chkr MaxSimplifierIterations
    simpl_switch_is_on  = switchIsOn sw_chkr

    core_iter_dump binds | opt_D_verbose_core2core = pprCoreBindings binds
		         | otherwise		   = empty

    iteration us iteration_no counts binds
      = do {
		-- Occurrence analysis
	   let { tagged_binds = _scc_ "OccAnal" occurAnalyseBinds simpl_switch_is_on binds };
	   dumpIfSet opt_D_dump_occur_anal "Occurrence analysis"
		     (pprCoreBindings tagged_binds);

		-- Simplify
	   let { (binds', counts') = initSmpl sw_chkr us1 (simplTopBinds tagged_binds);
	         all_counts        = counts `plusSimplCount` counts'
	       } ;

		-- Stop if nothing happened; don't dump output
	   if isZeroSimplCount counts' then
		return ("Simplifier reached fixed point", iteration_no, all_counts, binds')
	   else do {

		-- Dump the result of this iteration
	   dumpIfSet opt_D_dump_simpl_iterations
		     ("Simplifier iteration " ++ show iteration_no 
		      ++ " out of " ++ show max_iterations)
		     (vcat[pprSimplCount counts',
			   text "",
			   core_iter_dump binds']) ;

		-- Stop if we've run out of iterations
	   if iteration_no == max_iterations then
		do {
		    if  max_iterations > 1 then
			    hPutStr stderr ("NOTE: Simplifier still going after " ++ 
				    show max_iterations ++ 
				    " iterations; bailing out.\n")
		    else return ();

		    return ("Simplifier baled out", iteration_no, all_counts, binds')
		}

		-- Else loop
  	   else iteration us2 (iteration_no + 1) all_counts binds'
	}  }
      where
  	  (us1, us2) = splitUniqSupply us


simplTopBinds []              = returnSmpl []
simplTopBinds (bind1 : binds) = (simplBind bind1	$
			         simplTopBinds binds)	`thenSmpl` \ (binds1', binds') ->
				returnSmpl (binds1' ++ binds')
\end{code}


%************************************************************************
%*									*
\subsection{Tidying core}
%*									*
%************************************************************************

Several tasks are done by @tidyCorePgm@

1.  Make certain top-level bindings into Globals. The point is that 
    Global things get externally-visible labels at code generation
    time


2. Give all binders a nice print-name.  Their uniques aren't changed;
   rather we give them lexically unique occ-names, so that we can
   safely print the OccNae only in the interface file.  [Bad idea to
   change the uniques, because the code generator makes global labels
   from the uniques for local thunks etc.]


\begin{code}
tidyCorePgm :: Module -> [Class] -> [CoreBind] -> IO [CoreBind]
tidyCorePgm mod local_classes binds_in
  = do
	beginPass "Tidy Core"
	let (_, binds_out) = mapAccumL (tidyBind (Just mod)) init_tidy_env binds_in
	endPass "Tidy Core" (opt_D_dump_simpl || opt_D_verbose_core2core) binds_out
  where
	-- Make sure to avoid the names of class operations
	-- They don't have top-level bindings, so we won't see them
	-- in binds_in; so we must initialise the tidy_env appropriately
	--
	-- We also make sure to avoid any exported binders.  Consider
	--	f{-u1-} = 1	-- Local decl
	--	...
	--	f{-u2-} = 2	-- Exported decl
	--
	-- The second exported decl must 'get' the name 'f', so we
	-- have to put 'f' in the avoids list before we get to the first
	-- decl.  Name.tidyName then does a no-op on exported binders.
    init_tidy_env = (initTidyOccEnv avoids, emptyVarEnv)
    avoids	  = [getOccName sel_id | cls <- local_classes,
					 sel_id <- classSelIds cls]
		    ++
		    [getOccName bndr | bind <- binds_in,
				       bndr <- bindersOf bind,
				       isExported bndr]

tidyBind :: Maybe Module		-- (Just m) for top level, Nothing for nested
	 -> TidyEnv
	 -> CoreBind
	 -> (TidyEnv, CoreBind)
tidyBind maybe_mod env (NonRec bndr rhs)
  = let
	(env', bndr') = tidyBndr maybe_mod env bndr
	rhs'	      = tidyExpr env rhs
    in
    (env', NonRec bndr' rhs')

tidyBind maybe_mod env (Rec pairs)
  = let
	-- We use env' when tidying the rhss
	-- When tidying the binder itself we may tidy it's
	-- specialisations; if any of these mention other binders
	-- in the group we should really feed env' to them too;
	-- but that seems (a) unlikely and (b) a bit tiresome.
	-- So I left it out for now

	(bndrs, rhss)  = unzip pairs
	(env', bndrs') = mapAccumL (tidyBndr maybe_mod) env bndrs
	rhss'	       = map (tidyExpr env') rhss
  in
  (env', Rec (zip bndrs' rhss'))

tidyExpr env (Type ty)	     = Type (tidyType env ty)
tidyExpr env (Con con args)  = Con con (map (tidyExpr env) args)
tidyExpr env (App f a)       = App (tidyExpr env f) (tidyExpr env a)
tidyExpr env (Note n e)      = Note (tidyNote env n) (tidyExpr env e)

tidyExpr env (Let b e)       = Let b' (tidyExpr env' e)
			     where
			       (env', b') = tidyBind Nothing env b

tidyExpr env (Case e b alts) = Case (tidyExpr env e) b' (map (tidyAlt env') alts)
			     where
			       (env', b') = tidyNestedBndr env b

tidyExpr env (Var v)         = case lookupVarEnv var_env v of
				  Just v' -> Var v'
				  Nothing -> Var v
			     where
			       (_, var_env) = env

tidyExpr env (Lam b e)	     = Lam b' (tidyExpr env' e)
			     where
			       (env', b') = tidyNestedBndr env b

tidyAlt env (con, vs, rhs)   = (con, vs', tidyExpr env' rhs)
			     where
			       (env', vs') = mapAccumL tidyNestedBndr env vs

tidyNote env (Coerce t1 t2)  = Coerce (tidyType env t1) (tidyType env t2)
\end{code}

\begin{code}
tidyBndr (Just mod) env id  = tidyTopBndr mod env id
tidyBndr Nothing    env var = tidyNestedBndr  env var

tidyNestedBndr env tyvar
  | isTyVar tyvar
  = tidyTyVar env tyvar

tidyNestedBndr env@(tidy_env, var_env) id
  = 	-- Non-top-level variables
    let 
	-- Give the Id a fresh print-name, *and* rename its type
	name'        	  = mkLocalName (getUnique id) occ'
	(tidy_env', occ') = tidyOccName tidy_env (getOccName id)
        ty'          	  = tidyType env (idType id)
	id'          	  = mkUserId name' ty'
			-- NB: This throws away the IdInfo of the Id, which we
			-- no longer need.  That means we don't need to
			-- run over it with env, nor renumber it.
	var_env'	  = extendVarEnv var_env id id'
    in
    ((tidy_env', var_env'), id')

tidyTopBndr mod env@(tidy_env, var_env) id
  =	-- Top level variables
    let
	(tidy_env', name') = tidyTopName mod tidy_env (idName id)
	ty'	           = tidyTopType (idType id)
	idinfo'		   = tidyIdInfo env (idInfo id)
	id'		   = mkId name' ty' (idDetails id) idinfo'
	var_env'	   = extendVarEnv var_env id id'
    in
    ((tidy_env', var_env'), id')

-- tidyIdInfo does these things:
--	a) tidy the specialisation info (if any)
--	b) zap a complicated ICanSafelyBeINLINEd pragma,
--	c) zap the unfolding
-- The latter two are to avoid space leaks

tidyIdInfo env info
  = info3
  where
    spec_items = specEnvToList (specInfo info)
    spec_env'  = specEnvFromList (map tidy_item spec_items)
    info1 | null spec_items = info 
	  | otherwise	    = spec_env' `setSpecInfo` info
		
    info2 = case inlinePragInfo info of
		ICanSafelyBeINLINEd _ _ -> NoInlinePragInfo `setInlinePragInfo` info1
		other			-> info1

    info3 = noUnfolding `setUnfoldingInfo` info2

    tidy_item (tyvars, tys, rhs)
	= (tyvars', tidyTypes env' tys, tidyExpr env rhs)
	where
	  (env', tyvars') = tidyTyVars env tyvars
\end{code}



%************************************************************************
%*									*
\subsection{PostSimplification}
%*									*
%************************************************************************

Several tasks are performed by the post-simplification pass

1.  Make the representation of NoRep literals explicit, and
    float their bindings to the top level.  We only do the floating
    part for NoRep lits inside a lambda (else no gain).  We need to
    take care with	let x = "foo" in e
    that we don't end up with a silly binding
			let x = y in e
    with a floated "foo".  What a bore.
    
2.  *Mangle* cases involving par# in the discriminant.  The unfolding
    for par in PrelConc.lhs include case expressions with integer
    results solely to fool the strictness analyzer, the simplifier,
    and anyone else who might want to fool with the evaluation order.
    At this point in the compiler our evaluation order is safe.
    Therefore, we convert expressions of the form:

    	case par# e of
    	  0# -> rhs
    	  _  -> parError#
    ==>
    	case par# e of
    	  _ -> rhs

    fork# isn't handled like this - it's an explicit IO operation now.
    The reason is that fork# returns a ThreadId#, which gets in the
    way of the above scheme.  And anyway, IO is the only guaranteed
    way to enforce ordering  --SDM.

3.  Mangle cases involving seq# in the discriminant.  Up to this
    point, seq# will appear like this:

	  case seq# e of
		0# -> seqError#
		_  -> ...

    where the 0# branch is purely to bamboozle the strictness analyser
    (see case 4 above).  This code comes from an unfolding for 'seq'
    in Prelude.hs.  We translate this into

	  case e of
		_ -> ...

    Now that the evaluation order is safe.

4. Do eta reduction for lambda abstractions appearing in:
	- the RHS of case alternatives
	- the body of a let

   These will otherwise turn into local bindings during Core->STG;
   better to nuke them if possible.  (In general the simplifier does
   eta expansion not eta reduction, up to this point.  It does eta
   on the RHSs of bindings but not the RHSs of case alternatives and
   let bodies)


------------------- NOT DONE ANY MORE ------------------------
[March 98] Indirections are now elimianted by the occurrence analyser
1.  Eliminate indirections.  The point here is to transform
	x_local = E
	x_exported = x_local
    ==>
	x_exported = E

[Dec 98] [Not now done because there is no penalty in the code
	  generator for using the former form]
2.  Convert
	case x of {...; x' -> ...x'...}
    ==>
	case x of {...; _  -> ...x... }
    See notes in SimplCase.lhs, near simplDefault for the reasoning here.
--------------------------------------------------------------

Special case
~~~~~~~~~~~~

NOT ENABLED AT THE MOMENT (because the floated Ids are global-ish
things, and we need local Ids for non-floated stuff):

  Don't float stuff out of a binder that's marked as a bottoming Id.
  Reason: it doesn't do any good, and creates more CAFs that increase
  the size of SRTs.

eg.

	f = error "string"

is translated to

	f' = unpackCString# "string"
	f = error f'

hence f' and f become CAFs.  Instead, the special case for
tidyTopBinding below makes sure this comes out as

	f = let f' = unpackCString# "string" in error f'

and we can safely ignore f as a CAF, since it can only ever be entered once.



\begin{code}
doPostSimplification :: UniqSupply -> [CoreBind] -> IO [CoreBind]
doPostSimplification us binds_in
  = do
	beginPass "Post-simplification pass"
	let binds_out = initPM us (postSimplTopBinds binds_in)
	endPass "Post-simplification pass" opt_D_verbose_core2core binds_out

postSimplTopBinds :: [CoreBind] -> PostM [CoreBind]
postSimplTopBinds binds
  = mapPM postSimplTopBind binds	`thenPM` \ binds' ->
    returnPM (bagToList (unionManyBags binds'))

postSimplTopBind :: CoreBind -> PostM (Bag CoreBind)
postSimplTopBind (NonRec bndr rhs)
  | isBottomingId bndr		-- Don't lift out floats for bottoming Ids
				-- See notes above
  = getFloatsPM (postSimplExpr rhs)	`thenPM` \ (rhs', floats) ->
    returnPM (unitBag (NonRec bndr (foldrBag Let rhs' floats)))

postSimplTopBind bind
  = getFloatsPM (postSimplBind bind)	`thenPM` \ (bind', floats) ->
    returnPM (floats `snocBag` bind')

postSimplBind (NonRec bndr rhs)
  = postSimplExpr rhs		`thenPM` \ rhs' ->
    returnPM (NonRec bndr rhs')

postSimplBind (Rec pairs)
  = mapPM postSimplExpr rhss	`thenPM` \ rhss' ->
    returnPM (Rec (bndrs `zip` rhss'))
  where
    (bndrs, rhss) = unzip pairs
\end{code}


Expressions
~~~~~~~~~~~
\begin{code}
postSimplExpr (Var v)   = returnPM (Var v)
postSimplExpr (Type ty) = returnPM (Type ty)

postSimplExpr (App fun arg)
  = postSimplExpr fun	`thenPM` \ fun' ->
    postSimplExpr arg	`thenPM` \ arg' ->
    returnPM (App fun' arg')

postSimplExpr (Con (Literal lit) args)
  = ASSERT( null args )
    litToRep lit	`thenPM` \ (lit_ty, lit_expr) ->
    getInsideLambda	`thenPM` \ in_lam ->
    if in_lam && not (exprIsTrivial lit_expr) then
	-- It must have been a no-rep literal with a
	-- non-trivial representation; and we're inside a lambda;
	-- so float it to the top
	addTopFloat lit_ty lit_expr	`thenPM` \ v ->
	returnPM (Var v)
    else
	returnPM lit_expr

postSimplExpr (Con con args)
  = mapPM postSimplExpr args	`thenPM` \ args' ->
    returnPM (Con con args')

postSimplExpr (Lam bndr body)
  = insideLambda bndr		$
    postSimplExpr body		`thenPM` \ body' ->
    returnPM (Lam bndr body')

postSimplExpr (Let bind body)
  = postSimplBind bind		`thenPM` \ bind' ->
    postSimplExprEta body	`thenPM` \ body' ->
    returnPM (Let bind' body')

postSimplExpr (Note note body)
  = postSimplExprEta body	`thenPM` \ body' ->
    returnPM (Note note body')

-- seq#: see notes above.
-- NB: seq# :: forall a. a -> Int#
postSimplExpr (Case scrut@(Con (PrimOp SeqOp) [Type ty, e]) bndr alts)
  = postSimplExpr e			`thenPM` \ e' ->
    let 
	-- The old binder can't have been used, so we
	-- can gaily re-use it (yuk!)
	new_bndr = setIdType bndr ty
    in
    postSimplExprEta default_rhs	`thenPM` \ rhs' ->
    returnPM (Case e' new_bndr [(DEFAULT,[],rhs')])
  where
    (other_alts, maybe_default)  = findDefault alts
    Just default_rhs		 = maybe_default

-- par#: see notes above.
postSimplExpr (Case scrut@(Con (PrimOp op) args) bndr alts)
  | funnyParallelOp op && maybeToBool maybe_default
  = postSimplExpr scrut			`thenPM` \ scrut' ->
    postSimplExprEta default_rhs	`thenPM` \ rhs' ->
    returnPM (Case scrut' bndr [(DEFAULT,[],rhs')])
  where
    (other_alts, maybe_default)  = findDefault alts
    Just default_rhs		 = maybe_default

postSimplExpr (Case scrut case_bndr alts)
  = postSimplExpr scrut			`thenPM` \ scrut' ->
    mapPM ps_alt alts			`thenPM` \ alts' ->
    returnPM (Case scrut' case_bndr alts')
  where
    ps_alt (con,bndrs,rhs) = postSimplExprEta rhs	`thenPM` \ rhs' ->
			     returnPM (con, bndrs, rhs')

postSimplExprEta e = postSimplExpr e	`thenPM` \ e' ->
		     returnPM (etaCoreExpr e')
\end{code}

\begin{code}
funnyParallelOp ParOp  = True
funnyParallelOp _      = False
\end{code}  


%************************************************************************
%*									*
\subsection[coreToStg-lits]{Converting literals}
%*									*
%************************************************************************

Literals: the NoRep kind need to be de-no-rep'd.
We always replace them with a simple variable, and float a suitable
binding out to the top level.

\begin{code}
litToRep :: Literal -> PostM (Type, CoreExpr)

litToRep (NoRepStr s ty)
  = returnPM (ty, rhs)
  where
    rhs = if (any is_NUL (_UNPK_ s))

	  then	 -- Must cater for NULs in literal string
		mkApps (Var unpackCString2Id)
		       [mkLit (MachStr s),
		      	mkLit (mkMachInt (toInteger (_LENGTH_ s)))]

	  else	-- No NULs in the string
		App (Var unpackCStringId) (mkLit (MachStr s))

    is_NUL c = c == '\0'
\end{code}

If an Integer is small enough (Haskell implementations must support
Ints in the range $[-2^29+1, 2^29-1]$), wrap it up in @int2Integer@;
otherwise, wrap with @litString2Integer@.

\begin{code}
litToRep (NoRepInteger i integer_ty)
  = returnPM (integer_ty, rhs)
  where
    rhs | i == 0    = Var integerZeroId		-- Extremely convenient to look out for
  	| i == 1    = Var integerPlusOneId	-- a few very common Integer literals!
  	| i == 2    = Var integerPlusTwoId
  	| i == (-1) = Var integerMinusOneId
  
  	| i > tARGET_MIN_INT &&		-- Small enough, so start from an Int
	  i < tARGET_MAX_INT
	= App (Var int2IntegerId) (Con (Literal (mkMachInt i)) [])
  
  	| otherwise 			-- Big, so start from a string
	= App (Var addr2IntegerId) (Con (Literal (MachStr (_PK_ (show i)))) [])


litToRep (NoRepRational r rational_ty)
  = postSimplExpr (mkLit (NoRepInteger (numerator   r) integer_ty))	`thenPM` \ num_arg ->
    postSimplExpr (mkLit (NoRepInteger (denominator r) integer_ty))	`thenPM` \ denom_arg ->
    returnPM (rational_ty, mkConApp ratio_data_con [Type integer_ty, num_arg, denom_arg])
  where
    (ratio_data_con, integer_ty)
      = case (splitAlgTyConApp_maybe rational_ty) of
	  Just (tycon, [i_ty], [con])
	    -> ASSERT(isIntegerTy i_ty && getUnique tycon == ratioTyConKey)
	       (con, i_ty)

	  _ -> (panic "ratio_data_con", panic "integer_ty")

litToRep other_lit = returnPM (literalType other_lit, mkLit other_lit)
\end{code}


%************************************************************************
%*									*
\subsection{The monad}
%*									*
%************************************************************************

\begin{code}
type PostM a =  Bool				-- True <=> inside a *value* lambda
	     -> (UniqSupply, Bag CoreBind)	-- Unique supply and Floats in 
	     -> (a, (UniqSupply, Bag CoreBind))

initPM :: UniqSupply -> PostM a -> a
initPM us m
  = case m False {- not inside lambda -} (us, emptyBag) of 
	(result, _) -> result

returnPM v in_lam usf = (v, usf)
thenPM m k in_lam usf = case m in_lam usf of
			 	  (r, usf') -> k r in_lam usf'

mapPM f []     = returnPM []
mapPM f (x:xs) = f x		`thenPM` \ r ->
		 mapPM f xs	`thenPM` \ rs ->
		 returnPM (r:rs)

insideLambda :: CoreBndr -> PostM a -> PostM a
insideLambda bndr m in_lam usf | isId bndr = m True   usf
			       | otherwise = m in_lam usf

getInsideLambda :: PostM Bool
getInsideLambda in_lam usf = (in_lam, usf)

getFloatsPM :: PostM a -> PostM (a, Bag CoreBind)
getFloatsPM m in_lam (us, floats)
  = let
	(a, (us', floats')) = m in_lam (us, emptyBag)
    in
    ((a, floats'), (us', floats))

addTopFloat :: Type -> CoreExpr -> PostM Id
addTopFloat lit_ty lit_rhs in_lam (us, floats)
  = let
        (us1, us2) = splitUniqSupply us
	uniq	   = uniqFromSupply us1
        lit_id     = mkSysLocal SLIT("lf") uniq lit_ty
    in
    (lit_id, (us2, floats `snocBag` NonRec lit_id lit_rhs))
\end{code}


