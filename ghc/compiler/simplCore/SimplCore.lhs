%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[SimplCore]{Driver for simplifying @Core@ programs}

\begin{code}
module SimplCore ( core2core ) where

#include "HsVersions.h"

import CmdLineOpts	( CoreToDo(..), SimplifierSwitch(..), 
			  SwitchResult(..), switchIsOn, intSwitchSet,
			  opt_D_dump_occur_anal, opt_D_dump_rules,
			  opt_D_dump_simpl_iterations,
			  opt_D_dump_simpl_stats,
			  opt_D_dump_simpl, opt_D_dump_rules,
			  opt_D_verbose_core2core,
			  opt_D_dump_occur_anal,
                          opt_UsageSPOn,
			)
import CoreLint		( beginPass, endPass )
import CoreTidy		( tidyCorePgm )
import CoreSyn
import Rules		( RuleBase, ProtoCoreRule(..), pprProtoCoreRule, prepareRuleBase, orphanRule )
import CoreUnfold
import PprCore		( pprCoreBindings )
import OccurAnal	( occurAnalyseBinds )
import CoreUtils	( exprIsTrivial, coreExprType )
import Simplify		( simplTopBinds, simplExpr )
import SimplUtils	( etaCoreExpr, findDefault, simplBinders )
import SimplMonad
import Const		( Con(..), Literal(..), literalType, mkMachInt )
import ErrUtils		( dumpIfSet )
import FloatIn		( floatInwards )
import FloatOut		( floatOutwards )
import Id		( Id, mkSysLocal, mkVanillaId, isBottomingId,
			  idType, setIdType, idName, idInfo, setIdNoDiscard
			)
import IdInfo		( InlinePragInfo(..), specInfo, setSpecInfo,
			  inlinePragInfo, setInlinePragInfo,
			  setUnfoldingInfo, setDemandInfo
			)
import Demand		( wwLazy )
import VarEnv
import VarSet
import Module		( Module )
import Name		( mkLocalName, tidyOccName, tidyTopName, 
			  NamedThing(..), OccName
			)
import TyCon		( TyCon, isDataTyCon )
import PrimOp		( PrimOp(..) )
import PrelInfo		( unpackCStringId, unpackCString2Id, addr2IntegerId )
import Type		( Type, splitAlgTyConApp_maybe, 
			  isUnLiftedType,
			  tidyType, tidyTypes, tidyTopType, tidyTyVar, tidyTyVars,
			  Type
			)
import Class		( Class, classSelIds )
import TysWiredIn	( smallIntegerDataCon, isIntegerTy )
import LiberateCase	( liberateCase )
import SAT		( doStaticArgs )
import Specialise	( specProgram)
import UsageSPInf       ( doUsageSPInf )
import StrictAnal	( saBinds )
import WorkWrap	        ( wwTopBinds )
import CprAnalyse       ( cprAnalyse )

import Unique		( Unique, Uniquable(..),
			  ratioTyConKey
		        )
import UniqSupply	( UniqSupply, mkSplitUniqSupply, splitUniqSupply, uniqFromSupply )
import Constants	( tARGET_MIN_INT, tARGET_MAX_INT )
import Util		( mapAccumL )
import SrcLoc		( noSrcLoc )
import Bag
import Maybes
import IO		( hPutStr, stderr )
import Outputable

import Ratio 		( numerator, denominator )
\end{code}

%************************************************************************
%*									*
\subsection{The driver for the simplifier}
%*									*
%************************************************************************

\begin{code}
core2core :: [CoreToDo]		-- Spec of what core-to-core passes to do
	  -> [CoreBind]		-- Binds in
	  -> [ProtoCoreRule]	-- Rules
	  -> IO ([CoreBind], [ProtoCoreRule])

core2core core_todos binds rules
  = do
	us <-  mkSplitUniqSupply 's'
	let (cp_us, us1)   = splitUniqSupply us
	    (ru_us, ps_us) = splitUniqSupply us1

        better_rules <- simplRules ru_us rules binds

	let (binds1, rule_base) = prepareRuleBase binds better_rules

	-- Do the main business
	(stats, processed_binds) <- doCorePasses zeroSimplCount cp_us binds1 
						 rule_base core_todos

	dumpIfSet opt_D_dump_simpl_stats
		  "Grand total simplifier statistics"
		  (pprSimplCount stats)

	-- Do the post-simplification business
	post_simpl_binds <- doPostSimplification ps_us processed_binds

	-- Return results
	return (post_simpl_binds, filter orphanRule better_rules)
   

doCorePasses stats us binds irs []
  = return (stats, binds)

doCorePasses stats us binds irs (to_do : to_dos) 
  = do
	let (us1, us2) =  splitUniqSupply us
	(stats1, binds1) <- doCorePass us1 binds irs to_do
	doCorePasses (stats `plusSimplCount` stats1) us2 binds1 irs to_dos

doCorePass us binds rb (CoreDoSimplify sw_chkr) = _scc_ "Simplify"      simplifyPgm rb sw_chkr us binds
doCorePass us binds rb CoreLiberateCase	        = _scc_ "LiberateCase"  noStats (liberateCase binds)
doCorePass us binds rb CoreDoFloatInwards       = _scc_ "FloatInwards"  noStats (floatInwards binds)
doCorePass us binds rb CoreDoFullLaziness       = _scc_ "FloatOutwards" noStats (floatOutwards us binds)
doCorePass us binds rb CoreDoStaticArgs	        = _scc_ "StaticArgs"    noStats (doStaticArgs us binds)
doCorePass us binds rb CoreDoStrictness	        = _scc_ "Stranal"       noStats (saBinds binds)
doCorePass us binds rb CoreDoWorkerWrapper      = _scc_ "WorkWrap"      noStats (wwTopBinds us binds)
doCorePass us binds rb CoreDoSpecialising       = _scc_ "Specialise"    noStats (specProgram us binds)
doCorePass us binds rb CoreDoCPResult	        = _scc_ "CPResult"      noStats (cprAnalyse binds)
doCorePass us binds rb CoreDoPrintCore	        = _scc_ "PrintCore"     noStats (printCore binds)
doCorePass us binds rb CoreDoUSPInf
  = _scc_ "CoreUsageSPInf" 
    if opt_UsageSPOn then
      noStats (doUsageSPInf us binds)
    else
      trace "WARNING: ignoring requested -fusagesp pass; requires -fusagesp-on" $
      noStats (return binds)

printCore binds = do dumpIfSet True "Print Core"
			       (pprCoreBindings binds)
		     return binds

noStats thing = do { result <- thing; return (zeroSimplCount, result) }
\end{code}


%************************************************************************
%*									*
\subsection{Dealing with rules}
%*									*
%************************************************************************

We must do some gentle simplifiation on the template (but not the RHS)
of each rule.  The case that forced me to add this was the fold/build rule,
which without simplification looked like:
	fold k z (build (/\a. g a))  ==>  ...
This doesn't match unless you do eta reduction on the build argument.

\begin{code}
simplRules :: UniqSupply -> [ProtoCoreRule] -> [CoreBind] -> IO [ProtoCoreRule]
simplRules us rules binds
  = do  let (better_rules,_) = initSmpl sw_chkr us bind_vars black_list_all (mapSmpl simplRule rules)
	
	dumpIfSet opt_D_dump_rules
		  "Transformation rules"
		  (vcat (map pprProtoCoreRule better_rules))

	return better_rules
  where
    black_list_all v = True 		-- This stops all inlining
    sw_chkr any = SwBool False		-- A bit bogus

	-- Boringly, we need to gather the in-scope set.
	-- Typically this thunk won't even be force, but the test in
	-- simpVar fails if it isn't right, and it might conceivably matter
    bind_vars = foldr (unionVarSet . mkVarSet . bindersOf) emptyVarSet binds


simplRule rule@(ProtoCoreRule is_local id (Rule name bndrs args rhs))
  | not is_local
  = returnSmpl rule	-- No need to fiddle with imported rules
  | otherwise
  = simplBinders bndrs			$ \ bndrs' -> 
    mapSmpl simplExpr args		`thenSmpl` \ args' ->
    simplExpr rhs			`thenSmpl` \ rhs' ->
    returnSmpl (ProtoCoreRule is_local id (Rule name bndrs' args' rhs'))
\end{code}

%************************************************************************
%*									*
\subsection{The driver for the simplifier}
%*									*
%************************************************************************

\begin{code}
simplifyPgm :: RuleBase
	    -> (SimplifierSwitch -> SwitchResult)
	    -> UniqSupply
	    -> [CoreBind]				-- Input
	    -> IO (SimplCount, [CoreBind])		-- New bindings

simplifyPgm (imported_rule_ids, rule_lhs_fvs) 
	    sw_chkr us binds
  = do {
	beginPass "Simplify";

	-- Glom all binds together in one Rec, in case any
	-- transformations have introduced any new dependencies
	let { recd_binds = [Rec (flattenBinds binds)] };

	(termination_msg, it_count, counts_out, binds') <- iteration us 1 zeroSimplCount recd_binds;

	dumpIfSet (opt_D_verbose_core2core && opt_D_dump_simpl_stats)
		  "Simplifier statistics"
		  (vcat [text termination_msg <+> text "after" <+> ppr it_count <+> text "iterations",
			 text "",
			 pprSimplCount counts_out]);

	endPass "Simplify" 
		(opt_D_verbose_core2core && not opt_D_dump_simpl_iterations)
		binds' ;

	return (counts_out, binds')
    }
  where
    max_iterations = getSimplIntSwitch sw_chkr MaxSimplifierIterations
    black_list_fn  = blackListed rule_lhs_fvs (intSwitchSet sw_chkr SimplInlinePhase)

    core_iter_dump binds | opt_D_verbose_core2core = pprCoreBindings binds
		         | otherwise		   = empty

    iteration us iteration_no counts binds
      = do {
		-- Occurrence analysis
	   let { tagged_binds = _scc_ "OccAnal" occurAnalyseBinds binds } ;

	   dumpIfSet opt_D_dump_occur_anal "Occurrence analysis"
		     (pprCoreBindings tagged_binds);

		-- Simplify
	   let { (binds', counts') = initSmpl sw_chkr us1 imported_rule_ids 
					      black_list_fn 
					      (simplTopBinds tagged_binds);
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
		     (pprSimplCount counts') ;

	   if opt_D_dump_simpl_iterations then
		endPass ("Simplifier iteration " ++ show iteration_no ++ " result")
			opt_D_verbose_core2core
			binds'
	   else
		return [] ;

		-- Stop if we've run out of iterations
	   if iteration_no == max_iterations then
		do {
		    if  max_iterations > 2 then
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
otherwise, wrap with @addr2Integer@.

\begin{code}
litToRep (NoRepInteger i integer_ty)
  = returnPM (integer_ty, rhs)
  where
    rhs | i > tARGET_MIN_INT &&		-- Small enough, so start from an Int
	  i < tARGET_MAX_INT
	= Con (DataCon smallIntegerDataCon) [Con (Literal (mkMachInt i)) []]
  
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


