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
import Id		( Id, mkSysLocal, mkUserId,
			  setIdVisibility, setIdUnfolding,
			  getIdSpecialisation, setIdSpecialisation,
			  getInlinePragma, setInlinePragma,
			  idType, setIdType
			)
import IdInfo		( InlinePragInfo(..) )
import VarEnv
import VarSet
import Name		( isExported, mkSysLocalName,
			  Module, NamedThing(..), OccName(..)
			)
import TyCon		( TyCon, isDataTyCon )
import PrimOp		( PrimOp(..) )
import PrelInfo		( unpackCStringId, unpackCString2Id,
			  integerZeroId, integerPlusOneId,
			  integerPlusTwoId, integerMinusOneId,
			  int2IntegerId, addr2IntegerId
			)
import Type		( Type, splitAlgTyConApp_maybe, 
			  isUnLiftedType, mkTyVarTy, Type )
import TysWiredIn	( isIntegerTy )
import LiberateCase	( liberateCase )
import PprType		( nmbrType )
import SAT		( doStaticArgs )
import Specialise	( specProgram)
import SpecEnv		( specEnvToList, specEnvFromList )
import StrictAnal	( saWwTopBinds )
import Var		( TyVar, setTyVarName )
import Unique		( Unique, Uniquable(..),
			  ratioTyConKey, mkUnique, incrUnique, initTidyUniques
		        )
import UniqSupply	( UniqSupply, splitUniqSupply )
import Constants	( tARGET_MIN_INT, tARGET_MAX_INT )
import Bag
import Maybes
import IO		( hPutStr, stderr )
import Outputable
\end{code}

\begin{code}
core2core :: [CoreToDo]		-- Spec of what core-to-core passes to do
	  -> FAST_STRING	-- Module name (profiling only)
	  -> UniqSupply		-- A name supply
	  -> [CoreBind]		-- Input
	  -> IO [CoreBind]	-- Result

core2core core_todos module_name us binds
  = do
	-- Do the main business
	processed_binds <- doCorePasses us binds core_todos

	-- Do the final tidy-up
	final_binds <- tidyCorePgm module_name processed_binds

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
\subsection[SimplCore-indirections]{Eliminating indirections in Core code, and globalising}
%*									*
%************************************************************************

Several tasks are done by @tidyCorePgm@

----------------
	[March 98] Indirections are now elimianted by the occurrence analyser
	-- 1.  Eliminate indirections.  The point here is to transform
	--	x_local = E
	--	x_exported = x_local
	--    ==>
	--	x_exported = E

2.  Make certain top-level bindings into Globals. The point is that 
    Global things get externally-visible labels at code generation
    time

3.  Make the representation of NoRep literals explicit, and
    float their bindings to the top level.  We only do the floating
    part for NoRep lits inside a lambda (else no gain).  We need to
    take care with	let x = "foo" in e
    that we don't end up with a silly binding
			let x = y in e
    with a floated "foo".  What a bore.
    
4.  Convert
	case x of {...; x' -> ...x'...}
    ==>
	case x of {...; _  -> ...x... }
    See notes in SimplCase.lhs, near simplDefault for the reasoning here.

5.  *Mangle* cases involving par# in the discriminant.  The unfolding
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

6.  Mangle cases involving seq# in the discriminant.  Up to this
    point, seq# will appear like this:

	  case seq# e of
		0# -> seqError#
		_  -> ...

    where the 0# branch is purely to bamboozle the strictness analyser
    (see case 5 above).  This code comes from an unfolding for 'seq'
    in Prelude.hs.  We translate this into

	  case e of
		_ -> ...

    Now that the evaluation order is safe.  The code generator knows
    how to push a seq frame on the stack if 'e' is of function type,
    or is polymorphic.


7. Do eta reduction for lambda abstractions appearing in:
	- the RHS of case alternatives
	- the body of a let

   These will otherwise turn into local bindings during Core->STG;
   better to nuke them if possible.  (In general the simplifier does
   eta expansion not eta reduction, up to this point.)

9. Give all binders a nice print-name.  Their uniques aren't changed;
   rather we give them lexically unique occ-names, so that we can
   safely print the OccNae only in the interface file.  [Bad idea to
   change the uniques, because the code generator makes global labels
   from the uniques for local thunks etc.]


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
tidyCorePgm :: Module -> [CoreBind] -> IO [CoreBind]

tidyCorePgm mod binds_in
  = do
	beginPass "Tidy Core"

	let binds_out = bagToList (initTM mod (tidyTopBindings binds_in))

	endPass "Tidy Core" (opt_D_dump_simpl || opt_D_verbose_core2core) binds_out
\end{code}

Top level bindings
~~~~~~~~~~~~~~~~~~
\begin{code}
tidyTopBindings [] = returnTM emptyBag
tidyTopBindings (b:bs)
  = tidyTopBinding  b		$
    tidyTopBindings bs

tidyTopBinding :: CoreBind
	       -> TopTidyM (Bag CoreBind)
	       -> TopTidyM (Bag CoreBind)

tidyTopBinding (NonRec bndr rhs) thing_inside
  = initNestedTM (tidyCoreExpr rhs)		`thenTM` \ (rhs',floats) ->
    tidyTopBinder bndr				$ \ bndr' ->
    thing_inside 				`thenTM` \ binds ->
    let
	this_bind {- | isBottomingId bndr 	
			= unitBag (NonRec bndr' (foldrBag Let rhs' floats))
		  | otherwise  -}
			= floats `snocBag` NonRec bndr' rhs'
    in
    returnTM (this_bind `unionBags` binds)

tidyTopBinding (Rec pairs) thing_inside
  = tidyTopBinders binders			$ \ binders' ->
    initNestedTM (mapTM tidyCoreExpr rhss)	`thenTM` \ (rhss', floats) ->
    thing_inside				`thenTM` \ binds_inside ->
    returnTM ((floats `snocBag` Rec (binders' `zip` rhss')) `unionBags` binds_inside)
  where
    (binders, rhss) = unzip pairs
\end{code}

\begin{code}
tidyTopBinder :: Id -> (Id -> TopTidyM (Bag CoreBind)) -> TopTidyM (Bag CoreBind)
tidyTopBinder id thing_inside
  = mungeTopBndr id 				$ \ id' ->
    let
	spec_items = specEnvToList (getIdSpecialisation id')
    in
    if null spec_items then

	-- Common case, no specialisations to tidy
	thing_inside id'
    else

	-- Oh well, tidy those specialisations
    initNestedTM (mapTM tidySpecItem spec_items)	`thenTM` \ (spec_items', floats) ->
    let
	id'' = setIdSpecialisation id' (specEnvFromList spec_items')
    in
    extendEnvTM id (Var id'')		$
    thing_inside id''			`thenTM` \ binds ->
    returnTM (floats `unionBags` binds)

tidyTopBinders []     k = k []
tidyTopBinders (b:bs) k = tidyTopBinder b	$ \ b' ->
			  tidyTopBinders bs	$ \ bs' ->
			  k (b' : bs')

tidySpecItem (tyvars, tys, rhs)
  = newBndrs tyvars		$ \ tyvars' ->
    mapTM tidyTy tys		`thenTM` \ tys' ->
    tidyCoreExpr rhs		`thenTM` \ rhs' ->
    returnTM (tyvars', tys', rhs')
\end{code}

Expressions
~~~~~~~~~~~
\begin{code}
tidyCoreExpr (Var v) = lookupId v

tidyCoreExpr (Type ty)
  = tidyTy ty 	`thenTM` \ ty' ->
    returnTM (Type ty')

tidyCoreExpr (App fun arg)
  = tidyCoreExpr fun	`thenTM` \ fun' ->
    tidyCoreExpr arg	`thenTM` \ arg' ->
    returnTM (App fun' arg')

tidyCoreExpr (Con (Literal lit) args)
  = ASSERT( null args )
    litToRep lit	`thenTM` \ (lit_ty, lit_expr) ->
    getInsideLambda	`thenTM` \ in_lam ->
    if in_lam && not (exprIsTrivial lit_expr) then
	-- It must have been a no-rep literal with a
	-- non-trivial representation; and we're inside a lambda;
	-- so float it to the top
	addTopFloat lit_ty lit_expr	`thenTM` \ v ->
	returnTM (Var v)
    else
	returnTM lit_expr

tidyCoreExpr (Con con args)
  = mapTM tidyCoreExpr args	`thenTM` \ args' ->
    returnTM (Con con args')

tidyCoreExpr (Lam bndr body)
  = newBndr bndr 		$ \ bndr' ->
    insideLambda bndr		$
    tidyCoreExpr body		`thenTM` \ body' ->
    returnTM (Lam bndr' body')

tidyCoreExpr (Let (NonRec bndr rhs) body)
  = tidyCoreExpr rhs		`thenTM` \ rhs' ->
    tidyBindNonRec bndr rhs' body

tidyCoreExpr (Let (Rec pairs) body)
  = newBndrs bndrs		$ \ bndrs' ->
    mapTM tidyCoreExpr rhss	`thenTM` \ rhss' ->
    tidyCoreExprEta body	`thenTM` \ body' ->
    returnTM (Let (Rec (bndrs' `zip` rhss')) body')
  where
    (bndrs, rhss) = unzip pairs

tidyCoreExpr (Note (Coerce to_ty from_ty) body)
  = tidyCoreExprEta body	`thenTM` \ body' ->
    tidyTy to_ty		`thenTM` \ to_ty' ->
    tidyTy from_ty		`thenTM` \ from_ty' ->
    returnTM (Note (Coerce to_ty' from_ty') body')

tidyCoreExpr (Note note body)
  = tidyCoreExprEta body	`thenTM` \ body' ->
    returnTM (Note note body')

-- seq#: see notes above.
tidyCoreExpr (Case scrut@(Con (PrimOp SeqOp) [Type _, e]) bndr alts)
  = tidyCoreExpr e			`thenTM` \ e' ->
    newBndr bndr 			$ \ bndr' ->
    let new_bndr = setIdType bndr' (coreExprType e') in
    tidyCoreExprEta default_rhs		`thenTM` \ rhs' ->
    returnTM (Case e' new_bndr [(DEFAULT,[],rhs')])
  where
    (other_alts, maybe_default)  = findDefault alts
    Just default_rhs		 = maybe_default

-- par#: see notes above.
tidyCoreExpr (Case scrut@(Con (PrimOp op) args) bndr alts)
  | funnyParallelOp op && maybeToBool maybe_default
  = tidyCoreExpr scrut			`thenTM` \ scrut' ->
    newBndr bndr			$ \ bndr' ->
    tidyCoreExprEta default_rhs		`thenTM` \ rhs' ->
    returnTM (Case scrut' bndr' [(DEFAULT,[],rhs')])
  where
    (other_alts, maybe_default)  = findDefault alts
    Just default_rhs		 = maybe_default

tidyCoreExpr (Case scrut case_bndr alts)
  = tidyCoreExpr scrut			`thenTM` \ scrut' ->
    newBndr case_bndr			$ \ case_bndr' ->
    mapTM tidy_alt alts			`thenTM` \ alts' ->
    returnTM (Case scrut' case_bndr' alts')
  where
    tidy_alt (con,bndrs,rhs) = newBndrs bndrs		$ \ bndrs' ->
			       tidyCoreExprEta rhs	`thenTM` \ rhs' ->
			       returnTM (con, bndrs', rhs')

tidyCoreExprEta e = tidyCoreExpr e	`thenTM` \ e' ->
		    returnTM (etaCoreExpr e')

tidyBindNonRec bndr val' body
  | exprIsTrivial val'
  = extendEnvTM bndr val' (tidyCoreExpr body)

  | otherwise
  = newBndr bndr	$ \ bndr' ->
    tidyCoreExpr body	`thenTM` \ body' ->
    returnTM (Let (NonRec bndr' val') body')
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
		     
litToRep :: Literal -> NestTidyM (Type, CoreExpr)

litToRep (NoRepStr s ty)
  = returnTM (ty, rhs)
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
  = returnTM (integer_ty, rhs)
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
  = tidyCoreExpr (mkLit (NoRepInteger (numerator   r) integer_ty))	`thenTM` \ num_arg ->
    tidyCoreExpr (mkLit (NoRepInteger (denominator r) integer_ty))	`thenTM` \ denom_arg ->
    returnTM (rational_ty, mkConApp ratio_data_con [Type integer_ty, num_arg, denom_arg])
  where
    (ratio_data_con, integer_ty)
      = case (splitAlgTyConApp_maybe rational_ty) of
	  Just (tycon, [i_ty], [con])
	    -> ASSERT(isIntegerTy i_ty && getUnique tycon == ratioTyConKey)
	       (con, i_ty)

	  _ -> (panic "ratio_data_con", panic "integer_ty")

litToRep other_lit = returnTM (literalType other_lit, mkLit other_lit)
\end{code}

\begin{code}
funnyParallelOp ParOp  = True
funnyParallelOp _      = False
\end{code}  


%************************************************************************
%*									*
\subsection{The monad}
%*									*
%************************************************************************

\begin{code}
type TidyM a state =  Module
		      -> Bool		-- True <=> inside a *value* lambda
	     	      -> (TyVarEnv Type, IdEnv CoreExpr, IdOrTyVarSet)
				-- Substitution and in-scope binders
		      -> state
		      -> (a, state)

type TopTidyM  a = TidyM a Unique
type NestTidyM a = TidyM a (Unique,	 		-- Global names
			    Unique,	 		-- Local names
			    Bag CoreBind)		-- Floats


(initialTopTidyUnique, initialNestedTidyUnique) = initTidyUniques

initTM :: Module -> TopTidyM a -> a
initTM mod m
  = case m mod False {- not inside lambda -} empty_env initialTopTidyUnique of 
	(result, _) -> result
  where
    empty_env = (emptyVarEnv, emptyVarEnv, emptyVarSet)

initNestedTM :: NestTidyM a -> TopTidyM (a, Bag CoreBind)
initNestedTM m mod in_lam env global_us
  = case m mod in_lam env (global_us, initialNestedTidyUnique, emptyBag) of
	(result, (global_us', _, floats)) -> ((result, floats), global_us')

returnTM v mod in_lam env usf = (v, usf)
thenTM m k mod in_lam env usf = case m mod in_lam env usf of
			 	  (r, usf') -> k r mod in_lam env usf'

mapTM f []     = returnTM []
mapTM f (x:xs) = f x		`thenTM` \ r ->
		 mapTM f xs	`thenTM` \ rs ->
		 returnTM (r:rs)

insideLambda :: CoreBndr -> NestTidyM a -> NestTidyM a
insideLambda bndr m mod in_lam env usf | isId bndr = m mod True   env usf
				       | otherwise = m mod in_lam env usf

getInsideLambda :: NestTidyM Bool
getInsideLambda mod in_lam env usf = (in_lam, usf)
\end{code}

Need to extend the environment when we munge a binder, so that
occurrences of the binder will print the correct way (e.g. as a global
not a local).

In cases where we don't clone the binder (because it's an exported
id), we still zap the unfolding and inline pragma info so that
unnecessary gumph isn't carried into the code generator.  This fixes a
nasty space leak.

\begin{code}
mungeTopBndr id thing_inside mod in_lam env@(ty_env, val_env, in_scope) us
  = thing_inside id' mod in_lam (ty_env, val_env', in_scope') us'
  where
  (id', us') | isExported id = (zapSomeIdInfo id, us)
	     | otherwise = (zapSomeIdInfo (setIdVisibility (Just mod) us id),
			    incrUnique us)
  val_env'  = extendVarEnv val_env id (Var id')
  in_scope' = extendVarSet in_scope id'	
    
zapSomeIdInfo id = id `setIdUnfolding` noUnfolding `setInlinePragma` new_ip
  where new_ip = case getInlinePragma id of
			ICanSafelyBeINLINEd _ _ -> NoInlinePragInfo
			something_else	 	-> something_else

addTopFloat :: Type -> CoreExpr -> NestTidyM Id
addTopFloat lit_ty lit_rhs mod in_lam env (gus, lus, floats)
  = let
        gus'      = incrUnique gus
        lit_local = mkSysLocal gus lit_ty
        lit_id    = setIdVisibility (Just mod) gus lit_local
    in
    (lit_id, (gus', lus, floats `snocBag` NonRec lit_id lit_rhs))

lookupId :: Id -> TidyM CoreExpr state
lookupId v mod in_lam (_, val_env, _) usf
  = case lookupVarEnv val_env v of
	Nothing	-> (Var v, usf)
	Just e  -> (e,     usf)

extendEnvTM :: Id -> CoreExpr -> (TidyM a state) -> TidyM a state
extendEnvTM v e m mod in_lam (ty_env, val_env, in_scope) usf
  = m mod in_lam (ty_env, extendVarEnv val_env v e, in_scope) usf
\end{code}


Making new local binders
~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
newBndr tyvar thing_inside mod in_lam (ty_env, val_env, in_scope) (gus, local_uniq, floats)
  | isTyVar tyvar
  = let
	local_uniq' = incrUnique local_uniq	
	tyvar'      = setTyVarName tyvar (mkSysLocalName local_uniq)
	ty_env'	    = extendVarEnv ty_env tyvar (mkTyVarTy tyvar')
	in_scope'   = extendVarSet in_scope tyvar'
    in
    thing_inside tyvar' mod in_lam (ty_env', val_env, in_scope') (gus, local_uniq', floats)

newBndr id thing_inside mod in_lam (ty_env, val_env, in_scope) (gus, local_uniq, floats)
  | isId id
  = let 
	-- Give the Id a fresh print-name, *and* rename its type
	local_uniq'  = incrUnique local_uniq	
	name'        = mkSysLocalName local_uniq
        ty'          = nmbrType ty_env local_uniq' (idType id)

	id'          = mkUserId name' ty'
			-- NB: This throws away the IdInfo of the Id, which we
			-- no longer need.  That means we don't need to
			-- run over it with env, nor renumber it.

	val_env'     = extendVarEnv val_env id (Var id')
	in_scope'    = extendVarSet in_scope id'
    in
    thing_inside id' mod in_lam (ty_env, val_env', in_scope') (gus, local_uniq', floats)

newBndrs [] thing_inside
  = thing_inside []
newBndrs (bndr:bndrs) thing_inside
  = newBndr bndr	$ \ bndr' ->
    newBndrs bndrs	$ \ bndrs' ->
    thing_inside (bndr' : bndrs')
\end{code}

Re-numbering types
~~~~~~~~~~~~~~~~~~
\begin{code}
tidyTy ty mod in_lam (ty_env, val_env, in_scope) usf@(_, local_uniq, _)
  = (nmbrType ty_env local_uniq ty, usf)
	-- We can use local_uniq as a base for renaming forall'd variables
	-- in the type; we don't need to know how many are consumed.
\end{code}

-- Get rid of this function when we move to the new code generator.

\begin{code}
typeOkForCase :: Type -> Bool
typeOkForCase ty
  | isUnLiftedType ty 	-- Primitive case
  = True

  | otherwise
  = case (splitAlgTyConApp_maybe ty) of
      Just (tycon, ty_args, [])                 		    -> False
      Just (tycon, ty_args, non_null_data_cons) | isDataTyCon tycon -> True
      other	                                   		    -> False
      -- Null data cons => type is abstract, which code gen can't 
      -- currently handle.  (ToDo: when return-in-heap is universal we
      -- don't need to worry about this.)
\end{code}
