%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
%************************************************************************
%*									*
\section[OccurAnal]{Occurrence analysis pass}
%*									*
%************************************************************************

The occurrence analyser re-typechecks a core expression, returning a new
core expression with (hopefully) improved usage information.

\begin{code}
module OccurAnal (
	occurAnalyseBinds, occurAnalyseExpr, occurAnalyseGlobalExpr,
	markBinderInsideLambda, tagBinders,
	UsageDetails
    ) where

#include "HsVersions.h"

import BinderInfo
import CmdLineOpts	( SimplifierSwitch(..) )
import CoreSyn
import CoreFVs		( idRuleVars )
import CoreUtils	( exprIsTrivial )
import Const		( Con(..), Literal(..) )
import Id		( isSpecPragmaId,
			  getInlinePragma, setInlinePragma,
			  isExportedId, modifyIdInfo, idInfo,
			  getIdSpecialisation, 
			  idType, idUnique, Id
			)
import IdInfo		( InlinePragInfo(..), OccInfo(..), copyIdInfo )

import VarSet
import VarEnv

import ThinAir		( noRepStrIds, noRepIntegerIds )
import Name		( isLocallyDefined )
import Type		( splitFunTy_maybe, splitForAllTys )
import Maybes		( maybeToBool )
import Digraph		( stronglyConnCompR, SCC(..) )
import Unique		( u2i, buildIdKey, foldrIdKey, runSTRepIdKey, augmentIdKey )
import UniqFM		( keysUFM )  
import Util		( zipWithEqual, mapAndUnzip, count )
import Outputable
\end{code}


%************************************************************************
%*									*
\subsection[OccurAnal-main]{Counting occurrences: main function}
%*									*
%************************************************************************

Here's the externally-callable interface:

\begin{code}
occurAnalyseExpr :: (Id -> Bool)	-- Tells if a variable is interesting
		 -> CoreExpr
		 -> (IdEnv BinderInfo,	-- Occ info for interesting free vars
		     CoreExpr)

occurAnalyseExpr interesting expr
  = occAnal initial_env expr
  where
    initial_env = OccEnv interesting emptyVarSet []

occurAnalyseGlobalExpr :: CoreExpr -> CoreExpr
occurAnalyseGlobalExpr expr
  = 	-- Top level expr, so no interesting free vars, and
	-- discard occurence info returned
    snd (occurAnalyseExpr (\_ -> False) expr)
\end{code}


%************************************************************************
%*									*
\subsection{Top level stuff}
%*									*
%************************************************************************

In @occAnalTop@ we do indirection-shorting.  That is, if we have this:

	loc = <expression>
	...
	exp = loc

where exp is exported, and loc is not, then we replace it with this:

	loc = exp
	exp = <expression>
	...

Without this we never get rid of the exp = loc thing.
This save a gratuitous jump
(from \tr{x_exported} to \tr{x_local}), and makes strictness
information propagate better.
This used to happen in the final phase, but it's tidier to do it here.


If more than one exported thing is equal to a local thing (i.e., the
local thing really is shared), then we do one only:
\begin{verbatim}
	x_local = ....
	x_exported1 = x_local
	x_exported2 = x_local
==>
	x_exported1 = ....

	x_exported2 = x_exported1
\end{verbatim}

We rely on prior eta reduction to simplify things like
\begin{verbatim}
	x_exported = /\ tyvars -> x_local tyvars
==>
	x_exported = x_local
\end{verbatim}
Hence,there's a possibility of leaving unchanged something like this:
\begin{verbatim}
	x_local = ....
	x_exported1 = x_local Int
\end{verbatim}
By the time we've thrown away the types in STG land this 
could be eliminated.  But I don't think it's very common
and it's dangerous to do this fiddling in STG land 
because we might elminate a binding that's mentioned in the
unfolding for something.

\begin{code}
occurAnalyseBinds :: [CoreBind] -> [CoreBind]

occurAnalyseBinds binds
  = binds'
  where
    (_, _, binds') = go initialTopEnv binds

    go :: OccEnv -> [CoreBind]
       -> (UsageDetails, 	-- Occurrence info
	   IdEnv Id,		-- Indirection elimination info
	   [CoreBind])

    go env [] = (emptyDetails, emptyVarEnv, [])

    go env (bind : binds)
      = let
	    new_env			   = env `addNewCands` (bindersOf bind)
	    (scope_usage, ind_env, binds') = go new_env binds
	    (final_usage, new_binds)       = occAnalBind env (zapBind ind_env bind) scope_usage
						-- NB: I zap before occur-analysing, so
						-- I don't need to worry about getting the
						-- occ info on the new bindings right.
	in
        case bind of
	    NonRec exported_id (Var local_id) 
		| shortMeOut ind_env exported_id local_id
		-- Special case for eliminating indirections
		--   Note: it's a shortcoming that this only works for
		--	   non-recursive bindings.  Elminating indirections
		--	   makes perfect sense for recursive bindings too, but
		--	   it's more complicated to implement, so I haven't done so
		-> (scope_usage, ind_env', binds')
		where
		   ind_env' = extendVarEnv ind_env local_id exported_id

	    other -> 	-- Ho ho! The normal case
		   (final_usage, ind_env, new_binds ++ binds')
		   
initialTopEnv = OccEnv isLocallyDefined	-- Anything local is interesting
		       emptyVarSet
		       []


-- Deal with any indirections
zapBind ind_env (NonRec bndr rhs) 
  | bndr `elemVarEnv` ind_env 			   = Rec (zap ind_env (bndr,rhs))
		-- The Rec isn't strictly necessary, but it's convenient
zapBind ind_env (Rec pairs)
  | or [id `elemVarEnv` ind_env | (id,_) <- pairs] = Rec (concat (map (zap ind_env) pairs))

zapBind ind_env bind = bind

zap ind_env pair@(bndr,rhs)
  = case lookupVarEnv ind_env bndr of
	Nothing          -> [pair]
	Just exported_id -> [(bndr, Var exported_id),
			     (exported_id_w_info, rhs)]
		         where
			   exported_id_w_info = modifyIdInfo (copyIdInfo (idInfo bndr)) exported_id
				-- See notes with copyIdInfo about propagating IdInfo from
				-- one to t'other
			
shortMeOut ind_env exported_id local_id
  = isExportedId exported_id &&		-- Only if this is exported

    isLocallyDefined local_id &&	-- Only if this one is defined in this
					-- 	module, so that we *can* change its
				  	-- 	binding to be the exported thing!

    not (isExportedId local_id) &&	-- Only if this one is not itself exported,
					--	since the transformation will nuke it

    not (local_id `elemVarEnv` ind_env)		-- Only if not already substituted for
\end{code}


%************************************************************************
%*									*
\subsection[OccurAnal-main]{Counting occurrences: main function}
%*									*
%************************************************************************

Bindings
~~~~~~~~

\begin{code}
type IdWithOccInfo = Id			-- An Id with fresh PragmaInfo attached

type Node details = (details, Int, [Int])	-- The Ints are gotten from the Unique,
						-- which is gotten from the Id.
type Details1	  = (Id, UsageDetails, CoreExpr)
type Details2	  = (IdWithOccInfo, CoreExpr)


occAnalBind :: OccEnv
	    -> CoreBind
	    -> UsageDetails		-- Usage details of scope
	    -> (UsageDetails,		-- Of the whole let(rec)
		[CoreBind])

occAnalBind env (NonRec binder rhs) body_usage
  | not (binder `usedIn` body_usage)		-- It's not mentioned
  = (body_usage, [])

  | otherwise			-- It's mentioned in the body
  = (final_body_usage `combineUsageDetails` rhs_usage,
     [NonRec tagged_binder rhs'])

  where
    (final_body_usage, tagged_binder) = tagBinder body_usage binder
    (rhs_usage, rhs')		      = occAnalRhs env binder rhs
\end{code}

Dropping dead code for recursive bindings is done in a very simple way:

	the entire set of bindings is dropped if none of its binders are
	mentioned in its body; otherwise none are.

This seems to miss an obvious improvement.
@
	letrec  f = ...g...
		g = ...f...
	in
	...g...

===>

	letrec f = ...g...
	       g = ...(...g...)...
	in
	...g...
@

Now @f@ is unused. But dependency analysis will sort this out into a
@letrec@ for @g@ and a @let@ for @f@, and then @f@ will get dropped.
It isn't easy to do a perfect job in one blow.  Consider

@
	letrec f = ...g...
	       g = ...h...
	       h = ...k...
	       k = ...m...
	       m = ...m...
	in
	...m...
@


\begin{code}
occAnalBind env (Rec pairs) body_usage
  = foldr (_scc_ "occAnalBind.dofinal" do_final_bind) (body_usage, []) sccs
  where
    pp_item (_, bndr, _)     = ppr bndr

    binders = map fst pairs
    new_env = env `addNewCands` binders

    analysed_pairs :: [Details1]
    analysed_pairs  = [ (bndr, rhs_usage, rhs')
		      | (bndr, rhs) <- pairs,
			let (rhs_usage, rhs') = occAnalRhs new_env bndr rhs
		      ]

    sccs :: [SCC (Node Details1)]
    sccs = _scc_ "occAnalBind.scc" stronglyConnCompR edges


    ---- stuff for dependency analysis of binds -------------------------------
    edges :: [Node Details1]
    edges = _scc_ "occAnalBind.assoc"
	    [ (details, IBOX(u2i (idUnique id)), edges_from rhs_usage)
	    | details@(id, rhs_usage, rhs) <- analysed_pairs
	    ]

	-- (a -> b) means a mentions b
	-- Given the usage details (a UFM that gives occ info for each free var of
	-- the RHS) we can get the list of free vars -- or rather their Int keys --
	-- by just extracting the keys from the finite map.  Grimy, but fast.
	-- Previously we had this:
	-- 	[ bndr | bndr <- bndrs,
	--		 maybeToBool (lookupVarEnv rhs_usage bndr)]
	-- which has n**2 cost, and this meant that edges_from alone 
	-- consumed 10% of total runtime!
    edges_from :: UsageDetails -> [Int]
    edges_from rhs_usage = _scc_ "occAnalBind.edges_from"
			   keysUFM rhs_usage

    ---- stuff to "re-constitute" bindings from dependency-analysis info ------

	-- Non-recursive SCC
    do_final_bind (AcyclicSCC ((bndr, rhs_usage, rhs'), _, _)) (body_usage, binds_so_far)
      | not (bndr `usedIn` body_usage)
      = (body_usage, binds_so_far)			-- Dead code
      | otherwise
      = (combined_usage, new_bind : binds_so_far)	
      where
	total_usage       	      = combineUsageDetails body_usage rhs_usage
	(combined_usage, tagged_bndr) = tagBinder total_usage bndr
	new_bind		      = NonRec tagged_bndr rhs'

	-- Recursive SCC
    do_final_bind (CyclicSCC cycle) (body_usage, binds_so_far)
      | not (any (`usedIn` body_usage) bndrs)		-- NB: look at body_usage, not total_usage
      = (body_usage, binds_so_far)			-- Dead code
      | otherwise
      = (combined_usage, final_bind:binds_so_far)
      where
	details			       = [details   | (details, _, _) <- cycle]
	bndrs			       = [bndr      | (bndr, _, _)      <- details]
	rhs_usages		       = [rhs_usage | (_, rhs_usage, _) <- details]
	total_usage		       = foldr combineUsageDetails body_usage rhs_usages
	(combined_usage, tagged_bndrs) = tagBinders total_usage bndrs
	final_bind		       = Rec (reOrderRec env new_cycle)

	new_cycle = CyclicSCC (zipWithEqual "occAnalBind" mk_new_bind tagged_bndrs cycle)
	mk_new_bind tagged_bndr ((_, _, rhs'), key, keys) = ((tagged_bndr, rhs'), key, keys)
\end{code}

@reOrderRec@ is applied to the list of (binder,rhs) pairs for a cyclic
strongly connected component (there's guaranteed to be a cycle).  It returns the
same pairs, but 
	a) in a better order,
	b) with some of the Ids having a IMustNotBeINLINEd pragma

The "no-inline" Ids are sufficient to break all cycles in the SCC.  This means
that the simplifier can guarantee not to loop provided it never records an inlining
for these no-inline guys.

Furthermore, the order of the binds is such that if we neglect dependencies
on the no-inline Ids then the binds are topologically sorted.  This means
that the simplifier will generally do a good job if it works from top bottom,
recording inlinings for any Ids which aren't marked as "no-inline" as it goes.

==============
[June 98: I don't understand the following paragraphs, and I've 
	  changed the a=b case again so that it isn't a special case any more.]

Here's a case that bit me:

	letrec
		a = b
		b = \x. BIG
	in
	...a...a...a....

Re-ordering doesn't change the order of bindings, but there was no loop-breaker.

My solution was to make a=b bindings record b as Many, rather like INLINE bindings.
Perhaps something cleverer would suffice.
===============

You might think that you can prevent non-termination simply by making
sure that we simplify a recursive binding's RHS in an environment that
simply clones the recursive Id.  But no.  Consider

		letrec f = \x -> let z = f x' in ...

		in
		let n = f y
		in
		case n of { ... }

We bind n to its *simplified* RHS, we then *re-simplify* it when
we inline n.  Then we may well inline f; and then the same thing
happens with z!

I don't think it's possible to prevent non-termination by environment
manipulation in this way.  Apart from anything else, successive
iterations of the simplifier may unroll recursive loops in cases like
that above.  The idea of beaking every recursive loop with an
IMustNotBeINLINEd pragma is much much better.


\begin{code}
reOrderRec
	:: OccEnv
	-> SCC (Node Details2)
	-> [Details2]
			-- Sorted into a plausible order.  Enough of the Ids have
			--	dontINLINE pragmas that there are no loops left.

	-- Non-recursive case
reOrderRec env (AcyclicSCC (bind, _, _)) = [bind]

	-- Common case of simple self-recursion
reOrderRec env (CyclicSCC [bind])
  = [(setInlinePragma tagged_bndr IAmALoopBreaker, rhs)]
  where
    ((tagged_bndr, rhs), _, _) = bind

reOrderRec env (CyclicSCC (bind : binds))
  = 	-- Choose a loop breaker, mark it no-inline,
	-- do SCC analysis on the rest, and recursively sort them out
    concat (map (reOrderRec env) (stronglyConnCompR unchosen))
    ++ 
    [(setInlinePragma tagged_bndr IAmALoopBreaker, rhs)]

  where
    (chosen_pair, unchosen) = choose_loop_breaker bind (score bind) [] binds
    (tagged_bndr, rhs)      = chosen_pair

	-- This loop looks for the bind with the lowest score
	-- to pick as the loop  breaker.  The rest accumulate in 
    choose_loop_breaker (details,_,_) loop_sc acc []
	= (details, acc)	-- Done

    choose_loop_breaker loop_bind loop_sc acc (bind : binds)
	| sc < loop_sc	-- Lower score so pick this new one
	= choose_loop_breaker bind sc (loop_bind : acc) binds

	| otherwise	-- No lower so don't pick it
	= choose_loop_breaker loop_bind loop_sc (bind : acc) binds
	where
	  sc = score bind
	  
    score :: Node Details2 -> Int	-- Higher score => less likely to be picked as loop breaker
    score ((bndr, rhs), _, _)
	| exprIsTrivial rhs && 
	  not (isExportedId bndr)  = 3		-- Practically certain to be inlined
	| inlineCandidate bndr rhs = 3		-- Likely to be inlined
	| not_fun_ty (idType bndr) = 2		-- Data types help with cases
	| not (isEmptyCoreRules (getIdSpecialisation bndr)) = 1
		-- Avoid things with specialisations; we'd like
		-- to take advantage of them in the subsequent bindings
	| otherwise = 0

    inlineCandidate :: Id -> CoreExpr -> Bool
    inlineCandidate id (Note InlineMe _) = True
    inlineCandidate id rhs	         = case getInlinePragma id of
						IMustBeINLINEd          -> True
						ICanSafelyBeINLINEd _ _ -> True
						other		    -> False

	-- Real example (the Enum Ordering instance from PrelBase):
	--	rec	f = \ x -> case d of (p,q,r) -> p x
	--		g = \ x -> case d of (p,q,r) -> q x
	--		d = (v, f, g)
	--
	-- Here, f and g occur just once; but we can't inline them into d.
	-- On the other hand we *could* simplify those case expressions if
	-- we didn't stupidly choose d as the loop breaker.
	-- But we won't because constructor args are marked "Many".

    not_fun_ty ty = not (maybeToBool (splitFunTy_maybe rho_ty))
		  where
		    (_, rho_ty) = splitForAllTys ty
\end{code}

@occAnalRhs@ deals with the question of bindings where the Id is marked
by an INLINE pragma.  For these we record that anything which occurs
in its RHS occurs many times.  This pessimistically assumes that ths
inlined binder also occurs many times in its scope, but if it doesn't
we'll catch it next time round.  At worst this costs an extra simplifier pass.
ToDo: try using the occurrence info for the inline'd binder.

[March 97] We do the same for atomic RHSs.  Reason: see notes with reOrderRec.
[June 98, SLPJ]  I've undone this change; I don't understand it.  See notes with reOrderRec.


\begin{code}
occAnalRhs :: OccEnv
	   -> Id -> CoreExpr	-- Binder and rhs
	   -> (UsageDetails, CoreExpr)

occAnalRhs env id rhs
  = (final_usage, rhs')
  where
    (rhs_usage, rhs') = occAnal env rhs

	-- [March 98] A new wrinkle is that if the binder has specialisations inside
	-- it then we count the specialised Ids as "extra rhs's".  That way
	-- the "parent" keeps the specialised "children" alive.  If the parent
	-- dies (because it isn't referenced any more), then the children will
	-- die too unless they are already referenced directly.

    final_usage = foldVarSet add rhs_usage (idRuleVars id)
    add v u = addOneOcc u v noBinderInfo	-- Give a non-committal binder info
						-- (i.e manyOcc) because many copies
						-- of the specialised thing can appear
\end{code}

Expressions
~~~~~~~~~~~
\begin{code}
occAnal :: OccEnv
	-> CoreExpr
	-> (UsageDetails,	-- Gives info only about the "interesting" Ids
	    CoreExpr)

occAnal env (Type t)  = (emptyDetails, Type t)

occAnal env (Var v) 
  = (var_uds, Var v)
  where
    var_uds | isCandidate env v = unitVarEnv v funOccZero
	    | otherwise	        = emptyDetails

    -- At one stage, I gathered the idRuleVars for v here too,
    -- which in a way is the right thing to do.
    -- But that went wrong right after specialisation, when
    -- the *occurrences* of the overloaded function didn't have any
    -- rules in them, so the *specialised* versions looked as if they
    -- weren't used at all.

\end{code}

We regard variables that occur as constructor arguments as "dangerousToDup":

\begin{verbatim}
module A where
f x = let y = expensive x in 
      let z = (True,y) in 
      (case z of {(p,q)->q}, case z of {(p,q)->q})
\end{verbatim}

We feel free to duplicate the WHNF (True,y), but that means
that y may be duplicated thereby.

If we aren't careful we duplicate the (expensive x) call!
Constructors are rather like lambdas in this way.

\begin{code}
	-- For NoRep literals we have to report an occurrence of
	-- the things which tidyCore will later add, so that when
	-- we are compiling the very module in which those thin-air Ids
	-- are defined we have them in scope!
occAnal env expr@(Con (Literal lit) args)
  = ASSERT( null args )
    (mk_lit_uds lit, expr)
  where
    mk_lit_uds (NoRepStr _ _)     = try noRepStrIds
    mk_lit_uds (NoRepInteger _ _) = try noRepIntegerIds
    mk_lit_uds lit		  = emptyDetails

    try vs = foldr add emptyDetails vs
    add v uds | isCandidate env v = extendVarEnv uds v funOccZero
	      | otherwise	  = uds

occAnal env (Con con args)
  = case occAnalArgs env args of { (arg_uds, args') ->
    let	
	-- We mark the free vars of the argument of a constructor as "many"
	-- This means that nothing gets inlined into a constructor argument
	-- position, which is what we want.  Typically those constructor
	-- arguments are just variables, or trivial expressions.
	final_arg_uds    = case con of
				DataCon _ -> mapVarEnv markMany arg_uds
				other	  -> arg_uds
    in
    (final_arg_uds, Con con args')
    }
\end{code}

\begin{code}
occAnal env (Note InlineMe body)
  = case occAnal env body of { (usage, body') -> 
    (mapVarEnv markMany usage, Note InlineMe body')
    }

occAnal env (Note note@(SCC cc) body)
  = case occAnal env body of { (usage, body') ->
    (mapVarEnv markInsideSCC usage, Note note body')
    }

occAnal env (Note note body)
  = case occAnal env body of { (usage, body') ->
    (usage, Note note body')
    }
\end{code}

\begin{code}
occAnal env app@(App fun arg)
  = occAnalApp env (collectArgs app)

-- Ignore type variables altogether
--   (a) occurrences inside type lambdas only not marked as InsideLam
--   (b) type variables not in environment

occAnal env expr@(Lam x body) | isTyVar x
  = case occAnal env body of { (body_usage, body') ->
    (body_usage, Lam x body')
    }

-- For value lambdas we do a special hack.  Consider
-- 	(\x. \y. ...x...)
-- If we did nothing, x is used inside the \y, so would be marked
-- as dangerous to dup.  But in the common case where the abstraction
-- is applied to two arguments this is over-pessimistic.
-- So instead, we just mark each binder with its occurrence
-- info in the *body* of the multiple lambda.
-- Then, the simplifier is careful when partially applying lambdas.

occAnal env expr@(Lam _ _)
  = case occAnal (env_body `addNewCands` binders) body of { (body_usage, body') ->
    let
        (final_usage, tagged_binders) = tagBinders body_usage binders
	really_final_usage = if linear then
				final_usage
			     else
				mapVarEnv markInsideLam final_usage
    in
    (really_final_usage,
     mkLams tagged_binders body') }
  where
    (binders, body)    = collectBinders expr
    (linear, env_body) = getCtxt env (count isId binders)

occAnal env (Case scrut bndr alts)
  = case mapAndUnzip (occAnalAlt alt_env) alts of { (alts_usage_s, alts')   -> 
    case occAnal env scrut		       of { (scrut_usage, scrut') ->
    let
	alts_usage  = foldr1 combineAltsUsageDetails alts_usage_s
	(alts_usage1, tagged_bndr) = tagBinder alts_usage bndr
        total_usage = scrut_usage `combineUsageDetails` alts_usage1
    in
    total_usage `seq` (total_usage, Case scrut' tagged_bndr alts') }}
  where
    alt_env = env `addNewCand` bndr

occAnal env (Let bind body)
  = case occAnal new_env body            of { (body_usage, body') ->
    case occAnalBind env bind body_usage of { (final_usage, new_binds) ->
       (final_usage, mkLets new_binds body') }}
  where
    new_env = env `addNewCands` (bindersOf bind)

occAnalArgs env args
  = case mapAndUnzip (occAnal env) args of	{ (arg_uds_s, args') ->
    (foldr combineUsageDetails emptyDetails arg_uds_s, args')}
\end{code}

Applications are dealt with specially because we want
the "build hack" to work.

\begin{code}
-- Hack for build, fold, runST
occAnalApp env (Var fun, args)
  = case args_stuff of { (args_uds, args') ->
    let
	final_uds = fun_uds `combineUsageDetails` args_uds
    in
    (final_uds, mkApps (Var fun) args') }
  where
    fun_uniq = idUnique fun

    fun_uds | isCandidate env fun = unitVarEnv fun funOccZero
	    | otherwise	  	  = emptyDetails

    args_stuff	| fun_uniq == buildIdKey    = appSpecial env 2 [True,True]  args
		| fun_uniq == augmentIdKey  = appSpecial env 2 [True,True]  args
		| fun_uniq == foldrIdKey    = appSpecial env 3 [False,True] args
		| fun_uniq == runSTRepIdKey = appSpecial env 2 [True]	 args
		| otherwise		    = occAnalArgs env args

occAnalApp env (fun, args)
  = case occAnal env fun of		{ (fun_uds, fun') ->
    case occAnalArgs env args of	{ (args_uds, args') ->
    let
	final_uds = fun_uds `combineUsageDetails` args_uds
    in
    (final_uds, mkApps fun' args') }}
    
appSpecial :: OccEnv -> Int -> CtxtTy -> [CoreExpr] -> (UsageDetails, [CoreExpr])
appSpecial env n ctxt args
  = go n args
  where
    go n [] = (emptyDetails, [])	-- Too few args

    go 1 (arg:args)			-- The magic arg
      = case occAnal (setCtxt env ctxt) arg of	{ (arg_uds, arg') ->
	case occAnalArgs env args of		{ (args_uds, args') ->
	(combineUsageDetails arg_uds args_uds, arg':args') }}
    
    go n (arg:args)
      = case occAnal env arg of		{ (arg_uds, arg') ->
	case go (n-1) args of		{ (args_uds, args') ->
	(combineUsageDetails arg_uds args_uds, arg':args') }}
\end{code}

    
Case alternatives
~~~~~~~~~~~~~~~~~
\begin{code}
occAnalAlt env (con, bndrs, rhs)
  = case occAnal (env `addNewCands` bndrs) rhs of { (rhs_usage, rhs') ->
    let
        (final_usage, tagged_bndrs) = tagBinders rhs_usage bndrs
    in
    (final_usage, (con, tagged_bndrs, rhs')) }
\end{code}


%************************************************************************
%*									*
\subsection[OccurAnal-types]{Data types}
%*									*
%************************************************************************

\begin{code}
-- We gather inforamtion for variables that are either
--	(a) in scope or
--	(b) interesting

data OccEnv =
  OccEnv (Id -> Bool)	-- Tells whether an Id occurrence is interesting,
  	 IdSet		-- In-scope Ids
	 CtxtTy		-- Tells about linearity

type CtxtTy = [Bool]
	-- []	 	No info
	--
	-- True:ctxt  	Analysing a function-valued expression that will be
	--			applied just once
	--
	-- False:ctxt	Analysing a function-valued expression that may
	--			be applied many times; but when it is, 
	--			the CtxtTy inside applies

isCandidate :: OccEnv -> Id -> Bool
isCandidate (OccEnv ifun cands _) id = id `elemVarSet` cands || ifun id

addNewCands :: OccEnv -> [Id] -> OccEnv
addNewCands (OccEnv ifun cands ctxt) ids
  = OccEnv ifun (cands `unionVarSet` mkVarSet ids) ctxt

addNewCand :: OccEnv -> Id -> OccEnv
addNewCand (OccEnv ifun cands ctxt) id
  = OccEnv ifun (extendVarSet cands id) ctxt

setCtxt :: OccEnv -> CtxtTy -> OccEnv
setCtxt (OccEnv ifun cands _) ctxt = OccEnv ifun cands ctxt

getCtxt :: OccEnv -> Int -> (Bool, OccEnv)	-- True <=> this is a linear lambda
						-- The Int is the number of lambdas
getCtxt env@(OccEnv ifun cands []) n = (False, env)
getCtxt (OccEnv ifun cands ctxt)   n = (and (take n ctxt), OccEnv ifun cands (drop n ctxt))
		-- Only return True if *all* the lambdas are linear

type UsageDetails = IdEnv BinderInfo	-- A finite map from ids to their usage

combineUsageDetails, combineAltsUsageDetails
	:: UsageDetails -> UsageDetails -> UsageDetails

combineUsageDetails usage1 usage2
  = plusVarEnv_C addBinderInfo usage1 usage2

combineAltsUsageDetails usage1 usage2
  = plusVarEnv_C orBinderInfo usage1 usage2

addOneOcc :: UsageDetails -> Id -> BinderInfo -> UsageDetails
addOneOcc usage id info
  = plusVarEnv_C addBinderInfo usage (unitVarEnv id info)
	-- ToDo: make this more efficient

emptyDetails = (emptyVarEnv :: UsageDetails)

unitDetails id info = (unitVarEnv id info :: UsageDetails)

usedIn :: Id -> UsageDetails -> Bool
v `usedIn` details =  isExportedId v || v `elemVarEnv` details

tagBinders :: UsageDetails	    -- Of scope
	   -> [Id]		    -- Binders
	   -> (UsageDetails, 	    -- Details with binders removed
	      [IdWithOccInfo])    -- Tagged binders

tagBinders usage binders
 = let
     usage' = usage `delVarEnvList` binders
     uss    = map (setBinderPrag usage) binders
   in
   usage' `seq` (usage', uss)

tagBinder :: UsageDetails	    -- Of scope
	  -> Id			    -- Binders
	  -> (UsageDetails, 	    -- Details with binders removed
	      IdWithOccInfo)	    -- Tagged binders

tagBinder usage binder
 = let
     usage'  = usage `delVarEnv` binder
     binder' = setBinderPrag usage binder
   in
   usage' `seq` (usage', binder')


setBinderPrag :: UsageDetails -> CoreBndr -> CoreBndr
setBinderPrag usage bndr
  | isTyVar bndr
  = bndr

  | otherwise
  = case old_prag of
	NoInlinePragInfo	-> new_bndr
	IAmDead	     		-> new_bndr	-- The next three are annotations
	ICanSafelyBeINLINEd _ _ -> new_bndr	-- from the previous iteration of
	IAmALoopBreaker	        -> new_bndr	-- the occurrence analyser

	other | its_now_dead	-> new_bndr	-- Overwrite the others iff it's now dead
	      | otherwise	-> bndr

  where
    old_prag = getInlinePragma bndr 
    new_bndr = setInlinePragma bndr new_prag

    its_now_dead = case new_prag of
			IAmDead -> True
			other   -> False

    new_prag = occInfoToInlinePrag occ_info

    occ_info
	| isExportedId bndr = noBinderInfo
	-- Don't use local usage info for visible-elsewhere things
	-- But NB that we do set NoInlinePragma for exported things
	-- thereby nuking any IAmALoopBreaker from a previous pass.

	| otherwise       = case lookupVarEnv usage bndr of
				    Nothing   -> deadOccurrence
				    Just info -> info

markBinderInsideLambda :: CoreBndr -> CoreBndr
markBinderInsideLambda bndr
  | isTyVar bndr
  = bndr

  | otherwise
  = case getInlinePragma bndr of
	ICanSafelyBeINLINEd not_in_lam nalts
		-> bndr `setInlinePragma` ICanSafelyBeINLINEd InsideLam nalts
	other   -> bndr

funOccZero = funOccurrence 0
\end{code}
