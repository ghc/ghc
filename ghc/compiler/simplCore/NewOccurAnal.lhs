%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
%************************************************************************
%*									*
\section[NewOccurAnal]{The *New* Occurrence analysis pass}
%*									*
%************************************************************************

The occurrence analyser analyses the way in which variables are used
in their scope, and pins that information on the binder.  It does {\em
not} take any strategic decisions about what to do as a result (eg
discard binding, inline binding etc).  That's the job of the
simplifier.

The occurrence analyser {\em simply} records usage information.  That is,
it pins on each binder info on how that binder occurs in its scope.

Any uses within the RHS of a let(rec) binding for a variable which is
itself unused are ignored.  For example:
@
	let x = ...
 	    y = ...x...
	in
	x+1
@
Here, y is unused, so x will be marked as appearing just once.

An exported Id gets tagged as ManyOcc.

IT MUST OBSERVE SCOPING: CANNOT assume unique binders.

Lambdas
~~~~~~~
The occurrence analyser marks each binder in a lambda the same way.
Thus:
	\ x y -> f y x
will have both x and y marked as single occurrence, and *not* dangerous-to-dup.
Technically, x occurs inside a lambda, and therefore *is* dangerous-to-dup,
but the simplifer very carefully takes care of this special case.
(See the CoLam case in simplExpr.)

Why?  Because typically applications are saturated, in which case x is *not*
dangerous-to-dup.

Things to muse upon
~~~~~~~~~~~~~~~~~~~

There *is* a reason not to substitute for
variables applied to types: it can undo the effect of floating
Consider:
\begin{verbatim}
	c = /\a -> e
	f = /\b -> let d = c b
		   in \ x::b -> ...
\end{verbatim}
Here, inlining c would be a Bad Idea.

At present I've set it up so that the "inside-lambda" flag sets set On
for type-lambdas too, which effectively prevents such substitutions.
I don't *think* it disables any interesting ones either.

Oh yes it does.
Consider

 	 let { (u6.sAMi, <1,0>) = (_build s141374) ua.sALY } in
	 let {
	   (ua.sAMj, <1,0>) =
	       /\ s141380 -> \ (u5.sAM1, <2,0>)  (u6.sAMl, <2,0>) ->
		   let {
		     (u9.sAM7, <2,0>) =
			 \ (u7.sAM2, <3,0>) ->
			     let { (u8.sAM3, <3,0>) = f.sALV u7.sAM2
			     } in  u5.sAM1 u8.sAM3
		   } in  ((foldr s141374) s141380) u9.sAM7 u6.sAMl u6.sAMi
	 } in  (_build s141376) ua.sAMj]

I want to `inline' u6.sAMi, via the foldr/build rule,
but I cant. So I need to inline through /\. I only do it when
I've got a `linear' stack, ie actually real arguments still to apply.

\begin{code}
#include "HsVersions.h"

module NewOccurAnal (
	newOccurAnalyseBinds, newOccurAnalyseExpr,

	-- and to make the interface self-sufficient...
	CoreExpr, CoreBinding, Id, BinderInfo, GlobalSwitch,
	PlainCoreProgram(..), PlainCoreExpr(..),
	SimplifiableCoreExpr(..), SimplifiableCoreBinding(..)
    ) where

IMPORT_Trace
import Outputable	-- ToDo: rm; debugging
import Pretty

import PlainCore	-- the stuff we read...
import TaggedCore	-- ... and produce Simplifiable*

import AbsUniType
import BinderInfo
import CmdLineOpts	( GlobalSwitch(..), SimplifierSwitch(..) )
import Digraph		( stronglyConnComp )
import Id		( eqId, idWantsToBeINLINEd, isConstMethodId_maybe,
			  isSpecPragmaId_maybe, getIdArgUsageInfo,
			  SpecInfo
			)
import IdInfo		-- ( ArgUsage(..), ArgUsageInfo, OptIdInfo(..), getArgUsage)
import IdEnv
import Maybes
import UniqSet
import Util
\end{code}


%************************************************************************
%*									*
\subsection[OccurAnal-types]{Data types}
%*									*
%************************************************************************

\begin{code}
data OccEnv = OccEnv
		Bool		-- Keep-unused-bindings flag
				-- False <=> OK to chuck away binding
				-- 	     and ignore occurrences within it
		Bool		-- Keep-spec-pragma-ids flag
				-- False <=> OK to chuck away spec pragma bindings
				-- 	     and ignore occurrences within it
		Bool		-- Keep-conjurable flag
				-- False <=> OK to throw away *dead*
				-- "conjurable" Ids; at the moment, that
				-- *only* means constant methods, which
				-- are top-level.  A use of a "conjurable"
				-- Id may appear out of thin air -- e.g.,
				-- specialiser conjuring up refs to const
				-- methods.
		Bool		-- IgnoreINLINEPragma flag
				-- False <=> OK to use INLINEPragma information
				-- True  <=> ignore INLINEPragma information
		(UniqSet Id)	-- Candidates

addNewCands :: OccEnv -> [Id] -> OccEnv
addNewCands (OccEnv keep_dead keep_spec keep_conjurable ignore_inline_pragma cands) ids
  = OccEnv keep_dead keep_spec keep_conjurable ignore_inline_pragma (cands `unionUniqSets` mkUniqSet ids)

addNewCand :: OccEnv -> Id -> OccEnv
addNewCand (OccEnv keep_dead keep_spec keep_conjurable ignore_inline_pragma cands) id
  = OccEnv keep_dead keep_spec keep_conjurable ignore_inline_pragma (cands `unionUniqSets` singletonUniqSet id)

isCandidate :: OccEnv -> Id -> Bool
isCandidate (OccEnv _ _ _ _ cands) id = id `elementOfUniqSet` cands

ignoreINLINEPragma :: OccEnv -> Bool
ignoreINLINEPragma (OccEnv _ _ _ ignore_inline_pragma _) = ignore_inline_pragma

keepUnusedBinding :: OccEnv -> Id -> Bool
keepUnusedBinding (OccEnv keep_dead keep_spec keep_conjurable _ _) binder
  = keep_dead || (keep_spec && is_spec)
  where
    is_spec = maybeToBool (isSpecPragmaId_maybe binder)

keepBecauseConjurable :: OccEnv -> Id -> Bool
keepBecauseConjurable (OccEnv _ _ keep_conjurable _ _) binder
  = keep_conjurable && is_conjurable
  where
    is_conjurable = maybeToBool (isConstMethodId_maybe binder)

type UsageDetails = IdEnv BinderInfo	-- A finite map from ids to their usage

combineUsageDetails, combineAltsUsageDetails
	:: UsageDetails -> UsageDetails -> UsageDetails

combineUsageDetails usage1 usage2
  = --BSCC("combineUsages")
    combineIdEnvs combineBinderInfo usage1 usage2
    --ESCC

combineAltsUsageDetails usage1 usage2
  = --BSCC("combineUsages")
    combineIdEnvs combineAltsBinderInfo usage1 usage2
    --ESCC

addOneOcc :: UsageDetails -> Id -> BinderInfo -> UsageDetails
addOneOcc usage id info = combineIdEnvs combineBinderInfo usage (unitIdEnv id info)
	-- ToDo: make this more efficient

emptyDetails = (nullIdEnv :: UsageDetails)

unitDetails id info = (unitIdEnv id info :: UsageDetails)

tagBinders :: UsageDetails 		-- Of scope
	   -> [Id]			-- Binders
	   -> (UsageDetails, 		-- Details with binders removed
	      [(Id,BinderInfo)])	-- Tagged binders

tagBinders usage binders
  = (usage `delManyFromIdEnv` binders,
     [(binder, usage_of usage binder) | binder <- binders]
    )

tagBinder :: UsageDetails 		-- Of scope
	  -> Id				-- Binders
	  -> (UsageDetails, 		-- Details with binders removed
	      (Id,BinderInfo))		-- Tagged binders

tagBinder usage binder
  = (usage `delOneFromIdEnv` binder,
     (binder, usage_of usage binder)
    )

usage_of usage binder
  | isExported binder = ManyOcc	0 -- Exported things count as many
  | otherwise
  = case lookupIdEnv usage binder of
      Nothing   -> DeadCode
      Just info -> info

fixStkToZero :: Id -> UsageDetails -> UsageDetails
fixStkToZero id env = modifyIdEnv env setBinderInfoArityToZero id

isNeeded env usage binder
  = case usage_of usage binder of	
      DeadCode  -> keepUnusedBinding env binder	-- Maybe keep it anyway
      other     -> True
\end{code}


%************************************************************************
%*									*
\subsection[OccurAnal-main]{Counting occurrences: main function}
%*									*
%************************************************************************

Here's the externally-callable interface:

\begin{code}
newOccurAnalyseBinds
	:: [PlainCoreBinding]		-- input
	-> (GlobalSwitch -> Bool)
	-> (SimplifierSwitch -> Bool)
	-> [SimplifiableCoreBinding]	-- output

newOccurAnalyseBinds binds global_sw_chkr simplifier_sw_chkr
  | global_sw_chkr D_dump_occur_anal = pprTrace "OccurAnal:" (ppr PprDebug binds') binds'
  | otherwise	  		     = binds'
  where
    (_, binds') = do initial_env binds

    initial_env = OccEnv (simplifier_sw_chkr KeepUnusedBindings)
			 (simplifier_sw_chkr KeepSpecPragmaIds)
			 (not (simplifier_sw_chkr SimplMayDeleteConjurableIds))
			 (simplifier_sw_chkr IgnoreINLINEPragma)
			 emptyUniqSet

    do env [] = (emptyDetails, [])
    do env (bind:binds)
      = (final_usage, new_binds ++ the_rest)
      where
	new_env			 = env `addNewCands` (bindersOf bind)
	(binds_usage, the_rest)  = do new_env binds
	(final_usage, new_binds) = --BSCC("occAnalBind1")
				   occAnalBind env bind binds_usage
				   --ESCC
\end{code}

\begin{code}
newOccurAnalyseExpr :: UniqSet Id 			-- Set of interesting free vars
		 -> PlainCoreExpr 
		 -> (IdEnv BinderInfo,		-- Occ info for interesting free vars
		     SimplifiableCoreExpr)

newOccurAnalyseExpr candidates expr
  = occAnal initial_env initContext expr
  where
    initial_env = OccEnv False {- Drop unused bindings -}
			 False {- Drop SpecPragmaId bindings -}
			 True  {- Keep conjurable Ids -}
			 False {- Do not ignore INLINE Pragma -}
			 candidates

newOccurAnalyseGlobalExpr :: PlainCoreExpr -> SimplifiableCoreExpr
newOccurAnalyseGlobalExpr expr
  = 	-- Top level expr, so no interesting free vars, and 
	-- discard occurence info returned
    expr' where (_, expr') = newOccurAnalyseExpr emptyUniqSet expr
\end{code}

%************************************************************************
%*									*
\subsection[OccurAnal-main]{Counting occurrences: main function}
%*									*
%************************************************************************

Bindings
~~~~~~~~

\begin{code}
occAnalBind :: OccEnv
	    -> PlainCoreBinding
	    -> UsageDetails		-- Usage details of scope
	    -> (UsageDetails,		-- Of the whole let(rec)
		[SimplifiableCoreBinding])

occAnalBind env (CoNonRec binder rhs) body_usage
  | isNeeded env body_usage binder		-- It's mentioned in body
  = (final_body_usage `combineUsageDetails` rhs_usage,
     [CoNonRec tagged_binder rhs'])

  | otherwise
  = (body_usage, [])

  where
    stk = mkContextFromBinderInfo (usage_of body_usage binder)
    (rhs_usage, rhs')		      = occAnalRhs env binder stk rhs
    (final_body_usage, tagged_binder) = tagBinder body_usage binder

occAnalBind env (CoRec [(binder,rhs)]) body_usage
  | getContextSize after_stk < getContextSize stk && mentions_itself
				-- our pre-condition does not hold!
				-- so, we have to go back, and
				-- *make* of pre-condition hold.
	-- Will, you can leave out this trace
  = {-pprTrace ("after_stk < stk (BAD, BAD, VERY VERY BAD):" 
	++ show (getContextSize after_stk,getContextSize stk)) (ppr PprDebug binder) -}
    (occAnalBind env (CoRec [(binder,rhs)]) (fixStkToZero binder body_usage))

  | isNeeded env body_usage binder		-- It's mentioned in body
  = --BSCC("occAnalBindC")
    (final_usage, [final_bind])
    --ESCC

  | otherwise
  = --BSCC("occAnalBindD")
    (body_usage, [])
    --ESCC

  where
    stk = shareContext (mkContextFromBinderInfo (usage_of body_usage binder))
    new_env			 = env `addNewCand` binder
    (rhs_usage, rhs') 	  	 = occAnalRhs new_env binder stk rhs
    total_usage 		 = combineUsageDetails body_usage rhs_usage
    (final_usage, tagged_binder) = tagBinder total_usage binder

    after_stk = mkContextFromBinderInfo (usage_of rhs_usage binder)

    final_bind = if mentions_itself
		 then CoRec    [(tagged_binder,rhs')]
		 else CoNonRec tagged_binder rhs'

    mentions_itself = maybeToBool (lookupIdEnv rhs_usage binder)
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
occAnalBind env (CoRec pairs) body_usage
  = foldr do_final_bind (body_usage, []) sccs
  where

    (binders, rhss) = unzip pairs
    new_env	    = env `addNewCands` binders

    analysed_pairs :: [(Id, (UsageDetails, SimplifiableCoreExpr))]
    analysed_pairs  = [(id, occAnalRhs new_env id initContext rhs) | (id,rhs) <- pairs]
    
    lookup :: Id -> (UsageDetails, SimplifiableCoreExpr)
    lookup id =  assoc "occAnalBind:lookup" analysed_pairs id


    ---- stuff for dependency analysis of binds -------------------------------

    edges :: [(Id,Id)]		-- (a,b) means a mentions b
    edges = concat [ edges_from binder rhs_usage 
		   | (binder, (rhs_usage, _)) <- analysed_pairs]

    edges_from :: Id -> UsageDetails -> [(Id,Id)]
    edges_from id its_rhs_usage
      = [(id,mentioned) | mentioned <- binders,
		 	  maybeToBool (lookupIdEnv its_rhs_usage mentioned)
	]

    sccs :: [[Id]]
    sccs = case binders of
		[_]   -> [binders]	-- Singleton; no need to analyse
		other -> stronglyConnComp eqId edges binders

    ---- stuff to "re-constitute" bindings from dependency-analysis info ------

    do_final_bind sCC@[binder] (body_usage, binds_so_far)
      | isNeeded env body_usage binder
      = (combined_usage, new_bind:binds_so_far)

      | otherwise		-- Dead
      = (body_usage, binds_so_far)
      where
	total_usage       	        = combineUsageDetails body_usage rhs_usage
	(rhs_usage, rhs') 	        = lookup binder
	(combined_usage, tagged_binder) = tagBinder total_usage binder

	new_bind
	  | mentions_itself binder rhs_usage = CoRec [(tagged_binder,rhs')]
	  | otherwise		             = CoNonRec tagged_binder rhs'
	  where
	    mentions_itself binder usage
	      = maybeToBool (lookupIdEnv usage binder)

    do_final_bind sCC (body_usage, binds_so_far)
      | any (isNeeded env body_usage) sCC
      = (combined_usage, new_bind:binds_so_far)

      | otherwise		-- Dead
      = (body_usage, binds_so_far)
      where
	(rhs_usages, rhss')	         = unzip (map lookup sCC)
	total_usage		         = foldr combineUsageDetails body_usage rhs_usages
	(combined_usage, tagged_binders) = tagBinders total_usage sCC

	new_bind	    	         = CoRec (tagged_binders `zip` rhss')
\end{code}

@occAnalRhs@ deals with the question of bindings where the Id is marked
by an INLINE pragma.  For these we record that anything which occurs
in its RHS occurs many times.  This pessimistically assumes that ths
inlined binder also occurs many times in its scope, but if it doesn't
we'll catch it next time round.  At worst this costs an extra simplifier pass.
ToDo: try using the occurrence info for the inline'd binder.

\begin{code}
occAnalRhs :: OccEnv
	   -> Id		-- Binder
	   -> Context		-- Stack Style Context
	   -> PlainCoreExpr	-- Rhs
	   -> (UsageDetails, SimplifiableCoreExpr)

occAnalRhs env id stk rhs
  | idWantsToBeINLINEd id && not (ignoreINLINEPragma env)
  = (mapIdEnv markMany rhs_usage, rhs')

  | otherwise
  = (rhs_usage, rhs')

  where
    (rhs_usage, rhs') = occAnal env stk rhs
\end{code}

Expressions
~~~~~~~~~~~
\begin{code}
occAnal :: OccEnv
	-> Context
	-> PlainCoreExpr
	-> (UsageDetails, 		-- Gives info only about the "interesting" Ids
	    SimplifiableCoreExpr)

occAnal env stk (CoVar v)
  | isCandidate env v
  = (unitIdEnv v (funOccurrence (getContextSize stk)), CoVar v)

  | otherwise
  = (emptyDetails, CoVar v)

occAnal env _ (CoLit lit)	   = (emptyDetails, CoLit lit)
-- PERHAPS ASSERT THAT STACK == 0 ?
occAnal env _ (CoCon con tys args) = (occAnalAtoms env args, CoCon con tys args)
occAnal env _ (CoPrim op tys args) = (occAnalAtoms env args, CoPrim op tys args)

occAnal env stk (CoSCC lbl body)
  = (mapIdEnv markInsideSCC usage, CoSCC lbl body')
  where
    (usage, body') = occAnal env initContext body	

occAnal env stk (CoApp fun arg)
  = occAnalApp env (incContext stk) [ValArg arg] fun 
occAnal env stk (CoTyApp fun arg)
  = occAnalApp env stk [TypeArg arg] fun
{-
occAnal env (CoApp fun arg)
  = (fun_usage `combineUsageDetails` arg_usage, CoApp fun' arg)
  where
    (fun_usage, fun') = occAnal env fun
    arg_usage	      = occAnalAtom env arg
			
occAnal env (CoTyApp fun ty)
  = (fun_usage, CoTyApp fun' ty)
  where
    (fun_usage, fun') = occAnal env fun
-}
occAnal env stk (CoLam binders body) | isLinContext stk
  = (final_usage, mkCoLam tagged_binders body')
  where
    (lin_binders,other_binders)   = splitAt (getContextSize stk) binders
    new_env			  = env `addNewCands` lin_binders
    (body_usage, body') 	  = occAnal new_env (lamOnContext stk (length lin_binders))
						(mkCoLam other_binders body)
    (final_usage, tagged_binders) = tagBinders body_usage lin_binders

occAnal env stk (CoLam binders body)
  = (mapIdEnv markDangerousToDup final_usage, mkCoLam tagged_binders body')
  where
    new_env			  = env `addNewCands` binders
    (body_usage, body') 	  = occAnal new_env (lamOnContext stk (length binders)) body
    (final_usage, tagged_binders) = tagBinders body_usage binders

{-
occAnal env (CoLam binders body)
  = (mapIdEnv markDangerousToDup final_usage, mkCoLam tagged_binders body')
  where
    new_env			  = env `addNewCands` binders
    (body_usage, body') 	  = occAnal new_env body
    (final_usage, tagged_binders) = tagBinders body_usage binders
-}

occAnal env stk (CoTyLam tyvar body) 
  = (new_body_usage, CoTyLam tyvar body')
  where
    (body_usage, body') = occAnal env stk body
    new_body_usage = if isLinContext stk 
		     then body_usage
		     else mapIdEnv markDangerousToDup body_usage

occAnal env stk (CoCase scrut alts)
  = (scrut_usage `combineUsageDetails` alts_usage,
     CoCase scrut' alts')
  where
    (scrut_usage, scrut') = occAnal env initContext scrut
    (alts_usage, alts')   = occAnalAlts env stk alts


occAnal env stk (CoLet bind body)
  = (final_usage  , foldr CoLet body' new_binds) -- mkCoLets* wants PlainCore... (sigh)
  where
    new_env		     = env `addNewCands` (bindersOf bind)
    (body_usage, body')      = occAnal new_env stk {- ?? -} body
    (final_usage, new_binds) = --BSCC("occAnalBind2")
			       occAnalBind env bind body_usage
			       --ESCC
\end{code}

Case alternatives
~~~~~~~~~~~~~~~~~
\begin{code}
occAnalAlts env stk (CoAlgAlts alts deflt)
  = (foldr combineAltsUsageDetails deflt_usage alts_usage,
	-- Note: combine*Alts*UsageDetails...
     CoAlgAlts alts' deflt')
  where
    (alts_usage,  alts')  = unzip (map do_alt alts)
    (deflt_usage, deflt') = occAnalDeflt env stk deflt

    do_alt (con, args, rhs)
      = (final_usage, (con, tagged_args, rhs'))
      where
	new_env		   = env `addNewCands` args
	(rhs_usage, rhs')	   = occAnal new_env stk rhs
	(final_usage, tagged_args) = tagBinders rhs_usage args

occAnalAlts env stk (CoPrimAlts alts deflt)
  = (foldr combineAltsUsageDetails deflt_usage alts_usage,
	-- Note: combine*Alts*UsageDetails...
     CoPrimAlts alts' deflt')
  where
    (alts_usage, alts')   = unzip (map do_alt alts)
    (deflt_usage, deflt') = occAnalDeflt env stk deflt

    do_alt (lit, rhs)
      = (rhs_usage, (lit, rhs'))
      where
	(rhs_usage, rhs') = occAnal env stk rhs

occAnalDeflt env stk CoNoDefault = (emptyDetails, CoNoDefault)

occAnalDeflt env stk (CoBindDefault binder rhs)
  = (final_usage, CoBindDefault tagged_binder rhs')
  where
    new_env			 = env `addNewCand` binder
    (rhs_usage, rhs')		 = occAnal new_env stk rhs
    (final_usage, tagged_binder) = tagBinder rhs_usage binder
\end{code}


Atoms
~~~~~
\begin{code}
occAnalAtoms :: OccEnv -> [PlainCoreAtom] -> UsageDetails

occAnalAtoms env atoms
  = foldr do_one_atom emptyDetails atoms
  where
    do_one_atom (CoLitAtom lit) usage = usage
    do_one_atom (CoVarAtom v) usage
	| isCandidate env v = addOneOcc usage v (argOccurrence 0)
        | otherwise	    = usage


occAnalArgAtoms :: OccEnv -> [(PlainCoreAtom,ArgUsage)] -> UsageDetails
occAnalArgAtoms env atoms
  = foldr do_one_atom emptyDetails atoms
  where
    do_one_atom (CoLitAtom lit,_) usage = usage
    do_one_atom (CoVarAtom v,ArgUsage ar) usage
	| isCandidate env v = addOneOcc usage v (argOccurrence ar)
        | otherwise	    = usage
    do_one_atom (CoVarAtom v,UnknownArgUsage) usage
	| isCandidate env v = addOneOcc usage v (argOccurrence 0)
        | otherwise	    = usage

occAnalAtom  :: OccEnv -> PlainCoreAtom -> UsageDetails

occAnalAtom env (CoLitAtom lit) = emptyDetails
occAnalAtom env (CoVarAtom v)
  | isCandidate env v = unitDetails v (argOccurrence 0)
  | otherwise         = emptyDetails
--
-- This function looks for (fully) applied calls to special ids.
--
occAnalApp 
	:: OccEnv 
	-> Context 
	-> [PlainCoreArg]
	-> PlainCoreExpr 
	-> (UsageDetails, 	-- Gives info only about the "interesting" Ids
	    SimplifiableCoreExpr)
occAnalApp env stk args fun@(CoVar v)
  | not (null aut) 
  && getContextSize stk >= length aut	-- fully applied
  = (fun_usage `combineUsageDetails` arg_usages,
	applyToArgs fun' args)
    where
	val_args = [ x | ValArg x <- args ] 
	aut = getArgUsage (getIdArgUsageInfo v)
	(fun_usage, fun') = occAnal env stk fun
	arg_usages = occAnalArgAtoms env (zip val_args aut)
occAnalApp env stk args (CoApp fun arg)
  = occAnalApp env (incContext stk) (ValArg arg:args) fun 
occAnalApp env stk args (CoTyApp fun arg)
  = occAnalApp env stk (TypeArg arg:args) fun 
occAnalApp env stk args fun 
  = (fun_usage `combineUsageDetails` arg_usages,
	applyToArgs fun' args)
    where
	(fun_usage, fun') = occAnal env stk fun
	arg_usages = occAnalAtoms env val_args
	val_args = [ x | ValArg x <- args ] 
\end{code}

%************************************************************************
%*									*
\subsection[OccurAnal-main]{Counting occurrences: main function}
%*									*
%************************************************************************

Abstract, but simple rep. for stacks.
\begin{code}
data Context = Context Int Bool	
	-- if b then n > 0
	-- ie. you *can't* have a linear content with *no* arguments.

lamOnContext :: Context -> Int -> Context
lamOnContext (Context n b) i = mkContext (max 0 (n - i)) b

isLinContext :: Context -> Bool
isLinContext (Context n b) = b

getContextSize :: Context -> Int
getContextSize (Context n b) = n

incContext :: Context -> Context
incContext (Context n u) = Context (n + 1) u

initContext :: Context
initContext = Context 0 False

shareContext :: Context -> Context 
shareContext (Context n u) = mkContext n False

mkContext :: Int -> Bool -> Context
mkContext 0 _ = Context 0 False
mkContext i b = Context i b

mkContextFromBinderInfo :: BinderInfo -> Context
mkContextFromBinderInfo (DeadCode)         = mkContext 0 False
mkContextFromBinderInfo (ManyOcc i)        = mkContext i False
mkContextFromBinderInfo bi@(OneOcc _ _ _ _ i)
					   = mkContext i (oneSafeOcc True bi)
\end{code}
 
