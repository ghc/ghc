%
% (c) The AQUA Project, Glasgow University, 1993-1998
%
\section[SimplUtils]{The simplifier utilities}

\begin{code}
module SimplUtils (
	mkLam, mkCase, 

	-- Inlining,
	preInlineUnconditionally, postInlineUnconditionally, activeInline, activeRule,
	inlineMode,

	-- The continuation type
	SimplCont(..), DupFlag(..), LetRhsFlag(..), 
	contIsDupable, contResultType,
	countValArgs, countArgs, pushContArgs,
	mkBoringStop, mkLazyArgStop, mkRhsStop, contIsRhs, contIsRhsOrArg,
	getContArgs, interestingCallContext, interestingArgContext,
	interestingArg, isStrictType

    ) where

#include "HsVersions.h"

import SimplEnv
import DynFlags		( SimplifierSwitch(..), SimplifierMode(..),
			  DynFlags, DynFlag(..), dopt )
import StaticFlags	( opt_UF_UpdateInPlace, opt_SimplNoPreInlining,
			  opt_RulesOff )
import CoreSyn
import CoreFVs		( exprFreeVars )
import CoreUtils	( cheapEqExpr, exprType, exprIsTrivial, 
			  etaExpand, exprEtaExpandArity, bindNonRec, mkCoerce,
			  findDefault, exprOkForSpeculation, exprIsHNF, mergeAlts,
                          applyTypeToArgs
			)
import Literal		( mkStringLit )
import CoreUnfold	( smallEnoughToInline )
import MkId		( eRROR_ID, wrapNewTypeBody )
import Id		( Id, idType, isDataConWorkId, idOccInfo, isDictId, 
			  isDeadBinder, idNewDemandInfo, isExportedId, mkSysLocal,
			  idUnfolding, idNewStrictness, idInlinePragma, idHasRules
			)
import NewDemand	( isStrictDmd, isBotRes, splitStrictSig )
import SimplMonad
import Var              ( tyVarKind, mkTyVar )
import Name             ( mkSysTvName )
import Type		( Type, splitFunTys, dropForAlls, isStrictType,
			  splitTyConApp_maybe, tyConAppArgs, mkTyVarTys ) 
import Coercion         ( isEqPredTy
			)
import Coercion         ( Coercion, mkUnsafeCoercion, coercionKind )
import TyCon		( tyConDataCons_maybe, isClosedNewTyCon )
import DataCon		( DataCon, dataConRepArity, dataConInstArgTys, dataConTyCon )
import VarSet
import BasicTypes	( TopLevelFlag(..), isNotTopLevel, OccInfo(..), isLoopBreaker, isOneOcc,
			  Activation, isAlwaysActive, isActive )
import Util		( lengthExceeds )
import Outputable
\end{code}


%************************************************************************
%*									*
\subsection{The continuation data type}
%*									*
%************************************************************************

\begin{code}
data SimplCont		-- Strict contexts
  = Stop     OutType	-- Type of the result
	     LetRhsFlag
	     Bool	-- True <=> There is something interesting about
			--          the context, and hence the inliner
			--	    should be a bit keener (see interestingCallContext)
			-- Two cases:
			-- (a) This is the RHS of a thunk whose type suggests
			--     that update-in-place would be possible
			-- (b) This is an argument of a function that has RULES
			--     Inlining the call might allow the rule to fire

  | CoerceIt OutCoercion		-- The coercion simplified
	     SimplCont

  | ApplyTo  DupFlag 
	     CoreExpr		-- The argument
	     (Maybe SimplEnv)   -- (Just se) => the arg is un-simplified and this is its subst-env
				-- Nothing   => the arg is already simplified; don't repeatedly simplify it!
	     SimplCont		-- and its environment

  | Select   DupFlag 
	     InId [InAlt] SimplEnv	-- The case binder, alts, and subst-env
	     SimplCont

  | ArgOf    LetRhsFlag		-- An arbitrary strict context: the argument 
  	     			-- 	of a strict function, or a primitive-arg fn
				-- 	or a PrimOp
				-- No DupFlag, because we never duplicate it
	     OutType		-- arg_ty: type of the argument itself
	     OutType		-- cont_ty: the type of the expression being sought by the context
				--	f (error "foo") ==> coerce t (error "foo")
				-- when f is strict
				-- We need to know the type t, to which to coerce.

	     (SimplEnv -> OutExpr -> SimplM FloatsWithExpr)	-- What to do with the result
				-- The result expression in the OutExprStuff has type cont_ty

data LetRhsFlag = AnArg		-- It's just an argument not a let RHS
		| AnRhs 	-- It's the RHS of a let (so please float lets out of big lambdas)

instance Outputable LetRhsFlag where
  ppr AnArg = ptext SLIT("arg")
  ppr AnRhs = ptext SLIT("rhs")

instance Outputable SimplCont where
  ppr (Stop ty is_rhs _)  	     = ptext SLIT("Stop") <> brackets (ppr is_rhs) <+> ppr ty
  ppr (ApplyTo dup arg se cont)      = (ptext SLIT("ApplyTo") <+> ppr dup <+> ppr arg) $$ ppr cont
  ppr (ArgOf _ _ _ _)   	     = ptext SLIT("ArgOf...")
  ppr (Select dup bndr alts se cont) = (ptext SLIT("Select") <+> ppr dup <+> ppr bndr) $$ 
				       (nest 4 (ppr alts)) $$ ppr cont
  ppr (CoerceIt co cont)	     = (ptext SLIT("CoerceIt") <+> ppr co) $$ ppr cont

data DupFlag = OkToDup | NoDup

instance Outputable DupFlag where
  ppr OkToDup = ptext SLIT("ok")
  ppr NoDup   = ptext SLIT("nodup")



-------------------
mkBoringStop :: OutType -> SimplCont
mkBoringStop ty = Stop ty AnArg False

mkLazyArgStop :: OutType -> Bool -> SimplCont
mkLazyArgStop ty has_rules = Stop ty AnArg (canUpdateInPlace ty || has_rules)

mkRhsStop :: OutType -> SimplCont
mkRhsStop ty = Stop ty AnRhs (canUpdateInPlace ty)

contIsRhs :: SimplCont -> Bool
contIsRhs (Stop _ AnRhs _)    = True
contIsRhs (ArgOf AnRhs _ _ _) = True
contIsRhs other	              = False

contIsRhsOrArg (Stop _ _ _)    = True
contIsRhsOrArg (ArgOf _ _ _ _) = True
contIsRhsOrArg other	       = False

-------------------
contIsDupable :: SimplCont -> Bool
contIsDupable (Stop _ _ _)     		 = True
contIsDupable (ApplyTo  OkToDup _ _ _)   = True
contIsDupable (Select   OkToDup _ _ _ _) = True
contIsDupable (CoerceIt _ cont)          = contIsDupable cont
contIsDupable other	  	         = False

-------------------
discardableCont :: SimplCont -> Bool
discardableCont (Stop _ _ _)	    = False
discardableCont (CoerceIt _ cont)   = discardableCont cont
discardableCont other		    = True

discardCont :: Type             -- The type expected
            -> SimplCont	-- A continuation, expecting the previous type
	    -> SimplCont	-- Replace the continuation with a suitable coerce
discardCont from_ty cont = case cont of
		     Stop to_ty is_rhs _ -> cont
		     other               -> CoerceIt co (mkBoringStop to_ty)
		 where
                   co      = mkUnsafeCoercion from_ty to_ty
		   to_ty   = contResultType cont

-------------------
contResultType :: SimplCont -> OutType
contResultType (Stop to_ty _ _)	     = to_ty
contResultType (ArgOf _ _ to_ty _)   = to_ty
contResultType (ApplyTo _ _ _ cont)  = contResultType cont
contResultType (CoerceIt _ cont)     = contResultType cont
contResultType (Select _ _ _ _ cont) = contResultType cont

-------------------
countValArgs :: SimplCont -> Int
countValArgs (ApplyTo _ (Type ty) se cont) = countValArgs cont
countValArgs (ApplyTo _ val_arg   se cont) = 1 + countValArgs cont
countValArgs other			   = 0

countArgs :: SimplCont -> Int
countArgs (ApplyTo _ arg se cont) = 1 + countArgs cont
countArgs other			  = 0

-------------------
pushContArgs ::[OutArg] -> SimplCont -> SimplCont
-- Pushes args with the specified environment
pushContArgs []           cont = cont
pushContArgs (arg : args) cont = ApplyTo NoDup arg Nothing (pushContArgs args cont)
\end{code}


\begin{code}
getContArgs :: SwitchChecker
	    -> OutId -> SimplCont 
	    -> ([(InExpr, Maybe SimplEnv, Bool)],	-- Arguments; the Bool is true for strict args
		SimplCont)				-- Remaining continuation
-- getContArgs id k = (args, k', inl)
-- 	args are the leading ApplyTo items in k
--	(i.e. outermost comes first)
--	augmented with demand info from the functionn
getContArgs chkr fun orig_cont
  = let
		-- Ignore strictness info if the no-case-of-case
		-- flag is on.  Strictness changes evaluation order
		-- and that can change full laziness
	stricts | switchIsOn chkr NoCaseOfCase = vanilla_stricts
		| otherwise		       = computed_stricts
    in
    go [] stricts orig_cont
  where
    ----------------------------

	-- Type argument
    go acc ss (ApplyTo _ arg@(Type _) se cont)
	= go ((arg,se,False) : acc) ss cont
		-- NB: don't bother to instantiate the function type

	-- Value argument
    go acc (s:ss) (ApplyTo _ arg se cont)
	= go ((arg,se,s) : acc) ss cont

	-- We're run out of arguments, or else we've run out of demands
	-- The latter only happens if the result is guaranteed bottom
	-- This is the case for
	--	* case (error "hello") of { ... }
	--	* (error "Hello") arg
	--	* f (error "Hello") where f is strict
	--	etc
	-- Then, especially in the first of these cases, we'd like to discard
	-- the continuation, leaving just the bottoming expression.  But the
	-- type might not be right, so we may have to add a coerce.

    go acc ss cont 
	| null ss && discardableCont cont = (args, discardCont hole_ty cont)
	| otherwise			  = (args, cont)
	where
	  args = reverse acc
	  hole_ty = applyTypeToArgs (Var fun) (idType fun)
				    [substExpr_mb se arg | (arg,se,_) <- args]
          substExpr_mb Nothing   arg = arg
	  substExpr_mb (Just se) arg = substExpr se arg
    
    ----------------------------
    vanilla_stricts, computed_stricts :: [Bool]
    vanilla_stricts  = repeat False
    computed_stricts = zipWith (||) fun_stricts arg_stricts

    ----------------------------
    (val_arg_tys, res_ty) = splitFunTys (dropForAlls (idType fun))
    arg_stricts      = map isStrictType val_arg_tys ++ repeat False
	-- These argument types are used as a cheap and cheerful way to find
	-- unboxed arguments, which must be strict.  But it's an InType
	-- and so there might be a type variable where we expect a function
	-- type (the substitution hasn't happened yet).  And we don't bother
	-- doing the type applications for a polymorphic function.
	-- Hence the splitFunTys*IgnoringForAlls*

    ----------------------------
	-- If fun_stricts is finite, it means the function returns bottom
	-- after that number of value args have been consumed
	-- Otherwise it's infinite, extended with False
    fun_stricts
      = case splitStrictSig (idNewStrictness fun) of
	  (demands, result_info)
		| not (demands `lengthExceeds` countValArgs orig_cont)
		-> 	-- Enough args, use the strictness given.
			-- For bottoming functions we used to pretend that the arg
			-- is lazy, so that we don't treat the arg as an
			-- interesting context.  This avoids substituting
			-- top-level bindings for (say) strings into 
			-- calls to error.  But now we are more careful about
			-- inlining lone variables, so its ok (see SimplUtils.analyseCont)
		   if isBotRes result_info then
			map isStrictDmd demands		-- Finite => result is bottom
		   else
			map isStrictDmd demands ++ vanilla_stricts

	  other -> vanilla_stricts	-- Not enough args, or no strictness

-------------------
interestingArg :: OutExpr -> Bool
	-- An argument is interesting if it has *some* structure
	-- We are here trying to avoid unfolding a function that
	-- is applied only to variables that have no unfolding
	-- (i.e. they are probably lambda bound): f x y z
	-- There is little point in inlining f here.
interestingArg (Var v)	         = hasSomeUnfolding (idUnfolding v)
					-- Was: isValueUnfolding (idUnfolding v')
					-- But that seems over-pessimistic
				 || isDataConWorkId v
					-- This accounts for an argument like
					-- () or [], which is definitely interesting
interestingArg (Type _)	         = False
interestingArg (App fn (Type _)) = interestingArg fn
interestingArg (Note _ a)	 = interestingArg a
interestingArg other	         = True
	-- Consider 	let x = 3 in f x
	-- The substitution will contain (x -> ContEx 3), and we want to
	-- to say that x is an interesting argument.
	-- But consider also (\x. f x y) y
	-- The substitution will contain (x -> ContEx y), and we want to say
	-- that x is not interesting (assuming y has no unfolding)
\end{code}

Comment about interestingCallContext
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We want to avoid inlining an expression where there can't possibly be
any gain, such as in an argument position.  Hence, if the continuation
is interesting (eg. a case scrutinee, application etc.) then we
inline, otherwise we don't.  

Previously some_benefit used to return True only if the variable was
applied to some value arguments.  This didn't work:

	let x = _coerce_ (T Int) Int (I# 3) in
	case _coerce_ Int (T Int) x of
		I# y -> ....

we want to inline x, but can't see that it's a constructor in a case
scrutinee position, and some_benefit is False.

Another example:

dMonadST = _/\_ t -> :Monad (g1 _@_ t, g2 _@_ t, g3 _@_ t)

....  case dMonadST _@_ x0 of (a,b,c) -> ....

we'd really like to inline dMonadST here, but we *don't* want to
inline if the case expression is just

	case x of y { DEFAULT -> ... }

since we can just eliminate this case instead (x is in WHNF).  Similar
applies when x is bound to a lambda expression.  Hence
contIsInteresting looks for case expressions with just a single
default case.

\begin{code}
interestingCallContext :: Bool 		-- False <=> no args at all
		       -> Bool		-- False <=> no value args
		       -> SimplCont -> Bool
	-- The "lone-variable" case is important.  I spent ages
	-- messing about with unsatisfactory varaints, but this is nice.
	-- The idea is that if a variable appear all alone
	--	as an arg of lazy fn, or rhs	Stop
	-- 	as scrutinee of a case		Select
	--	as arg of a strict fn		ArgOf
	-- then we should not inline it (unless there is some other reason,
	-- e.g. is is the sole occurrence).  We achieve this by making
	-- interestingCallContext return False for a lone variable.
	--
	-- Why?  At least in the case-scrutinee situation, turning
	--	let x = (a,b) in case x of y -> ...
	-- into
	--	let x = (a,b) in case (a,b) of y -> ...
	-- and thence to 
	--	let x = (a,b) in let y = (a,b) in ...
	-- is bad if the binding for x will remain.
	--
	-- Another example: I discovered that strings
	-- were getting inlined straight back into applications of 'error'
	-- because the latter is strict.
	--	s = "foo"
	--	f = \x -> ...(error s)...

	-- Fundamentally such contexts should not ecourage inlining because
	-- the context can ``see'' the unfolding of the variable (e.g. case or a RULE)
	-- so there's no gain.
	--
	-- However, even a type application or coercion isn't a lone variable.
	-- Consider
	--	case $fMonadST @ RealWorld of { :DMonad a b c -> c }
	-- We had better inline that sucker!  The case won't see through it.
	--
	-- For now, I'm treating treating a variable applied to types 
	-- in a *lazy* context "lone". The motivating example was
	--	f = /\a. \x. BIG
	--	g = /\a. \y.  h (f a)
	-- There's no advantage in inlining f here, and perhaps
	-- a significant disadvantage.  Hence some_val_args in the Stop case

interestingCallContext some_args some_val_args cont
  = interesting cont
  where
    interesting (Select {})              = some_args
    interesting (ApplyTo {})             = True	-- Can happen if we have (coerce t (f x)) y
						-- Perhaps True is a bit over-keen, but I've
						-- seen (coerce f) x, where f has an INLINE prag,
						-- So we have to give some motivaiton for inlining it
    interesting (ArgOf {})	         = some_val_args
    interesting (Stop ty _ interesting)  = some_val_args && interesting
    interesting (CoerceIt _ cont)        = interesting cont
	-- If this call is the arg of a strict function, the context
	-- is a bit interesting.  If we inline here, we may get useful
	-- evaluation information to avoid repeated evals: e.g.
	--	x + (y * z)
	-- Here the contIsInteresting makes the '*' keener to inline,
	-- which in turn exposes a constructor which makes the '+' inline.
	-- Assuming that +,* aren't small enough to inline regardless.
	--
	-- It's also very important to inline in a strict context for things
	-- like
	--		foldr k z (f x)
	-- Here, the context of (f x) is strict, and if f's unfolding is
	-- a build it's *great* to inline it here.  So we must ensure that
	-- the context for (f x) is not totally uninteresting.


-------------------
interestingArgContext :: Id -> SimplCont -> Bool
-- If the argument has form (f x y), where x,y are boring,
-- and f is marked INLINE, then we don't want to inline f.
-- But if the context of the argument is
--	g (f x y) 
-- where g has rules, then we *do* want to inline f, in case it
-- exposes a rule that might fire.  Similarly, if the context is
--	h (g (f x x))
-- where h has rules, then we do want to inline f.
-- The interesting_arg_ctxt flag makes this happen; if it's
-- set, the inliner gets just enough keener to inline f 
-- regardless of how boring f's arguments are, if it's marked INLINE
--
-- The alternative would be to *always* inline an INLINE function,
-- regardless of how boring its context is; but that seems overkill
-- For example, it'd mean that wrapper functions were always inlined
interestingArgContext fn cont
  = idHasRules fn || go cont
  where
    go (Select {})	      = False
    go (ApplyTo {})	      = False
    go (ArgOf {})	      = True
    go (CoerceIt _ c)	      = go c
    go (Stop _ _ interesting) = interesting

-------------------
canUpdateInPlace :: Type -> Bool
-- Consider   let x = <wurble> in ...
-- If <wurble> returns an explicit constructor, we might be able
-- to do update in place.  So we treat even a thunk RHS context
-- as interesting if update in place is possible.  We approximate
-- this by seeing if the type has a single constructor with a
-- small arity.  But arity zero isn't good -- we share the single copy
-- for that case, so no point in sharing.

canUpdateInPlace ty 
  | not opt_UF_UpdateInPlace = False
  | otherwise
  = case splitTyConApp_maybe ty of 
	Nothing		-> False 
	Just (tycon, _) -> case tyConDataCons_maybe tycon of
				Just [dc]  -> arity == 1 || arity == 2
				           where
					      arity = dataConRepArity dc
				other -> False
\end{code}



%************************************************************************
%*									*
\subsection{Decisions about inlining}
%*									*
%************************************************************************

Inlining is controlled partly by the SimplifierMode switch.  This has two
settings:

	SimplGently	(a) Simplifying before specialiser/full laziness
			(b) Simplifiying inside INLINE pragma
			(c) Simplifying the LHS of a rule
			(d) Simplifying a GHCi expression or Template 
				Haskell splice

	SimplPhase n	Used at all other times

The key thing about SimplGently is that it does no call-site inlining.
Before full laziness we must be careful not to inline wrappers,
because doing so inhibits floating
    e.g. ...(case f x of ...)...
    ==> ...(case (case x of I# x# -> fw x#) of ...)...
    ==> ...(case x of I# x# -> case fw x# of ...)...
and now the redex (f x) isn't floatable any more.

The no-inlining thing is also important for Template Haskell.  You might be 
compiling in one-shot mode with -O2; but when TH compiles a splice before
running it, we don't want to use -O2.  Indeed, we don't want to inline
anything, because the byte-code interpreter might get confused about 
unboxed tuples and suchlike.

INLINE pragmas
~~~~~~~~~~~~~~
SimplGently is also used as the mode to simplify inside an InlineMe note.

\begin{code}
inlineMode :: SimplifierMode
inlineMode = SimplGently
\end{code}

It really is important to switch off inlinings inside such
expressions.  Consider the following example 

     	let f = \pq -> BIG
     	in
     	let g = \y -> f y y
	    {-# INLINE g #-}
     	in ...g...g...g...g...g...

Now, if that's the ONLY occurrence of f, it will be inlined inside g,
and thence copied multiple times when g is inlined.


This function may be inlinined in other modules, so we
don't want to remove (by inlining) calls to functions that have
specialisations, or that may have transformation rules in an importing
scope.

E.g. 	{-# INLINE f #-}
		f x = ...g...

and suppose that g is strict *and* has specialisations.  If we inline
g's wrapper, we deny f the chance of getting the specialised version
of g when f is inlined at some call site (perhaps in some other
module).

It's also important not to inline a worker back into a wrapper.
A wrapper looks like
	wraper = inline_me (\x -> ...worker... )
Normally, the inline_me prevents the worker getting inlined into
the wrapper (initially, the worker's only call site!).  But,
if the wrapper is sure to be called, the strictness analyser will
mark it 'demanded', so when the RHS is simplified, it'll get an ArgOf
continuation.  That's why the keep_inline predicate returns True for
ArgOf continuations.  It shouldn't do any harm not to dissolve the
inline-me note under these circumstances.

Note that the result is that we do very little simplification
inside an InlineMe.  

	all xs = foldr (&&) True xs
	any p = all . map p  {-# INLINE any #-}

Problem: any won't get deforested, and so if it's exported and the
importer doesn't use the inlining, (eg passes it as an arg) then we
won't get deforestation at all.  We havn't solved this problem yet!


preInlineUnconditionally
~~~~~~~~~~~~~~~~~~~~~~~~
@preInlineUnconditionally@ examines a bndr to see if it is used just
once in a completely safe way, so that it is safe to discard the
binding inline its RHS at the (unique) usage site, REGARDLESS of how
big the RHS might be.  If this is the case we don't simplify the RHS
first, but just inline it un-simplified.

This is much better than first simplifying a perhaps-huge RHS and then
inlining and re-simplifying it.  Indeed, it can be at least quadratically
better.  Consider

	x1 = e1
	x2 = e2[x1]
	x3 = e3[x2]
	...etc...
	xN = eN[xN-1]

We may end up simplifying e1 N times, e2 N-1 times, e3 N-3 times etc.
This can happen with cascades of functions too:

	f1 = \x1.e1
	f2 = \xs.e2[f1]
	f3 = \xs.e3[f3]
	...etc...

THE MAIN INVARIANT is this:

	----  preInlineUnconditionally invariant -----
   IF preInlineUnconditionally chooses to inline x = <rhs>
   THEN doing the inlining should not change the occurrence
	info for the free vars of <rhs>
	----------------------------------------------

For example, it's tempting to look at trivial binding like
	x = y
and inline it unconditionally.  But suppose x is used many times,
but this is the unique occurrence of y.  Then inlining x would change
y's occurrence info, which breaks the invariant.  It matters: y
might have a BIG rhs, which will now be dup'd at every occurrenc of x.


Evne RHSs labelled InlineMe aren't caught here, because there might be
no benefit from inlining at the call site.

[Sept 01] Don't unconditionally inline a top-level thing, because that
can simply make a static thing into something built dynamically.  E.g.
	x = (a,b)
	main = \s -> h x

[Remember that we treat \s as a one-shot lambda.]  No point in
inlining x unless there is something interesting about the call site.

But watch out: if you aren't careful, some useful foldr/build fusion
can be lost (most notably in spectral/hartel/parstof) because the
foldr didn't see the build.  Doing the dynamic allocation isn't a big
deal, in fact, but losing the fusion can be.  But the right thing here
seems to be to do a callSiteInline based on the fact that there is
something interesting about the call site (it's strict).  Hmm.  That
seems a bit fragile.

Conclusion: inline top level things gaily until Phase 0 (the last
phase), at which point don't.

\begin{code}
preInlineUnconditionally :: SimplEnv -> TopLevelFlag -> InId -> InExpr -> Bool
preInlineUnconditionally env top_lvl bndr rhs
  | not active 		   = False
  | opt_SimplNoPreInlining = False
  | otherwise = case idOccInfo bndr of
		  IAmDead	     	     -> True	-- Happens in ((\x.1) v)
	  	  OneOcc in_lam True int_cxt -> try_once in_lam int_cxt
		  other 		     -> False
  where
    phase = getMode env
    active = case phase of
		   SimplGently  -> isAlwaysActive prag
		   SimplPhase n -> isActive n prag
    prag = idInlinePragma bndr

    try_once in_lam int_cxt	-- There's one textual occurrence
	| not in_lam = isNotTopLevel top_lvl || early_phase
	| otherwise  = int_cxt && canInlineInLam rhs

-- Be very careful before inlining inside a lambda, becuase (a) we must not 
-- invalidate occurrence information, and (b) we want to avoid pushing a
-- single allocation (here) into multiple allocations (inside lambda).  
-- Inlining a *function* with a single *saturated* call would be ok, mind you.
--	|| (if is_cheap && not (canInlineInLam rhs) then pprTrace "preinline" (ppr bndr <+> ppr rhs) ok else ok)
--	where 
--	 	is_cheap = exprIsCheap rhs
--		ok = is_cheap && int_cxt

	-- 	int_cxt		The context isn't totally boring
	-- E.g. let f = \ab.BIG in \y. map f xs
	-- 	Don't want to substitute for f, because then we allocate
	--	its closure every time the \y is called
	-- But: let f = \ab.BIG in \y. map (f y) xs
	--	Now we do want to substitute for f, even though it's not 
	--	saturated, because we're going to allocate a closure for 
	--	(f y) every time round the loop anyhow.

	-- canInlineInLam => free vars of rhs are (Once in_lam) or Many,
	-- so substituting rhs inside a lambda doesn't change the occ info.
	-- Sadly, not quite the same as exprIsHNF.
    canInlineInLam (Lit l)		= True
    canInlineInLam (Lam b e)		= isRuntimeVar b || canInlineInLam e
    canInlineInLam (Note _ e)		= canInlineInLam e
    canInlineInLam _			= False

    early_phase = case phase of
			SimplPhase 0 -> False
			other	     -> True
-- If we don't have this early_phase test, consider
--	x = length [1,2,3]
-- The full laziness pass carefully floats all the cons cells to
-- top level, and preInlineUnconditionally floats them all back in.
-- Result is (a) static allocation replaced by dynamic allocation
--	     (b) many simplifier iterations because this tickles
--		 a related problem; only one inlining per pass
-- 
-- On the other hand, I have seen cases where top-level fusion is
-- lost if we don't inline top level thing (e.g. string constants)
-- Hence the test for phase zero (which is the phase for all the final
-- simplifications).  Until phase zero we take no special notice of
-- top level things, but then we become more leery about inlining
-- them.  

\end{code}

postInlineUnconditionally
~~~~~~~~~~~~~~~~~~~~~~~~~
@postInlineUnconditionally@ decides whether to unconditionally inline
a thing based on the form of its RHS; in particular if it has a
trivial RHS.  If so, we can inline and discard the binding altogether.

NB: a loop breaker has must_keep_binding = True and non-loop-breakers
only have *forward* references Hence, it's safe to discard the binding
	
NOTE: This isn't our last opportunity to inline.  We're at the binding
site right now, and we'll get another opportunity when we get to the
ocurrence(s)

Note that we do this unconditional inlining only for trival RHSs.
Don't inline even WHNFs inside lambdas; doing so may simply increase
allocation when the function is called. This isn't the last chance; see
NOTE above.

NB: Even inline pragmas (e.g. IMustBeINLINEd) are ignored here Why?
Because we don't even want to inline them into the RHS of constructor
arguments. See NOTE above

NB: At one time even NOINLINE was ignored here: if the rhs is trivial
it's best to inline it anyway.  We often get a=E; b=a from desugaring,
with both a and b marked NOINLINE.  But that seems incompatible with
our new view that inlining is like a RULE, so I'm sticking to the 'active'
story for now.

\begin{code}
postInlineUnconditionally 
    :: SimplEnv -> TopLevelFlag
    -> InId		-- The binder (an OutId would be fine too)
    -> OccInfo 		-- From the InId
    -> OutExpr
    -> Unfolding
    -> Bool
postInlineUnconditionally env top_lvl bndr occ_info rhs unfolding
  | not active		   = False
  | isLoopBreaker occ_info = False
  | isExportedId bndr      = False
  | exprIsTrivial rhs 	   = True
  | otherwise
  = case occ_info of
	-- The point of examining occ_info here is that for *non-values* 
	-- that occur outside a lambda, the call-site inliner won't have
	-- a chance (becuase it doesn't know that the thing
	-- only occurs once).   The pre-inliner won't have gotten
	-- it either, if the thing occurs in more than one branch
	-- So the main target is things like
	--	let x = f y in
	--	case v of
	--	   True  -> case x of ...
	--	   False -> case x of ...
	-- I'm not sure how important this is in practice
      OneOcc in_lam one_br int_cxt	-- OneOcc => no work-duplication issue
	->     smallEnoughToInline unfolding	-- Small enough to dup
			-- ToDo: consider discount on smallEnoughToInline if int_cxt is true
			--
		 	-- NB: Do NOT inline arbitrarily big things, even if one_br is True
			-- Reason: doing so risks exponential behaviour.  We simplify a big
			--	   expression, inline it, and simplify it again.  But if the
			--	   very same thing happens in the big expression, we get 
			--	   exponential cost!
			-- PRINCIPLE: when we've already simplified an expression once, 
			-- make sure that we only inline it if it's reasonably small.

	   &&  ((isNotTopLevel top_lvl && not in_lam) || 
			-- But outside a lambda, we want to be reasonably aggressive
			-- about inlining into multiple branches of case
			-- e.g. let x = <non-value> 
			--	in case y of { C1 -> ..x..; C2 -> ..x..; C3 -> ... } 
			-- Inlining can be a big win if C3 is the hot-spot, even if
			-- the uses in C1, C2 are not 'interesting'
			-- An example that gets worse if you add int_cxt here is 'clausify'

		(isCheapUnfolding unfolding && int_cxt))
			-- isCheap => acceptable work duplication; in_lam may be true
			-- int_cxt to prevent us inlining inside a lambda without some 
			-- good reason.  See the notes on int_cxt in preInlineUnconditionally

      IAmDead -> True	-- This happens; for example, the case_bndr during case of
			-- known constructor:  case (a,b) of x { (p,q) -> ... }
			-- Here x isn't mentioned in the RHS, so we don't want to
			-- create the (dead) let-binding  let x = (a,b) in ...

      other -> False

-- Here's an example that we don't handle well:
--	let f = if b then Left (\x.BIG) else Right (\y.BIG)
--	in \y. ....case f of {...} ....
-- Here f is used just once, and duplicating the case work is fine (exprIsCheap).
-- But
-- * We can't preInlineUnconditionally because that woud invalidate
--   the occ info for b.  
-- * We can't postInlineUnconditionally because the RHS is big, and
--   that risks exponential behaviour
-- * We can't call-site inline, because the rhs is big
-- Alas!

  where
    active = case getMode env of
		   SimplGently  -> isAlwaysActive prag
		   SimplPhase n -> isActive n prag
    prag = idInlinePragma bndr

activeInline :: SimplEnv -> OutId -> OccInfo -> Bool
activeInline env id occ
  = case getMode env of
      SimplGently -> isOneOcc occ && isAlwaysActive prag
	-- No inlining at all when doing gentle stuff,
	-- except for local things that occur once
	-- The reason is that too little clean-up happens if you 
	-- don't inline use-once things.   Also a bit of inlining is *good* for
	-- full laziness; it can expose constant sub-expressions.
	-- Example in spectral/mandel/Mandel.hs, where the mandelset 
	-- function gets a useful let-float if you inline windowToViewport

	-- NB: we used to have a second exception, for data con wrappers.
	-- On the grounds that we use gentle mode for rule LHSs, and 
	-- they match better when data con wrappers are inlined.
	-- But that only really applies to the trivial wrappers (like (:)),
	-- and they are now constructed as Compulsory unfoldings (in MkId)
	-- so they'll happen anyway.

      SimplPhase n -> isActive n prag
  where
    prag = idInlinePragma id

activeRule :: SimplEnv -> Maybe (Activation -> Bool)
-- Nothing => No rules at all
activeRule env
  | opt_RulesOff = Nothing
  | otherwise
  = case getMode env of
	SimplGently  -> Just isAlwaysActive
			-- Used to be Nothing (no rules in gentle mode)
			-- Main motivation for changing is that I wanted
			-- 	lift String ===> ...
			-- to work in Template Haskell when simplifying
			-- splices, so we get simpler code for literal strings
	SimplPhase n -> Just (isActive n)
\end{code}	


%************************************************************************
%*									*
\subsection{Rebuilding a lambda}
%*									*
%************************************************************************

\begin{code}
mkLam :: SimplEnv -> [OutBinder] -> OutExpr -> SimplCont -> SimplM FloatsWithExpr
\end{code}

Try three things
	a) eta reduction, if that gives a trivial expression
	b) eta expansion [only if there are some value lambdas]
	c) floating lets out through big lambdas 
		[only if all tyvar lambdas, and only if this lambda
		 is the RHS of a let]

\begin{code}
mkLam env bndrs body cont
 = getDOptsSmpl	 `thenSmpl` \dflags ->
   mkLam' dflags env bndrs body cont
 where
 mkLam' dflags env bndrs body cont
   | dopt Opt_DoEtaReduction dflags,
     Just etad_lam <- tryEtaReduce bndrs body
   = tick (EtaReduction (head bndrs)) 	`thenSmpl_`
     returnSmpl (emptyFloats env, etad_lam)

   | dopt Opt_DoLambdaEtaExpansion dflags,
     any isRuntimeVar bndrs
   = tryEtaExpansion dflags body	`thenSmpl` \ body' ->
     returnSmpl (emptyFloats env, mkLams bndrs body')

{-	Sept 01: I'm experimenting with getting the
	full laziness pass to float out past big lambdsa
 | all isTyVar bndrs,	-- Only for big lambdas
   contIsRhs cont	-- Only try the rhs type-lambda floating
			-- if this is indeed a right-hand side; otherwise
			-- we end up floating the thing out, only for float-in
			-- to float it right back in again!
 = tryRhsTyLam env bndrs body		`thenSmpl` \ (floats, body') ->
   returnSmpl (floats, mkLams bndrs body')
-}

   | otherwise 
   = returnSmpl (emptyFloats env, mkLams bndrs body)
\end{code}


%************************************************************************
%*									*
\subsection{Eta expansion and reduction}
%*									*
%************************************************************************

We try for eta reduction here, but *only* if we get all the 
way to an exprIsTrivial expression.    
We don't want to remove extra lambdas unless we are going 
to avoid allocating this thing altogether

\begin{code}
tryEtaReduce :: [OutBinder] -> OutExpr -> Maybe OutExpr
tryEtaReduce bndrs body 
	-- We don't use CoreUtils.etaReduce, because we can be more
	-- efficient here:
	--  (a) we already have the binders
	--  (b) we can do the triviality test before computing the free vars
  = go (reverse bndrs) body
  where
    go (b : bs) (App fun arg) | ok_arg b arg = go bs fun	-- Loop round
    go []       fun           | ok_fun fun   = Just fun		-- Success!
    go _        _			     = Nothing		-- Failure!

    ok_fun fun =  exprIsTrivial fun
	       && not (any (`elemVarSet` (exprFreeVars fun)) bndrs)
	       && (exprIsHNF fun || all ok_lam bndrs)
    ok_lam v = isTyVar v || isDictId v
	-- The exprIsHNF is because eta reduction is not 
	-- valid in general:  \x. bot  /=  bot
	-- So we need to be sure that the "fun" is a value.
	--
	-- However, we always want to reduce (/\a -> f a) to f
	-- This came up in a RULE: foldr (build (/\a -> g a))
	--	did not match 	   foldr (build (/\b -> ...something complex...))
	-- The type checker can insert these eta-expanded versions,
	-- with both type and dictionary lambdas; hence the slightly 
	-- ad-hoc isDictTy

    ok_arg b arg = varToCoreExpr b `cheapEqExpr` arg
\end{code}


	Try eta expansion for RHSs

We go for:
   f = \x1..xn -> N  ==>   f = \x1..xn y1..ym -> N y1..ym
				 (n >= 0)

where (in both cases) 

	* The xi can include type variables

	* The yi are all value variables

	* N is a NORMAL FORM (i.e. no redexes anywhere)
	  wanting a suitable number of extra args.

We may have to sandwich some coerces between the lambdas
to make the types work.   exprEtaExpandArity looks through coerces
when computing arity; and etaExpand adds the coerces as necessary when
actually computing the expansion.

\begin{code}
tryEtaExpansion :: DynFlags -> OutExpr -> SimplM OutExpr
-- There is at least one runtime binder in the binders
tryEtaExpansion dflags body
  = getUniquesSmpl 			`thenSmpl` \ us ->
    returnSmpl (etaExpand fun_arity us body (exprType body))
  where
    fun_arity = exprEtaExpandArity dflags body
\end{code}


%************************************************************************
%*									*
\subsection{Floating lets out of big lambdas}
%*									*
%************************************************************************

tryRhsTyLam tries this transformation, when the big lambda appears as
the RHS of a let(rec) binding:

	/\abc -> let(rec) x = e in b
   ==>
	let(rec) x' = /\abc -> let x = x' a b c in e
	in 
	/\abc -> let x = x' a b c in b

This is good because it can turn things like:

	let f = /\a -> letrec g = ... g ... in g
into
	letrec g' = /\a -> ... g' a ...
	in
	let f = /\ a -> g' a

which is better.  In effect, it means that big lambdas don't impede
let-floating.

This optimisation is CRUCIAL in eliminating the junk introduced by
desugaring mutually recursive definitions.  Don't eliminate it lightly!

So far as the implementation is concerned:

	Invariant: go F e = /\tvs -> F e
	
	Equalities:
		go F (Let x=e in b)
		= Let x' = /\tvs -> F e 
		  in 
		  go G b
		where
		    G = F . Let x = x' tvs
	
		go F (Letrec xi=ei in b)
		= Letrec {xi' = /\tvs -> G ei} 
		  in
		  go G b
		where
		  G = F . Let {xi = xi' tvs}

[May 1999]  If we do this transformation *regardless* then we can
end up with some pretty silly stuff.  For example, 

	let 
	    st = /\ s -> let { x1=r1 ; x2=r2 } in ...
	in ..
becomes
	let y1 = /\s -> r1
	    y2 = /\s -> r2
	    st = /\s -> ...[y1 s/x1, y2 s/x2]
	in ..

Unless the "..." is a WHNF there is really no point in doing this.
Indeed it can make things worse.  Suppose x1 is used strictly,
and is of the form

	x1* = case f y of { (a,b) -> e }

If we abstract this wrt the tyvar we then can't do the case inline
as we would normally do.


\begin{code}
{-	Trying to do this in full laziness

tryRhsTyLam :: SimplEnv -> [OutTyVar] -> OutExpr -> SimplM FloatsWithExpr
-- Call ensures that all the binders are type variables

tryRhsTyLam env tyvars body 		-- Only does something if there's a let
  |  not (all isTyVar tyvars)
  || not (worth_it body)		-- inside a type lambda, 
  = returnSmpl (emptyFloats env, body)	-- and a WHNF inside that

  | otherwise
  = go env (\x -> x) body

  where
    worth_it e@(Let _ _) = whnf_in_middle e
    worth_it e		 = False

    whnf_in_middle (Let (NonRec x rhs) e) | isUnLiftedType (idType x) = False
    whnf_in_middle (Let _ e) = whnf_in_middle e
    whnf_in_middle e	     = exprIsCheap e

    main_tyvar_set = mkVarSet tyvars

    go env fn (Let bind@(NonRec var rhs) body)
      | exprIsTrivial rhs
      = go env (fn . Let bind) body

    go env fn (Let (NonRec var rhs) body)
      = mk_poly tyvars_here var							`thenSmpl` \ (var', rhs') ->
	addAuxiliaryBind env (NonRec var' (mkLams tyvars_here (fn rhs)))	$ \ env -> 
	go env (fn . Let (mk_silly_bind var rhs')) body

      where

	tyvars_here = varSetElems (main_tyvar_set `intersectVarSet` exprSomeFreeVars isTyVar rhs)
		-- Abstract only over the type variables free in the rhs
		-- wrt which the new binding is abstracted.  But the naive
		-- approach of abstract wrt the tyvars free in the Id's type
		-- fails. Consider:
		--	/\ a b -> let t :: (a,b) = (e1, e2)
		--		      x :: a     = fst t
		--		  in ...
		-- Here, b isn't free in x's type, but we must nevertheless
		-- abstract wrt b as well, because t's type mentions b.
		-- Since t is floated too, we'd end up with the bogus:
		--	poly_t = /\ a b -> (e1, e2)
		--	poly_x = /\ a   -> fst (poly_t a *b*)
		-- So for now we adopt the even more naive approach of
		-- abstracting wrt *all* the tyvars.  We'll see if that
		-- gives rise to problems.   SLPJ June 98

    go env fn (Let (Rec prs) body)
       = mapAndUnzipSmpl (mk_poly tyvars_here) vars	`thenSmpl` \ (vars', rhss') ->
	 let
	    gn body = fn (foldr Let body (zipWith mk_silly_bind vars rhss'))
	    pairs   = vars' `zip` [mkLams tyvars_here (gn rhs) | rhs <- rhss]
	 in
	 addAuxiliaryBind env (Rec pairs)		$ \ env ->
	 go env gn body 
       where
	 (vars,rhss) = unzip prs
	 tyvars_here = varSetElems (main_tyvar_set `intersectVarSet` exprsSomeFreeVars isTyVar (map snd prs))
		-- See notes with tyvars_here above

    go env fn body = returnSmpl (emptyFloats env, fn body)

    mk_poly tyvars_here var
      = getUniqueSmpl		`thenSmpl` \ uniq ->
	let
	    poly_name = setNameUnique (idName var) uniq		-- Keep same name
	    poly_ty   = mkForAllTys tyvars_here (idType var)	-- But new type of course
	    poly_id   = mkLocalId poly_name poly_ty 

		-- In the olden days, it was crucial to copy the occInfo of the original var, 
		-- because we were looking at occurrence-analysed but as yet unsimplified code!
		-- In particular, we mustn't lose the loop breakers.  BUT NOW we are looking
		-- at already simplified code, so it doesn't matter
		-- 
		-- It's even right to retain single-occurrence or dead-var info:
		-- Suppose we started with  /\a -> let x = E in B
		-- where x occurs once in B. Then we transform to:
		--	let x' = /\a -> E in /\a -> let x* = x' a in B
		-- where x* has an INLINE prag on it.  Now, once x* is inlined,
		-- the occurrences of x' will be just the occurrences originally
		-- pinned on x.
	in
	returnSmpl (poly_id, mkTyApps (Var poly_id) (mkTyVarTys tyvars_here))

    mk_silly_bind var rhs = NonRec var (Note InlineMe rhs)
		-- Suppose we start with:
		--
		--	x = /\ a -> let g = G in E
		--
		-- Then we'll float to get
		--
		--	x = let poly_g = /\ a -> G
		--	    in /\ a -> let g = poly_g a in E
		--
		-- But now the occurrence analyser will see just one occurrence
		-- of poly_g, not inside a lambda, so the simplifier will
		-- PreInlineUnconditionally poly_g back into g!  Badk to square 1!
		-- (I used to think that the "don't inline lone occurrences" stuff
		--  would stop this happening, but since it's the *only* occurrence,
		--  PreInlineUnconditionally kicks in first!)
		--
		-- Solution: put an INLINE note on g's RHS, so that poly_g seems
		--	     to appear many times.  (NB: mkInlineMe eliminates
		--	     such notes on trivial RHSs, so do it manually.)
-}
\end{code}

%************************************************************************
%*									*
\subsection{Case absorption and identity-case elimination}
%*									*
%************************************************************************


mkCase puts a case expression back together, trying various transformations first.

\begin{code}
mkCase :: OutExpr -> OutId -> OutType
       -> [OutAlt]		-- Increasing order
       -> SimplM OutExpr

mkCase scrut case_bndr ty alts
  = getDOptsSmpl  			`thenSmpl` \dflags ->
    mkAlts dflags scrut case_bndr alts	`thenSmpl` \ better_alts ->
    mkCase1 scrut case_bndr ty better_alts
\end{code}


mkAlts tries these things:

1.  If several alternatives are identical, merge them into
    a single DEFAULT alternative.  I've occasionally seen this 
    making a big difference:

	case e of		=====>     case e of
	  C _ -> f x			     D v -> ....v....
	  D v -> ....v....		     DEFAULT -> f x
	  DEFAULT -> f x

   The point is that we merge common RHSs, at least for the DEFAULT case.
   [One could do something more elaborate but I've never seen it needed.]
   To avoid an expensive test, we just merge branches equal to the *first*
   alternative; this picks up the common cases
	a) all branches equal
	b) some branches equal to the DEFAULT (which occurs first)

2.  Case merging:
       case e of b {             ==>   case e of b {
    	 p1 -> rhs1	                 p1 -> rhs1
    	 ...	                         ...
    	 pm -> rhsm                      pm -> rhsm
    	 _  -> case b of b' {            pn -> let b'=b in rhsn
 		     pn -> rhsn          ...
 		     ...                 po -> let b'=b in rhso
 		     po -> rhso          _  -> let b'=b in rhsd
 		     _  -> rhsd
       }  
    
    which merges two cases in one case when -- the default alternative of
    the outer case scrutises the same variable as the outer case This
    transformation is called Case Merging.  It avoids that the same
    variable is scrutinised multiple times.


The case where transformation (1) showed up was like this (lib/std/PrelCError.lhs):

	x | p `is` 1 -> e1
	  | p `is` 2 -> e2
	...etc...

where @is@ was something like
	
	p `is` n = p /= (-1) && p == n

This gave rise to a horrible sequence of cases

	case p of
	  (-1) -> $j p
	  1    -> e1
	  DEFAULT -> $j p

and similarly in cascade for all the join points!



\begin{code}
--------------------------------------------------
--	1. Merge identical branches
--------------------------------------------------
mkAlts dflags scrut case_bndr alts@((con1,bndrs1,rhs1) : con_alts)
  | all isDeadBinder bndrs1,			-- Remember the default 
    length filtered_alts < length con_alts	-- alternative comes first
  = tick (AltMerge case_bndr)			`thenSmpl_`
    returnSmpl better_alts
  where
    filtered_alts	 = filter keep con_alts
    keep (con,bndrs,rhs) = not (all isDeadBinder bndrs && rhs `cheapEqExpr` rhs1)
    better_alts		 = (DEFAULT, [], rhs1) : filtered_alts


--------------------------------------------------
--	2.  Merge nested cases
--------------------------------------------------

mkAlts dflags scrut outer_bndr outer_alts
  | dopt Opt_CaseMerge dflags,
    (outer_alts_without_deflt, maybe_outer_deflt)   <- findDefault outer_alts,
    Just (Case (Var scrut_var) inner_bndr _ inner_alts) <- maybe_outer_deflt,
    scruting_same_var scrut_var
  = let
  	munged_inner_alts = [(con, args, munge_rhs rhs) | (con, args, rhs) <- inner_alts]
  	munge_rhs rhs = bindCaseBndr inner_bndr (Var outer_bndr) rhs
  
  	new_alts = mergeAlts outer_alts_without_deflt munged_inner_alts
		-- The merge keeps the inner DEFAULT at the front, if there is one
		-- and eliminates any inner_alts that are shadowed by the outer_alts
    in
    tick (CaseMerge outer_bndr)				`thenSmpl_`
    returnSmpl new_alts
    	-- Warning: don't call mkAlts recursively!
    	-- Firstly, there's no point, because inner alts have already had
    	-- mkCase applied to them, so they won't have a case in their default
    	-- Secondly, if you do, you get an infinite loop, because the bindCaseBndr
    	-- in munge_rhs may put a case into the DEFAULT branch!
  where
    	-- We are scrutinising the same variable if it's
    	-- the outer case-binder, or if the outer case scrutinises a variable
    	-- (and it's the same).  Testing both allows us not to replace the
    	-- outer scrut-var with the outer case-binder (Simplify.simplCaseBinder).
    scruting_same_var = case scrut of
			  Var outer_scrut -> \ v -> v == outer_bndr || v == outer_scrut
			  other		  -> \ v -> v == outer_bndr

------------------------------------------------
--	Catch-all
------------------------------------------------

mkAlts dflags scrut case_bndr other_alts = returnSmpl other_alts
\end{code}



=================================================================================

mkCase1 tries these things

1.  Eliminate the case altogether if possible

2.  Case-identity:

	case e of 		===> e
		True  -> True;
		False -> False

    and similar friends.


Start with a simple situation:

	case x# of	===>   e[x#/y#]
	  y# -> e

(when x#, y# are of primitive type, of course).  We can't (in general)
do this for algebraic cases, because we might turn bottom into
non-bottom!

Actually, we generalise this idea to look for a case where we're
scrutinising a variable, and we know that only the default case can
match.  For example:
\begin{verbatim}
	case x of
	  0#    -> ...
	  other -> ...(case x of
			 0#    -> ...
			 other -> ...) ...
\end{code}
Here the inner case can be eliminated.  This really only shows up in
eliminating error-checking code.

We also make sure that we deal with this very common case:

 	case e of 
	  x -> ...x...

Here we are using the case as a strict let; if x is used only once
then we want to inline it.  We have to be careful that this doesn't 
make the program terminate when it would have diverged before, so we
check that 
	- x is used strictly, or
	- e is already evaluated (it may so if e is a variable)

Lastly, we generalise the transformation to handle this:

	case e of	===> r
	   True  -> r
	   False -> r

We only do this for very cheaply compared r's (constructors, literals
and variables).  If pedantic bottoms is on, we only do it when the
scrutinee is a PrimOp which can't fail.

We do it *here*, looking at un-simplified alternatives, because we
have to check that r doesn't mention the variables bound by the
pattern in each alternative, so the binder-info is rather useful.

So the case-elimination algorithm is:

	1. Eliminate alternatives which can't match

	2. Check whether all the remaining alternatives
		(a) do not mention in their rhs any of the variables bound in their pattern
	   and  (b) have equal rhss

	3. Check we can safely ditch the case:
		   * PedanticBottoms is off,
		or * the scrutinee is an already-evaluated variable
		or * the scrutinee is a primop which is ok for speculation
			-- ie we want to preserve divide-by-zero errors, and
			-- calls to error itself!

		or * [Prim cases] the scrutinee is a primitive variable

		or * [Alg cases] the scrutinee is a variable and
		     either * the rhs is the same variable
			(eg case x of C a b -> x  ===>   x)
		     or     * there is only one alternative, the default alternative,
				and the binder is used strictly in its scope.
				[NB this is helped by the "use default binder where
				 possible" transformation; see below.]


If so, then we can replace the case with one of the rhss.

Further notes about case elimination
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider:	test :: Integer -> IO ()
		test = print

Turns out that this compiles to:
    Print.test
      = \ eta :: Integer
	  eta1 :: State# RealWorld ->
	  case PrelNum.< eta PrelNum.zeroInteger of wild { __DEFAULT ->
	  case hPutStr stdout
		 (PrelNum.jtos eta ($w[] @ Char))
		 eta1
	  of wild1 { (# new_s, a4 #) -> PrelIO.lvl23 new_s  }}

Notice the strange '<' which has no effect at all. This is a funny one.  
It started like this:

f x y = if x < 0 then jtos x
          else if y==0 then "" else jtos x

At a particular call site we have (f v 1).  So we inline to get

	if v < 0 then jtos x 
	else if 1==0 then "" else jtos x

Now simplify the 1==0 conditional:

	if v<0 then jtos v else jtos v

Now common-up the two branches of the case:

	case (v<0) of DEFAULT -> jtos v

Why don't we drop the case?  Because it's strict in v.  It's technically
wrong to drop even unnecessary evaluations, and in practice they
may be a result of 'seq' so we *definitely* don't want to drop those.
I don't really know how to improve this situation.


\begin{code}
--------------------------------------------------
--	0. Check for empty alternatives
--------------------------------------------------

-- This isn't strictly an error.  It's possible that the simplifer might "see"
-- that an inner case has no accessible alternatives before it "sees" that the
-- entire branch of an outer case is inaccessible.  So we simply
-- put an error case here insteadd
mkCase1 scrut case_bndr ty []
  = pprTrace "mkCase1: null alts" (ppr case_bndr <+> ppr scrut) $
    return (mkApps (Var eRROR_ID)
		   [Type ty, Lit (mkStringLit "Impossible alternative")])

--------------------------------------------------
--	1. Eliminate the case altogether if poss
--------------------------------------------------

mkCase1 scrut case_bndr ty [(con,bndrs,rhs)]
  -- See if we can get rid of the case altogether
  -- See the extensive notes on case-elimination above
  -- mkCase made sure that if all the alternatives are equal, 
  -- then there is now only one (DEFAULT) rhs
 |  all isDeadBinder bndrs,

	-- Check that the scrutinee can be let-bound instead of case-bound
    exprOkForSpeculation scrut
		-- OK not to evaluate it
		-- This includes things like (==# a# b#)::Bool
		-- so that we simplify 
		-- 	case ==# a# b# of { True -> x; False -> x }
		-- to just
		--	x
		-- This particular example shows up in default methods for
		-- comparision operations (e.g. in (>=) for Int.Int32)
	|| exprIsHNF scrut			-- It's already evaluated
	|| var_demanded_later scrut		-- It'll be demanded later

--      || not opt_SimplPedanticBottoms)	-- Or we don't care!
--	We used to allow improving termination by discarding cases, unless -fpedantic-bottoms was on,
-- 	but that breaks badly for the dataToTag# primop, which relies on a case to evaluate
-- 	its argument:  case x of { y -> dataToTag# y }
--	Here we must *not* discard the case, because dataToTag# just fetches the tag from
--	the info pointer.  So we'll be pedantic all the time, and see if that gives any
-- 	other problems
--	Also we don't want to discard 'seq's
  = tick (CaseElim case_bndr)			`thenSmpl_` 
    returnSmpl (bindCaseBndr case_bndr scrut rhs)

  where
	-- The case binder is going to be evaluated later, 
	-- and the scrutinee is a simple variable
    var_demanded_later (Var v) = isStrictDmd (idNewDemandInfo case_bndr)
    var_demanded_later other   = False


--------------------------------------------------
--	2. Identity case
--------------------------------------------------

mkCase1 scrut case_bndr ty alts	-- Identity case
  | all identity_alt alts
  = tick (CaseIdentity case_bndr)		`thenSmpl_`
    returnSmpl (re_cast scrut)
  where
    identity_alt (con, args, rhs) = de_cast rhs `cheapEqExpr` mk_id_rhs con args

    mk_id_rhs (DataAlt con) args = mkConApp con (arg_tys ++ varsToCoreExprs args)
    mk_id_rhs (LitAlt lit)  _    = Lit lit
    mk_id_rhs DEFAULT       _    = Var case_bndr

    arg_tys = map Type (tyConAppArgs (idType case_bndr))

	-- We've seen this:
	--	case e of x { _ -> x `cast` c }
	-- And we definitely want to eliminate this case, to give
	--	e `cast` c
	-- So we throw away the cast from the RHS, and reconstruct
	-- it at the other end.  All the RHS casts must be the same
	-- if (all identity_alt alts) holds.
	-- 
	-- Don't worry about nested casts, because the simplifier combines them
    de_cast (Cast e _) = e
    de_cast e	       = e

    re_cast scrut = case head alts of
			(_,_,Cast _ co) -> Cast scrut co
			other	        -> scrut



--------------------------------------------------
--	Catch-all
--------------------------------------------------
mkCase1 scrut bndr ty alts = returnSmpl (Case scrut bndr ty alts)
\end{code}


When adding auxiliary bindings for the case binder, it's worth checking if
its dead, because it often is, and occasionally these mkCase transformations
cascade rather nicely.

\begin{code}
bindCaseBndr bndr rhs body
  | isDeadBinder bndr = body
  | otherwise	      = bindNonRec bndr rhs body
\end{code}
