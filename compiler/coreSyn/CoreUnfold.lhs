%
% (c) The AQUA Project, Glasgow University, 1994-1998
%
\section[CoreUnfold]{Core-syntax unfoldings}

Unfoldings (which can travel across module boundaries) are in Core
syntax (namely @CoreExpr@s).

The type @Unfolding@ sits ``above'' simply-Core-expressions
unfoldings, capturing ``higher-level'' things we know about a binding,
usually things that the simplifier found out (e.g., ``it's a
literal'').  In the corner of a @CoreUnfolding@ unfolding, you will
find, unsurprisingly, a Core expression.

\begin{code}
module CoreUnfold (
	Unfolding, UnfoldingGuidance,	-- Abstract types

	noUnfolding, mkTopUnfolding, mkUnfolding, mkCompulsoryUnfolding, seqUnfolding,
	evaldUnfolding, mkOtherCon, otherCons,
	unfoldingTemplate, maybeUnfoldingTemplate,
	isEvaldUnfolding, isValueUnfolding, isCheapUnfolding, isCompulsoryUnfolding,
	hasUnfolding, hasSomeUnfolding, neverUnfold,

	couldBeSmallEnoughToInline, 
	certainlyWillInline, smallEnoughToInline,

	callSiteInline
    ) where

#include "HsVersions.h"

import StaticFlags	( opt_UF_CreationThreshold, opt_UF_UseThreshold,
			  opt_UF_FunAppDiscount, opt_UF_KeenessFactor,
			  opt_UF_DearOp,
			)
import DynFlags		( DynFlags, DynFlag(..), dopt )
import CoreSyn
import PprCore		( pprCoreExpr )
import OccurAnal	( occurAnalyseExpr )
import CoreUtils	( exprIsHNF, exprIsCheap, exprIsTrivial )
import Id		( Id, idType, isId,
			  idUnfolding, globalIdDetails
			)
import DataCon		( isUnboxedTupleCon )
import Literal		( litSize )
import PrimOp		( primOpIsDupable, primOpOutOfLine )
import IdInfo		( OccInfo(..), GlobalIdDetails(..) )
import Type		( isUnLiftedType )
import PrelNames	( hasKey, buildIdKey, augmentIdKey )
import Bag
import FastTypes
import Outputable

#if __GLASGOW_HASKELL__ >= 404
import GLAEXTS		( Int# )
#endif
\end{code}


%************************************************************************
%*									*
\subsection{Making unfoldings}
%*									*
%************************************************************************

\begin{code}
mkTopUnfolding expr = mkUnfolding True {- Top level -} expr

mkUnfolding top_lvl expr
  = CoreUnfolding (occurAnalyseExpr expr)
		  top_lvl

		  (exprIsHNF expr)
			-- Already evaluated

		  (exprIsCheap expr)
			-- OK to inline inside a lambda

		  (calcUnfoldingGuidance opt_UF_CreationThreshold expr)
	-- Sometimes during simplification, there's a large let-bound thing	
	-- which has been substituted, and so is now dead; so 'expr' contains
	-- two copies of the thing while the occurrence-analysed expression doesn't
	-- Nevertheless, we don't occ-analyse before computing the size because the
	-- size computation bales out after a while, whereas occurrence analysis does not.
	--
	-- This can occasionally mean that the guidance is very pessimistic;
	-- it gets fixed up next round

mkCompulsoryUnfolding expr	-- Used for things that absolutely must be unfolded
  = CompulsoryUnfolding (occurAnalyseExpr expr)
\end{code}


%************************************************************************
%*									*
\subsection{The UnfoldingGuidance type}
%*									*
%************************************************************************

\begin{code}
instance Outputable UnfoldingGuidance where
    ppr UnfoldNever	= ptext SLIT("NEVER")
    ppr (UnfoldIfGoodArgs v cs size discount)
      = hsep [ ptext SLIT("IF_ARGS"), int v,
	       brackets (hsep (map int cs)),
	       int size,
	       int discount ]
\end{code}


\begin{code}
calcUnfoldingGuidance
	:: Int		    	-- bomb out if size gets bigger than this
	-> CoreExpr    		-- expression to look at
	-> UnfoldingGuidance
calcUnfoldingGuidance bOMB_OUT_SIZE expr
  = case collect_val_bndrs expr of { (inline, val_binders, body) ->
    let
	n_val_binders = length val_binders

	max_inline_size = n_val_binders+2
	-- The idea is that if there is an INLINE pragma (inline is True)
	-- and there's a big body, we give a size of n_val_binders+2.  This
	-- This is just enough to fail the no-size-increase test in callSiteInline,
	--   so that INLINE things don't get inlined into entirely boring contexts,
	--   but no more.

    in
    case (sizeExpr (iUnbox bOMB_OUT_SIZE) val_binders body) of

      TooBig 
	| not inline -> UnfoldNever
		-- A big function with an INLINE pragma must
		-- have an UnfoldIfGoodArgs guidance
	| otherwise  -> UnfoldIfGoodArgs n_val_binders
					 (map (const 0) val_binders)
					 max_inline_size 0

      SizeIs size cased_args scrut_discount
	-> UnfoldIfGoodArgs
			n_val_binders
			(map discount_for val_binders)
			final_size
			(iBox scrut_discount)
	where        
	    boxed_size    = iBox size

	    final_size | inline     = boxed_size `min` max_inline_size
		       | otherwise  = boxed_size

		-- Sometimes an INLINE thing is smaller than n_val_binders+2.
		-- A particular case in point is a constructor, which has size 1.
		-- We want to inline this regardless, hence the `min`

	    discount_for b = foldlBag (\acc (b',n) -> if b==b' then acc+n else acc) 
				      0 cased_args
	}
  where
    collect_val_bndrs e = go False [] e
	-- We need to be a bit careful about how we collect the
	-- value binders.  In ptic, if we see 
	--	__inline_me (\x y -> e)
	-- We want to say "2 value binders".  Why?  So that 
	-- we take account of information given for the arguments

    go inline rev_vbs (Note InlineMe e)     = go True   rev_vbs     e
    go inline rev_vbs (Lam b e) | isId b    = go inline (b:rev_vbs) e
				| otherwise = go inline rev_vbs     e
    go inline rev_vbs e			    = (inline, reverse rev_vbs, e)
\end{code}

\begin{code}
sizeExpr :: Int# 	    -- Bomb out if it gets bigger than this
	 -> [Id]	    -- Arguments; we're interested in which of these
			    -- get case'd
	 -> CoreExpr
	 -> ExprSize

sizeExpr bOMB_OUT_SIZE top_args expr
  = size_up expr
  where
    size_up (Type t)	      = sizeZero	-- Types cost nothing
    size_up (Var v)           = sizeOne

    size_up (Note InlineMe body) = sizeOne	-- Inline notes make it look very small
	-- This can be important.  If you have an instance decl like this:
	-- 	instance Foo a => Foo [a] where
	--	   {-# INLINE op1, op2 #-}
	--	   op1 = ...
	--	   op2 = ...
	-- then we'll get a dfun which is a pair of two INLINE lambdas

    size_up (Note _        body) = size_up body	-- Other notes cost nothing

    size_up (App fun (Type t)) = size_up fun
    size_up (App fun arg)      = size_up_app fun [arg]

    size_up (Lit lit) 	       = sizeN (litSize lit)

    size_up (Lam b e) | isId b    = lamScrutDiscount (size_up e `addSizeN` 1)
		      | otherwise = size_up e

    size_up (Let (NonRec binder rhs) body)
      = nukeScrutDiscount (size_up rhs)		`addSize`
	size_up body				`addSizeN`
	(if isUnLiftedType (idType binder) then 0 else 1)
		-- For the allocation
		-- If the binder has an unlifted type there is no allocation

    size_up (Let (Rec pairs) body)
      = nukeScrutDiscount rhs_size		`addSize`
	size_up body				`addSizeN`
	length pairs		-- For the allocation
      where
	rhs_size = foldr (addSize . size_up . snd) sizeZero pairs

    size_up (Case (Var v) _ _ alts) 
	| v `elem` top_args		-- We are scrutinising an argument variable
	= 
{-	I'm nuking this special case; BUT see the comment with case alternatives.

	(a) It's too eager.  We don't want to inline a wrapper into a
	    context with no benefit.  
	    E.g.  \ x. f (x+x)   	no point in inlining (+) here!

	(b) It's ineffective. Once g's wrapper is inlined, its case-expressions 
	    aren't scrutinising arguments any more

	    case alts of

		[alt] -> size_up_alt alt `addSize` SizeIs 0# (unitBag (v, 1)) 0#
		-- We want to make wrapper-style evaluation look cheap, so that
		-- when we inline a wrapper it doesn't make call site (much) bigger
		-- Otherwise we get nasty phase ordering stuff: 
		--	f x = g x x
		--	h y = ...(f e)...
		-- If we inline g's wrapper, f looks big, and doesn't get inlined
		-- into h; if we inline f first, while it looks small, then g's 
		-- wrapper will get inlined later anyway.  To avoid this nasty
		-- ordering difference, we make (case a of (x,y) -> ...), 
		--  *where a is one of the arguments* look free.

		other -> 
-}
			 alts_size (foldr addSize sizeOne alt_sizes)	-- The 1 is for the scrutinee
				   (foldr1 maxSize alt_sizes)

		-- Good to inline if an arg is scrutinised, because
		-- that may eliminate allocation in the caller
		-- And it eliminates the case itself

	where
	  alt_sizes = map size_up_alt alts

		-- alts_size tries to compute a good discount for
		-- the case when we are scrutinising an argument variable
	  alts_size (SizeIs tot tot_disc tot_scrut)		-- Size of all alternatives
		    (SizeIs max max_disc max_scrut)		-- Size of biggest alternative
	 	= SizeIs tot (unitBag (v, iBox (_ILIT 1 +# tot -# max)) `unionBags` max_disc) max_scrut
			-- If the variable is known, we produce a discount that
			-- will take us back to 'max', the size of rh largest alternative
			-- The 1+ is a little discount for reduced allocation in the caller
	  alts_size tot_size _ = tot_size

-- gaw 2004
    size_up (Case e _ _ alts) = nukeScrutDiscount (size_up e) `addSize` 
			         foldr (addSize . size_up_alt) sizeZero alts
	  	-- We don't charge for the case itself
		-- It's a strict thing, and the price of the call
		-- is paid by scrut.  Also consider
		--	case f x of DEFAULT -> e
		-- This is just ';'!  Don't charge for it.

    ------------ 
    size_up_app (App fun arg) args   
	| isTypeArg arg		     = size_up_app fun args
	| otherwise		     = size_up_app fun (arg:args)
    size_up_app fun 	      args   = foldr (addSize . nukeScrutDiscount . size_up) 
					     (size_up_fun fun args)
					     args

	-- A function application with at least one value argument
	-- so if the function is an argument give it an arg-discount
	--
	-- Also behave specially if the function is a build
	--
	-- Also if the function is a constant Id (constr or primop)
	-- compute discounts specially
    size_up_fun (Var fun) args
      | fun `hasKey` buildIdKey   = buildSize
      | fun `hasKey` augmentIdKey = augmentSize
      | otherwise 
      = case globalIdDetails fun of
	  DataConWorkId dc -> conSizeN dc (valArgCount args)

	  FCallId fc   -> sizeN opt_UF_DearOp
	  PrimOpId op  -> primOpSize op (valArgCount args)
			  -- foldr addSize (primOpSize op) (map arg_discount args)
			  -- At one time I tried giving an arg-discount if a primop 
			  -- is applied to one of the function's arguments, but it's
			  -- not good.  At the moment, any unlifted-type arg gets a
			  -- 'True' for 'yes I'm evald', so we collect the discount even
			  -- if we know nothing about it.  And just having it in a primop
			  -- doesn't help at all if we don't know something more.

	  other	       -> fun_discount fun `addSizeN` 
			  (1 + length (filter (not . exprIsTrivial) args))
				-- The 1+ is for the function itself
				-- Add 1 for each non-trivial arg;
				-- the allocation cost, as in let(rec)
				-- Slight hack here: for constructors the args are almost always
				--	trivial; and for primops they are almost always prim typed
				-- 	We should really only count for non-prim-typed args in the
				--	general case, but that seems too much like hard work

    size_up_fun other args = size_up other

    ------------ 
    size_up_alt (con, bndrs, rhs) = size_up rhs
 	-- Don't charge for args, so that wrappers look cheap
	-- (See comments about wrappers with Case)

    ------------
	-- We want to record if we're case'ing, or applying, an argument
    fun_discount v | v `elem` top_args = SizeIs 0# (unitBag (v, opt_UF_FunAppDiscount)) 0#
    fun_discount other		       = sizeZero

    ------------
	-- These addSize things have to be here because
	-- I don't want to give them bOMB_OUT_SIZE as an argument

    addSizeN TooBig          _  = TooBig
    addSizeN (SizeIs n xs d) m 	= mkSizeIs bOMB_OUT_SIZE (n +# iUnbox m) xs d
    
    addSize TooBig	      _			= TooBig
    addSize _		      TooBig		= TooBig
    addSize (SizeIs n1 xs d1) (SizeIs n2 ys d2) 
	= mkSizeIs bOMB_OUT_SIZE (n1 +# n2) (xs `unionBags` ys) (d1 +# d2)
\end{code}

Code for manipulating sizes

\begin{code}
data ExprSize = TooBig
	      | SizeIs FastInt		-- Size found
		       (Bag (Id,Int))	-- Arguments cased herein, and discount for each such
		       FastInt		-- Size to subtract if result is scrutinised 
					-- by a case expression

-- subtract the discount before deciding whether to bale out. eg. we
-- want to inline a large constructor application into a selector:
--  	tup = (a_1, ..., a_99)
--  	x = case tup of ...
--
mkSizeIs max n xs d | (n -# d) ># max = TooBig
		    | otherwise	      = SizeIs n xs d
 
maxSize TooBig         _ 				  = TooBig
maxSize _              TooBig				  = TooBig
maxSize s1@(SizeIs n1 _ _) s2@(SizeIs n2 _ _) | n1 ># n2  = s1
					      | otherwise = s2

sizeZero     	= SizeIs (_ILIT 0)  emptyBag (_ILIT 0)
sizeOne      	= SizeIs (_ILIT 1)  emptyBag (_ILIT 0)
sizeN n 	= SizeIs (iUnbox n) emptyBag (_ILIT 0)
conSizeN dc n   
  | isUnboxedTupleCon dc = SizeIs (_ILIT 0) emptyBag (iUnbox n +# _ILIT 1)
  | otherwise		 = SizeIs (_ILIT 1) emptyBag (iUnbox n +# _ILIT 1)
	-- Treat constructors as size 1; we are keen to expose them
	-- (and we charge separately for their args).  We can't treat
	-- them as size zero, else we find that (iBox x) has size 1,
	-- which is the same as a lone variable; and hence 'v' will 
	-- always be replaced by (iBox x), where v is bound to iBox x.
	--
	-- However, unboxed tuples count as size zero
	-- I found occasions where we had 
	--	f x y z = case op# x y z of { s -> (# s, () #) }
	-- and f wasn't getting inlined

primOpSize op n_args
 | not (primOpIsDupable op) = sizeN opt_UF_DearOp
 | not (primOpOutOfLine op) = sizeN (2 - n_args)
	-- Be very keen to inline simple primops.
	-- We give a discount of 1 for each arg so that (op# x y z) costs 2.
	-- We can't make it cost 1, else we'll inline let v = (op# x y z) 
	-- at every use of v, which is excessive.
	--
	-- A good example is:
	--	let x = +# p q in C {x}
	-- Even though x get's an occurrence of 'many', its RHS looks cheap,
	-- and there's a good chance it'll get inlined back into C's RHS. Urgh!
 | otherwise	      	    = sizeOne

buildSize = SizeIs (-2#) emptyBag 4#
	-- We really want to inline applications of build
	-- build t (\cn -> e) should cost only the cost of e (because build will be inlined later)
	-- Indeed, we should add a result_discount becuause build is 
	-- very like a constructor.  We don't bother to check that the
	-- build is saturated (it usually is).  The "-2" discounts for the \c n, 
	-- The "4" is rather arbitrary.

augmentSize = SizeIs (-2#) emptyBag 4#
	-- Ditto (augment t (\cn -> e) ys) should cost only the cost of
	-- e plus ys. The -2 accounts for the \cn 
						
nukeScrutDiscount (SizeIs n vs d) = SizeIs n vs 0#
nukeScrutDiscount TooBig	  = TooBig

-- When we return a lambda, give a discount if it's used (applied)
lamScrutDiscount  (SizeIs n vs d) = case opt_UF_FunAppDiscount of { d -> SizeIs n vs (iUnbox d) }
lamScrutDiscount TooBig	 	  = TooBig
\end{code}


%************************************************************************
%*									*
\subsection[considerUnfolding]{Given all the info, do (not) do the unfolding}
%*									*
%************************************************************************

We have very limited information about an unfolding expression: (1)~so
many type arguments and so many value arguments expected---for our
purposes here, we assume we've got those.  (2)~A ``size'' or ``cost,''
a single integer.  (3)~An ``argument info'' vector.  For this, what we
have at the moment is a Boolean per argument position that says, ``I
will look with great favour on an explicit constructor in this
position.'' (4)~The ``discount'' to subtract if the expression
is being scrutinised. 

Assuming we have enough type- and value arguments (if not, we give up
immediately), then we see if the ``discounted size'' is below some
(semi-arbitrary) threshold.  It works like this: for every argument
position where we're looking for a constructor AND WE HAVE ONE in our
hands, we get a (again, semi-arbitrary) discount [proportion to the
number of constructors in the type being scrutinized].

If we're in the context of a scrutinee ( \tr{(case <expr > of A .. -> ...;.. )})
and the expression in question will evaluate to a constructor, we use
the computed discount size *for the result only* rather than
computing the argument discounts. Since we know the result of
the expression is going to be taken apart, discounting its size
is more accurate (see @sizeExpr@ above for how this discount size
is computed).

We use this one to avoid exporting inlinings that we ``couldn't possibly
use'' on the other side.  Can be overridden w/ flaggery.
Just the same as smallEnoughToInline, except that it has no actual arguments.

\begin{code}
couldBeSmallEnoughToInline :: Int -> CoreExpr -> Bool
couldBeSmallEnoughToInline threshold rhs = case calcUnfoldingGuidance threshold rhs of
						UnfoldNever -> False
						other	    -> True

certainlyWillInline :: Unfolding -> Bool
  -- Sees if the unfolding is pretty certain to inline	
certainlyWillInline (CoreUnfolding _ _ _ is_cheap (UnfoldIfGoodArgs n_vals _ size _))
  = is_cheap && size - (n_vals +1) <= opt_UF_UseThreshold
certainlyWillInline other
  = False

smallEnoughToInline :: Unfolding -> Bool
smallEnoughToInline (CoreUnfolding _ _ _ _ (UnfoldIfGoodArgs _ _ size _))
  = size <= opt_UF_UseThreshold
smallEnoughToInline other
  = False
\end{code}

%************************************************************************
%*									*
\subsection{callSiteInline}
%*									*
%************************************************************************

This is the key function.  It decides whether to inline a variable at a call site

callSiteInline is used at call sites, so it is a bit more generous.
It's a very important function that embodies lots of heuristics.
A non-WHNF can be inlined if it doesn't occur inside a lambda,
and occurs exactly once or 
    occurs once in each branch of a case and is small

If the thing is in WHNF, there's no danger of duplicating work, 
so we can inline if it occurs once, or is small

NOTE: we don't want to inline top-level functions that always diverge.
It just makes the code bigger.  Tt turns out that the convenient way to prevent
them inlining is to give them a NOINLINE pragma, which we do in 
StrictAnal.addStrictnessInfoToTopId

\begin{code}
callSiteInline :: DynFlags
	       -> Bool			-- True <=> the Id can be inlined
	       -> Bool			-- 'inline' note at call site
	       -> OccInfo
	       -> Id			-- The Id
	       -> [Bool]		-- One for each value arg; True if it is interesting
	       -> Bool			-- True <=> continuation is interesting
	       -> Maybe CoreExpr	-- Unfolding, if any


callSiteInline dflags active_inline inline_call occ id arg_infos interesting_cont
  = case idUnfolding id of {
	NoUnfolding -> Nothing ;
	OtherCon cs -> Nothing ;

	CompulsoryUnfolding unf_template -> Just unf_template ;
		-- CompulsoryUnfolding => there is no top-level binding
		-- for these things, so we must inline it.
		-- Only a couple of primop-like things have 
		-- compulsory unfoldings (see MkId.lhs).
		-- We don't allow them to be inactive

	CoreUnfolding unf_template is_top is_value is_cheap guidance ->

    let
	result | yes_or_no = Just unf_template
	       | otherwise = Nothing

	n_val_args  = length arg_infos

 	yes_or_no 
	  | not active_inline = False
	  | otherwise = case occ of
				IAmDead		     -> pprTrace "callSiteInline: dead" (ppr id) False
				IAmALoopBreaker      -> False
				--OneOcc in_lam _ _    -> (not in_lam || is_cheap) && consider_safe True
				other		     -> is_cheap && consider_safe False
		-- we consider even the once-in-one-branch
		-- occurrences, because they won't all have been
		-- caught by preInlineUnconditionally.  In particular,
		-- if the occurrence is once inside a lambda, and the
		-- rhs is cheap but not a manifest lambda, then
		-- pre-inline will not have inlined it for fear of
		-- invalidating the occurrence info in the rhs.

	consider_safe once
		-- consider_safe decides whether it's a good idea to
		-- inline something, given that there's no
		-- work-duplication issue (the caller checks that).
	  | inline_call  = True

	  | otherwise
	  = case guidance of
	      UnfoldNever  -> False
	      UnfoldIfGoodArgs n_vals_wanted arg_discounts size res_discount

		  | enough_args && size <= (n_vals_wanted + 1)
			-- Inline unconditionally if there no size increase
			-- Size of call is n_vals_wanted (+1 for the function)
		  -> True

	  	  | otherwise
		  -> some_benefit && small_enough

		  where
		    some_benefit = or arg_infos || really_interesting_cont || 
		    		   (not is_top && ({- once || -} (n_vals_wanted > 0 && enough_args)))
				-- [was (once && not in_lam)]
		-- If it occurs more than once, there must be
		-- something interesting about some argument, or the
		-- result context, to make it worth inlining
		--
		-- If a function has a nested defn we also record
		-- some-benefit, on the grounds that we are often able
		-- to eliminate the binding, and hence the allocation,
		-- for the function altogether; this is good for join
		-- points.  But this only makes sense for *functions*;
		-- inlining a constructor doesn't help allocation
		-- unless the result is scrutinised.  UNLESS the
		-- constructor occurs just once, albeit possibly in
		-- multiple case branches.  Then inlining it doesn't
		-- increase allocation, but it does increase the
		-- chance that the constructor won't be allocated at
		-- all in the branches that don't use it.

		    enough_args		  = n_val_args >= n_vals_wanted
		    really_interesting_cont | n_val_args <  n_vals_wanted = False	-- Too few args
		    			    | n_val_args == n_vals_wanted = interesting_cont
		    			    | otherwise		          = True	-- Extra args
		    	-- really_interesting_cont tells if the result of the
		    	-- call is in an interesting context.

		    small_enough = (size - discount) <= opt_UF_UseThreshold
		    discount     = computeDiscount n_vals_wanted arg_discounts res_discount 
						 arg_infos really_interesting_cont
		
    in    
    if dopt Opt_D_dump_inlinings dflags then
	pprTrace "Considering inlining"
		 (ppr id <+> vcat [text "active:" <+> ppr active_inline,
				   text "occ info:" <+> ppr occ,
			  	   text "arg infos" <+> ppr arg_infos,
				   text "interesting continuation" <+> ppr interesting_cont,
				   text "is value:" <+> ppr is_value,
				   text "is cheap:" <+> ppr is_cheap,
				   text "guidance" <+> ppr guidance,
				   text "ANSWER =" <+> if yes_or_no then text "YES" else text "NO"])
		  result
    else
    result
    }

computeDiscount :: Int -> [Int] -> Int -> [Bool] -> Bool -> Int
computeDiscount n_vals_wanted arg_discounts res_discount arg_infos result_used
 	-- We multiple the raw discounts (args_discount and result_discount)
	-- ty opt_UnfoldingKeenessFactor because the former have to do with
	--  *size* whereas the discounts imply that there's some extra 
	--  *efficiency* to be gained (e.g. beta reductions, case reductions) 
	-- by inlining.

	-- we also discount 1 for each argument passed, because these will
	-- reduce with the lambdas in the function (we count 1 for a lambda
 	-- in size_up).
  = 1 +			-- Discount of 1 because the result replaces the call
			-- so we count 1 for the function itself
    length (take n_vals_wanted arg_infos) +
			-- Discount of 1 for each arg supplied, because the 
			-- result replaces the call
    round (opt_UF_KeenessFactor * 
	   fromIntegral (arg_discount + result_discount))
  where
    arg_discount = sum (zipWith mk_arg_discount arg_discounts arg_infos)

    mk_arg_discount discount is_evald | is_evald  = discount
				      | otherwise = 0

	-- Don't give a result discount unless there are enough args
    result_discount | result_used = res_discount	-- Over-applied, or case scrut
	            | otherwise	  = 0
\end{code}
