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
	mkOtherCon, otherCons,
	unfoldingTemplate, maybeUnfoldingTemplate,
	isEvaldUnfolding, isValueUnfolding, isCheapUnfolding, isCompulsoryUnfolding,
	hasUnfolding, hasSomeUnfolding,

	couldBeSmallEnoughToInline, 
	certainlyWillInline, 
	okToUnfoldInHiFile,

	callSiteInline, blackListed
    ) where

#include "HsVersions.h"

import CmdLineOpts	( opt_UF_CreationThreshold,
			  opt_UF_UseThreshold,
			  opt_UF_ScrutConDiscount,
			  opt_UF_FunAppDiscount,
			  opt_UF_PrimArgDiscount,
			  opt_UF_KeenessFactor,
			  opt_UF_CheapOp, opt_UF_DearOp,
			  opt_UnfoldCasms, opt_PprStyle_Debug,
			  opt_D_dump_inlinings
			)
import CoreSyn
import PprCore		( pprCoreExpr )
import OccurAnal	( occurAnalyseGlobalExpr )
import BinderInfo	( )
import CoreUtils	( exprIsValue, exprIsCheap, exprIsBottom, exprIsTrivial )
import Id		( Id, idType, idFlavour, idUnique, isId, idWorkerInfo,
			  idSpecialisation, idInlinePragma, idUnfolding,
			  isPrimOpId_maybe
			)
import VarSet
import Name		( isLocallyDefined )
import Literal		( isLitLitLit )
import PrimOp		( PrimOp(..), primOpIsDupable, primOpOutOfLine, ccallIsCasm )
import IdInfo		( ArityInfo(..), InlinePragInfo(..), OccInfo(..), IdFlavour(..), CprInfo(..), insideLam, workerExists )
import TyCon		( tyConFamilySize )
import Type		( splitAlgTyConApp_maybe, splitFunTy_maybe, isUnLiftedType )
import Unique		( Unique, buildIdKey, augmentIdKey )
import Maybes		( maybeToBool )
import Bag
import List		( maximumBy )
import Util		( isIn, lengthExceeds )
import Outputable

#if __GLASGOW_HASKELL__ >= 404
import GlaExts		( fromInt )
#endif
\end{code}


%************************************************************************
%*									*
\subsection{Making unfoldings}
%*									*
%************************************************************************

\begin{code}
mkTopUnfolding cpr_info expr = mkUnfolding True {- Top level -} cpr_info expr

mkUnfolding top_lvl cpr_info expr
  = CoreUnfolding (occurAnalyseGlobalExpr expr)
		  top_lvl
		  (exprIsCheap expr)
		  (exprIsValue expr)
		  (exprIsBottom expr)
		  (calcUnfoldingGuidance opt_UF_CreationThreshold cpr_info expr)
	-- Sometimes during simplification, there's a large let-bound thing	
	-- which has been substituted, and so is now dead; so 'expr' contains
	-- two copies of the thing while the occurrence-analysed expression doesn't
	-- Nevertheless, we don't occ-analyse before computing the size because the
	-- size computation bales out after a while, whereas occurrence analysis does not.
	--
	-- This can occasionally mean that the guidance is very pessimistic;
	-- it gets fixed up next round

mkCompulsoryUnfolding expr	-- Used for things that absolutely must be unfolded
  = CompulsoryUnfolding (occurAnalyseGlobalExpr expr)
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
	-> CprInfo		-- CPR info for this RHS
	-> CoreExpr    		-- expression to look at
	-> UnfoldingGuidance
calcUnfoldingGuidance bOMB_OUT_SIZE cpr_info expr
  = case collect_val_bndrs expr of { (inline, val_binders, body) ->
    let
	n_val_binders = length val_binders

	max_inline_size = n_val_binders+2
	-- The idea is that if there is an INLINE pragma (inline is True)
	-- and there's a big body, we give a size of n_val_binders+2.  This
	-- This is just enough to fail the no-size-increase test in callSiteInline,
	--   so that INLINE things don't get inlined into entirely boring contexts,
	--   but no more.

-- Experimental thing commented in for now
--        max_inline_size = case cpr_info of
--			NoCPRInfo  -> n_val_binders + 2
--			ReturnsCPR -> n_val_binders + 1

	-- However, the wrapper for a CPR'd function is particularly good to inline,
	-- even in a boring context, because we may get to do update in place:
	--	let x = case y of { I# y# -> I# (y# +# 1#) }
	-- Hence the case on cpr_info

    in
    case (sizeExpr bOMB_OUT_SIZE val_binders body) of

      TooBig 
	| not inline -> UnfoldNever
		-- A big function with an INLINE pragma must
		-- have an UnfoldIfGoodArgs guidance
	| inline     -> UnfoldIfGoodArgs n_val_binders
					 (map (const 0) val_binders)
					 max_inline_size 0

      SizeIs size cased_args scrut_discount
	-> UnfoldIfGoodArgs
			n_val_binders
			(map discount_for val_binders)
			final_size
			(I# scrut_discount)
	where        
	    boxed_size    = I# size

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
sizeExpr :: Int 	    -- Bomb out if it gets bigger than this
	 -> [Id]	    -- Arguments; we're interested in which of these
			    -- get case'd
	 -> CoreExpr
	 -> ExprSize

sizeExpr (I# bOMB_OUT_SIZE) top_args expr
  = size_up expr
  where
    size_up (Type t)	      = sizeZero	-- Types cost nothing
    size_up (Var v)           = sizeOne

    size_up (Note _ body)     = size_up body	-- Notes cost nothing

    size_up (App fun (Type t))  = size_up fun
    size_up (App fun arg)     = size_up_app fun [arg]

    size_up (Lit lit) = sizeOne

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

	-- We want to make wrapper-style evaluation look cheap, so that
	-- when we inline a wrapper it doesn't make call site (much) bigger
	-- Otherwise we get nasty phase ordering stuff: 
	--	f x = g x x
	--	h y = ...(f e)...
	-- If we inline g's wrapper, f looks big, and doesn't get inlined
	-- into h; if we inline f first, while it looks small, then g's 
	-- wrapper will get inlined later anyway.  To avoid this nasty
	-- ordering difference, we make (case a of (x,y) -> ...) look free.
    size_up (Case (Var v) _ [alt]) 
	| v `elem` top_args
	= size_up_alt alt `addSize` SizeIs 0# (unitBag (v, 1)) 0#
		-- Good to inline if an arg is scrutinised, because
		-- that may eliminate allocation in the caller
		-- And it eliminates the case itself
	| otherwise	
	= size_up_alt alt

	-- Scrutinising one of the argument variables,
	-- with more than one alternative
    size_up (Case (Var v) _ alts)
	| v `elem` top_args
	= alts_size (foldr addSize sizeOne alt_sizes)	-- The 1 is for the scrutinee
		    (foldr1 maxSize alt_sizes)
	where
	  v_in_args = v `elem` top_args
	  alt_sizes = map size_up_alt alts

	  alts_size (SizeIs tot tot_disc tot_scrut)		-- Size of all alternatives
		    (SizeIs max max_disc max_scrut)		-- Size of biggest alternative
	 	= SizeIs tot (unitBag (v, I# (1# +# tot -# max)) `unionBags` max_disc) max_scrut
			-- If the variable is known, we produce a discount that
			-- will take us back to 'max', the size of rh largest alternative
			-- The 1+ is a little discount for reduced allocation in the caller

	  alts_size tot_size _ = tot_size


    size_up (Case e _ alts) = nukeScrutDiscount (size_up e) `addSize` 
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
      | idUnique fun == buildIdKey   = buildSize
      | idUnique fun == augmentIdKey = augmentSize
      | otherwise 
      = case idFlavour fun of
	  DataConId dc -> conSizeN (valArgCount args)

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

    ------------
	-- We want to record if we're case'ing, or applying, an argument
    fun_discount v | v `elem` top_args = SizeIs 0# (unitBag (v, opt_UF_FunAppDiscount)) 0#
    fun_discount other			  = sizeZero

    ------------
	-- These addSize things have to be here because
	-- I don't want to give them bOMB_OUT_SIZE as an argument

    addSizeN TooBig          _      = TooBig
    addSizeN (SizeIs n xs d) (I# m)
      | n_tot ># bOMB_OUT_SIZE	    = TooBig
      | otherwise 		    = SizeIs n_tot xs d
      where
	n_tot = n +# m
    
    addSize TooBig _ = TooBig
    addSize _ TooBig = TooBig
    addSize (SizeIs n1 xs d1) (SizeIs n2 ys d2)
      | n_tot ># bOMB_OUT_SIZE = TooBig
      | otherwise 	       = SizeIs n_tot xys d_tot
      where
	n_tot = n1 +# n2
	d_tot = d1 +# d2
	xys   = xs `unionBags` ys
\end{code}

Code for manipulating sizes

\begin{code}

data ExprSize = TooBig
	      | SizeIs Int#		-- Size found
		       (Bag (Id,Int))	-- Arguments cased herein, and discount for each such
		       Int#		-- Size to subtract if result is scrutinised 
					-- by a case expression

isTooBig TooBig = True
isTooBig _      = False

maxSize TooBig         _ 				  = TooBig
maxSize _              TooBig				  = TooBig
maxSize s1@(SizeIs n1 _ _) s2@(SizeIs n2 _ _) | n1 ># n2  = s1
					      | otherwise = s2

sizeZero     	= SizeIs 0# emptyBag 0#
sizeOne      	= SizeIs 1# emptyBag 0#
sizeTwo      	= SizeIs 2# emptyBag 0#
sizeN (I# n) 	= SizeIs n  emptyBag 0#
conSizeN (I# n) = SizeIs 1# emptyBag (n +# 1#)
	-- Treat constructors as size 1; we are keen to expose them
	-- (and we charge separately for their args).  We can't treat
	-- them as size zero, else we find that (I# x) has size 1,
	-- which is the same as a lone variable; and hence 'v' will 
	-- always be replaced by (I# x), where v is bound to I# x.

primOpSize op n_args
 | not (primOpIsDupable op) = sizeN opt_UF_DearOp
 | not (primOpOutOfLine op) = sizeZero			-- These are good to inline
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
lamScrutDiscount  (SizeIs n vs d) = case opt_UF_FunAppDiscount of { I# d -> SizeIs n vs d }
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
couldBeSmallEnoughToInline threshold rhs = case calcUnfoldingGuidance threshold NoCPRInfo rhs of
						UnfoldNever -> False
						other	    -> True

certainlyWillInline :: Id -> Bool
	-- Sees if the Id is pretty certain to inline	
certainlyWillInline v
  = case idUnfolding v of

	CoreUnfolding _ _ _ is_value _ (UnfoldIfGoodArgs n_vals _ size _)
	   ->    is_value 
	      && size - (n_vals +1) <= opt_UF_UseThreshold
	      && not never_inline

	other -> False
  where
    never_inline = case idInlinePragma v of
			IMustNotBeINLINEd False Nothing -> True
			other			        -> False
\end{code}

@okToUnfoldInHifile@ is used when emitting unfolding info into an interface
file to determine whether an unfolding candidate really should be unfolded.
The predicate is needed to prevent @_casm_@s (+ lit-lits) from being emitted
into interface files. 

The reason for inlining expressions containing _casm_s into interface files
is that these fragments of C are likely to mention functions/#defines that
will be out-of-scope when inlined into another module. This is not an
unfixable problem for the user (just need to -#include the approp. header
file), but turning it off seems to the simplest thing to do.

\begin{code}
okToUnfoldInHiFile :: CoreExpr -> Bool
okToUnfoldInHiFile e = opt_UnfoldCasms || go e
 where
    -- Race over an expression looking for CCalls..
    go (Var v)                = case isPrimOpId_maybe v of
				  Just op -> okToUnfoldPrimOp op
				  Nothing -> True
    go (Lit lit)	      = not (isLitLitLit lit)
    go (App fun arg)          = go fun && go arg
    go (Lam _ body)           = go body
    go (Let binds body)       = and (map go (body :rhssOfBind binds))
    go (Case scrut bndr alts) = and (map go (scrut:rhssOfAlts alts))
    go (Note _ body)          = go body
    go (Type _)		      = True

    -- ok to unfold a PrimOp as long as it's not a _casm_
    okToUnfoldPrimOp (CCallOp ccall) = not (ccallIsCasm ccall)
    okToUnfoldPrimOp _               = True
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
callSiteInline :: Bool			-- True <=> the Id is black listed
	       -> Bool			-- 'inline' note at call site
	       -> OccInfo
	       -> Id			-- The Id
	       -> [Bool]		-- One for each value arg; True if it is interesting
	       -> Bool			-- True <=> continuation is interesting
	       -> Maybe CoreExpr	-- Unfolding, if any


callSiteInline black_listed inline_call occ id arg_infos interesting_cont
  = case idUnfolding id of {
	NoUnfolding -> Nothing ;
	OtherCon _  -> Nothing ;
	CompulsoryUnfolding unf_template | black_listed -> Nothing 
					 | otherwise 	-> Just unf_template ;
		-- Constructors have compulsory unfoldings, but
		-- may have rules, in which case they are 
		-- black listed till later
	CoreUnfolding unf_template is_top is_cheap is_value is_bot guidance ->

    let
	result | yes_or_no = Just unf_template
	       | otherwise = Nothing

	n_val_args  = length arg_infos

	ok_inside_lam = is_value || is_bot || (is_cheap && not is_top)
				-- I'm experimenting with is_cheap && not is_top

 	yes_or_no 
	  | black_listed = False
	  | otherwise    = case occ of
				IAmDead		     -> pprTrace "callSiteInline: dead" (ppr id) False
				IAmALoopBreaker      -> False
				OneOcc in_lam one_br -> (not in_lam || ok_inside_lam) && consider_safe in_lam True  one_br
				NoOccInfo	     -> ok_inside_lam 		      && consider_safe True   False False

	consider_safe in_lam once once_in_one_branch
		-- consider_safe decides whether it's a good idea to inline something,
		-- given that there's no work-duplication issue (the caller checks that).
		-- once_in_one_branch = True means there's a unique textual occurrence
	  | inline_call  = True

	  | once_in_one_branch
		-- Be very keen to inline something if this is its unique occurrence:
		--
		--   a) Inlining gives a good chance of eliminating the original 
		--	binding (and hence the allocation) for the thing.  
		--	(Provided it's not a top level binding, in which case the 
		--	 allocation costs nothing.)
		--
		--   b) Inlining a function that is called only once exposes the 
		--	body function to the call site.
		--
		-- The only time we hold back is when substituting inside a lambda;
		-- then if the context is totally uninteresting (not applied, not scrutinised)
		-- there is no point in substituting because it might just increase allocation,
		-- by allocating the function itself many times
		--
		-- Note: there used to be a '&& not top_level' in the guard above,
		--	 but that stopped us inlining top-level functions used only once,
		--	 which is stupid
	  = not in_lam || not (null arg_infos) || interesting_cont

	  | otherwise
	  = case guidance of
	      UnfoldNever  -> False ;
	      UnfoldIfGoodArgs n_vals_wanted arg_discounts size res_discount

		  | enough_args && size <= (n_vals_wanted + 1)
			-- No size increase
			-- Size of call is n_vals_wanted (+1 for the function)
		  -> True

	  	  | otherwise
		  -> some_benefit && small_enough

		  where
		    some_benefit = or arg_infos || really_interesting_cont || 
		    		   (not is_top && (once || (n_vals_wanted > 0 && enough_args)))
		    	-- If it occurs more than once, there must be something interesting 
		    	-- about some argument, or the result context, to make it worth inlining
		    	--
		    	-- If a function has a nested defn we also record some-benefit,
		    	-- on the grounds that we are often able to eliminate the binding,
		    	-- and hence the allocation, for the function altogether; this is good
		    	-- for join points.  But this only makes sense for *functions*;
		    	-- inlining a constructor doesn't help allocation unless the result is
		    	-- scrutinised.  UNLESS the constructor occurs just once, albeit possibly
		    	-- in multiple case branches.  Then inlining it doesn't increase allocation,
		    	-- but it does increase the chance that the constructor won't be allocated at all
		    	-- in the branches that don't use it.
	    
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
#ifdef DEBUG
    if opt_D_dump_inlinings then
	pprTrace "Considering inlining"
		 (ppr id <+> vcat [text "black listed" <+> ppr black_listed,
				   text "occ info:" <+> ppr occ,
			  	   text "arg infos" <+> ppr arg_infos,
				   text "interesting continuation" <+> ppr interesting_cont,
				   text "is value:" <+> ppr is_value,
				   text "is cheap:" <+> ppr is_cheap,
				   text "is bottom:" <+> ppr is_bot,
				   text "is top-level:"    <+> ppr is_top,
				   text "guidance" <+> ppr guidance,
				   text "ANSWER =" <+> if yes_or_no then text "YES" else text "NO",
				   if yes_or_no then
					text "Unfolding =" <+> pprCoreExpr unf_template
				   else empty])
		  result
    else
#endif
    result
    }

computeDiscount :: Int -> [Int] -> Int -> [Bool] -> Bool -> Int
computeDiscount n_vals_wanted arg_discounts res_discount arg_infos result_used
 	-- We multiple the raw discounts (args_discount and result_discount)
	-- ty opt_UnfoldingKeenessFactor because the former have to do with
	-- *size* whereas the discounts imply that there's some extra 
	-- *efficiency* to be gained (e.g. beta reductions, case reductions) 
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
	   fromInt (arg_discount + result_discount))
  where
    arg_discount = sum (zipWith mk_arg_discount arg_discounts arg_infos)

    mk_arg_discount discount is_evald | is_evald  = discount
				      | otherwise = 0

	-- Don't give a result discount unless there are enough args
    result_discount | result_used = res_discount	-- Over-applied, or case scrut
	            | otherwise	  = 0
\end{code}


%************************************************************************
%*									*
\subsection{Black-listing}
%*									*
%************************************************************************

Inlining is controlled by the "Inline phase" number, which is set
by the per-simplification-pass '-finline-phase' flag.

For optimisation we use phase 1,2 and nothing (i.e. no -finline-phase flag)
in that order.  The meanings of these are determined by the @blackListed@ function
here.

The final simplification doesn't have a phase number

Pragmas
~~~~~~~
	Pragma		Black list if

(least black listing, most inlining)
	INLINE n foo	phase is Just p *and* p<n *and* foo appears on LHS of rule
	INLINE foo	phase is Just p *and*           foo appears on LHS of rule
	NOINLINE n foo	phase is Just p *and* (p<n *or* foo appears on LHS of rule)
	NOINLINE foo	always
(most black listing, least inlining)

\begin{code}
blackListed :: IdSet 		-- Used in transformation rules
	    -> Maybe Int	-- Inline phase
	    -> Id -> Bool	-- True <=> blacklisted
	
-- The blackListed function sees whether a variable should *not* be 
-- inlined because of the inline phase we are in.  This is the sole
-- place that the inline phase number is looked at.

blackListed rule_vars Nothing		-- Last phase
  = \v -> case idInlinePragma v of
		IMustNotBeINLINEd False Nothing -> True		-- An unconditional NOINLINE pragma
		other				-> False

blackListed rule_vars (Just phase)
  = \v -> normal_case rule_vars phase v

normal_case rule_vars phase v 
  = case idInlinePragma v of
	NoInlinePragInfo -> has_rules

	IMustNotBeINLINEd from_INLINE Nothing
	  | from_INLINE -> has_rules	-- Black list until final phase
	  | otherwise   -> True		-- Always blacklisted

	IMustNotBeINLINEd from_inline (Just threshold)
	  | from_inline -> phase < threshold && has_rules
	  | otherwise   -> phase < threshold || has_rules
  where
    has_rules =  v `elemVarSet` rule_vars
	      || not (isEmptyCoreRules (idSpecialisation v))
\end{code}


SLPJ 95/04: Why @runST@ must be inlined very late:
\begin{verbatim}
f x =
  runST ( \ s -> let
		    (a, s')  = newArray# 100 [] s
		    (_, s'') = fill_in_array_or_something a x s'
		  in
		  freezeArray# a s'' )
\end{verbatim}
If we inline @runST@, we'll get:
\begin{verbatim}
f x = let
	(a, s')  = newArray# 100 [] realWorld#{-NB-}
	(_, s'') = fill_in_array_or_something a x s'
      in
      freezeArray# a s''
\end{verbatim}
And now the @newArray#@ binding can be floated to become a CAF, which
is totally and utterly wrong:
\begin{verbatim}
f = let
    (a, s')  = newArray# 100 [] realWorld#{-NB-} -- YIKES!!!
    in
    \ x ->
	let (_, s'') = fill_in_array_or_something a x s' in
	freezeArray# a s''
\end{verbatim}
All calls to @f@ will share a {\em single} array!  

Yet we do want to inline runST sometime, so we can avoid
needless code.  Solution: black list it until the last moment.

