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
	Unfolding, UnfoldingGuidance, -- types

	noUnfolding, mkTopUnfolding, mkUnfolding, mkCompulsoryUnfolding, seqUnfolding,
	mkOtherCon, otherCons,
	unfoldingTemplate, maybeUnfoldingTemplate,
	isEvaldUnfolding, isCheapUnfolding, isCompulsoryUnfolding,
	hasUnfolding, hasSomeUnfolding,

	couldBeSmallEnoughToInline, 
	certainlySmallEnoughToInline, 
	okToUnfoldInHiFile,

	calcUnfoldingGuidance, 

	callSiteInline, blackListed
    ) where

#include "HsVersions.h"

import CmdLineOpts	( opt_UF_CreationThreshold,
			  opt_UF_UseThreshold,
			  opt_UF_ScrutConDiscount,
			  opt_UF_FunAppDiscount,
			  opt_UF_PrimArgDiscount,
			  opt_UF_KeenessFactor,
			  opt_UF_CheapOp, opt_UF_DearOp, opt_UF_NoRepLit,
			  opt_UnfoldCasms, opt_PprStyle_Debug,
			  opt_D_dump_inlinings
			)
import CoreSyn
import PprCore		( pprCoreExpr )
import OccurAnal	( occurAnalyseGlobalExpr )
import BinderInfo	( )
import CoreUtils	( coreExprType, exprIsTrivial, exprIsValue, exprIsCheap )
import Id		( Id, idType, idUnique, isId, getIdWorkerInfo,
			  getIdSpecialisation, getInlinePragma, getIdUnfolding,
			  isConstantId_maybe
			)
import VarSet
import Name		( isLocallyDefined )
import Const		( Con(..), isLitLitLit, isWHNFCon )
import PrimOp		( PrimOp(..), primOpIsDupable )
import IdInfo		( ArityInfo(..), InlinePragInfo(..), OccInfo(..), insideLam, workerExists )
import TyCon		( tyConFamilySize )
import Type		( splitAlgTyConApp_maybe, splitFunTy_maybe, isUnLiftedType )
import Const		( isNoRepLit )
import Unique		( Unique, buildIdKey, augmentIdKey )
import Maybes		( maybeToBool )
import Bag
import Util		( isIn, lengthExceeds )
import Outputable

#if __GLASGOW_HASKELL__ >= 404
import GlaExts		( fromInt )
#endif
\end{code}

%************************************************************************
%*									*
\subsection{@Unfolding@ and @UnfoldingGuidance@ types}
%*									*
%************************************************************************

\begin{code}
data Unfolding
  = NoUnfolding

  | OtherCon [Con]		-- It ain't one of these
				-- (OtherCon xs) also indicates that something has been evaluated
				-- and hence there's no point in re-evaluating it.
				-- OtherCon [] is used even for non-data-type values
				-- to indicated evaluated-ness.  Notably:
				--	data C = C !(Int -> Int)
				-- 	case x of { C f -> ... }
				-- Here, f gets an OtherCon [] unfolding.

  | CompulsoryUnfolding CoreExpr	-- There is no "original" definition,
					-- so you'd better unfold.

  | CoreUnfolding			-- An unfolding with redundant cached information
		CoreExpr		-- Template; binder-info is correct
		Bool			-- This is a top-level binding
		Bool			-- exprIsCheap template (cached); it won't duplicate (much) work 
					--	if you inline this in more than one place
		Bool			-- exprIsValue template (cached); it is ok to discard a `seq` on
					--	this variable
		UnfoldingGuidance	-- Tells about the *size* of the template.

seqUnfolding :: Unfolding -> ()
seqUnfolding (CoreUnfolding e top b1 b2 g)
  = seqExpr e `seq` top `seq` b1 `seq` b2 `seq` seqGuidance g
seqUnfolding other = ()
\end{code}

\begin{code}
noUnfolding = NoUnfolding
mkOtherCon  = OtherCon

mkTopUnfolding expr = mkUnfolding True expr

mkUnfolding top_lvl expr
  = CoreUnfolding (occurAnalyseGlobalExpr expr)
		  top_lvl
		  (exprIsCheap expr)
		  (exprIsValue expr)
		  (calcUnfoldingGuidance opt_UF_CreationThreshold expr)

mkCompulsoryUnfolding expr	-- Used for things that absolutely must be unfolded
  = CompulsoryUnfolding (occurAnalyseGlobalExpr expr)

unfoldingTemplate :: Unfolding -> CoreExpr
unfoldingTemplate (CoreUnfolding expr _ _ _ _) = expr
unfoldingTemplate (CompulsoryUnfolding expr)   = expr
unfoldingTemplate other = panic "getUnfoldingTemplate"

maybeUnfoldingTemplate :: Unfolding -> Maybe CoreExpr
maybeUnfoldingTemplate (CoreUnfolding expr _ _ _ _) = Just expr
maybeUnfoldingTemplate (CompulsoryUnfolding expr)   = Just expr
maybeUnfoldingTemplate other 			    = Nothing

otherCons (OtherCon cons) = cons
otherCons other		  = []

isEvaldUnfolding :: Unfolding -> Bool
isEvaldUnfolding (OtherCon _)		          = True
isEvaldUnfolding (CoreUnfolding _ _ _ is_evald _) = is_evald
isEvaldUnfolding other			          = False

isCheapUnfolding :: Unfolding -> Bool
isCheapUnfolding (CoreUnfolding _ _ is_cheap _ _) = is_cheap
isCheapUnfolding other				  = False

isCompulsoryUnfolding :: Unfolding -> Bool
isCompulsoryUnfolding (CompulsoryUnfolding _) = True
isCompulsoryUnfolding other		      = False

hasUnfolding :: Unfolding -> Bool
hasUnfolding (CoreUnfolding _ _ _ _ _) = True
hasUnfolding (CompulsoryUnfolding _)   = True
hasUnfolding other 	 	       = False

hasSomeUnfolding :: Unfolding -> Bool
hasSomeUnfolding NoUnfolding = False
hasSomeUnfolding other	     = True

data UnfoldingGuidance
  = UnfoldNever
  | UnfoldIfGoodArgs	Int	-- and "n" value args

			[Int]	-- Discount if the argument is evaluated.
				-- (i.e., a simplification will definitely
				-- be possible).  One elt of the list per *value* arg.

			Int	-- The "size" of the unfolding; to be elaborated
				-- later. ToDo

			Int	-- Scrutinee discount: the discount to substract if the thing is in
				-- a context (case (thing args) of ...),
				-- (where there are the right number of arguments.)

seqGuidance (UnfoldIfGoodArgs n ns a b) = n `seq` sum ns `seq` a `seq` b `seq` ()
seqGuidance other			= ()
\end{code}

\begin{code}
instance Outputable UnfoldingGuidance where
    ppr UnfoldNever	= ptext SLIT("NEVER")
    ppr (UnfoldIfGoodArgs v cs size discount)
      = hsep [ ptext SLIT("IF_ARGS"), int v,
	       brackets (hsep (map int cs)),
	       int size,
	       int discount ]
\end{code}


%************************************************************************
%*									*
\subsection[calcUnfoldingGuidance]{Calculate ``unfolding guidance'' for an expression}
%*									*
%************************************************************************

\begin{code}
calcUnfoldingGuidance
	:: Int		    	-- bomb out if size gets bigger than this
	-> CoreExpr    		-- expression to look at
	-> UnfoldingGuidance
calcUnfoldingGuidance bOMB_OUT_SIZE expr
  = case collect_val_bndrs expr of { (inline, val_binders, body) ->
    let
	n_val_binders = length val_binders
    in
    case (sizeExpr bOMB_OUT_SIZE val_binders body) of

      TooBig 
	| not inline -> UnfoldNever
		-- A big function with an INLINE pragma must
		-- have an UnfoldIfGoodArgs guidance
	| inline     -> UnfoldIfGoodArgs n_val_binders
					 (map (const 0) val_binders)
					 (n_val_binders + 2) 0
				-- See comments with final_size below

      SizeIs size cased_args scrut_discount
	-> UnfoldIfGoodArgs
			n_val_binders
			(map discount_for val_binders)
			final_size
			(I# scrut_discount)
	where        
	    boxed_size    = I# size

	    final_size | inline     = 0	-- Trying very agresssive inlining of INLINE things.
					-- Reason: we don't want to call the un-inlined version,
					--	   because its body is awful
					-- boxed_size `min` (n_val_binders + 2)	-- Trying "+2" again...
		       | otherwise  = boxed_size
		-- The idea is that if there is an INLINE pragma (inline is True)
		-- and there's a big body, we give a size of n_val_binders+1.  This
		-- This is enough to pass the no-size-increase test in callSiteInline,
		--   but no more.
		-- I tried n_val_binders+2, to just defeat the test, on the grounds that
		--   we don't want to inline an INLINE thing into a totally boring context,
		--   but I found that some wrappers (notably one for a join point) weren't
		--   getting inlined, and that was terrible.  In that particular case, the
		--   call site applied the wrapper to realWorld#, so if we made that an 
		--   "interesting" value the inlining would have happened... but it was
		--   simpler to inline wrappers a little more eagerly instead.
		--
		-- Sometimes, though, an INLINE thing is smaller than n_val_binders+2.
		-- A particular case in point is a constructor, which has size 1.
		-- We want to inline this regardless, hence the `min`

	    discount_for b 
		| num_cases == 0 = 0
		| is_fun_ty  	 = num_cases * opt_UF_FunAppDiscount
		| is_data_ty 	 = num_cases * opt_UF_ScrutConDiscount
		| otherwise  	 = num_cases * opt_UF_PrimArgDiscount
		where
		  num_cases	      = foldlBag (\n b' -> if b==b' then n+1 else n) 0 cased_args
					-- Count occurrences of b in cased_args
		  arg_ty	      = idType b
		  is_fun_ty	      = maybeToBool (splitFunTy_maybe arg_ty)
		  (is_data_ty, tycon) = case (splitAlgTyConApp_maybe (idType b)) of
					  Nothing       -> (False, panic "discount")
					  Just (tc,_,_) -> (True,  tc)
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
    size_up (App fun arg)       = size_up_app fun [arg]

    size_up (Con con args) = foldr (addSize . nukeScrutDiscount . size_up) 
				   (size_up_con con args)
				   args

    size_up (Lam b e) | isId b    = size_up e `addSizeN` 1
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

    size_up (Case scrut _ alts)
      = nukeScrutDiscount (size_up scrut)		`addSize`
	arg_discount scrut				`addSize`
	foldr (addSize . size_up_alt) sizeZero alts	
	  `addSizeN` 1  -- charge one for the case itself.

-- Just charge for the alts that exist, not the ones that might exist
--	`addSizeN`
--	case (splitAlgTyConApp_maybe (coreExprType scrut)) of
--	      	Nothing       -> 1
--	      	Just (tc,_,_) -> tyConFamilySize tc

    ------------ 
    size_up_app (App fun arg) args   = size_up_app fun (arg:args)
    size_up_app fun 	      args   = foldr (addSize . nukeScrutDiscount . size_up) 
					     (size_up_fun fun args)
					     args

	-- A function application with at least one value argument
	-- so if the function is an argument give it an arg-discount
	-- Also behave specially if the function is a build
	-- Also if the function is a constant Id (constr or primop)
	-- compute discounts as if it were actually a Con; in the early
	-- stages these constructors and primops may not yet be inlined
    size_up_fun (Var fun) args | idUnique fun == buildIdKey   = buildSize
    			       | idUnique fun == augmentIdKey = augmentSize
    			       | fun `is_elem` top_args	      = scrutArg fun `addSize` fun_size
			       | otherwise		      = fun_size
			  where
			    fun_size = case isConstantId_maybe fun of
					     Just con -> size_up_con con args
					     Nothing  -> sizeOne

    size_up_fun other args = size_up other

    ------------ 
    size_up_alt (con, bndrs, rhs) = size_up rhs
	    -- Don't charge for args, so that wrappers look cheap

    ------------
    size_up_con (Literal lit) args | isNoRepLit lit = sizeN opt_UF_NoRepLit
			           | otherwise      = sizeOne

    size_up_con (DataCon dc) args = conSizeN (valArgCount args)
			     
    size_up_con (PrimOp op) args = foldr addSize (sizeN op_cost) (map arg_discount args)
		-- Give an arg-discount if a primop is applies to
		-- one of the function's arguments
      where
	op_cost | primOpIsDupable op = opt_UF_CheapOp
		| otherwise 	     = opt_UF_DearOp

	-- We want to record if we're case'ing, or applying, an argument
    arg_discount (Var v) | v `is_elem` top_args = scrutArg v
    arg_discount other			        = sizeZero

    ------------
    is_elem :: Id -> [Id] -> Bool
    is_elem = isIn "size_up_scrut"

    ------------
	-- These addSize things have to be here because
	-- I don't want to give them bOMB_OUT_SIZE as an argument

    addSizeN TooBig          _ = TooBig
    addSizeN (SizeIs n xs d) (I# m)
      | n_tot -# d <# bOMB_OUT_SIZE = SizeIs n_tot xs d
      | otherwise 		    = TooBig
      where
	n_tot = n +# m
    
    addSize TooBig _ = TooBig
    addSize _ TooBig = TooBig
    addSize (SizeIs n1 xs d1) (SizeIs n2 ys d2)
      | (n_tot -# d_tot) <# bOMB_OUT_SIZE = SizeIs n_tot xys d_tot
      | otherwise 			  = TooBig
      where
	n_tot = n1 +# n2
	d_tot = d1 +# d2
	xys   = xs `unionBags` ys
\end{code}

Code for manipulating sizes

\begin{code}

data ExprSize = TooBig
	      | SizeIs Int#	-- Size found
		       (Bag Id)	-- Arguments cased herein
		       Int#	-- Size to subtract if result is scrutinised 
				-- by a case expression

sizeZero     	= SizeIs 0# emptyBag 0#
sizeOne      	= SizeIs 1# emptyBag 0#
sizeTwo      	= SizeIs 2# emptyBag 0#
sizeN (I# n) 	= SizeIs n  emptyBag 0#
conSizeN (I# n) = SizeIs 1# emptyBag (n +# 1#)
	-- Treat constructors as size 1, that unfoldAlways responsds 'False'
	-- when asked about 'x' when x is bound to (C 3#).
	-- This avoids gratuitous 'ticks' when x itself appears as an
	-- atomic constructor argument.

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
						
scrutArg v	= SizeIs 0# (unitBag v) 0#

nukeScrutDiscount (SizeIs n vs d) = SizeIs n vs 0#
nukeScrutDiscount TooBig	  = TooBig
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
couldBeSmallEnoughToInline :: UnfoldingGuidance -> Bool
couldBeSmallEnoughToInline UnfoldNever = False
couldBeSmallEnoughToInline other       = True

certainlySmallEnoughToInline :: UnfoldingGuidance -> Bool
certainlySmallEnoughToInline UnfoldNever		   = False
certainlySmallEnoughToInline (UnfoldIfGoodArgs _ _ size _) = size <= opt_UF_UseThreshold
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
    go (Var _)                = True
    go (Con (Literal lit) _)  = not (isLitLitLit lit)
    go (Con (PrimOp op) args) = okToUnfoldPrimOp op && all go args
    go (Con con args)         = all go args -- might be litlits in here
    go (App fun arg)          = go fun && go arg
    go (Lam _ body)           = go body
    go (Let binds body)       = and (map go (body :rhssOfBind binds))
    go (Case scrut bndr alts) = and (map go (scrut:rhssOfAlts alts))
    go (Note _ body)          = go body
    go (Type _)		      = True

    -- ok to unfold a PrimOp as long as it's not a _casm_
    okToUnfoldPrimOp (CCallOp _ is_casm _ _) = not is_casm
    okToUnfoldPrimOp _                       = True
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

\begin{code}
callSiteInline :: Bool			-- True <=> the Id is black listed
	       -> Bool			-- 'inline' note at call site
	       -> OccInfo
	       -> Id			-- The Id
	       -> [Bool]		-- One for each value arg; True if it is interesting
	       -> Bool			-- True <=> continuation is interesting
	       -> Maybe CoreExpr	-- Unfolding, if any


callSiteInline black_listed inline_call occ id arg_infos interesting_cont
  = case getIdUnfolding id of {
	NoUnfolding -> Nothing ;
	OtherCon _  -> Nothing ;
	CompulsoryUnfolding unf_template | black_listed -> Nothing 
					 | otherwise 	-> Just unf_template ;
		-- Primops have compulsory unfoldings, but
		-- may have rules, in which case they are 
		-- black listed till later
	CoreUnfolding unf_template is_top is_cheap _ guidance ->

    let
	result | yes_or_no = Just unf_template
	       | otherwise = Nothing

	n_val_args  = length arg_infos

 	yes_or_no 
	  | black_listed = False
	  | otherwise    = case occ of
				IAmDead		     -> pprTrace "callSiteInline: dead" (ppr id) False
				IAmALoopBreaker      -> False
				OneOcc in_lam one_br -> (not in_lam || is_cheap) && consider_safe in_lam True  one_br
				NoOccInfo	     -> is_cheap 		 && consider_safe True   False False

	consider_safe in_lam once once_in_one_branch
		-- consider_safe decides whether it's a good idea to inline something,
		-- given that there's no work-duplication issue (the caller checks that).
		-- once_in_one_branch = True means there's a unique textual occurrence
	  | inline_call  = True

	  | once_in_one_branch	-- Be very keen to inline something if this is its unique occurrence; that
				-- gives a good chance of eliminating the original binding for the thing.
				-- The only time we hold back is when substituting inside a lambda;
				-- then if the context is totally uninteresting (not applied, not scrutinised)
				-- there is no point in substituting because it might just increase allocation.
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
				   text "is cheap" <+> ppr is_cheap,
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
  = \v -> case getInlinePragma v of
		IMustNotBeINLINEd False Nothing -> True		-- An unconditional NOINLINE pragma
		other				-> False

blackListed rule_vars (Just 0)
-- Phase 0: used for 'no imported inlinings please'
-- This prevents wrappers getting inlined which in turn is bad for full laziness
-- NEW: try using 'not a wrapper' rather than 'not imported' in this phase.
-- This allows a little more inlining, which seems to be important, sometimes.
-- For example PrelArr.newIntArr gets better.
  = \v -> -- workerExists (getIdWorkerInfo v) || normal_case rule_vars 0 v
	  -- True	-- Try going back to no inlinings at all
			-- BUT: I found that there is some advantage in doing 
			-- local inlinings first.  For example in fish/Main.hs
			-- it's advantageous to inline scale_vec2 before inlining
			-- wrappers from PrelNum that make it look big.
	  not (isLocallyDefined v) || normal_case rule_vars 0 v		-- This seems best at the moment

blackListed rule_vars (Just phase)
  = \v -> normal_case rule_vars phase v

normal_case rule_vars phase v 
  = case getInlinePragma v of
	NoInlinePragInfo -> has_rules

	IMustNotBeINLINEd from_INLINE Nothing
	  | from_INLINE -> has_rules	-- Black list until final phase
	  | otherwise   -> True		-- Always blacklisted

	IMustNotBeINLINEd from_inline (Just threshold)
	  | from_inline -> phase < threshold && has_rules
	  | otherwise   -> phase < threshold || has_rules
  where
    has_rules =  v `elemVarSet` rule_vars
	      || not (isEmptyCoreRules (getIdSpecialisation v))
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

