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

	noUnfolding, mkUnfolding, 
	mkOtherCon, otherCons,
	unfoldingTemplate, maybeUnfoldingTemplate,
	isEvaldUnfolding, isCheapUnfolding,
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
import Id		( Id, idType, idUnique, isId, 
			  getIdSpecialisation, getInlinePragma, getIdUnfolding
			)
import VarSet
import Name		( isLocallyDefined )
import Const		( Con(..), isLitLitLit, isWHNFCon )
import PrimOp		( PrimOp(..), primOpIsDupable )
import IdInfo		( ArityInfo(..), InlinePragInfo(..), OccInfo(..) )
import TyCon		( tyConFamilySize )
import Type		( splitAlgTyConApp_maybe, splitFunTy_maybe, isUnLiftedType )
import Const		( isNoRepLit )
import Unique		( Unique, buildIdKey, augmentIdKey, runSTRepIdKey )
import Maybes		( maybeToBool )
import Bag
import Util		( isIn, lengthExceeds )
import Outputable
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

  | CoreUnfolding			-- An unfolding with redundant cached information
		CoreExpr		-- Template; binder-info is correct
		Bool			-- exprIsCheap template (cached); it won't duplicate (much) work 
					--	if you inline this in more than one place
		Bool			-- exprIsValue template (cached); it is ok to discard a `seq` on
					--	this variable
		UnfoldingGuidance	-- Tells about the *size* of the template.
\end{code}

\begin{code}
noUnfolding = NoUnfolding
mkOtherCon  = OtherCon

mkUnfolding expr
  = CoreUnfolding (occurAnalyseGlobalExpr expr)
		  (exprIsCheap expr)
		  (exprIsValue expr)
		  (calcUnfoldingGuidance opt_UF_CreationThreshold expr)

unfoldingTemplate :: Unfolding -> CoreExpr
unfoldingTemplate (CoreUnfolding expr _ _ _) = expr
unfoldingTemplate other = panic "getUnfoldingTemplate"

maybeUnfoldingTemplate :: Unfolding -> Maybe CoreExpr
maybeUnfoldingTemplate (CoreUnfolding expr _ _ _) = Just expr
maybeUnfoldingTemplate other 			  = Nothing

otherCons (OtherCon cons) = cons
otherCons other		  = []

isEvaldUnfolding :: Unfolding -> Bool
isEvaldUnfolding (OtherCon _)		        = True
isEvaldUnfolding (CoreUnfolding _ _ is_evald _) = is_evald
isEvaldUnfolding other			        = False

isCheapUnfolding :: Unfolding -> Bool
isCheapUnfolding (CoreUnfolding _ is_cheap _ _) = is_cheap
isCheapUnfolding other				= False

hasUnfolding :: Unfolding -> Bool
hasUnfolding (CoreUnfolding _ _ _ _) = True
hasUnfolding other 	 	     = False

hasSomeUnfolding :: Unfolding -> Bool
hasSomeUnfolding NoUnfolding = False
hasSomeUnfolding other	     = True

data UnfoldingGuidance
  = UnfoldNever
  | UnfoldAlways		-- There is no "original" definition,
				-- so you'd better unfold.  Or: something
				-- so cheap to unfold (e.g., 1#) that
				-- you should do it absolutely always.

  | UnfoldIfGoodArgs	Int	-- and "n" value args

			[Int]	-- Discount if the argument is evaluated.
				-- (i.e., a simplification will definitely
				-- be possible).  One elt of the list per *value* arg.

			Int	-- The "size" of the unfolding; to be elaborated
				-- later. ToDo

			Int	-- Scrutinee discount: the discount to substract if the thing is in
				-- a context (case (thing args) of ...),
				-- (where there are the right number of arguments.)
\end{code}

\begin{code}
instance Outputable UnfoldingGuidance where
    ppr UnfoldAlways    = ptext SLIT("ALWAYS")
    ppr UnfoldNever	= ptext SLIT("NEVER")
    ppr (UnfoldIfGoodArgs v cs size discount)
      = hsep [ptext SLIT("IF_ARGS"), int v,
	       if null cs	-- always print *something*
	       	then char 'X'
		else hcat (map (text . show) cs),
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
  | exprIsTrivial expr		-- Often trivial expressions are never bound
				-- to an expression, but it can happen.  For
				-- example, the Id for a nullary constructor has
				-- a trivial expression as its unfolding, and
				-- we want to make sure that we always unfold it.
  = UnfoldAlways
 
  | otherwise
  = case collectBinders expr of { (binders, body) ->
    let
	val_binders = filter isId binders
    in
    case (sizeExpr bOMB_OUT_SIZE val_binders body) of

      TooBig -> UnfoldNever

      SizeIs size cased_args scrut_discount
	-> UnfoldIfGoodArgs
			(length val_binders)
			(map discount_for val_binders)
			(I# size)
			(I# scrut_discount)
	where        
	    discount_for b 
		| num_cases == 0 = 0
		| is_fun_ty  	 = num_cases * opt_UF_FunAppDiscount
		| is_data_ty 	 = num_cases * tyConFamilySize tycon * opt_UF_ScrutConDiscount
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
\end{code}

\begin{code}
sizeExpr :: Int 	    -- Bomb out if it gets bigger than this
	 -> [Id]	    -- Arguments; we're interested in which of these
			    -- get case'd
	 -> CoreExpr
	 -> ExprSize

sizeExpr (I# bOMB_OUT_SIZE) args expr
  = size_up expr
  where
    size_up (Type t)	      = sizeZero	-- Types cost nothing
    size_up (Var v)           = sizeOne

    size_up (Note InlineMe _) = sizeTwo		-- The idea is that this is one more
						-- than the size of the "call" (i.e. 1)
						-- We want to reply "no" to noSizeIncrease
						-- for a bare reference (i.e. applied to no args) 
						-- to an INLINE thing

    size_up (Note _ body)     = size_up body	-- Notes cost nothing

    size_up (App fun (Type t))  = size_up fun
    size_up (App fun arg)       = size_up_app fun [arg]

    size_up (Con con args) = foldr (addSize . size_up) 
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

-- Just charge for the alts that exist, not the ones that might exist
--	`addSizeN`
--	case (splitAlgTyConApp_maybe (coreExprType scrut)) of
--	      	Nothing       -> 1
--	      	Just (tc,_,_) -> tyConFamilySize tc

    ------------ 
    size_up_app (App fun arg) args   = size_up_app fun (arg:args)
    size_up_app fun 	      args   = foldr (addSize . size_up) (fun_discount fun) args

	-- A function application with at least one value argument
	-- so if the function is an argument give it an arg-discount
	-- Also behave specially if the function is a build
    fun_discount (Var fun) | idUnique fun == buildIdKey   = buildSize
    			   | idUnique fun == augmentIdKey = augmentSize
    			   | fun `is_elem` args 	= scrutArg fun
    fun_discount other					= sizeZero

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
    arg_discount (Var v) | v `is_elem` args = scrutArg v
    arg_discount other			    = sizeZero

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
certainlySmallEnoughToInline UnfoldAlways		   = True
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
    go (Con con args)         = True -- con args are always atomic
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
	       -> Id			-- The Id
	       -> [Bool]		-- One for each value arg; True if it is interesting
	       -> Bool			-- True <=> continuation is interesting
	       -> Maybe CoreExpr	-- Unfolding, if any


callSiteInline black_listed inline_call id arg_infos interesting_cont
  = case getIdUnfolding id of {
	NoUnfolding -> Nothing ;
	OtherCon _  -> Nothing ;
	CoreUnfolding unf_template is_cheap _ guidance ->

    let
	result | yes_or_no = Just unf_template
	       | otherwise = Nothing

	inline_prag = getInlinePragma id
	n_val_args  = length arg_infos

	yes_or_no =
	    case inline_prag of
		IAmDead		  -> pprTrace "callSiteInline: dead" (ppr id) False
		IMustNotBeINLINEd -> False
		IAmALoopBreaker   -> False
		IMustBeINLINEd    -> True	-- Overrides absolutely everything, including the black list
		ICanSafelyBeINLINEd in_lam one_br -> consider in_lam    True  one_br
		NoInlinePragInfo		  -> consider InsideLam False False

	consider in_lam once once_in_one_branch
	  | black_listed = False
	  | inline_call  = True
	  | once_in_one_branch	-- Be very keen to inline something if this is its unique occurrence; that
				-- gives a good chance of eliminating the original binding for the thing.
				-- The only time we hold back is when substituting inside a lambda;
				-- then if the context is totally uninteresting (not applied, not scrutinised)
				-- there is no point in substituting because it might just increase allocation.
	  = WARN( case in_lam of { NotInsideLam -> True; other -> False },
		  text "callSiteInline:oneOcc" <+> ppr id )
		-- If it has one occurrence, not inside a lambda, PreInlineUnconditionally
		-- should have zapped it already
	    is_cheap && (not (null arg_infos) || interesting_cont)

	  | otherwise	-- Occurs (textually) more than once, so look at its size
	  = case guidance of
	      UnfoldAlways -> True
	      UnfoldNever  -> False
	      UnfoldIfGoodArgs n_vals_wanted arg_discounts size res_discount
		| enough_args && size <= (n_vals_wanted + 1)
			-- No size increase
			-- Size of call is n_vals_wanted (+1 for the function)
		-> case in_lam of
			NotInsideLam -> True
			InsideLam    -> is_cheap

		| not (or arg_infos || really_interesting_cont || once)
			-- If it occurs more than once, there must be something interesting 
			-- about some argument, or the result, to make it worth inlining
			-- We also drop this case if the thing occurs once, although perhaps in 
			-- several branches.  In this case we are keener about inlining in the hope
			-- that we'll be able to drop the allocation for the function altogether.
		-> False
  
		| otherwise
		-> case in_lam of
			NotInsideLam -> small_enough
			InsideLam    -> is_cheap && small_enough

		where
		  enough_args		  = n_val_args >= n_vals_wanted
		  really_interesting_cont | n_val_args <  n_vals_wanted = False	-- Too few args
					  | n_val_args == n_vals_wanted = interesting_cont
					  | otherwise		        = True	-- Extra args
			-- This rather elaborate defn for really_interesting_cont is important
			-- Consider an I# = INLINE (\x -> I# {x})
			-- The unfolding guidance deems it to have size 2, and no arguments.
			-- So in an application (I# y) we must take the extra arg 'y' as
			-- evidence of an interesting context!
			
		  small_enough = (size - discount) <= opt_UF_UseThreshold
		  discount     = computeDiscount n_vals_wanted arg_discounts res_discount 
						 arg_infos really_interesting_cont

				
    in    
#ifdef DEBUG
    if opt_D_dump_inlinings then
	pprTrace "Considering inlining"
		 (ppr id <+> vcat [text "black listed" <+> ppr black_listed,
				   text "inline prag:" <+> ppr inline_prag,
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
  = length (take n_vals_wanted arg_infos) +
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

\begin{code}
blackListed :: IdSet 		-- Used in transformation rules
	    -> Maybe Int	-- Inline phase
	    -> Id -> Bool	-- True <=> blacklisted
	
-- The blackListed function sees whether a variable should *not* be 
-- inlined because of the inline phase we are in.  This is the sole
-- place that the inline phase number is looked at.

-- Phase 0: used for 'no imported inlinings please'
-- This prevents wrappers getting inlined which in turn is bad for full laziness
blackListed rule_vars (Just 0)
  = \v -> not (isLocallyDefined v)

-- Phase 1: don't inline any rule-y things or things with specialisations
blackListed rule_vars (Just 1)
  = \v -> let v_uniq = idUnique v
	  in v `elemVarSet` rule_vars
	  || not (isEmptyCoreRules (getIdSpecialisation v))
	  || v_uniq == runSTRepIdKey

-- Phase 2: allow build/augment to inline, and specialisations
blackListed rule_vars (Just 2)
  = \v -> let v_uniq = idUnique v
	  in (v `elemVarSet` rule_vars && not (v_uniq == buildIdKey || 
					       v_uniq == augmentIdKey))
	  || v_uniq == runSTRepIdKey

-- Otherwise just go for it
blackListed rule_vars phase
  = \v -> False
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

