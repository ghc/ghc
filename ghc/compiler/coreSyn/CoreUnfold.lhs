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
	Unfolding(..), UnfoldingGuidance(..), -- types

	noUnfolding, mkMagicUnfolding, mkUnfolding, getUnfoldingTemplate,
	isEvaldUnfolding, hasUnfolding,

	smallEnoughToInline, unfoldAlways, couldBeSmallEnoughToInline, 
	certainlySmallEnoughToInline, 
	okToUnfoldInHiFile,

	calcUnfoldingGuidance
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} MagicUFs	( MagicUnfoldingFun, mkMagicUnfoldingFun )

import CmdLineOpts	( opt_UnfoldingCreationThreshold,
			  opt_UnfoldingUseThreshold,
			  opt_UnfoldingConDiscount,
			  opt_UnfoldingKeenessFactor,
			  opt_UnfoldCasms, opt_PprStyle_Debug 
			)
import Constants	( uNFOLDING_CHEAP_OP_COST,
			  uNFOLDING_DEAR_OP_COST,
			  uNFOLDING_NOREP_LIT_COST
			)
import CoreSyn
import OccurAnal	( occurAnalyseGlobalExpr )
import CoreUtils	( coreExprType, exprIsTrivial, mkFormSummary, 
			  FormSummary(..) )
import Id		( Id, idType, isId )
import Const		( Con(..), isLitLitLit )
import PrimOp		( PrimOp(..), primOpOutOfLine )
import IdInfo		( ArityInfo(..), InlinePragInfo(..) )
import TyCon		( tyConFamilySize )
import Type		( splitAlgTyConApp_maybe )
import Const		( isNoRepLit )
import Unique           ( Unique )
import Util		( isIn )
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
		FormSummary		-- Tells whether the template is a WHNF or bottom
		UnfoldingGuidance	-- Tells about the *size* of the template.
		CoreExpr		-- Template; binder-info is correct

  | MagicUnfolding
	Unique				-- Unique of the Id whose magic unfolding this is
	MagicUnfoldingFun
\end{code}

\begin{code}
noUnfolding = NoUnfolding

mkUnfolding expr
  = let
     -- strictness mangling (depends on there being no CSE)
     ufg = calcUnfoldingGuidance opt_UnfoldingCreationThreshold expr
     occ = occurAnalyseGlobalExpr expr
    in
    CoreUnfolding (mkFormSummary expr) ufg occ

mkMagicUnfolding :: Unique -> Unfolding
mkMagicUnfolding tag  = MagicUnfolding tag (mkMagicUnfoldingFun tag)

getUnfoldingTemplate :: Unfolding -> CoreExpr
getUnfoldingTemplate (CoreUnfolding _ _ expr) = expr
getUnfoldingTemplate other = panic "getUnfoldingTemplate"

isEvaldUnfolding :: Unfolding -> Bool
isEvaldUnfolding (OtherCon _)		          = True
isEvaldUnfolding (CoreUnfolding ValueForm _ expr) = True
isEvaldUnfolding other			          = False

hasUnfolding :: Unfolding -> Bool
hasUnfolding NoUnfolding = False
hasUnfolding other 	 = True

data UnfoldingGuidance
  = UnfoldNever
  | UnfoldAlways		-- There is no "original" definition,
				-- so you'd better unfold.  Or: something
				-- so cheap to unfold (e.g., 1#) that
				-- you should do it absolutely always.

  | UnfoldIfGoodArgs	Int	-- if "m" type args 
			Int	-- and "n" value args

			[Int]	-- Discount if the argument is evaluated.
				-- (i.e., a simplification will definitely
				-- be possible).  One elt of the list per *value* arg.

			Int	-- The "size" of the unfolding; to be elaborated
				-- later. ToDo

			Int	-- Scrutinee discount: the discount to substract if the thing is in
				-- a context (case (thing args) of ...),
				-- (where there are the right number of arguments.)

unfoldAlways :: UnfoldingGuidance -> Bool
unfoldAlways UnfoldAlways = True
unfoldAlways other	  = False
\end{code}

\begin{code}
instance Outputable UnfoldingGuidance where
    ppr UnfoldAlways    	= ptext SLIT("_ALWAYS_")
    ppr (UnfoldIfGoodArgs t v cs size discount)
      = hsep [ptext SLIT("_IF_ARGS_"), int t, int v,
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
  = case collectTyAndValBinders expr of { (ty_binders, val_binders, body) ->
    case (sizeExpr bOMB_OUT_SIZE val_binders body) of

      TooBig -> UnfoldNever

      SizeIs size cased_args scrut_discount
	-> UnfoldIfGoodArgs
			(length ty_binders)
			(length val_binders)
			(map discount_for val_binders)
			(I# size)
			(I# scrut_discount)
	where        
	    discount_for b 
		| num_cases == 0 = 0
		| otherwise
		= if is_data 
			then tyConFamilySize tycon * num_cases
			else num_cases -- prim cases are pretty cheap
	  
		 where
		   (is_data, tycon)
		     = case (splitAlgTyConApp_maybe (idType b)) of
			  Nothing       -> (False, panic "discount")
			  Just (tc,_,_) -> (True,  tc)
		   num_cases = length (filter (==b) cased_args)
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
    size_up (Type t)	   = sizeZero		-- Types cost nothing
    size_up (Note _ body)  = size_up body	-- Notes cost nothing
    size_up (Var v)        = sizeOne
    size_up (App fun arg)  = size_up fun `addSize` size_up arg

    size_up (Con con args) = foldr (addSize . size_up) 
				   (size_up_con con (valArgCount args))
				   args

    size_up (Lam b e) | isId b    = size_up e `addSizeN` 1
		      | otherwise = size_up e

    size_up (Let (NonRec binder rhs) body)
      = nukeScrutDiscount (size_up rhs)		`addSize`
	size_up body				`addSizeN`
	1	-- For the allocation

    size_up (Let (Rec pairs) body)
      = nukeScrutDiscount rhs_size		`addSize`
	size_up body				`addSizeN`
	length pairs		-- For the allocation
      where
	rhs_size = foldr (addSize . size_up . snd) sizeZero pairs

    size_up (Case scrut _ alts)
      = nukeScrutDiscount (size_up scrut)		`addSize`
	arg_discount scrut				`addSize`
	foldr (addSize . size_up_alt) sizeZero alts	`addSizeN`
	case (splitAlgTyConApp_maybe (coreExprType scrut)) of
	      	Nothing       -> 1
	      	Just (tc,_,_) -> tyConFamilySize tc

    ------------ 
    size_up_alt (con, bndrs, rhs) = size_up rhs
	    -- Don't charge for args, so that wrappers look cheap

    ------------
    size_up_con (Literal lit) nv | isNoRepLit lit = sizeN uNFOLDING_NOREP_LIT_COST
			         | otherwise      = sizeOne

    size_up_con (DataCon dc) n_val_args = conSizeN n_val_args
			     
    size_up_con (PrimOp op) nv = sizeN op_cost
      where
	op_cost = if primOpOutOfLine op
		  then uNFOLDING_DEAR_OP_COST
			-- these *tend* to be more expensive;
			-- number chosen to avoid unfolding (HACK)
		  else uNFOLDING_CHEAP_OP_COST

    ------------
	-- We want to record if we're case'ing an argument
    arg_discount (Var v) | v `is_elem` args = scrutArg v
    arg_discount other			    = sizeZero

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
	xys   = xs ++ ys


\end{code}

Code for manipulating sizes

\begin{code}

data ExprSize = TooBig
	      | SizeIs Int#	-- Size found
		       [Id]	-- Arguments cased herein
		       Int#	-- Size to subtract if result is scrutinised 
				-- by a case expression

sizeZero     	= SizeIs 0# [] 0#
sizeOne      	= SizeIs 1# [] 0#
sizeN (I# n) 	= SizeIs n  [] 0#
conSizeN (I# n) = SizeIs 0# [] n   -- We don't count 1 for the constructor because we're
				   -- quite keen to get constructors into the open
scrutArg v	= SizeIs 0# [v] 0#

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

\begin{code}
smallEnoughToInline :: Id			-- The function (trace msg only)
		    -> [Bool]			-- Evaluated-ness of value arguments
						-- ** May be infinite in don't care cases **
						--    see couldBeSmallEnoughToInline etc
		    -> Bool			-- Result is scrutinised
		    -> UnfoldingGuidance
		    -> Bool			-- True => unfold it

smallEnoughToInline _ _ _ UnfoldAlways = True
smallEnoughToInline _ _ _ UnfoldNever  = False
smallEnoughToInline id arg_evals result_is_scruted
		    (UnfoldIfGoodArgs m_tys_wanted n_vals_wanted discount_vec size scrut_discount)
  | fun_with_no_args
  = False
  
  | (size - discount) > opt_UnfoldingUseThreshold
  = if opt_PprStyle_Debug then 
	pprTrace " too big:" stuff False
    else
	False

  | otherwise		-- All right!
  = if opt_PprStyle_Debug then 
	pprTrace " small enough:" stuff True
    else
	True

  where
    stuff = braces (ppr id <+> ppr (take 10 arg_evals) <+> ppr result_is_scruted <+> 
		    ppr size <+> ppr discount)

    fun_with_no_args = n_vals_wanted > 0 && null arg_evals
		-- A *function* with *no* value args => don't unfold
		-- Otherwise it's ok to try

	-- We multiple the raw discounts (args_discount and result_discount)
	-- ty opt_UnfoldingKeenessFactor because the former have to do with
	-- *size* whereas the discounts imply that there's some extra 
	-- *efficiency* to be gained (e.g. beta reductions, case reductions) 
	-- by inlining.

	-- we also discount 1 for each argument passed, because these will
	-- reduce with the lambdas in the function (we count 1 for a lambda
 	-- in size_up).

	-- NB: we never take the length of arg_evals because it might be infinite
    discount :: Int
    discount = length (take n_vals_wanted arg_evals) +
	       round (opt_UnfoldingKeenessFactor * 
		      fromInt (arg_discount + result_discount))

    arg_discount    = sum (zipWith mk_arg_discount discount_vec arg_evals)
    result_discount = mk_result_discount (drop n_vals_wanted arg_evals)

    mk_arg_discount no_of_constrs is_evald
      | is_evald  = no_of_constrs * opt_UnfoldingConDiscount
      | otherwise = 0

    mk_result_discount extra_args
	| not (null extra_args) || result_is_scruted = scrut_discount	-- Over-applied, or case scrut
        | otherwise	      			     = 0
\end{code}

We use this one to avoid exporting inlinings that we ``couldn't possibly
use'' on the other side.  Can be overridden w/ flaggery.
Just the same as smallEnoughToInline, except that it has no actual arguments.

\begin{code}
couldBeSmallEnoughToInline :: Id -> UnfoldingGuidance -> Bool
couldBeSmallEnoughToInline id guidance = smallEnoughToInline id (repeat True) True guidance

certainlySmallEnoughToInline :: Id -> UnfoldingGuidance -> Bool
certainlySmallEnoughToInline id guidance = smallEnoughToInline id (repeat False) False guidance
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
