%
% (c) The AQUA Project, Glasgow University, 1994-1996
%
\section[CoreUnfold]{Core-syntax unfoldings}

Unfoldings (which can travel across module boundaries) are in Core
syntax (namely @CoreExpr@s).

The type @Unfolding@ sits ``above'' simply-Core-expressions
unfoldings, capturing ``higher-level'' things we know about a binding,
usually things that the simplifier found out (e.g., ``it's a
literal'').  In the corner of a @SimpleUnfolding@ unfolding, you will
find, unsurprisingly, a Core expression.

\begin{code}
#include "HsVersions.h"

module CoreUnfold (
	SimpleUnfolding(..), Unfolding(..), UnfoldingGuidance(..), -- types
	UfExpr,	RdrName, -- For closure (delete in 1.3)

	FormSummary(..), mkFormSummary, whnfOrBottom, exprSmallEnoughToDup, exprIsTrivial,

	noUnfolding, mkMagicUnfolding, mkUnfolding, getUnfoldingTemplate,

	smallEnoughToInline, couldBeSmallEnoughToInline, certainlySmallEnoughToInline,
	okToInline,

	calcUnfoldingGuidance,

	PragmaInfo(..)		-- Re-export
    ) where

IMP_Ubiq()
#if defined (__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ <= 201
IMPORT_DELOOPER(IdLoop)	 -- for paranoia checking;
		 -- and also to get mkMagicUnfoldingFun
IMPORT_DELOOPER(PrelLoop)  -- for paranoia checking
IMPORT_DELOOPER(SmplLoop)
#else
import {-# SOURCE #-} MagicUFs
import {-# SOURCE #-} Id ( Id )
#endif

import Bag		( emptyBag, unitBag, unionBags, Bag )

import CmdLineOpts	( opt_UnfoldingCreationThreshold,
			  opt_UnfoldingUseThreshold,
			  opt_UnfoldingConDiscount
			)
import Constants	( uNFOLDING_CHEAP_OP_COST,
			  uNFOLDING_DEAR_OP_COST,
			  uNFOLDING_NOREP_LIT_COST
			)
import BinderInfo	( BinderInfo(..), FunOrArg, DuplicationDanger, InsideSCC, isDupDanger )
import PragmaInfo	( PragmaInfo(..) )
import CoreSyn
import CoreUtils	( unTagBinders )
import HsCore		( UfExpr )
import RdrHsSyn		( RdrName )
import OccurAnal	( occurAnalyseGlobalExpr )
import CoreUtils	( coreExprType )
--import CostCentre	( ccMentionsId )
import Id		( idType, getIdArity,  isBottomingId, isDataCon, isPrimitiveId_maybe,
			  SYN_IE(IdSet), GenId{-instances-} )
import PrimOp		( primOpCanTriggerGC, fragilePrimOp, PrimOp(..) )
import IdInfo		( ArityInfo(..), bottomIsGuaranteed )
import Literal		( isNoRepLit, isLitLitLit )
import Pretty
import TyCon		( tyConFamilySize )
import Type		( maybeAppDataTyConExpandingDicts )
import Unique           ( Unique )
import UniqSet		( emptyUniqSet, unitUniqSet, mkUniqSet,
			  addOneToUniqSet, unionUniqSets
			)
import Usage		( SYN_IE(UVar) )
import Maybes		( maybeToBool )
import Util		( isIn, panic, assertPanic )
#if __GLASGOW_HASKELL__ >= 202
import Outputable

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

  | CoreUnfolding SimpleUnfolding

  | MagicUnfolding
	Unique				-- Unique of the Id whose magic unfolding this is
	MagicUnfoldingFun


data SimpleUnfolding
  = SimpleUnfolding			-- An unfolding with redundant cached information
		FormSummary		-- Tells whether the template is a WHNF or bottom
		UnfoldingGuidance	-- Tells about the *size* of the template.
		SimplifiableCoreExpr	-- Template


noUnfolding = NoUnfolding

mkUnfolding inline_prag expr
  = let
     -- strictness mangling (depends on there being no CSE)
     ufg = calcUnfoldingGuidance inline_prag opt_UnfoldingCreationThreshold expr
     occ = occurAnalyseGlobalExpr expr
     cuf = CoreUnfolding (SimpleUnfolding (mkFormSummary expr) ufg occ)
					  
     cont = case occ of { Var _ -> cuf; _ -> cuf }
    in
    case ufg of { UnfoldAlways -> cont; _ -> cont }

mkMagicUnfolding :: Unique -> Unfolding
mkMagicUnfolding tag  = MagicUnfolding tag (mkMagicUnfoldingFun tag)

getUnfoldingTemplate :: Unfolding -> CoreExpr
getUnfoldingTemplate (CoreUnfolding (SimpleUnfolding _ _ expr))
  = unTagBinders expr
getUnfoldingTemplate other = panic "getUnfoldingTemplate"


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
\end{code}

\begin{code}
instance Outputable UnfoldingGuidance where
    ppr sty UnfoldAlways    	= ptext SLIT("_ALWAYS_")
    ppr sty (UnfoldIfGoodArgs t v cs size discount)
      = hsep [ptext SLIT("_IF_ARGS_"), int t, int v,
	       if null cs	-- always print *something*
	       	then char 'X'
		else hcat (map (text . show) cs),
	       int size,
	       int discount ]
\end{code}


%************************************************************************
%*									*
\subsection{Figuring out things about expressions}
%*									*
%************************************************************************

\begin{code}
data FormSummary
  = VarForm		-- Expression is a variable (or scc var, etc)
  | ValueForm		-- Expression is a value: i.e. a value-lambda,constructor, or literal
  | BottomForm		-- Expression is guaranteed to be bottom. We're more gung
			-- ho about inlining such things, because it can't waste work
  | OtherForm		-- Anything else

instance Outputable FormSummary where
   ppr sty VarForm    = ptext SLIT("Var")
   ppr sty ValueForm  = ptext SLIT("Value")
   ppr sty BottomForm = ptext SLIT("Bot")
   ppr sty OtherForm  = ptext SLIT("Other")

mkFormSummary ::GenCoreExpr bndr Id tyvar uvar -> FormSummary

mkFormSummary expr
  = go (0::Int) expr		-- The "n" is the number of (value) arguments so far
  where
    go n (Lit _)	= ASSERT(n==0) ValueForm
    go n (Con _ _)      = ASSERT(n==0) ValueForm
    go n (Prim _ _)	= OtherForm
    go n (SCC _ e)      = go n e
    go n (Coerce _ _ e) = go n e

    go n (Let (NonRec b r) e) | exprIsTrivial r = go n e	-- let f = f' alpha in (f,g) 
								-- should be treated as a value
    go n (Let _ e)      = OtherForm
    go n (Case _ _)     = OtherForm

    go 0 (Lam (ValBinder x) e) = ValueForm	-- NB: \x.bottom /= bottom!
    go n (Lam (ValBinder x) e) = go (n-1) e	-- Applied lambda
    go n (Lam other_binder e)  = go n e

    go n (App fun arg) | isValArg arg = go (n+1) fun
    go n (App fun other_arg)          = go n fun

    go n (Var f) | isBottomingId f = BottomForm
		 | isDataCon f	   = ValueForm		-- Can happen inside imported unfoldings
    go 0 (Var f)		   = VarForm
    go n (Var f)		   = case getIdArity f of
					  ArityExactly a | n < a -> ValueForm
					  ArityAtLeast a | n < a -> ValueForm
					  other			 -> OtherForm

whnfOrBottom :: GenCoreExpr bndr Id tyvar uvar -> Bool
whnfOrBottom e = case mkFormSummary e of 
			VarForm    -> True
			ValueForm  -> True
			BottomForm -> True
			OtherForm  -> False
\end{code}

@exprIsTrivial@ is true of expressions we are unconditionally happy to duplicate;
simple variables and constants, and type applications.

\begin{code}
exprIsTrivial (Var v) 		= True
exprIsTrivial (Lit lit)         = not (isNoRepLit lit)
exprIsTrivial (App e (TyArg _)) = exprIsTrivial e
exprIsTrivial (Coerce _ _ e)    = exprIsTrivial e
exprIsTrivial other		= False
\end{code}

\begin{code}
exprSmallEnoughToDup (Con _ _)      = True	-- Could check # of args
exprSmallEnoughToDup (Prim op _)    = not (fragilePrimOp op) -- Could check # of args
exprSmallEnoughToDup (Lit lit)      = not (isNoRepLit lit)
exprSmallEnoughToDup (Coerce _ _ e) = exprSmallEnoughToDup e
exprSmallEnoughToDup expr
  = case (collectArgs expr) of { (fun, _, _, vargs) ->
    case fun of
      Var v | length vargs <= 4 -> True
      _				-> False
    }

\end{code}


%************************************************************************
%*									*
\subsection[calcUnfoldingGuidance]{Calculate ``unfolding guidance'' for an expression}
%*									*
%************************************************************************

\begin{code}
calcUnfoldingGuidance
	:: PragmaInfo	    	-- INLINE pragma stuff
	-> Int		    	-- bomb out if size gets bigger than this
	-> CoreExpr    		-- expression to look at
	-> UnfoldingGuidance

calcUnfoldingGuidance IMustBeINLINEd    bOMB_OUT_SIZE expr = UnfoldAlways	-- Always inline if the INLINE pragma says so
calcUnfoldingGuidance IWantToBeINLINEd  bOMB_OUT_SIZE expr = UnfoldAlways	-- Always inline if the INLINE pragma says so
calcUnfoldingGuidance IMustNotBeINLINEd bOMB_OUT_SIZE expr = UnfoldNever	-- ...and vice versa...

calcUnfoldingGuidance NoPragmaInfo bOMB_OUT_SIZE expr
  = case collectBinders expr of { (use_binders, ty_binders, val_binders, body) ->
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
	         | is_data && b `is_elem` cased_args = tyConFamilySize tycon
		 | otherwise = 0
		 where
		   (is_data, tycon)
		     = case (maybeAppDataTyConExpandingDicts (idType b)) of
			  Nothing       -> (False, panic "discount")
			  Just (tc,_,_) -> (True,  tc)

	    is_elem = isIn "calcUnfoldingGuidance" }
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
    size_up (Var v)        	       = sizeZero
    size_up (Lit lit) | isNoRepLit lit = sizeN uNFOLDING_NOREP_LIT_COST
		      | otherwise      = sizeZero

    size_up (SCC lbl body)    = size_up body		-- SCCs cost nothing
    size_up (Coerce _ _ body) = size_up body		-- Coercions cost nothing

    size_up (App fun arg)  = size_up fun `addSize` size_up_arg arg
				-- NB Zero cost for for type applications;
				-- others cost 1 or more

    size_up (Con con args) = conSizeN (numValArgs args)
			     -- We don't count 1 for the constructor because we're
			     -- quite keen to get constructors into the open
			     
    size_up (Prim op args) = sizeN op_cost -- NB: no charge for PrimOp args
      where
	op_cost = if primOpCanTriggerGC op
		  then uNFOLDING_DEAR_OP_COST
			-- these *tend* to be more expensive;
			-- number chosen to avoid unfolding (HACK)
		  else uNFOLDING_CHEAP_OP_COST

    size_up expr@(Lam _ _)
      = let
	    (uvars, tyvars, args, body) = collectBinders expr
	in
	size_up body `addSizeN` length args

    size_up (Let (NonRec binder rhs) body)
      = nukeScrutDiscount (size_up rhs)
		`addSize`
	size_up body

    size_up (Let (Rec pairs) body)
      = nukeScrutDiscount (foldr addSize sizeZero [size_up rhs | (_,rhs) <- pairs])
		`addSize`
	size_up body

    size_up (Case scrut alts)
      = nukeScrutDiscount (size_up scrut)
		`addSize`
	arg_discount scrut
		`addSize`
	size_up_alts (coreExprType scrut) alts
	    -- We charge for the "case" itself in "size_up_alts"

    ------------
	-- In an application we charge	0 for type application
	-- 				1 for most anything else
	--				N for norep_lits
    size_up_arg (LitArg lit) | isNoRepLit lit = sizeN uNFOLDING_NOREP_LIT_COST
    size_up_arg (TyArg _)		      = sizeZero
    size_up_arg other			      = sizeOne

    ------------
    size_up_alts scrut_ty (AlgAlts alts deflt)
      = (foldr (addSize . size_alg_alt) (size_up_deflt deflt) alts)
	`addSizeN`
	alt_cost
      where
	size_alg_alt (con,args,rhs) = size_up rhs
	    -- Don't charge for args, so that wrappers look cheap

	-- NB: we charge N for an alg. "case", where N is
	-- the number of constructors in the thing being eval'd.
	-- (You'll eventually get a "discount" of N if you
	-- think the "case" is likely to go away.)
	-- It's important to charge for alternatives.  If you don't then you
	-- get size 1 for things like:
	--		case x of { A -> 1#; B -> 2#; ... lots }

	alt_cost :: Int
	alt_cost
	  = case (maybeAppDataTyConExpandingDicts scrut_ty) of
	      Nothing       -> 1
	      Just (tc,_,_) -> tyConFamilySize tc

    size_up_alts _ (PrimAlts alts deflt)
      = foldr (addSize . size_prim_alt) (size_up_deflt deflt) alts
	    -- *no charge* for a primitive "case"!
      where
	size_prim_alt (lit,rhs) = size_up rhs

    ------------
    size_up_deflt NoDefault		   = sizeZero
    size_up_deflt (BindDefault binder rhs) = size_up rhs

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
conSizeN (I# n) = SizeIs n [] n
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
smallEnoughToInline :: [Bool]			-- Evaluated-ness of value arguments
		    -> Bool			-- Result is scrutinised
		    -> UnfoldingGuidance
		    -> Bool			-- True => unfold it

smallEnoughToInline _ _ UnfoldAlways = True
smallEnoughToInline _ _ UnfoldNever  = False
smallEnoughToInline arg_is_evald_s result_is_scruted
	      (UnfoldIfGoodArgs m_tys_wanted n_vals_wanted discount_vec size scrut_discount)
  = enough_args n_vals_wanted arg_is_evald_s &&
    discounted_size <= opt_UnfoldingUseThreshold
  where

    enough_args n [] | n > 0 = False	-- A function with no value args => don't unfold
    enough_args _ _	     = True	-- Otherwise it's ok to try

{-	OLD: require saturated args
    enough_args 0 evals  = True
    enough_args n []     = False
    enough_args n (e:es) = enough_args (n-1) es
	-- NB: don't take the length of arg_is_evald_s because when
	-- called from couldBeSmallEnoughToInline it is infinite!
-}

    discounted_size = size - args_discount - result_discount

    args_discount = sum (zipWith arg_discount discount_vec arg_is_evald_s)
    result_discount | result_is_scruted = scrut_discount
		    | otherwise		= 0

    arg_discount no_of_constrs is_evald
      | is_evald  = 1 + no_of_constrs * opt_UnfoldingConDiscount
      | otherwise = 1
\end{code}

We use this one to avoid exporting inlinings that we ``couldn't possibly
use'' on the other side.  Can be overridden w/ flaggery.
Just the same as smallEnoughToInline, except that it has no actual arguments.

\begin{code}
--UNUSED?
couldBeSmallEnoughToInline :: UnfoldingGuidance -> Bool
couldBeSmallEnoughToInline guidance = smallEnoughToInline (repeat True) True guidance

certainlySmallEnoughToInline :: UnfoldingGuidance -> Bool
certainlySmallEnoughToInline guidance = smallEnoughToInline (repeat False) False guidance
\end{code}

Predicates
~~~~~~~~~~

\begin{code}
okToInline
	:: FormSummary	-- What the thing to be inlined is like
	-> BinderInfo 	-- How the thing to be inlined occurs
	-> Bool		-- True => it's small enough to inline
	-> Bool		-- True => yes, inline it

-- If there's no danger of duplicating work, we can inline if it occurs once, or is small
okToInline form occ_info small_enough
 | no_dup_danger form
 = small_enough || one_occ
 where
   one_occ = case occ_info of
		OneOcc _ _ _ n_alts _ -> n_alts <= 1
		other		      -> False
   	
   no_dup_danger VarForm    = True
   no_dup_danger ValueForm  = True
   no_dup_danger BottomForm = True
   no_dup_danger other      = False
    
-- A non-WHNF can be inlined if it doesn't occur inside a lambda,
-- and occurs exactly once or 
--     occurs once in each branch of a case and is small
okToInline OtherForm (OneOcc _ dup_danger _ n_alts _) small_enough 
  = not (isDupDanger dup_danger) && (n_alts <= 1 || small_enough)

okToInline form any_occ small_enough = False
\end{code}

