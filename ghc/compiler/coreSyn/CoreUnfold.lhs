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

	FormSummary(..), mkFormSummary, whnfOrBottom, exprSmallEnoughToDup,

	noUnfolding, mkMagicUnfolding, mkUnfolding, getUnfoldingTemplate,

	smallEnoughToInline, couldBeSmallEnoughToInline, certainlySmallEnoughToInline,
	okToInline,

	calcUnfoldingGuidance
    ) where

IMP_Ubiq()
IMPORT_DELOOPER(IdLoop)	 -- for paranoia checking;
		 -- and also to get mkMagicUnfoldingFun
IMPORT_DELOOPER(PrelLoop)  -- for paranoia checking

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
import CoreSyn
import CoreUtils	( unTagBinders )
import HsCore		( UfExpr )
import RdrHsSyn		( RdrName )
import OccurAnal	( occurAnalyseGlobalExpr )
import CoreUtils	( coreExprType )
import CostCentre	( ccMentionsId )
import Id		( idType, getIdArity,  isBottomingId, 
			  SYN_IE(IdSet), GenId{-instances-} )
import PrimOp		( primOpCanTriggerGC, fragilePrimOp, PrimOp(..) )
import IdInfo		( ArityInfo(..), bottomIsGuaranteed )
import Literal		( isNoRepLit, isLitLitLit )
import Pretty
import TyCon		( tyConFamilySize )
import Type		( maybeAppDataTyConExpandingDicts )
import UniqSet		( emptyUniqSet, unitUniqSet, mkUniqSet,
			  addOneToUniqSet, unionUniqSets
			)
import Usage		( SYN_IE(UVar) )
import Util		( isIn, panic, assertPanic )

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

mkUnfolding inline_me expr
  = CoreUnfolding (SimpleUnfolding
			(mkFormSummary expr)
			(calcUnfoldingGuidance inline_me opt_UnfoldingCreationThreshold expr)
			(occurAnalyseGlobalExpr expr))

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
\end{code}

\begin{code}
instance Outputable UnfoldingGuidance where
    ppr sty UnfoldAlways    	= ppStr "_ALWAYS_"
--    ppr sty EssentialUnfolding	= ppStr "_ESSENTIAL_" -- shouldn't appear in an iface
    ppr sty (UnfoldIfGoodArgs t v cs size)
      = ppCat [ppStr "_IF_ARGS_", ppInt t, ppInt v,
	       if null cs	-- always print *something*
	       	then ppChar 'X'
		else ppBesides (map (ppStr . show) cs),
	       ppInt size ]
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
   ppr sty VarForm    = ppStr "Var"
   ppr sty ValueForm  = ppStr "Value"
   ppr sty BottomForm = ppStr "Bot"
   ppr sty OtherForm  = ppStr "Other"

mkFormSummary ::GenCoreExpr bndr Id tyvar uvar -> FormSummary

mkFormSummary expr
  = go (0::Int) expr		-- The "n" is the number of (value) arguments so far
  where
    go n (Lit _)	= ASSERT(n==0) ValueForm
    go n (Con _ _)      = ASSERT(n==0) ValueForm
    go n (Prim _ _)	= OtherForm
    go n (SCC _ e)      = go n e
    go n (Coerce _ _ e) = go n e
    go n (Let _ e)      = OtherForm
    go n (Case _ _)     = OtherForm

    go 0 (Lam (ValBinder x) e) = ValueForm	-- NB: \x.bottom /= bottom!
    go n (Lam (ValBinder x) e) = go (n-1) e	-- Applied lambda
    go n (Lam other_binder e)  = go n e

    go n (App fun arg) | isValArg arg = go (n+1) fun
    go n (App fun other_arg)          = go n fun

    go n (Var f) | isBottomingId f = BottomForm
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


\begin{code}
exprSmallEnoughToDup (Con _ _)   = True	-- Could check # of args
exprSmallEnoughToDup (Prim op _) = not (fragilePrimOp op) -- Could check # of args
exprSmallEnoughToDup (Lit lit)   = not (isNoRepLit lit)
exprSmallEnoughToDup expr
  = case (collectArgs expr) of { (fun, _, _, vargs) ->
    case fun of
      Var v | length vargs == 0 -> True
      _				-> False
    }

{- LATER:
WAS: MORE CLEVER:
exprSmallEnoughToDup expr  -- for now, just: <var> applied to <args>
  = case (collectArgs expr) of { (fun, _, _, vargs) ->
    case fun of
      Var v -> v /= buildId
		 && v /= augmentId
		 && length vargs <= 6 -- or 10 or 1 or 4 or anything smallish.
      _       -> False
    }
-}
\end{code}
Question (ADR): What is the above used for?  Is a _ccall_ really small
enough?

%************************************************************************
%*									*
\subsection[calcUnfoldingGuidance]{Calculate ``unfolding guidance'' for an expression}
%*									*
%************************************************************************

\begin{code}
calcUnfoldingGuidance
	:: Bool		    	-- True <=> there's an INLINE pragma on this thing
	-> Int		    	-- bomb out if size gets bigger than this
	-> CoreExpr    		-- expression to look at
	-> UnfoldingGuidance

calcUnfoldingGuidance True bOMB_OUT_SIZE expr = UnfoldAlways	-- Always inline if the INLINE pragma says so

calcUnfoldingGuidance False bOMB_OUT_SIZE expr
  = let
    	(use_binders, ty_binders, val_binders, body) = collectBinders expr
    in
    case (sizeExpr bOMB_OUT_SIZE val_binders body) of

      Nothing		     -> UnfoldNever

      Just (size, cased_args)
	-> let
	       uf = UnfoldIfGoodArgs
			(length ty_binders)
			(length val_binders)
			(map discount_for val_binders)
			size

	       discount_for b
	         | is_data && b `is_elem` cased_args = tyConFamilySize tycon
		 | otherwise = 0
		 where
		   (is_data, tycon)
		     = --trace "CoreUnfold.getAppDataTyConExpandingDicts:1" $ 
			case (maybeAppDataTyConExpandingDicts (idType b)) of
			  Nothing       -> (False, panic "discount")
			  Just (tc,_,_) -> (True,  tc)
	   in
	   -- pprTrace "calcUnfold:" (ppAbove (ppr PprDebug uf) (ppr PprDebug expr))
	   uf
  where
    is_elem = isIn "calcUnfoldingGuidance"
\end{code}

\begin{code}
sizeExpr :: Int 	    -- Bomb out if it gets bigger than this
	 -> [Id]	    -- Arguments; we're interested in which of these
			    -- get case'd
	 -> CoreExpr
	 -> Maybe (Int,	    -- Size
		   [Id]	    -- Subset of args which are cased
	    )

sizeExpr bOMB_OUT_SIZE args expr
  = size_up expr
  where
    size_up (Var v)        = sizeOne
    size_up (App fun arg)  = size_up fun `addSize` size_up_arg arg
    size_up (Lit lit)      = if isNoRepLit lit
			     then sizeN uNFOLDING_NOREP_LIT_COST
			     else sizeOne

-- I don't understand this hack so I'm removing it!  SLPJ Nov 96
--    size_up (SCC _ (Con _ _)) = Nothing -- **** HACK *****

    size_up (SCC lbl body)    = size_up body		-- SCCs cost nothing
    size_up (Coerce _ _ body) = size_up body		-- Coercions cost nothing

    size_up (Con con args) = -- 1 + # of val args
			     sizeN (1 + numValArgs args)
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
      = size_up rhs
		`addSize`
	size_up body
		`addSizeN`
	1

    size_up (Let (Rec pairs) body)
      = foldr addSize sizeZero [size_up rhs | (_,rhs) <- pairs]
		`addSize`
	size_up body
		`addSizeN`
	length pairs

    size_up (Case scrut alts)
      = size_up_scrut scrut
		`addSize`
	size_up_alts (coreExprType scrut) alts
	    -- We charge for the "case" itself in "size_up_alts"

    ------------
    size_up_arg arg = if isValArg arg then sizeOne else sizeZero{-it's free-}

    ------------
    size_up_alts scrut_ty (AlgAlts alts deflt)
      = foldr (addSize . size_alg_alt) (size_up_deflt deflt) alts
		`addSizeN` (if is_data then tyConFamilySize tycon else 1{-??-})
	-- NB: we charge N for an alg. "case", where N is
	-- the number of constructors in the thing being eval'd.
	-- (You'll eventually get a "discount" of N if you
	-- think the "case" is likely to go away.)
      where
	size_alg_alt (con,args,rhs) = size_up rhs
	    -- Don't charge for args, so that wrappers look cheap

	(is_data,tycon)
	  = --trace "CoreUnfold.getAppDataTyConExpandingDicts:2" $ 
	    case (maybeAppDataTyConExpandingDicts scrut_ty) of
	      Nothing       -> (False, panic "size_up_alts")
	      Just (tc,_,_) -> (True, tc)

    size_up_alts _ (PrimAlts alts deflt)
      = foldr (addSize . size_prim_alt) (size_up_deflt deflt) alts
	    -- *no charge* for a primitive "case"!
      where
	size_prim_alt (lit,rhs) = size_up rhs

    ------------
    size_up_deflt NoDefault = sizeZero
    size_up_deflt (BindDefault binder rhs) = size_up rhs

    ------------
	-- Scrutinees.  There are two things going on here.
	-- First, we want to record if we're case'ing an argument
	-- Second, we want to charge nothing for the srutinee if it's just
	-- a variable.  That way wrapper-like things look cheap.
    size_up_scrut (Var v) | v `is_elem` args = Just (0, [v])
			    | otherwise	       = Just (0, [])
    size_up_scrut other			       = size_up other

    is_elem :: Id -> [Id] -> Bool
    is_elem = isIn "size_up_scrut"

    ------------
    sizeZero  = Just (0, [])
    sizeOne   = Just (1, [])
    sizeN n   = Just (n, [])

    addSizeN Nothing _ = Nothing
    addSizeN (Just (n, xs)) m
      | tot < bOMB_OUT_SIZE = Just (tot, xs)
      | otherwise = Nothing
      where
	tot = n+m

    addSize Nothing _ = Nothing
    addSize _ Nothing = Nothing
    addSize (Just (n, xs)) (Just (m, ys))
      | tot < bOMB_OUT_SIZE = Just (tot, xys)
      | otherwise  = Nothing
      where
	tot = n+m
	xys = xs ++ ys
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
position.''

Assuming we have enough type- and value arguments (if not, we give up
immediately), then we see if the ``discounted size'' is below some
(semi-arbitrary) threshold.  It works like this: for every argument
position where we're looking for a constructor AND WE HAVE ONE in our
hands, we get a (again, semi-arbitrary) discount [proportion to the
number of constructors in the type being scrutinized].

\begin{code}
smallEnoughToInline :: [Bool]			-- Evaluated-ness of value arguments
		    -> UnfoldingGuidance
		    -> Bool			-- True => unfold it

smallEnoughToInline _ UnfoldAlways = True
smallEnoughToInline _ UnfoldNever  = False
smallEnoughToInline arg_is_evald_s
	      (UnfoldIfGoodArgs m_tys_wanted n_vals_wanted discount_vec size)
  = enough_args n_vals_wanted arg_is_evald_s &&
    discounted_size <= opt_UnfoldingUseThreshold
  where
    enough_args 0 evals  = True
    enough_args n []     = False
    enough_args n (e:es) = enough_args (n-1) es
	-- NB: don't take the length of arg_is_evald_s because when
	-- called from couldBeSmallEnoughToInline it is infinite!

    discounted_size = size - sum (zipWith arg_discount discount_vec arg_is_evald_s)

    arg_discount no_of_constrs is_evald
      | is_evald  = 1 + no_of_constrs * opt_UnfoldingConDiscount
      | otherwise = 1
\end{code}

We use this one to avoid exporting inlinings that we ``couldn't possibly
use'' on the other side.  Can be overridden w/ flaggery.
Just the same as smallEnoughToInline, except that it has no actual arguments.

\begin{code}
couldBeSmallEnoughToInline :: UnfoldingGuidance -> Bool
couldBeSmallEnoughToInline guidance = smallEnoughToInline (repeat True) guidance

certainlySmallEnoughToInline :: UnfoldingGuidance -> Bool
certainlySmallEnoughToInline guidance = smallEnoughToInline (repeat False) guidance
\end{code}

Predicates
~~~~~~~~~~

\begin{code}
okToInline
	:: FormSummary	-- What the thing to be inlined is like
	-> BinderInfo 	-- How the thing to be inlined occurs
	-> Bool		-- True => it's small enough to inline
	-> Bool		-- True => yes, inline it

-- Always inline bottoms
okToInline BottomForm occ_info small_enough
  = True	-- Unless one of the type args is unboxed??
		-- This used to be checked for, but I can't
		-- see why so I've left it out.

-- A WHNF can be inlined if it occurs once, or is small
okToInline form occ_info small_enough
 | is_whnf_form form
 = small_enough || one_occ
 where
   one_occ = case occ_info of
		OneOcc _ _ _ n_alts _ -> n_alts <= 1
		other		      -> False
   	
   is_whnf_form VarForm   = True
   is_whnf_form ValueForm = True
   is_whnf_form other     = False
    
-- A non-WHNF can be inlined if it doesn't occur inside a lambda,
-- and occurs exactly once or 
--     occurs once in each branch of a case and is small
okToInline OtherForm (OneOcc _ dup_danger _ n_alts _) small_enough 
  = not (isDupDanger dup_danger) && (n_alts <= 1 || small_enough)

okToInline form any_occ small_enough = False
\end{code}

