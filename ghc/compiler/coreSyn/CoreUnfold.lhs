%
% (c) The AQUA Project, Glasgow University, 1994-1996
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

	FormSummary(..), mkFormSummary, whnfOrBottom, exprSmallEnoughToDup, 
	exprIsTrivial,

	noUnfolding, mkMagicUnfolding, mkUnfolding, getUnfoldingTemplate,

	smallEnoughToInline, couldBeSmallEnoughToInline, 
	certainlySmallEnoughToInline, inlineUnconditionally, okToInline,
	okToUnfoldInHiFile,

	calcUnfoldingGuidance
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} MagicUFs	( MagicUnfoldingFun, mkMagicUnfoldingFun )

import CmdLineOpts	( opt_UnfoldingCreationThreshold,
			  opt_UnfoldingUseThreshold,
			  opt_UnfoldingConDiscount,
			  opt_UnfoldingKeenessFactor,
			  opt_UnfoldCasms
			)
import Constants	( uNFOLDING_CHEAP_OP_COST,
			  uNFOLDING_DEAR_OP_COST,
			  uNFOLDING_NOREP_LIT_COST
			)
import BinderInfo	( BinderInfo, isOneSameSCCFunOcc, isDeadOcc,
			  isInlinableOcc, isOneSafeFunOcc
			)
import CoreSyn
import Literal		( Literal )
import CoreUtils	( unTagBinders )
import OccurAnal	( occurAnalyseGlobalExpr )
import CoreUtils	( coreExprType )
import Id		( Id, idType, getIdArity,  isBottomingId, isDataCon,
			  idWantsToBeINLINEd, idMustBeINLINEd, idMustNotBeINLINEd,
			  IdSet )
import PrimOp		( fragilePrimOp, primOpCanTriggerGC, PrimOp(..) )
import IdInfo		( ArityInfo(..), InlinePragInfo(..) )
import Name		( isExported )
import Literal		( isNoRepLit, isLitLitLit )
import TyCon		( tyConFamilySize )
import Type		( splitAlgTyConApp_maybe )
import Unique           ( Unique )
import Util		( isIn, panic, assertPanic )
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

  | OtherLit [Literal]		-- It ain't one of these
  | OtherCon [Id]		-- It ain't one of these

  | CoreUnfolding			-- An unfolding with redundant cached information
		FormSummary		-- Tells whether the template is a WHNF or bottom
		UnfoldingGuidance	-- Tells about the *size* of the template.
		SimplifiableCoreExpr	-- Template

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
     cuf = CoreUnfolding (mkFormSummary expr) ufg occ
					  
     cont = case occ of { Var _ -> cuf; _ -> cuf }
    in
    case ufg of { UnfoldAlways -> cont; _ -> cont }

mkMagicUnfolding :: Unique -> Unfolding
mkMagicUnfolding tag  = MagicUnfolding tag (mkMagicUnfoldingFun tag)

getUnfoldingTemplate :: Unfolding -> CoreExpr
getUnfoldingTemplate (CoreUnfolding _ _ expr)
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
   ppr VarForm    = ptext SLIT("Var")
   ppr ValueForm  = ptext SLIT("Value")
   ppr BottomForm = ptext SLIT("Bot")
   ppr OtherForm  = ptext SLIT("Other")

mkFormSummary ::GenCoreExpr bndr Id flexi -> FormSummary

mkFormSummary expr
  = go (0::Int) expr		-- The "n" is the number of (value) arguments so far
  where
    go n (Lit _)	= ASSERT(n==0) ValueForm
    go n (Con _ _)      = ASSERT(n==0) ValueForm
    go n (Prim _ _)	= OtherForm
    go n (Note _ e)     = go n e

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

whnfOrBottom :: FormSummary -> Bool
whnfOrBottom VarForm    = True
whnfOrBottom ValueForm  = True
whnfOrBottom BottomForm = True
whnfOrBottom OtherForm  = False
\end{code}

@exprIsTrivial@ is true of expressions we are unconditionally happy to duplicate;
simple variables and constants, and type applications.

\begin{code}
exprIsTrivial (Var v) 		= True
exprIsTrivial (Lit lit)         = not (isNoRepLit lit)
exprIsTrivial (App e (TyArg _)) = exprIsTrivial e
exprIsTrivial (Note _ e)        = exprIsTrivial e
exprIsTrivial other		= False
\end{code}

\begin{code}
exprSmallEnoughToDup (Con _ _)      = True	-- Could check # of args
exprSmallEnoughToDup (Prim op _)    = not (fragilePrimOp op) -- Could check # of args
exprSmallEnoughToDup (Lit lit)      = not (isNoRepLit lit)
exprSmallEnoughToDup (Note _ e)     = exprSmallEnoughToDup e
exprSmallEnoughToDup expr
  = case (collectArgs expr) of { (fun, _, vargs) ->
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
	:: Int		    	-- bomb out if size gets bigger than this
	-> CoreExpr    		-- expression to look at
	-> UnfoldingGuidance
calcUnfoldingGuidance bOMB_OUT_SIZE expr
  = case collectBinders expr of { (ty_binders, val_binders, body) ->
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
		     = case (splitAlgTyConApp_maybe (idType b)) of
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

    size_up (Note _ body)  = size_up body		-- Notes cost nothing

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
	    (tyvars, args, body) = collectBinders expr
	in
	size_up body `addSizeN` length args

    size_up (Let (NonRec binder rhs) body)
      = nukeScrutDiscount (size_up rhs)
		`addSize`
	size_up body
		`addSizeN`
	1	-- For the allocation

    size_up (Let (Rec pairs) body)
      = nukeScrutDiscount (foldr addSize sizeZero [size_up rhs | (_,rhs) <- pairs])
		`addSize`
	size_up body
		`addSizeN`
	length pairs	-- For the allocation

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
	  = case (splitAlgTyConApp_maybe scrut_ty) of
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
conSizeN (I# n) = SizeIs n  [] n
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
		    -> Bool			-- Result is scrutinised
		    -> UnfoldingGuidance
		    -> Bool			-- True => unfold it

smallEnoughToInline _ _ _ UnfoldAlways = True
smallEnoughToInline _ _ _ UnfoldNever  = False
smallEnoughToInline id arg_is_evald_s result_is_scruted
	      (UnfoldIfGoodArgs m_tys_wanted n_vals_wanted discount_vec size scrut_discount)
  = if enough_args n_vals_wanted arg_is_evald_s &&
       size - discount <= opt_UnfoldingUseThreshold
    then
       -- pprTrace "small enough" (ppr id <+> int size <+> int discount) 
       True
    else
       False
  where

    enough_args n [] | n > 0 = False	-- A function with no value args => don't unfold
    enough_args _ _	     = True	-- Otherwise it's ok to try

	-- We multiple the raw discounts (args_discount and result_discount)
	-- ty opt_UnfoldingKeenessFactor because the former have to do with
	-- *size* whereas the discounts imply that there's some extra *efficiency*
	-- to be gained (e.g. beta reductions, case reductions) by inlining.
    discount :: Int
    discount = round (
		      opt_UnfoldingKeenessFactor * 
		      fromInt (args_discount + result_discount)
		     )

    args_discount = sum (zipWith arg_discount discount_vec arg_is_evald_s)
    result_discount | result_is_scruted = scrut_discount
		    | otherwise		= 0

    arg_discount no_of_constrs is_evald
      | is_evald  = no_of_constrs * opt_UnfoldingConDiscount
      | otherwise = 0
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

Predicates
~~~~~~~~~~

@inlineUnconditionally@ decides whether a let-bound thing can
*definitely* be inlined at each of its call sites.  If so, then
we can drop the binding right away.  But remember, you have to be 
certain that every use can be inlined.  So, notably, any ArgOccs 
rule this out.  Since ManyOcc doesn't record FunOcc/ArgOcc 

\begin{code}
inlineUnconditionally :: (Id,BinderInfo) -> Bool

inlineUnconditionally (id, occ_info)
  |  idMustNotBeINLINEd id 
  || isExported id
  =  False

  |  isOneSameSCCFunOcc occ_info
  && idWantsToBeINLINEd id = True

  |  isOneSafeFunOcc occ_info
  =  True

  |  otherwise
  = False
\end{code}

okToInline is used at call sites, so it is a bit more generous

\begin{code}
okToInline :: Id		-- The Id
	   -> Bool		-- The thing is WHNF or bottom; 
	   -> Bool		-- It's small enough to duplicate the code
	   -> BinderInfo
	   -> Bool		-- True <=> inline it

okToInline id _ _ _		-- Check the Id first
  | idWantsToBeINLINEd id = True
  | idMustNotBeINLINEd id = False

okToInline id whnf small binder_info 
#ifdef DEBUG
  | isDeadOcc binder_info
  = pprTrace "okToInline: dead" (ppr id) False
  | otherwise
#endif
  = isInlinableOcc whnf small binder_info
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
    go (Var _)   = True
    go (Lit lit) = not (isLitLitLit lit)
    go (Note _ body)  = go body
    go (App fun arg)  = go fun
    go (Con con args) = True
    go (Prim op args) = okToUnfoldPrimOp op
    go (Lam _ body) = go body
    go (Let (NonRec binder rhs) body) = go rhs && go body
    go (Let (Rec pairs) body) = and (map go (body:rhses))
      where
        rhses = [ rhs | (_, rhs) <- pairs ]
    go (Case scrut alts) = and (map go (scrut:rhses))
      where
        rhses = getAltRhs alts

        getAltRhs (PrimAlts alts deflt) =
	    let ls = map snd alts  in
	    case deflt of
	      NoDefault -> ls
	      BindDefault _ rhs -> rhs:ls
        getAltRhs (AlgAlts alts deflt) =
	    let ls = map (\ (_,_,r) -> r) alts  in
	    case deflt of
	      NoDefault -> ls
	      BindDefault _ rhs -> rhs:ls

    -- ok to unfold a PrimOp as long as it's not a _casm_
    okToUnfoldPrimOp (CCallOp _ is_casm _ _ _ _) = not is_casm
    okToUnfoldPrimOp _                           = True
     
\end{code}
