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

	FormSummary(..), mkFormSummary, whnfOrBottom, exprSmallEnoughToDup,

	smallEnoughToInline, couldBeSmallEnoughToInline,

	mkSimpleUnfolding,
	mkMagicUnfolding,
	calcUnfoldingGuidance,
	mentionedInUnfolding
    ) where

IMP_Ubiq()
IMPORT_DELOOPER(IdLoop)	 -- for paranoia checking;
		 -- and also to get mkMagicUnfoldingFun
IMPORT_DELOOPER(PrelLoop)  -- for paranoia checking

import Bag		( emptyBag, unitBag, unionBags, Bag )
import CgCompInfo	( uNFOLDING_CHEAP_OP_COST,
			  uNFOLDING_DEAR_OP_COST,
			  uNFOLDING_NOREP_LIT_COST
			)
import CoreSyn
import CoreUtils	( coreExprType )
import CostCentre	( ccMentionsId )
import Id		( idType, getIdArity,  isBottomingId, 
			  SYN_IE(IdSet), GenId{-instances-} )
import PrimOp		( primOpCanTriggerGC, fragilePrimOp, PrimOp(..) )
import IdInfo		( arityMaybe, bottomIsGuaranteed )
import Literal		( isNoRepLit, isLitLitLit )
import Pretty
import TyCon		( tyConFamilySize )
import Type		( getAppDataTyConExpandingDicts )
import UniqSet		( emptyUniqSet, unitUniqSet, mkUniqSet,
			  addOneToUniqSet, unionUniqSets
			)
import Usage		( SYN_IE(UVar) )
import Util		( isIn, panic, assertPanic )

whatsMentionedInId = panic "whatsMentionedInId (CoreUnfold)"
getMentionedTyConsAndClassesFromType = panic "getMentionedTyConsAndClassesFromType (CoreUnfold)"
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
	Unique			-- of the Id whose magic unfolding this is
	MagicUnfoldingFun


data SimpleUnfolding
  = SimpleUnfolding	FormSummary		-- Tells whether the template is a WHNF or bottom
			UnfoldingGuidance	-- Tells about the *size* of the template.
			TemplateOutExpr		-- The template

type TemplateOutExpr = GenCoreExpr (Id, BinderInfo) Id TyVar UVar
	-- An OutExpr with occurrence info attached.  This is used as
	-- a template in GeneralForms.


mkSimpleUnfolding form guidance    template 
  = SimpleUnfolding form guidance template

mkMagicUnfolding :: Unique -> Unfolding
mkMagicUnfolding tag  = MagicUnfolding tag (mkMagicUnfoldingFun tag)


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
    go n (Var f)		   = case (arityMaybe (getIdArity f)) of
					  Just arity | n < arity -> ValueForm
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
	:: Bool		    	-- True <=> OK if _scc_s appear in expr
	-> Int		    	-- bomb out if size gets bigger than this
	-> CoreExpr    		-- expression to look at
	-> UnfoldingGuidance

calcUnfoldingGuidance scc_s_OK bOMB_OUT_SIZE expr
  = let
    	(use_binders, ty_binders, val_binders, body) = collectBinders expr
    in
    case (sizeExpr scc_s_OK bOMB_OUT_SIZE val_binders body) of

      Nothing		     -> UnfoldNever

      Just (size, cased_args)
	-> let
	       uf = UnfoldIfGoodArgs
			(length ty_binders)
			(length val_binders)
			(map discount_for val_binders)
			size
	       discount_for b | b `is_elem` cased_args = tyConFamilySize tycon
			      | otherwise	       = 0
			      where
				(tycon, _, _) = getAppDataTyConExpandingDicts (idType b)
	   in
	   -- pprTrace "calcUnfold:" (ppAbove (ppr PprDebug uf) (ppr PprDebug expr))
	   uf
  where
    is_elem = isIn "calcUnfoldingGuidance"
\end{code}

\begin{code}
sizeExpr :: Bool	    -- True <=> _scc_s OK
	 -> Int 	    -- Bomb out if it gets bigger than this
	 -> [Id]	    -- Arguments; we're interested in which of these
			    -- get case'd
	 -> CoreExpr
	 -> Maybe (Int,	    -- Size
		   [Id]	    -- Subset of args which are cased
	    )

sizeExpr scc_s_OK bOMB_OUT_SIZE args expr
  = size_up expr
  where
    size_up (Var v)        = sizeOne
    size_up (App fun arg)  = size_up fun `addSize` size_up_arg arg
    size_up (Lit lit)      = if isNoRepLit lit
			       then sizeN uNFOLDING_NOREP_LIT_COST
			       else sizeOne

    size_up (SCC _ (Con _ _)) = Nothing -- **** HACK *****
    size_up (SCC lbl body)
      = if scc_s_OK then size_up body else Nothing

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
		`addSizeN` (tyConFamilySize tycon)
	-- NB: we charge N for an alg. "case", where N is
	-- the number of constructors in the thing being eval'd.
	-- (You'll eventually get a "discount" of N if you
	-- think the "case" is likely to go away.)
      where
	size_alg_alt (con,args,rhs) = size_up rhs
	    -- Don't charge for args, so that wrappers look cheap

	(tycon, _, _) = --trace "CoreUnfold.getAppDataTyConExpandingDicts" $ 
			getAppDataTyConExpandingDicts scrut_ty

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
    sizeVar v = Just (0, [v])

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
smallEnoughToInline :: Int -> Int	-- Constructor discount and size threshold
	      -> [Bool]			-- Evaluated-ness of value arguments
	      -> UnfoldingGuidance
	      -> Bool			-- True => unfold it

smallEnoughToInline con_discount size_threshold _ UnfoldAlways = True
smallEnoughToInline con_discount size_threshold _ UnfoldNever  = False
smallEnoughToInline con_discount size_threshold arg_is_evald_s
	      (UnfoldIfGoodArgs m_tys_wanted n_vals_wanted discount_vec size)
  = n_vals_wanted <= length arg_is_evald_s &&
    discounted_size <= size_threshold

  where
    discounted_size = size - sum (zipWith arg_discount discount_vec arg_is_evald_s)

    arg_discount no_of_constrs is_evald
      | is_evald  = 1 + no_of_constrs * con_discount
      | otherwise = 1
\end{code}

We use this one to avoid exporting inlinings that we ``couldn't possibly
use'' on the other side.  Can be overridden w/ flaggery.
Just the same as smallEnoughToInline, except that it has no actual arguments.

\begin{code}
couldBeSmallEnoughToInline :: Int -> Int	-- Constructor discount and size threshold
	      	       	   -> UnfoldingGuidance
		      	   -> Bool		-- True => unfold it

couldBeSmallEnoughToInline con_discount size_threshold guidance
  = smallEnoughToInline con_discount size_threshold (repeat True) guidance
\end{code}

%************************************************************************
%*									*
\subsection[unfoldings-for-ifaces]{Processing unfoldings for interfaces}
%*									*
%************************************************************************

Of course, the main thing we do to unfoldings-for-interfaces is {\em
print} them.  But, while we're at it, we collect info about
``mentioned'' Ids, etc., etc.---we're going to need this stuff anyway.

%************************************************************************
%*									*
\subsubsection{Monad stuff for the unfolding-generation game}
%*									*
%************************************************************************

\begin{code}
type UnfoldM bndr thing
	=  IdSet	-- in-scope Ids (passed downwards only)
	-> (bndr -> Id)	-- to extract an Id from a binder (down only)

	-> (Bag Id,	-- mentioned global vars (ditto)
	    Bag TyCon,	-- ditto, tycons
	    Bag Class,	-- ditto, classes
	    Bool)	-- True <=> mentions something litlit-ish

	-> (thing, (Bag Id, Bag TyCon, Bag Class, Bool)) -- accumulated...
\end{code}

A little stuff for in-scopery:
\begin{code}
no_in_scopes :: IdSet
add1	     :: IdSet -> Id   -> IdSet
add_some     :: IdSet -> [Id] -> IdSet

no_in_scopes		= emptyUniqSet
in_scopes `add1`     x  = addOneToUniqSet in_scopes x
in_scopes `add_some` xs = in_scopes `unionUniqSets` mkUniqSet xs
\end{code}

The can-see-inside-monad functions are the usual sorts of things.

\begin{code}
thenUf :: UnfoldM bndr a -> (a -> UnfoldM bndr b) -> UnfoldM bndr b
thenUf m k in_scopes get_id mentioneds
  = case m in_scopes get_id mentioneds of { (v, mentioneds1) ->
    k v in_scopes get_id mentioneds1 }

thenUf_ :: UnfoldM bndr a -> UnfoldM bndr b -> UnfoldM bndr b
thenUf_ m k in_scopes get_id mentioneds
  = case m in_scopes get_id mentioneds of { (_, mentioneds1) ->
    k in_scopes get_id mentioneds1 }

mapUf :: (a -> UnfoldM bndr b) -> [a] -> UnfoldM bndr [b]
mapUf f []     = returnUf []
mapUf f (x:xs)
  = f x		`thenUf` \ r ->
    mapUf f xs  `thenUf` \ rs ->
    returnUf (r:rs)

returnUf :: a -> UnfoldM bndr a
returnUf v in_scopes get_id mentioneds = (v, mentioneds)

addInScopesUf :: [Id] -> UnfoldM bndr a -> UnfoldM bndr a
addInScopesUf more_in_scopes m in_scopes get_id mentioneds
  = m (in_scopes `add_some` more_in_scopes) get_id mentioneds

getInScopesUf :: UnfoldM bndr IdSet
getInScopesUf in_scopes get_id mentioneds = (in_scopes, mentioneds)

extractIdsUf :: [bndr] -> UnfoldM bndr [Id]
extractIdsUf binders in_scopes get_id mentioneds
  = (map get_id binders, mentioneds)

consider_Id :: Id -> UnfoldM bndr ()
consider_Id var in_scopes get_id (ids, tcs, clss, has_litlit)
  = let
	(ids2, tcs2, clss2) = whatsMentionedInId in_scopes var
    in
    ((), (ids `unionBags` ids2,
	  tcs `unionBags` tcs2,
	  clss `unionBags`clss2,
	  has_litlit))
\end{code}

\begin{code}
addToMentionedIdsUf 	:: Id -> UnfoldM bndr ()
addToMentionedTyConsUf 	:: Bag TyCon -> UnfoldM bndr ()
addToMentionedClassesUf	:: Bag Class -> UnfoldM bndr ()
litlit_oops		:: UnfoldM bndr ()

addToMentionedIdsUf add_me in_scopes get_id (ids, tcs, clss, has_litlit)
  = ((), (ids `unionBags` unitBag add_me, tcs, clss, has_litlit))

addToMentionedTyConsUf add_mes in_scopes get_id (ids, tcs, clss, has_litlit)
  = ((), (ids, tcs `unionBags` add_mes, clss, has_litlit))

addToMentionedClassesUf add_mes in_scopes get_id (ids, tcs, clss, has_litlit)
  = ((), (ids, tcs, clss `unionBags` add_mes, has_litlit))

litlit_oops in_scopes get_id (ids, tcs, clss, _)
  = ((), (ids, tcs, clss, True))
\end{code}


%************************************************************************
%*									*
\subsubsection{Gathering up info for an interface-unfolding}
%*									*
%************************************************************************

\begin{code}
{-
mentionedInUnfolding
	:: (bndr -> Id)		-- so we can get Ids out of binders
	-> GenCoreExpr bndr Id	-- input expression
	-> (Bag Id, Bag TyCon, Bag Class,
				-- what we found mentioned in the expr
	    Bool		-- True <=> mentions a ``litlit''-ish thing
				-- (the guy on the other side of an interface
				-- may not be able to handle it)
	   )
-}

mentionedInUnfolding get_id expr
  = case (ment_expr expr no_in_scopes get_id (emptyBag, emptyBag, emptyBag, False)) of
      (_, (ids_bag, tcs_bag, clss_bag, has_litlit)) ->
	(ids_bag, tcs_bag, clss_bag, has_litlit)
\end{code}

\begin{code}
--ment_expr :: GenCoreExpr bndr Id -> UnfoldM bndr ()

ment_expr (Var v) = consider_Id  v
ment_expr (Lit l) = consider_lit l

ment_expr expr@(Lam _ _)
  = let
	(uvars, tyvars, args, body) = collectBinders expr
    in
    extractIdsUf args		`thenUf` \ bs_ids ->
    addInScopesUf bs_ids (
	-- this considering is just to extract any mentioned types/classes
	mapUf consider_Id bs_ids   `thenUf_`
	ment_expr body
    )

ment_expr (App fun arg)
  = ment_expr fun	`thenUf_`
    ment_arg  arg

ment_expr (Con c as)
  = consider_Id c	`thenUf_`
    mapUf ment_arg as	`thenUf_`
    returnUf ()

ment_expr (Prim op as)
  = ment_op op		`thenUf_`
    mapUf ment_arg as	`thenUf_`
    returnUf ()
  where
    ment_op (CCallOp str is_asm may_gc arg_tys res_ty)
      = mapUf ment_ty arg_tys	`thenUf_`
    	ment_ty res_ty
    ment_op other_op = returnUf ()

ment_expr (Case scrutinee alts)
  = ment_expr scrutinee	`thenUf_`
    ment_alts alts

ment_expr (Let (NonRec bind rhs) body)
  = ment_expr rhs	`thenUf_`
    extractIdsUf [bind]	`thenUf` \ bi@[bind_id] ->
    addInScopesUf bi	(
    ment_expr body	`thenUf_`
    consider_Id bind_id )

ment_expr (Let (Rec pairs) body)
  = let
	binders = map fst pairs
	rhss	= map snd pairs
    in
    extractIdsUf binders	`thenUf` \ binder_ids ->
    addInScopesUf binder_ids (
	mapUf ment_expr rhss	     `thenUf_`
	mapUf consider_Id binder_ids `thenUf_`
	ment_expr body )

ment_expr (SCC cc expr)
  = (case (ccMentionsId cc) of
      Just id -> consider_Id id
      Nothing -> returnUf ()
    )
    `thenUf_` ment_expr expr

ment_expr (Coerce _ _ _) = panic "ment_expr:Coerce"

-------------
ment_ty ty
  = let
	(tycons, clss) = getMentionedTyConsAndClassesFromType ty
    in
    addToMentionedTyConsUf  tycons  `thenUf_`
    addToMentionedClassesUf clss

-------------

ment_alts alg_alts@(AlgAlts alts deflt)
  = mapUf ment_alt alts   `thenUf_`
    ment_deflt deflt
  where
    ment_alt alt@(con, params, rhs)
      = consider_Id con		`thenUf_`
	extractIdsUf params	`thenUf` \ param_ids ->
	addInScopesUf param_ids (
	  -- "consider" them so we can chk out their types...
	  mapUf consider_Id param_ids `thenUf_`
	  ment_expr rhs )

ment_alts (PrimAlts alts deflt)
  = mapUf ment_alt alts   `thenUf_`
    ment_deflt deflt
  where
    ment_alt alt@(lit, rhs) = ment_expr rhs

----------------
ment_deflt NoDefault
  = returnUf ()

ment_deflt d@(BindDefault b rhs)
  = extractIdsUf [b]		`thenUf` \ bi@[b_id] ->
    addInScopesUf bi		(
	consider_Id b_id `thenUf_`
	ment_expr rhs )

-----------
ment_arg (VarArg   v)  = consider_Id  v
ment_arg (LitArg   l)  = consider_lit l
ment_arg (TyArg    ty) = ment_ty ty
ment_arg (UsageArg _)  = returnUf ()

-----------
consider_lit lit
  | isLitLitLit lit = litlit_oops `thenUf_` returnUf ()
  | otherwise	    = returnUf ()
\end{code}

%************************************************************************
%*									*
\subsubsection{Printing unfoldings in interfaces}
%*									*
%************************************************************************

Printing Core-expression unfoldings is sufficiently delicate that we
give it its own function.
\begin{code}
{- OLD:
pprCoreUnfolding
	:: CoreExpr
	-> Pretty

pprCoreUnfolding expr
  = let
	(_, renamed) = instCoreExpr uniqSupply_u expr
	    -- We rename every unfolding with a "steady" unique supply,
	    -- so that the names won't constantly change.
	    -- One place we *MUST NOT* use a splittable UniqueSupply!
    in
    ppr_uf_Expr emptyUniqSet renamed

ppr_Unfolding = PprUnfolding (panic "CoreUnfold:ppr_Unfolding")
\end{code}

\begin{code}
ppr_uf_Expr in_scopes (Var v) = pprIdInUnfolding in_scopes v
ppr_uf_Expr in_scopes (Lit l) = ppr ppr_Unfolding l

ppr_uf_Expr in_scopes (Con c as)
  = ppBesides [ppPStr SLIT("_!_ "), pprIdInUnfolding no_in_scopes c, ppSP,
	   ppLbrack, ppIntersperse pp'SP{-'-} (map (pprParendUniType ppr_Unfolding) ts), ppRbrack,
	   ppSP, ppLbrack, ppIntersperse pp'SP{-'-} (map (ppr_uf_Atom in_scopes) as), ppRbrack]
ppr_uf_Expr in_scopes (Prim op as)
  = ppBesides [ppPStr SLIT("_#_ "), ppr ppr_Unfolding op, ppSP,
	   ppLbrack, ppIntersperse pp'SP{-'-} (map (pprParendUniType ppr_Unfolding) ts), ppRbrack,
	   ppSP, ppLbrack, ppIntersperse pp'SP{-'-} (map (ppr_uf_Atom in_scopes) as), ppRbrack]

ppr_uf_Expr in_scopes (Lam binder body)
  = ppCat [ppChar '\\', ppr_uf_Binder binder,
	   ppPStr SLIT("->"), ppr_uf_Expr (in_scopes `add1` binder) body]

ppr_uf_Expr in_scopes (CoTyLam tyvar expr)
  = ppCat [ppPStr SLIT("_/\\_"), interppSP ppr_Unfolding (tyvar:tyvars), ppStr "->",
    	   ppr_uf_Expr in_scopes body]
  where
    (tyvars, body) = collect_tyvars expr

    collect_tyvars (CoTyLam tyv e) = ( tyv:tyvs, e_after )
      where (tyvs, e_after) = collect_tyvars e
    collect_tyvars other_e	   = ( [], other_e )

ppr_uf_Expr in_scopes expr@(App fun_expr atom)
  = let
	(fun, args) = collect_args expr []
    in
    ppCat [ppPStr SLIT("_APP_ "), ppr_uf_Expr in_scopes fun, ppLbrack,
	   ppIntersperse pp'SP{-'-} (map (ppr_uf_Atom in_scopes) args), ppRbrack]
  where
    collect_args (App fun arg) args = collect_args fun (arg:args)
    collect_args fun		 args = (fun, args)

ppr_uf_Expr in_scopes (CoTyApp expr ty)
  = ppCat [ppPStr SLIT("_TYAPP_ "), ppr_uf_Expr in_scopes expr,
	ppChar '{', pprParendUniType ppr_Unfolding ty, ppChar '}']

ppr_uf_Expr in_scopes (Case scrutinee alts)
  = ppCat [ppPStr SLIT("case"), ppr_uf_Expr in_scopes scrutinee, ppStr "of {",
	   pp_alts alts, ppChar '}']
  where
    pp_alts (AlgAlts  alts deflt)
      = ppCat [ppPStr SLIT("_ALG_"),  ppCat (map pp_alg  alts), pp_deflt deflt]
    pp_alts (PrimAlts alts deflt)
      = ppCat [ppPStr SLIT("_PRIM_"), ppCat (map pp_prim alts), pp_deflt deflt]

    pp_alg (con, params, rhs)
      = ppBesides [pprIdInUnfolding no_in_scopes con, ppSP,
		   ppIntersperse ppSP (map ppr_uf_Binder params),
		   ppPStr SLIT(" -> "), ppr_uf_Expr (in_scopes `add_some` params) rhs, ppSemi]

    pp_prim (lit, rhs)
      = ppBesides [ppr ppr_Unfolding lit,
		   ppPStr SLIT(" -> "), ppr_uf_Expr in_scopes rhs, ppSemi]

    pp_deflt NoDefault = ppPStr SLIT("_NO_DEFLT_")
    pp_deflt (BindDefault binder rhs)
      = ppBesides [ppr_uf_Binder binder, ppPStr SLIT(" -> "),
		   ppr_uf_Expr (in_scopes `add1` binder) rhs]

ppr_uf_Expr in_scopes (Let (NonRec binder rhs) body)
  = ppBesides [ppStr "let {", ppr_uf_Binder binder, ppPStr SLIT(" = "), ppr_uf_Expr in_scopes rhs,
	ppStr "} in ", ppr_uf_Expr (in_scopes `add1` binder) body]

ppr_uf_Expr in_scopes (Let (Rec pairs) body)
  = ppBesides [ppStr "_LETREC_ {", ppIntersperse sep (map pp_pair pairs),
	ppStr "} in ", ppr_uf_Expr new_in_scopes body]
  where
    sep = ppBeside ppSemi ppSP
    new_in_scopes = in_scopes `add_some` map fst pairs

    pp_pair (b, rhs) = ppCat [ppr_uf_Binder b, ppEquals, ppr_uf_Expr new_in_scopes rhs]

ppr_uf_Expr in_scopes (SCC cc body)
  = ASSERT(not (noCostCentreAttached cc))
    ASSERT(not (currentOrSubsumedCosts cc))
    ppBesides [ppStr "_scc_ { ", ppStr (showCostCentre ppr_Unfolding False{-not as string-} cc), ppStr " } ",  ppr_uf_Expr in_scopes body]

ppr_uf_Expr in_scopes (Coerce _ _ _) = panic "ppr_uf_Expr:Coerce"
\end{code}

\begin{code}
ppr_uf_Binder :: Id -> Pretty
ppr_uf_Binder v
  = ppBesides [ppLparen, pprIdInUnfolding (unitUniqSet v) v, ppPStr SLIT(" :: "),
	       ppr ppr_Unfolding (idType v), ppRparen]

ppr_uf_Atom in_scopes (LitArg l) = ppr ppr_Unfolding l
ppr_uf_Atom in_scopes (VarArg v) = pprIdInUnfolding in_scopes v
END OLD -}
\end{code}
