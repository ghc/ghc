%
% (c) The AQUA Project, Glasgow University, 1994-1995
%
\section[CoreUnfold]{Core-syntax functions to do with unfoldings}

\begin{code}
#include "HsVersions.h"

module CoreUnfold (
	calcUnfoldingGuidance,

	pprCoreUnfolding,
	mentionedInUnfolding

    ) where

import AbsPrel		( primOpCanTriggerGC, PrimOp(..), PrimKind
			  IF_ATTACK_PRAGMAS(COMMA tagOf_PrimOp)
			  IF_ATTACK_PRAGMAS(COMMA pprPrimOp)
			)
import AbsUniType	( getMentionedTyConsAndClassesFromUniType,
			  getUniDataTyCon, getTyConFamilySize,
			  pprParendUniType, Class, TyCon, TyVar,
			  UniType, TauType(..)
			  IF_ATTACK_PRAGMAS(COMMA cmpTyCon COMMA cmpClass)
			  IF_ATTACK_PRAGMAS(COMMA cmpTyVar)
			  IF_ATTACK_PRAGMAS(COMMA cmpUniType)
			)
import Bag
import BasicLit		( isNoRepLit, isLitLitLit, BasicLit(..){-.. is for pragmas-} )
import CgCompInfo	( uNFOLDING_CHEAP_OP_COST,
			  uNFOLDING_DEAR_OP_COST,
			  uNFOLDING_NOREP_LIT_COST
			)
import CoreFuns		( digForLambdas, typeOfCoreExpr )
import CoreSyn		-- mostly re-exporting this stuff
import CostCentre	( showCostCentre, noCostCentreAttached,
			  currentOrSubsumedCosts, ccMentionsId, CostCentre
			)
import Id		( pprIdInUnfolding, getIdUniType,
			  whatsMentionedInId, Id, DataCon(..)
			)
import IdInfo
import Maybes
import Outputable
import PlainCore	( instCoreExpr )
import Pretty
import SimplEnv		( UnfoldingGuidance(..) )
import UniqSet
import Unique		( uniqSupply_u, UniqueSupply )
import Util
\end{code}

%************************************************************************
%*									*
\subsection[calcUnfoldingGuidance]{Calculate ``unfolding guidance'' for an expression}
%*									*
%************************************************************************

\begin{code}
calcUnfoldingGuidance
	:: Bool		    -- True <=> OK if _scc_s appear in expr
	-> Int		    -- bomb out if size gets bigger than this
	-> PlainCoreExpr    -- expression to look at
	-> UnfoldingGuidance

calcUnfoldingGuidance scc_s_OK bOMB_OUT_SIZE expr
  = let
    	(ty_binders, val_binders, body) = digForLambdas expr
    in
    case (sizeExpr scc_s_OK bOMB_OUT_SIZE val_binders body) of

      Nothing		     -> UnfoldNever

      Just (size, cased_args)
        -> let
	       uf = UnfoldIfGoodArgs
			(length ty_binders)
			(length val_binders)
			[ b `is_elem` cased_args | b <- val_binders ]
			size
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
	 -> PlainCoreExpr   
	 -> Maybe (Int,	    -- Size
		   [Id]	    -- Subset of args which are cased
	    )

sizeExpr scc_s_OK bOMB_OUT_SIZE args expr
  = size_up expr
  where
    size_up (CoVar v)        = sizeOne
    size_up (CoApp fun arg)  = size_up fun `addSizeN` 1
    size_up (CoTyApp fun ty) = size_up fun	-- They're free
    size_up (CoLit lit)      = if isNoRepLit lit
			       then sizeN uNFOLDING_NOREP_LIT_COST
			       else sizeOne

    size_up (CoSCC _ (CoCon _ _ _)) = Nothing -- **** HACK *****
    size_up (CoSCC lbl body)
      = if scc_s_OK then size_up body else Nothing

    size_up (CoCon con tys args) = sizeN (length args + 1)
    size_up (CoPrim op tys args) = sizeN op_cost -- NB: no charge for PrimOp args
      where
	op_cost = if primOpCanTriggerGC op
		  then uNFOLDING_DEAR_OP_COST
			-- these *tend* to be more expensive;
			-- number chosen to avoid unfolding (HACK)
		  else uNFOLDING_CHEAP_OP_COST

    size_up (CoLam binders body) = size_up body `addSizeN` length binders
    size_up (CoTyLam tyvar body) = size_up body

    size_up (CoLet (CoNonRec binder rhs) body) 
      = size_up rhs
		`addSize`
	size_up body
		`addSizeN`
	1

    size_up (CoLet (CoRec pairs) body) 
      = foldr addSize sizeZero [size_up rhs | (_,rhs) <- pairs]
		`addSize`
	size_up body
		`addSizeN`
	length pairs
	
    size_up (CoCase scrut alts)
      = size_up_scrut scrut 
		`addSize`
	size_up_alts (typeOfCoreExpr scrut) alts
	    -- We charge for the "case" itself in "size_up_alts"

    ------------
    size_up_alts scrut_ty (CoAlgAlts alts deflt)
      = foldr (addSize . size_alg_alt) (size_up_deflt deflt) alts
		`addSizeN`
    	(case (getTyConFamilySize tycon) of { Just n -> n })
	-- NB: we charge N for an alg. "case", where N is
	-- the number of constructors in the thing being eval'd.
	-- (You'll eventually get a "discount" of N if you
	-- think the "case" is likely to go away.)
      where
	size_alg_alt (con,args,rhs) = size_up rhs
	    -- Don't charge for args, so that wrappers look cheap

	(tycon, _, _) = getUniDataTyCon scrut_ty


    size_up_alts _ (CoPrimAlts alts deflt)
      = foldr (addSize . size_prim_alt) (size_up_deflt deflt) alts  
	    -- *no charge* for a primitive "case"!
      where
	size_prim_alt (lit,rhs) = size_up rhs

    ------------
    size_up_deflt CoNoDefault = sizeZero
    size_up_deflt (CoBindDefault binder rhs) = size_up rhs

    ------------
	-- Scrutinees.  There are two things going on here.
	-- First, we want to record if we're case'ing an argument
	-- Second, we want to charge nothing for the srutinee if it's just
	-- a variable.  That way wrapper-like things look cheap.
    size_up_scrut (CoVar v) | v `is_elem` args = Just (0, [v])
			    | otherwise	       = Just (0, [])
    size_up_scrut other			       = size_up other

    is_elem = isIn "size_up_scrut"

    ------------
    sizeZero  = Just (0, [])
    sizeOne   = Just (1, [])
    sizeN n   = Just (n, [])
    sizeVar v = Just (0, [v])

    addSizeN Nothing _ = Nothing
    addSizeN (Just (n, xs)) m
      | tot < bOMB_OUT_SIZE = Just (tot, xs)
      | otherwise = -- pprTrace "bomb1:" (ppCat [ppInt tot, ppInt bOMB_OUT_SIZE, ppr PprDebug expr])
		    Nothing
      where
	tot = n+m

    addSize Nothing _ = Nothing
    addSize _ Nothing = Nothing
    addSize (Just (n, xs)) (Just (m, ys))
      | tot < bOMB_OUT_SIZE = Just (tot, xys)
      | otherwise  = -- pprTrace "bomb2:" (ppCat [ppInt tot, ppInt bOMB_OUT_SIZE, ppr PprDebug expr])
		     Nothing
      where
	tot = n+m
	xys = xs ++ ys
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
in_scopes `add1`     x  = in_scopes `unionUniqSets` singletonUniqSet x
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
mentionedInUnfolding
	:: (bndr -> Id)		-- so we can get Ids out of binders
	-> CoreExpr bndr Id	-- input expression
	-> ([Id], [TyCon], [Class],
				-- what we found mentioned in the expr
	    Bool		-- True <=> mentions a ``litlit''-ish thing
				-- (the guy on the other side of an interface
				-- may not be able to handle it)
	   )

mentionedInUnfolding get_id expr
  = case (ment_expr expr no_in_scopes get_id (emptyBag, emptyBag, emptyBag, False)) of
      (_, (ids_bag, tcs_bag, clss_bag, has_litlit)) ->
	(bagToList ids_bag, bagToList tcs_bag, bagToList clss_bag, has_litlit)
\end{code}

\begin{code}
ment_expr :: CoreExpr bndr Id -> UnfoldM bndr ()

ment_expr (CoVar v) = consider_Id  v
ment_expr (CoLit l) = consider_lit l

ment_expr (CoLam bs body)
  = extractIdsUf bs		`thenUf` \ bs_ids ->
    addInScopesUf bs_ids (
	-- this considering is just to extract any mentioned types/classes
	mapUf consider_Id bs_ids   `thenUf_`
	ment_expr body
    )

ment_expr (CoTyLam _ body) = ment_expr body

ment_expr (CoApp fun arg)
  = ment_expr fun	`thenUf_`
    ment_atom arg

ment_expr (CoTyApp expr ty)
  = ment_ty   ty	`thenUf_`
    ment_expr expr

ment_expr (CoCon c ts as)
  = consider_Id c	`thenUf_`
    mapUf ment_ty ts	`thenUf_`
    mapUf ment_atom as	`thenUf_`
    returnUf ()

ment_expr (CoPrim op ts as)
  = ment_op op		`thenUf_`
    mapUf ment_ty   ts	`thenUf_`
    mapUf ment_atom as	`thenUf_`
    returnUf ()
  where
    ment_op (CCallOp str is_asm may_gc arg_tys res_ty)
      = mapUf ment_ty arg_tys	`thenUf_`
    	ment_ty res_ty
    ment_op other_op = returnUf ()

ment_expr (CoCase scrutinee alts)
  = ment_expr scrutinee	`thenUf_`
    ment_alts alts

ment_expr (CoLet (CoNonRec bind rhs) body)
  = ment_expr rhs	`thenUf_`
    extractIdsUf [bind]	`thenUf` \ bi@[bind_id] ->
    addInScopesUf bi	(
    ment_expr body	`thenUf_`
    consider_Id bind_id )

ment_expr (CoLet (CoRec pairs) body)
  = let
	binders = map fst pairs
	rhss	= map snd pairs
    in
    extractIdsUf binders	`thenUf` \ binder_ids ->
    addInScopesUf binder_ids (
        mapUf ment_expr rhss	     `thenUf_`
	mapUf consider_Id binder_ids `thenUf_`
        ment_expr body )

ment_expr (CoSCC cc expr)
  = (case (ccMentionsId cc) of
      Just id -> consider_Id id
      Nothing -> returnUf ()
    )
    `thenUf_` ment_expr expr

-------------
ment_ty ty
  = let
	(tycons, clss) = getMentionedTyConsAndClassesFromUniType ty
    in
    addToMentionedTyConsUf  tycons  `thenUf_`
    addToMentionedClassesUf clss

-------------

ment_alts alg_alts@(CoAlgAlts alts deflt)
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

ment_alts (CoPrimAlts alts deflt)
  = mapUf ment_alt alts   `thenUf_`
    ment_deflt deflt
  where
    ment_alt alt@(lit, rhs) = ment_expr rhs

----------------
ment_deflt CoNoDefault
  = returnUf ()

ment_deflt d@(CoBindDefault b rhs)
  = extractIdsUf [b]		`thenUf` \ bi@[b_id] ->
    addInScopesUf bi		(
	consider_Id b_id `thenUf_`
	ment_expr rhs )

-----------
ment_atom (CoVarAtom v) = consider_Id  v
ment_atom (CoLitAtom l) = consider_lit l

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
pprCoreUnfolding
	:: PlainCoreExpr
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
ppr_uf_Expr in_scopes (CoVar v) = pprIdInUnfolding in_scopes v
ppr_uf_Expr in_scopes (CoLit l) = ppr ppr_Unfolding l

ppr_uf_Expr in_scopes (CoCon c ts as)
  = ppBesides [ppPStr SLIT("_!_ "), pprIdInUnfolding no_in_scopes c, ppSP,
	   ppLbrack, ppIntersperse pp'SP{-'-} (map (pprParendUniType ppr_Unfolding) ts), ppRbrack,
	   ppSP, ppLbrack, ppIntersperse pp'SP{-'-} (map (ppr_uf_Atom in_scopes) as), ppRbrack]
ppr_uf_Expr in_scopes (CoPrim op ts as)
  = ppBesides [ppPStr SLIT("_#_ "), ppr ppr_Unfolding op, ppSP,
	   ppLbrack, ppIntersperse pp'SP{-'-} (map (pprParendUniType ppr_Unfolding) ts), ppRbrack,
	   ppSP, ppLbrack, ppIntersperse pp'SP{-'-} (map (ppr_uf_Atom in_scopes) as), ppRbrack]

ppr_uf_Expr in_scopes (CoLam binders body)
  = ppCat [ppChar '\\', ppIntersperse ppSP (map ppr_uf_Binder binders),
	   ppPStr SLIT("->"), ppr_uf_Expr (in_scopes `add_some` binders) body]

ppr_uf_Expr in_scopes (CoTyLam tyvar expr)
  = ppCat [ppPStr SLIT("_/\\_"), interppSP ppr_Unfolding (tyvar:tyvars), ppStr "->",
    	   ppr_uf_Expr in_scopes body]
  where
    (tyvars, body) = collect_tyvars expr

    collect_tyvars (CoTyLam tyv e) = ( tyv:tyvs, e_after )
      where (tyvs, e_after) = collect_tyvars e
    collect_tyvars other_e	   = ( [], other_e )

ppr_uf_Expr in_scopes expr@(CoApp fun_expr atom)
  = let
	(fun, args) = collect_args expr []
    in
    ppCat [ppPStr SLIT("_APP_ "), ppr_uf_Expr in_scopes fun, ppLbrack,
	   ppIntersperse pp'SP{-'-} (map (ppr_uf_Atom in_scopes) args), ppRbrack]
  where
    collect_args (CoApp fun arg) args = collect_args fun (arg:args)
    collect_args fun		 args = (fun, args)

ppr_uf_Expr in_scopes (CoTyApp expr ty)
  = ppCat [ppPStr SLIT("_TYAPP_ "), ppr_uf_Expr in_scopes expr,
	ppChar '{', pprParendUniType ppr_Unfolding ty, ppChar '}']

ppr_uf_Expr in_scopes (CoCase scrutinee alts)
  = ppCat [ppPStr SLIT("case"), ppr_uf_Expr in_scopes scrutinee, ppStr "of {",
	   pp_alts alts, ppChar '}']
  where
    pp_alts (CoAlgAlts  alts deflt)
      = ppCat [ppPStr SLIT("_ALG_"),  ppCat (map pp_alg  alts), pp_deflt deflt]
    pp_alts (CoPrimAlts alts deflt)
      = ppCat [ppPStr SLIT("_PRIM_"), ppCat (map pp_prim alts), pp_deflt deflt]

    pp_alg (con, params, rhs)
      = ppBesides [pprIdInUnfolding no_in_scopes con, ppSP,
		   ppIntersperse ppSP (map ppr_uf_Binder params),
		   ppPStr SLIT(" -> "), ppr_uf_Expr (in_scopes `add_some` params) rhs, ppSemi]

    pp_prim (lit, rhs)
      = ppBesides [ppr ppr_Unfolding lit,
		   ppPStr SLIT(" -> "), ppr_uf_Expr in_scopes rhs, ppSemi]

    pp_deflt CoNoDefault = ppPStr SLIT("_NO_DEFLT_")
    pp_deflt (CoBindDefault binder rhs)
      = ppBesides [ppr_uf_Binder binder, ppPStr SLIT(" -> "),
		   ppr_uf_Expr (in_scopes `add1` binder) rhs]

ppr_uf_Expr in_scopes (CoLet (CoNonRec binder rhs) body)
  = ppBesides [ppStr "let {", ppr_uf_Binder binder, ppPStr SLIT(" = "), ppr_uf_Expr in_scopes rhs,
	ppStr "} in ", ppr_uf_Expr (in_scopes `add1` binder) body]

ppr_uf_Expr in_scopes (CoLet (CoRec pairs) body)
  = ppBesides [ppStr "_LETREC_ {", ppIntersperse sep (map pp_pair pairs),
	ppStr "} in ", ppr_uf_Expr new_in_scopes body]
  where
    sep = ppBeside ppSemi ppSP
    new_in_scopes = in_scopes `add_some` map fst pairs

    pp_pair (b, rhs) = ppCat [ppr_uf_Binder b, ppEquals, ppr_uf_Expr new_in_scopes rhs]

ppr_uf_Expr in_scopes (CoSCC cc body)
  = ASSERT(not (noCostCentreAttached cc))
    ASSERT(not (currentOrSubsumedCosts cc))
    ppBesides [ppStr "_scc_ { ", ppStr (showCostCentre ppr_Unfolding False{-not as string-} cc), ppStr " } ",  ppr_uf_Expr in_scopes body]
\end{code}

\begin{code}
ppr_uf_Binder :: Id -> Pretty
ppr_uf_Binder v
  = ppBesides [ppLparen, pprIdInUnfolding (singletonUniqSet v) v, ppPStr SLIT(" :: "),
	       ppr ppr_Unfolding (getIdUniType v), ppRparen]

ppr_uf_Atom in_scopes (CoLitAtom l) = ppr ppr_Unfolding l
ppr_uf_Atom in_scopes (CoVarAtom v) = pprIdInUnfolding in_scopes v
\end{code}
