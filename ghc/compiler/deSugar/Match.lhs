%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[Main_match]{The @match@ function}

\begin{code}
#include "HsVersions.h"

module Match ( match, matchWrapper, matchSimply ) where

import Ubiq
import DsLoop		-- here for paranoia-checking reasons
			-- and to break dsExpr/dsBinds-ish loop

import HsSyn		hiding ( collectBinders{-also from CoreSyn-} )
import TcHsSyn		( TypecheckedPat(..), TypecheckedMatch(..),
			  TypecheckedHsBinds(..), TypecheckedHsExpr(..)	)
import DsHsSyn		( outPatType, collectTypedPatBinders )
import CoreSyn

import CoreUtils	( coreExprType )
import DsMonad
import DsGRHSs		( dsGRHSs )
import DsUtils
import MatchCon		( matchConFamily )
import MatchLit		( matchLiterals )

import FieldLabel	( allFieldLabelTags, fieldLabelTag )
import Id		( idType, mkTupleCon, dataConSig,
			  dataConArgTys, recordSelectorFieldLabel,
			  GenId{-instance-}
			)
import PprStyle		( PprStyle(..) )
import PprType		( GenType{-instance-}, GenTyVar{-ditto-} )
import PrelVals		( pAT_ERROR_ID )
import Type		( isPrimType, eqTy, getAppDataTyConExpandingDicts,
			  instantiateTauTy
			)
import TyVar		( GenTyVar{-instance Eq-} )
import TysPrim		( intPrimTy, charPrimTy, floatPrimTy, doublePrimTy,
			  addrPrimTy, wordPrimTy
			)
import TysWiredIn	( nilDataCon, consDataCon, mkTupleTy, mkListTy,
			  charTy, charDataCon, intTy, intDataCon,
			  floatTy, floatDataCon, doubleTy,
			  doubleDataCon, integerTy, stringTy, addrTy,
			  addrDataCon, wordTy, wordDataCon
			)
import Unique		( Unique{-instance Eq-} )
import Util		( panic, pprPanic, assertPanic )
\end{code}

The function @match@ is basically the same as in the Wadler chapter,
except it is monadised, to carry around the name supply, info about
annotations, etc.

Notes on @match@'s arguments, assuming $m$ equations and $n$ patterns:
\begin{enumerate}
\item
A list of $n$ variable names, those variables presumably bound to the
$n$ expressions being matched against the $n$ patterns.  Using the
list of $n$ expressions as the first argument showed no benefit and
some inelegance.

\item
The second argument, a list giving the ``equation info'' for each of
the $m$ equations:
\begin{itemize}
\item
the $n$ patterns for that equation, and
\item
a list of Core bindings [@(Id, CoreExpr)@ pairs] to be ``stuck on
the front'' of the matching code, as in:
\begin{verbatim}
let <binds>
in  <matching-code>
\end{verbatim}
\item
and finally: (ToDo: fill in)

The right way to think about the ``after-match function'' is that it
is an embryonic @CoreExpr@ with a ``hole'' at the end for the
final ``else expression''.
\end{itemize}

There is a type synonym, @EquationInfo@, defined in module @DsUtils@.

An experiment with re-ordering this information about equations (in
particular, having the patterns available in column-major order)
showed no benefit.

\item
A default expression---what to evaluate if the overall pattern-match
fails.  This expression will (almost?) always be
a measly expression @Var@, unless we know it will only be used once
(as we do in @glue_success_exprs@).

Leaving out this third argument to @match@ (and slamming in lots of
@Var "fail"@s) is a positively {\em bad} idea, because it makes it
impossible to share the default expressions.  (Also, it stands no
chance of working in our post-upheaval world of @Locals@.)
\end{enumerate}
So, the full type signature:
\begin{code}
match :: [Id]		  -- Variables rep'ing the exprs we're matching with
      -> [EquationInfo]	  -- Info about patterns, etc. (type synonym below)
      -> [EquationInfo]	  -- Potentially shadowing equations above this one
      -> DsM MatchResult  -- Desugared result!
\end{code}

Note: @match@ is often called via @matchWrapper@ (end of this module),
a function that does much of the house-keeping that goes with a call
to @match@.

It is also worth mentioning the {\em typical} way a block of equations
is desugared with @match@.  At each stage, it is the first column of
patterns that is examined.  The steps carried out are roughly:
\begin{enumerate}
\item
Tidy the patterns in column~1 with @tidyEqnInfo@ (this may add
bindings to the second component of the equation-info):
\begin{itemize}
\item
Remove the `as' patterns from column~1.
\item
Make all constructor patterns in column~1 into @ConPats@, notably
@ListPats@ and @TuplePats@.
\item
Handle any irrefutable (or ``twiddle'') @LazyPats@.
\end{itemize}
\item
Now {\em unmix} the equations into {\em blocks} [w/ local function
@unmix_eqns@], in which the equations in a block all have variable
patterns in column~1, or they all have constructor patterns in ...
(see ``the mixture rule'' in SLPJ).
\item
Call @matchUnmixedEqns@ on each block of equations; it will do the
appropriate thing for each kind of column-1 pattern, usually ending up
in a recursive call to @match@.
\end{enumerate}

%************************************************************************
%*									*
%*  match: empty rule							*
%*									*
%************************************************************************
\subsection[Match-empty-rule]{The ``empty rule''}

We are a little more paranoid about the ``empty rule'' (SLPJ, p.~87)
than the Wadler-chapter code for @match@ (p.~93, first @match@ clause).
And gluing the ``success expressions'' together isn't quite so pretty.

\begin{code}
match [] eqns_info shadows
  = pin_eqns eqns_info		`thenDs` \ match_result@(MatchResult _ _ _ cxt) ->

	-- If at this stage we find that at least one of the shadowing
	-- equations is guaranteed not to fail, then warn of an overlapping pattern
    if not (all shadow_can_fail shadows) then
	dsShadowError cxt 	`thenDs` \ _ ->
	returnDs match_result
    else
	returnDs match_result

  where
    pin_eqns [EqnInfo [] match_result] = returnDs match_result
      -- Last eqn... can't have pats ...

    pin_eqns (EqnInfo [] match_result1 : more_eqns)
      = pin_eqns more_eqns 			`thenDs` \ match_result2 ->
	combineMatchResults match_result1 match_result2

    pin_eqns other_pat = panic "match: pin_eqns"

    shadow_can_fail :: EquationInfo -> Bool

    shadow_can_fail (EqnInfo [] (MatchResult CanFail  _ _ _)) = True
    shadow_can_fail (EqnInfo [] (MatchResult CantFail _ _ _)) = False
    shadow_can_fail other = panic "match:shadow_can_fail"
\end{code}

%************************************************************************
%*									*
%*  match: non-empty rule						*
%*									*
%************************************************************************
\subsection[Match-nonempty]{@match@ when non-empty: unmixing}

This (more interesting) clause of @match@ uses @tidy_and_unmix_eqns@
(a)~to get `as'- and `twiddle'-patterns out of the way (tidying), and
(b)~to do ``the mixture rule'' (SLPJ, p.~88) [which really {\em
un}mixes the equations], producing a list of equation-info
blocks, each block having as its first column of patterns either all
constructors, or all variables (or similar beasts), etc.

@match_unmixed_eqn_blks@ simply takes the place of the @foldr@ in the
Wadler-chapter @match@ (p.~93, last clause), and @match_unmixed_blk@
corresponds roughly to @matchVarCon@.

\begin{code}
match vars@(v:vs) eqns_info shadows
  = mapDs (tidyEqnInfo v) eqns_info	`thenDs` \ tidy_eqns_info ->
    mapDs (tidyEqnInfo v) shadows	`thenDs` \ tidy_shadows ->
    let
	tidy_eqns_blks = unmix_eqns tidy_eqns_info
    in
    match_unmixed_eqn_blks vars tidy_eqns_blks tidy_shadows
  where
    unmix_eqns []    = []
    unmix_eqns [eqn] = [ [eqn] ]
    unmix_eqns (eq1@(EqnInfo (p1:p1s) _) : eq2@(EqnInfo (p2:p2s) _) : eqs)
      = if (  (unfailablePat p1 && unfailablePat p2)
	   || (isConPat      p1 && isConPat p2)
	   || (isLitPat      p1 && isLitPat p2) ) then
	    eq1 `tack_onto` unmixed_rest
	else
	    [ eq1 ] : unmixed_rest
      where
	unmixed_rest = unmix_eqns (eq2:eqs)

	x `tack_onto` xss   = ( x : head xss) : tail xss

    -----------------------------------------------------------------------
    -- loop through the blocks:
    -- subsequent blocks create a "fail expr" for the first one...
    match_unmixed_eqn_blks :: [Id]
			   -> [ [EquationInfo] ]	-- List of eqn BLOCKS
			   -> [EquationInfo]	 	-- Shadows
			   -> DsM MatchResult

    match_unmixed_eqn_blks vars [] shadows = panic "match_unmixed_eqn_blks"

    match_unmixed_eqn_blks vars [eqn_blk] shadows = matchUnmixedEqns vars eqn_blk shadows

    match_unmixed_eqn_blks vars (eqn_blk:eqn_blks) shadows
      = matchUnmixedEqns vars eqn_blk shadows    	`thenDs` \ match_result1 ->  -- try to match with first blk
	match_unmixed_eqn_blks vars eqn_blks shadows'	`thenDs` \ match_result2 ->
	combineMatchResults match_result1 match_result2
      where
	shadows' = eqn_blk ++ shadows
\end{code}

Tidy up the leftmost pattern in an @EquationInfo@, given the variable @v@
which will be scrutinised.  This means:
\begin{itemize}
\item
Replace variable patterns @x@ (@x /= v@) with the pattern @_@,
together with the binding @x = v@.
\item
Replace the `as' pattern @x@@p@ with the pattern p and a binding @x = v@.
\item
Removing lazy (irrefutable) patterns (you don't want to know...).
\item
Converting explicit tuple- and list-pats into ordinary @ConPats@.
\end{itemize}

The result of this tidying is that the column of patterns will include
{\em only}:
\begin{description}
\item[@WildPats@:]
The @VarPat@ information isn't needed any more after this.

\item[@ConPats@:]
@ListPats@, @TuplePats@, etc., are all converted into @ConPats@.

\item[@LitPats@ and @NPats@:]
@LitPats@/@NPats@ of ``known friendly types'' (Int, Char,
Float, 	Double, at least) are converted to unboxed form; e.g.,
\tr{(NPat (HsInt i) _ _)} is converted to:
\begin{verbatim}
(ConPat I# _ _ [LitPat (HsIntPrim i) _])
\end{verbatim}
\end{description}

\begin{code}
tidyEqnInfo :: Id -> EquationInfo -> DsM EquationInfo
	-- DsM'd because of internal call to "match".
	-- "tidy1" does the interesting stuff, looking at
	-- one pattern and fiddling the list of bindings.
tidyEqnInfo v (EqnInfo (pat : pats) match_result)
  = tidy1 v pat match_result	`thenDs` \ (pat', match_result') ->
    returnDs (EqnInfo (pat' : pats) match_result')

tidy1 :: Id 					-- The Id being scrutinised
      -> TypecheckedPat 			-- The pattern against which it is to be matched
      -> MatchResult				-- Current thing do do after matching
      -> DsM (TypecheckedPat, 			-- Equivalent pattern
	      MatchResult)			-- Augmented thing to do afterwards
						-- The augmentation usually takes the form
						-- of new bindings to be added to the front

tidy1 v (VarPat var) match_result
  = returnDs (WildPat (idType var),
	      mkCoLetsMatchResult extra_binds match_result)
  where
    extra_binds | v == var  = []
		| otherwise = [NonRec var (Var v)]

tidy1 v (AsPat var pat) match_result
  = tidy1 v pat (mkCoLetsMatchResult extra_binds match_result)
  where
    extra_binds | v == var  = []
		| otherwise = [NonRec var (Var v)]

tidy1 v (WildPat ty) match_result
  = returnDs (WildPat ty, match_result)

{- now, here we handle lazy patterns:
    tidy1 v ~p bs = (v, v1 = case v of p -> v1 :
			v2 = case v of p -> v2 : ... : bs )

    where the v_i's are the binders in the pattern.

    ToDo: in "v_i = ... -> v_i", are the v_i's really the same thing?

    The case expr for v_i is just: match [v] [(p, [], \ x -> Var v_i)] any_expr
-}

tidy1 v (LazyPat pat) match_result
  = mkSelectorBinds [] pat l_to_l (Var v)	`thenDs` \ sel_binds ->
    returnDs (WildPat (idType v),
	      mkCoLetsMatchResult [NonRec b rhs | (b,rhs) <- sel_binds] match_result)
  where
    l_to_l = binders `zip` binders 	-- Boring
    binders = collectTypedPatBinders pat

-- re-express <con-something> as (ConPat ...) [directly]

tidy1 v (ConOpPat pat1 id pat2 ty) match_result
  = returnDs (ConPat id ty [pat1, pat2], match_result)

tidy1 v (RecPat con_id pat_ty rpats) match_result
  = returnDs (ConPat con_id pat_ty pats, match_result)
  where
    pats 	     = map mk_pat tagged_arg_tys

	-- Boring stuff to find the arg-tys of the constructor
    (_, inst_tys, _) = {-_trace "Match.getAppDataTyConExpandingDicts" $-} getAppDataTyConExpandingDicts pat_ty
    con_arg_tys'     = dataConArgTys con_id inst_tys 
    tagged_arg_tys   = con_arg_tys' `zip` allFieldLabelTags

	-- mk_pat picks a WildPat of the appropriate type for absent fields,
	-- and the specified pattern for present fields
    mk_pat (arg_ty, tag) = case [pat | (sel_id,pat,_) <- rpats,
					fieldLabelTag (recordSelectorFieldLabel sel_id) == tag 
				] of
				(pat:pats) -> ASSERT( null pats )
					      pat
				[]	   -> WildPat arg_ty

tidy1 v (ListPat ty pats) match_result
  = returnDs (list_ConPat, match_result)
  where
    list_ty = mkListTy ty
    list_ConPat
      = foldr (\ x -> \y -> ConPat consDataCon list_ty [x, y])
	      (ConPat nilDataCon  list_ty [])
	      pats

tidy1 v (TuplePat pats) match_result
  = returnDs (tuple_ConPat, match_result)
  where
    arity = length pats
    tuple_ConPat
      = ConPat (mkTupleCon arity)
	       (mkTupleTy arity (map outPatType pats))
	       pats

tidy1 v (DictPat dicts methods) match_result
  = case num_of_d_and_ms of
	0 -> tidy1 v (TuplePat []) match_result
	1 -> tidy1 v (head dict_and_method_pats) match_result
	_ -> tidy1 v (TuplePat dict_and_method_pats) match_result
  where
    num_of_d_and_ms	 = length dicts + length methods
    dict_and_method_pats = map VarPat (dicts ++ methods)


-- deeply ugly mangling for some (common) NPats/LitPats

-- LitPats: the desugarer only sees these at well-known types

tidy1 v pat@(LitPat lit lit_ty) match_result
  | isPrimType lit_ty
  = returnDs (pat, match_result)

  | lit_ty `eqTy` charTy
  = returnDs (ConPat charDataCon charTy [LitPat (mk_char lit) charPrimTy],
	      match_result)

  | otherwise = pprPanic "tidy1:LitPat:" (ppr PprDebug pat)
  where
    mk_char (HsChar c)    = HsCharPrim c

-- NPats: we *might* be able to replace these w/ a simpler form

tidy1 v pat@(NPat lit lit_ty _) match_result
  = returnDs (better_pat, match_result)
  where
    better_pat
      | lit_ty `eqTy` charTy   = ConPat charDataCon   lit_ty [LitPat (mk_char lit)   charPrimTy]
      | lit_ty `eqTy` intTy    = ConPat intDataCon    lit_ty [LitPat (mk_int lit)    intPrimTy]
      | lit_ty `eqTy` wordTy   = ConPat wordDataCon   lit_ty [LitPat (mk_word lit)   wordPrimTy]
      | lit_ty `eqTy` addrTy   = ConPat addrDataCon   lit_ty [LitPat (mk_addr lit)   addrPrimTy]
      | lit_ty `eqTy` floatTy  = ConPat floatDataCon  lit_ty [LitPat (mk_float lit)  floatPrimTy]
      | lit_ty `eqTy` doubleTy = ConPat doubleDataCon lit_ty [LitPat (mk_double lit) doublePrimTy]
      | otherwise	   = pat

    mk_int    (HsInt i)      = HsIntPrim i
    mk_int    l@(HsLitLit s) = l

    mk_char   (HsChar c)     = HsCharPrim c
    mk_char   l@(HsLitLit s) = l

    mk_word   l@(HsLitLit s) = l

    mk_addr   l@(HsLitLit s) = l

    mk_float  (HsInt i)      = HsFloatPrim (fromInteger i)
    mk_float  (HsFrac f)     = HsFloatPrim f
    mk_float  l@(HsLitLit s) = l

    mk_double (HsInt i)      = HsDoublePrim (fromInteger i)
    mk_double (HsFrac f)     = HsDoublePrim f
    mk_double l@(HsLitLit s) = l

-- and everything else goes through unchanged...

tidy1 v non_interesting_pat match_result
  = returnDs (non_interesting_pat, match_result)
\end{code}

PREVIOUS matchTwiddled STUFF:

Now we get to the only interesting part; note: there are choices for
translation [from Simon's notes]; translation~1:
\begin{verbatim}
deTwiddle [s,t] e
\end{verbatim}
returns
\begin{verbatim}
[ w = e,
  s = case w of [s,t] -> s
  t = case w of [s,t] -> t
]
\end{verbatim}

Here \tr{w} is a fresh variable, and the \tr{w}-binding prevents multiple
evaluation of \tr{e}.  An alternative translation (No.~2):
\begin{verbatim}
[ w = case e of [s,t] -> (s,t)
  s = case w of (s,t) -> s
  t = case w of (s,t) -> t
]
\end{verbatim}

%************************************************************************
%*									*
\subsubsection[improved-unmixing]{UNIMPLEMENTED idea for improved unmixing}
%*									*
%************************************************************************

We might be able to optimise unmixing when confronted by
only-one-constructor-possible, of which tuples are the most notable
examples.  Consider:
\begin{verbatim}
f (a,b,c) ... = ...
f d ... (e:f) = ...
f (g,h,i) ... = ...
f j ...       = ...
\end{verbatim}
This definition would normally be unmixed into four equation blocks,
one per equation.  But it could be unmixed into just one equation
block, because if the one equation matches (on the first column),
the others certainly will.

You have to be careful, though; the example
\begin{verbatim}
f j ...       = ...
-------------------
f (a,b,c) ... = ...
f d ... (e:f) = ...
f (g,h,i) ... = ...
\end{verbatim}
{\em must} be broken into two blocks at the line shown; otherwise, you
are forcing unnecessary evaluation.  In any case, the top-left pattern
always gives the cue.  You could then unmix blocks into groups of...
\begin{description}
\item[all variables:]
As it is now.
\item[constructors or variables (mixed):]
Need to make sure the right names get bound for the variable patterns.
\item[literals or variables (mixed):]
Presumably just a variant on the constructor case (as it is now).
\end{description}

%************************************************************************
%*									*
%* match on an unmixed block: the real business				*
%*									*
%************************************************************************
\subsection[matchUnmixedEqns]{@matchUnmixedEqns@: getting down to business}

The function @matchUnmixedEqns@ is where the matching stuff sets to
work a block of equations, to which the mixture rule has been applied.
Its arguments and results are the same as for the ``top-level'' @match@.

\begin{code}
matchUnmixedEqns :: [Id]
		  -> [EquationInfo]
		  -> [EquationInfo]		-- Shadows
		  -> DsM MatchResult

matchUnmixedEqns [] _ _ = panic "matchUnmixedEqns: no names"

matchUnmixedEqns all_vars@(var:vars) eqns_info shadows
  | unfailablePat first_pat
  = ASSERT( unfailablePats column_1_pats )	-- Sanity check
  	-- Real true variables, just like in matchVar, SLPJ p 94
    match vars remaining_eqns_info remaining_shadows

  | isConPat first_pat
  = ASSERT( patsAreAllCons column_1_pats )
    matchConFamily all_vars eqns_info shadows

  | isLitPat first_pat
  = ASSERT( patsAreAllLits column_1_pats )
  	-- see notes in MatchLiteral
	-- not worried about the same literal more than once in a column
	-- (ToDo: sort this out later)
    matchLiterals all_vars eqns_info shadows

  where
    first_pat		= head column_1_pats
    column_1_pats 	= [pat                       | EqnInfo (pat:_)  _            <- eqns_info]
    remaining_eqns_info = [EqnInfo pats match_result | EqnInfo (_:pats) match_result <- eqns_info]
    remaining_shadows   = [EqnInfo pats match_result | EqnInfo (pat:pats) match_result <- shadows,
						       irrefutablePat pat ]
	-- Discard shadows which can be refuted, since they don't shadow
	-- a variable
\end{code}

%************************************************************************
%*									*
%*  matchWrapper: a convenient way to call @match@			*
%*									*
%************************************************************************
\subsection[matchWrapper]{@matchWrapper@: a convenient interface to @match@}

Calls to @match@ often involve similar (non-trivial) work; that work
is collected here, in @matchWrapper@.  This function takes as
arguments:
\begin{itemize}
\item
Typchecked @Matches@ (of a function definition, or a case or lambda
expression)---the main input;
\item
An error message to be inserted into any (runtime) pattern-matching
failure messages.
\end{itemize}

As results, @matchWrapper@ produces:
\begin{itemize}
\item
A list of variables (@Locals@) that the caller must ``promise'' to
bind to appropriate values; and
\item
a @CoreExpr@, the desugared output (main result).
\end{itemize}

The main actions of @matchWrapper@ include:
\begin{enumerate}
\item
Flatten the @[TypecheckedMatch]@ into a suitable list of
@EquationInfo@s.
\item
Create as many new variables as there are patterns in a pattern-list
(in any one of the @EquationInfo@s).
\item
Create a suitable ``if it fails'' expression---a call to @error@ using
the error-string input; the {\em type} of this fail value can be found
by examining one of the RHS expressions in one of the @EquationInfo@s.
\item
Call @match@ with all of this information!
\end{enumerate}

\begin{code}
matchWrapper :: DsMatchKind			-- For shadowing warning messages
	     -> [TypecheckedMatch]		-- Matches being desugared
	     -> String 				-- Error message if the match fails
	     -> DsM ([Id], CoreExpr) 	-- Results

-- a special case for the common ...:
--	just one Match
--	lots of (all?) unfailable pats
--  e.g.,
--	f x y z = ....

matchWrapper kind [(PatMatch (VarPat var) match)] error_string
  = matchWrapper kind [match] error_string `thenDs` \ (vars, core_expr) ->
    returnDs (var:vars, core_expr)

matchWrapper kind [(PatMatch (WildPat ty) match)] error_string
  = newSysLocalDs ty		      `thenDs` \ var ->
    matchWrapper kind [match] error_string `thenDs` \ (vars, core_expr) ->
    returnDs (var:vars, core_expr)

matchWrapper kind [(GRHSMatch
		     (GRHSsAndBindsOut [OtherwiseGRHS expr _] binds _))] error_string
  = dsBinds binds	`thenDs` \ core_binds ->
    dsExpr  expr	`thenDs` \ core_expr ->
    returnDs ([], mkCoLetsAny core_binds core_expr)

----------------------------------------------------------------------------
-- and all the rest... (general case)

matchWrapper kind matches error_string
  = flattenMatches kind matches	`thenDs` \ eqns_info@(EqnInfo arg_pats (MatchResult _ result_ty _ _) : _) ->

    selectMatchVars arg_pats				`thenDs` \ new_vars ->
    match new_vars eqns_info []				`thenDs` \ match_result ->

    mkErrorAppDs pAT_ERROR_ID result_ty error_string	`thenDs` \ fail_expr ->
    extractMatchResult match_result fail_expr		`thenDs` \ result_expr ->

    returnDs (new_vars, result_expr)
\end{code}

%************************************************************************
%*									*
\subsection[matchSimply]{@matchSimply@: match a single expression against a single pattern}
%*									*
%************************************************************************

@mkSimpleMatch@ is a wrapper for @match@ which deals with the
situation where we want to match a single expression against a single
pattern. It returns an expression.

\begin{code}
matchSimply :: CoreExpr			-- Scrutinee
	    -> TypecheckedPat			-- Pattern it should match
	    -> Type				-- Type of result
	    -> CoreExpr			-- Return this if it matches
	    -> CoreExpr			-- Return this if it does
	    -> DsM CoreExpr

matchSimply (Var var) pat result_ty result_expr fail_expr
  = match [var] [eqn_info] []	`thenDs` \ match_result ->
    extractMatchResult match_result fail_expr
  where
    eqn_info = EqnInfo [pat] initial_match_result
    initial_match_result = MatchResult CantFail
				       result_ty
				       (\ ignore -> result_expr)
				       NoMatchContext

matchSimply scrut_expr pat result_ty result_expr msg
  = newSysLocalDs (outPatType pat) 				`thenDs` \ scrut_var ->
    matchSimply (Var scrut_var) pat result_ty result_expr msg	`thenDs` \ expr ->
    returnDs (Let (NonRec scrut_var scrut_expr) expr)


extractMatchResult (MatchResult CantFail _ match_fn _) fail_expr
  = returnDs (match_fn (error "It can't fail!"))

extractMatchResult (MatchResult CanFail result_ty match_fn _) fail_expr
  = mkFailurePair result_ty 	`thenDs` \ (fail_bind_fn, if_it_fails) ->
    returnDs (Let (fail_bind_fn fail_expr) (match_fn if_it_fails))
\end{code}

%************************************************************************
%*									*
%*  flattenMatches : create a list of EquationInfo			*
%*									*
%************************************************************************
\subsection[flattenMatches]{@flattenMatches@: create @[EquationInfo]@}

This is actually local to @matchWrapper@.

\begin{code}
flattenMatches
	:: DsMatchKind
	-> [TypecheckedMatch]
	-> DsM [EquationInfo]

flattenMatches kind [] = returnDs []

flattenMatches kind (match : matches)
  = flatten_match [] match	`thenDs` \ eqn_info ->
    flattenMatches kind matches	`thenDs` \ eqn_infos ->
    returnDs (eqn_info : eqn_infos)
  where
    flatten_match :: [TypecheckedPat] 		-- Reversed list of patterns encountered so far
		  -> TypecheckedMatch
		  -> DsM EquationInfo

    flatten_match pats_so_far (PatMatch pat match)
      = flatten_match (pat:pats_so_far) match

    flatten_match pats_so_far (GRHSMatch (GRHSsAndBindsOut grhss binds ty))
      = dsBinds binds				`thenDs` \ core_binds ->
	dsGRHSs ty kind pats grhss 		`thenDs` \ match_result ->
	returnDs (EqnInfo pats (mkCoLetsMatchResult core_binds match_result))
      where
	pats = reverse pats_so_far	-- They've accumulated in reverse order

    flatten_match pats_so_far (SimpleMatch expr) 
      = dsExpr expr		`thenDs` \ core_expr ->
	returnDs (EqnInfo pats
		    (MatchResult CantFail (coreExprType core_expr) 
			      (\ ignore -> core_expr)
			      NoMatchContext))
	-- The NoMatchContext is just a place holder.  In a simple match,
	-- the matching can't fail, so we won't generate an error message.
      where
	pats = reverse pats_so_far	-- They've accumulated in reverse order
\end{code}
