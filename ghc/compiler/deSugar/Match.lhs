%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[Main_match]{The @match@ function}

\begin{code}
module Match ( match, matchExport, matchWrapper, matchSimply, matchSinglePat ) where

#include "HsVersions.h"

import {-# SOURCE #-} DsExpr( dsExpr )
import CmdLineOpts	( DynFlag(..), dopt )
import HsSyn		
import TcHsSyn		( TypecheckedPat, TypecheckedMatch, TypecheckedMatchContext, hsPatType )
import Check            ( check, ExhaustivePat )
import CoreSyn
import CoreUtils	( bindNonRec )
import DsMonad
import DsGRHSs		( dsGRHSs )
import DsUtils
import Id		( idType, recordSelectorFieldLabel, Id )
import DataCon		( dataConFieldLabels, dataConInstOrigArgTys )
import MatchCon		( matchConFamily )
import MatchLit		( matchLiterals )
import PrelInfo		( pAT_ERROR_ID )
import TcType		( mkTyVarTys, Type, tcTyConAppArgs, tcEqType )
import TysWiredIn	( consDataCon, mkTupleTy, mkListTy,
			  tupleCon, parrFakeCon, mkPArrTy )
import BasicTypes	( Boxity(..) )
import UniqSet
import SrcLoc		( noSrcLoc )
import Util             ( lengthExceeds, isSingleton, notNull )
import Outputable
\end{code}

This function is a wrapper of @match@, it must be called from all the parts where 
it was called match, but only substitutes the firs call, ....
if the associated flags are declared, warnings will be issued.
It can not be called matchWrapper because this name already exists :-(

JJCQ 30-Nov-1997

\begin{code}
matchExport :: [Id]	        -- Vars rep'ing the exprs we're matching with
            -> [EquationInfo]   -- Info about patterns, etc. (type synonym below)
            -> DsM MatchResult  -- Desugared result!


matchExport vars qs
   = getDOptsDs 				`thenDs` \ dflags ->
     matchExport_really dflags vars qs

matchExport_really dflags vars qs@((EqnInfo _ ctx _ (MatchResult _ _)) : _)
  | incomplete && shadow = 
      dsShadowWarn ctx eqns_shadow		`thenDs`   \ () ->
      dsIncompleteWarn ctx pats			`thenDs`   \ () ->
      match vars qs
  | incomplete            = 
      dsIncompleteWarn ctx pats			`thenDs`   \ () ->
      match vars qs
  | shadow                = 
      dsShadowWarn ctx eqns_shadow		`thenDs`   \ () ->
      match vars qs
  | otherwise             =
      match vars qs
  where (pats,indexs) = check qs
        incomplete    = dopt Opt_WarnIncompletePatterns dflags
			&& (notNull pats)
        shadow        = dopt Opt_WarnOverlappingPatterns dflags
			&& sizeUniqSet indexs < no_eqns
        no_eqns       = length qs
	unused_eqns   = uniqSetToList (mkUniqSet [1..no_eqns] `minusUniqSet` indexs)
	eqns_shadow   = map (\n -> qs!!(n - 1)) unused_eqns
\end{code}

This variable shows the maximum number of lines of output generated for warnings.
It will limit the number of patterns/equations displayed to@ maximum_output@.

(ToDo: add command-line option?)

\begin{code}
maximum_output = 4
\end{code}

The next two functions create the warning message.

\begin{code}
dsShadowWarn :: DsMatchContext -> [EquationInfo] -> DsM ()
dsShadowWarn ctx@(DsMatchContext kind _ _) qs = dsWarn warn 
	where
	  warn | qs `lengthExceeds` maximum_output
               = pp_context ctx (ptext SLIT("are overlapped"))
		            (\ f -> vcat (map (ppr_eqn f kind) (take maximum_output qs)) $$
			    ptext SLIT("..."))
	       | otherwise
               = pp_context ctx (ptext SLIT("are overlapped"))
	                    (\ f -> vcat $ map (ppr_eqn f kind) qs)


dsIncompleteWarn :: DsMatchContext -> [ExhaustivePat] -> DsM ()
dsIncompleteWarn ctx@(DsMatchContext kind _ _) pats = dsWarn warn 
	where
	  warn = pp_context ctx (ptext SLIT("are non-exhaustive"))
                    	    (\f -> hang (ptext SLIT("Patterns not matched:"))
		                   4 ((vcat $ map (ppr_incomplete_pats kind)
						  (take maximum_output pats))
		                      $$ dots))

	  dots | pats `lengthExceeds` maximum_output = ptext SLIT("...")
	       | otherwise                           = empty

pp_context NoMatchContext msg rest_of_msg_fun
  = (noSrcLoc, ptext SLIT("Some match(es)") <+> hang msg 8 (rest_of_msg_fun id))

pp_context (DsMatchContext kind pats loc) msg rest_of_msg_fun
  = (loc, vcat [ptext SLIT("Pattern match(es)") <+> msg,
	        sep [ptext SLIT("In") <+> ppr_match <> char ':', nest 4 (rest_of_msg_fun pref)]])
  where
    (ppr_match, pref)
	= case kind of
	     FunRhs fun -> (pprMatchContext kind, \ pp -> ppr fun <+> pp)
	     other	-> (pprMatchContext kind, \ pp -> pp)

ppr_pats pats = sep (map ppr pats)

ppr_shadow_pats kind pats
  = sep [ppr_pats pats, matchSeparator kind, ptext SLIT("...")]
    
ppr_incomplete_pats kind (pats,[]) = ppr_pats pats
ppr_incomplete_pats kind (pats,constraints) = 
	                 sep [ppr_pats pats, ptext SLIT("with"), 
	                      sep (map ppr_constraint constraints)]
    

ppr_constraint (var,pats) = sep [ppr var, ptext SLIT("`notElem`"), ppr pats]

ppr_eqn prefixF kind (EqnInfo _ _ pats _) = prefixF (ppr_shadow_pats kind pats)
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
Call @matchEqnBlock@ on each block of equations; it will do the
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
match [] eqns_info
  = returnDs (foldr1 combineMatchResults match_results)
  where
    match_results = [ ASSERT( null pats) mr
		    | EqnInfo _ _ pats mr <- eqns_info ]
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
match vars@(v:vs) eqns_info
  = mapDs (tidyEqnInfo v) eqns_info	`thenDs` \ tidy_eqns_info ->
    let
	tidy_eqns_blks = unmix_eqns tidy_eqns_info
    in
    mapDs (matchEqnBlock vars) tidy_eqns_blks	`thenDs` \ match_results ->
    returnDs (foldr1 combineMatchResults match_results)
  where
    unmix_eqns []    = []
    unmix_eqns [eqn] = [ [eqn] ]
    unmix_eqns (eq1@(EqnInfo _ _ (p1:p1s) _) : eq2@(EqnInfo _ _ (p2:p2s) _) : eqs)
      = if (  (isWildPat p1 && isWildPat p2)
	   || (isConPat  p1 && isConPat  p2)
	   || (isLitPat  p1 && isLitPat  p2) ) then
	    eq1 `tack_onto` unmixed_rest
	else
	    [ eq1 ] : unmixed_rest
      where
	unmixed_rest = unmix_eqns (eq2:eqs)

	x `tack_onto` xss   = ( x : head xss) : tail xss
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
Converting explicit tuple-, list-, and parallel-array-pats into ordinary
@ConPats@. 
\item
Convert the literal pat "" to [].
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
(ConPat I# _ _ [LitPat (HsIntPrim i)])
\end{verbatim}
\end{description}

\begin{code}
tidyEqnInfo :: Id -> EquationInfo -> DsM EquationInfo
	-- DsM'd because of internal call to "match".
	-- "tidy1" does the interesting stuff, looking at
	-- one pattern and fiddling the list of bindings.
	--
	-- POST CONDITION: head pattern in the EqnInfo is
	--	WildPat
	--	ConPat
	--	NPat
	--	LitPat
	--	NPlusKPat
	--	SigPat
	-- but no other

tidyEqnInfo v (EqnInfo n ctx (pat : pats) match_result)
  = tidy1 v pat match_result	`thenDs` \ (pat', match_result') ->
    returnDs (EqnInfo n ctx (pat' : pats) match_result')


tidy1 :: Id 			-- The Id being scrutinised
      -> TypecheckedPat 	-- The pattern against which it is to be matched
      -> MatchResult		-- Current thing do do after matching
      -> DsM (TypecheckedPat, 	-- Equivalent pattern
	      MatchResult)	-- Augmented thing to do afterwards
				-- The augmentation usually takes the form
				-- of new bindings to be added to the front

-------------------------------------------------------
--	(pat', mr') = tidy1 v pat mr
-- tidies the *outer level only* of pat, giving pat'
-- It eliminates many pattern forms (as-patterns, variable patterns,
-- list patterns, etc) yielding one of:
--	WildPat
--	ConPat
--	LitPat
--	NPat
--	NPlusKPat
--

tidy1 v (ParPat pat) match_result 
  = tidy1 v pat match_result

	-- case v of { x -> mr[] }
	-- = case v of { _ -> let x=v in mr[] }
tidy1 v (VarPat var) match_result
  = returnDs (WildPat (idType var), match_result')
  where
    match_result' | v == var  = match_result
		  | otherwise = adjustMatchResult (bindNonRec var (Var v)) match_result

	-- case v of { x@p -> mr[] }
	-- = case v of { p -> let x=v in mr[] }
tidy1 v (AsPat var pat) match_result
  = tidy1 v pat match_result'
  where
    match_result' | v == var  = match_result
		  | otherwise = adjustMatchResult (bindNonRec var (Var v)) match_result

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
  = mkSelectorBinds pat (Var v)		`thenDs` \ sel_binds ->
    returnDs (WildPat (idType v),
	      mkCoLetsMatchResult [NonRec b rhs | (b,rhs) <- sel_binds] match_result)

-- re-express <con-something> as (ConPat ...) [directly]

tidy1 v (ConPatOut con ps pat_ty ex_tvs dicts) match_result
  = returnDs (ConPatOut con tidy_ps pat_ty ex_tvs dicts, match_result)
  where
    tidy_ps = PrefixCon (tidy_con con pat_ty ex_tvs ps)

tidy1 v (ListPat pats ty) match_result
  = returnDs (list_ConPat, match_result)
  where
    list_ty     = mkListTy ty
    list_ConPat = foldr (\ x y -> mkPrefixConPat consDataCon [x, y] list_ty)
	      	  	(mkNilPat list_ty)
	      	  	pats

-- introduce fake parallel array constructors to be able to handle parallel
-- arrays with the existing machinery for constructor pattern
--
tidy1 v (PArrPat pats ty) match_result
  = returnDs (parrConPat, match_result)
  where
    arity      = length pats
    parrConPat = mkPrefixConPat (parrFakeCon arity) pats (mkPArrTy ty)

tidy1 v (TuplePat pats boxity) match_result
  = returnDs (tuple_ConPat, match_result)
  where
    arity = length pats
    tuple_ConPat = mkPrefixConPat (tupleCon boxity arity) pats
				  (mkTupleTy boxity arity (map hsPatType pats))

tidy1 v (DictPat dicts methods) match_result
  = case num_of_d_and_ms of
	0 -> tidy1 v (TuplePat [] Boxed) match_result
	1 -> tidy1 v (head dict_and_method_pats) match_result
	_ -> tidy1 v (TuplePat dict_and_method_pats Boxed) match_result
  where
    num_of_d_and_ms	 = length dicts + length methods
    dict_and_method_pats = map VarPat (dicts ++ methods)

-- LitPats: we *might* be able to replace these w/ a simpler form
tidy1 v pat@(LitPat lit) match_result
  = returnDs (tidyLitPat lit pat, match_result)

-- NPats: we *might* be able to replace these w/ a simpler form
tidy1 v pat@(NPatOut lit lit_ty _) match_result
  = returnDs (tidyNPat lit lit_ty pat, match_result)

-- and everything else goes through unchanged...

tidy1 v non_interesting_pat match_result
  = returnDs (non_interesting_pat, match_result)


tidy_con data_con pat_ty ex_tvs (PrefixCon ps)   = ps
tidy_con data_con pat_ty ex_tvs (InfixCon p1 p2) = [p1,p2]
tidy_con data_con pat_ty ex_tvs (RecCon rpats)
  | null rpats
  =	-- Special case for C {}, which can be used for 
	-- a constructor that isn't declared to have
	-- fields at all
    map WildPat con_arg_tys'

  | otherwise
  = map mk_pat tagged_arg_tys
  where
	-- Boring stuff to find the arg-tys of the constructor
    inst_tys         = tcTyConAppArgs pat_ty	-- Newtypes must be opaque
    con_arg_tys'     = dataConInstOrigArgTys data_con (inst_tys ++ mkTyVarTys ex_tvs)
    tagged_arg_tys   = con_arg_tys' `zip` (dataConFieldLabels data_con)

	-- mk_pat picks a WildPat of the appropriate type for absent fields,
	-- and the specified pattern for present fields
    mk_pat (arg_ty, lbl) = case [pat | (sel_id,pat) <- rpats,
					recordSelectorFieldLabel sel_id == lbl
				] of
				(pat:pats) -> ASSERT( null pats )
					      pat
				[]	   -> WildPat arg_ty
\end{code}

\noindent
{\bf Previous @matchTwiddled@ stuff:}

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
\subsection[matchEqnBlock]{@matchEqnBlock@: getting down to business}

The function @matchEqnBlock@ is where the matching stuff sets to
work a block of equations, to which the mixture rule has been applied.
Its arguments and results are the same as for the ``top-level'' @match@.

\begin{code}
matchEqnBlock :: [Id]
	      -> [EquationInfo]
	      -> DsM MatchResult

matchEqnBlock [] _ = panic "matchEqnBlock: no names"

matchEqnBlock all_vars@(var:vars) eqns_info 
  | isWildPat first_pat
  = ASSERT( all isWildPat column_1_pats )	-- Sanity check
  	-- Real true variables, just like in matchVar, SLPJ p 94
	-- No binding to do: they'll all be wildcards by now (done in tidy)
    match vars remaining_eqns_info

  | isConPat first_pat
  = ASSERT( patsAreAllCons column_1_pats )
    matchConFamily all_vars eqns_info 

  | isLitPat first_pat
  = ASSERT( patsAreAllLits column_1_pats )
  	-- see notes in MatchLiteral
	-- not worried about the same literal more than once in a column
	-- (ToDo: sort this out later)
    matchLiterals all_vars eqns_info

  | isSigPat first_pat
  = ASSERT( isSingleton eqns_info )
    matchSigPat all_vars (head eqns_info)
  where
    first_pat		= head column_1_pats
    column_1_pats 	= [pat		                   | EqnInfo _ _   (pat:_)  _            <- eqns_info]
    remaining_eqns_info = [EqnInfo n ctx pats match_result | EqnInfo n ctx (_:pats) match_result <- eqns_info]
\end{code}

A SigPat is a type coercion and must be handled one at at time.  We can't
combine them unless the type of the pattern inside is identical, and we don't
bother to check for that.  For example:

	data T = T1 Int | T2 Bool
	f :: (forall a. a -> a) -> T -> t
	f (g::Int->Int)   (T1 i) = T1 (g i)
	f (g::Bool->Bool) (T2 b) = T2 (g b)

We desugar this as follows:

	f = \ g::(forall a. a->a) t::T ->
	    let gi = g Int
	    in case t of { T1 i -> T1 (gi i)
			   other ->
	    let	gb = g Bool
	    in case t of { T2 b -> T2 (gb b)
			   other -> fail }}

Note that we do not treat the first column of patterns as a
column of variables, because the coerced variables (gi, gb)
would be of different types.  So we get rather grotty code.
But I don't think this is a common case, and if it was we could
doubtless improve it.

Meanwhile, the strategy is:
	* treat each SigPat coercion (always non-identity coercions)
		as a separate block
	* deal with the stuff inside, and then wrap a binding round
		the result to bind the new variable (gi, gb, etc)

\begin{code}
matchSigPat :: [Id] -> EquationInfo -> DsM MatchResult
matchSigPat (var:vars) (EqnInfo n ctx (SigPatOut pat ty co_fn : pats) result)
  = selectMatchVar pat						`thenDs` \ new_var ->
    dsExpr (HsApp co_fn (HsVar var))				`thenDs` \ rhs ->
    match (new_var:vars) [EqnInfo n ctx (pat:pats) result]	`thenDs` \ result' ->
    returnDs (adjustMatchResult (bindNonRec new_var rhs) result')
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
matchWrapper :: TypecheckedMatchContext	-- For shadowing warning messages
	     -> [TypecheckedMatch]	-- Matches being desugared
	     -> DsM ([Id], CoreExpr) 	-- Results
\end{code}

 There is one small problem with the Lambda Patterns, when somebody
 writes something similar to:
\begin{verbatim}
    (\ (x:xs) -> ...)
\end{verbatim}
 he/she don't want a warning about incomplete patterns, that is done with 
 the flag @opt_WarnSimplePatterns@.
 This problem also appears in the:
\begin{itemize}
\item @do@ patterns, but if the @do@ can fail
      it creates another equation if the match can fail
      (see @DsExpr.doDo@ function)
\item @let@ patterns, are treated by @matchSimply@
   List Comprension Patterns, are treated by @matchSimply@ also
\end{itemize}

We can't call @matchSimply@ with Lambda patterns,
due to the fact that lambda patterns can have more than
one pattern, and match simply only accepts one pattern.

JJQC 30-Nov-1997

\begin{code}
matchWrapper ctxt matches
  = getDOptsDs					`thenDs` \ dflags ->
    flattenMatches ctxt matches			`thenDs` \ (result_ty, eqns_info) ->
    let
	EqnInfo _ _ arg_pats _ : _ = eqns_info
	error_string = matchContextErrString ctxt
    in
    mapDs selectMatchVar arg_pats		`thenDs` \ new_vars ->
    match_fun dflags new_vars eqns_info 	`thenDs` \ match_result ->

    mkErrorAppDs pAT_ERROR_ID result_ty error_string	`thenDs` \ fail_expr ->
    extractMatchResult match_result fail_expr		`thenDs` \ result_expr ->
    returnDs (new_vars, result_expr)
  where match_fun dflags
           = case ctxt of 
                LambdaExpr | dopt Opt_WarnSimplePatterns dflags -> matchExport 
                           | otherwise                          -> match
                _                                               -> matchExport
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
	    -> TypecheckedMatchContext	-- Match kind
	    -> TypecheckedPat		-- Pattern it should match
	    -> CoreExpr			-- Return this if it matches
	    -> CoreExpr			-- Return this if it doesn't
	    -> DsM CoreExpr

matchSimply scrut kind pat result_expr fail_expr
  = getSrcLocDs				        `thenDs` \ locn ->
    let
      ctx 	   = DsMatchContext kind [pat] locn
      match_result = cantFailMatchResult result_expr
    in 
    matchSinglePat scrut ctx pat match_result	`thenDs` \ match_result' ->
    extractMatchResult match_result' fail_expr


matchSinglePat :: CoreExpr -> DsMatchContext -> TypecheckedPat
	       -> MatchResult -> DsM MatchResult

matchSinglePat (Var var) ctx pat match_result
  = getDOptsDs					`thenDs` \ dflags ->
    match_fn dflags [var] [EqnInfo 1 ctx [pat] match_result]
  where
    match_fn dflags
       | dopt Opt_WarnSimplePatterns dflags = matchExport
       | otherwise	                    = match

matchSinglePat scrut ctx pat match_result
  = selectMatchVar pat		 	 		`thenDs` \ var ->
    matchSinglePat (Var var) ctx pat match_result	`thenDs` \ match_result' ->
    returnDs (adjustMatchResult (bindNonRec var scrut) match_result')
\end{code}

%************************************************************************
%*									*
%*  flattenMatches : create a list of EquationInfo			*
%*									*
%************************************************************************

\subsection[flattenMatches]{@flattenMatches@: create @[EquationInfo]@}

This is actually local to @matchWrapper@.

\begin{code}
flattenMatches :: TypecheckedMatchContext
	       -> [TypecheckedMatch]
	       -> DsM (Type, [EquationInfo])

flattenMatches kind matches
  = mapAndUnzipDs flatten_match (matches `zip` [1..])	`thenDs` \ (result_tys, eqn_infos) ->
    let
	result_ty = head result_tys
    in
    ASSERT( all (tcEqType result_ty) result_tys )
    returnDs (result_ty, eqn_infos)
  where
    flatten_match (Match pats _ grhss, n)
      = dsGRHSs kind pats grhss 		`thenDs` \ (ty, match_result) ->
        getSrcLocDs				`thenDs` \ locn ->
	returnDs (ty, EqnInfo n (DsMatchContext kind pats locn) pats match_result)
\end{code}
