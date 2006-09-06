%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[Main_match]{The @match@ function}

\begin{code}
module Match ( match, matchEquations, matchWrapper, matchSimply, matchSinglePat ) where

#include "HsVersions.h"

import DynFlags	( DynFlag(..), dopt )
import HsSyn		
import TcHsSyn		( mkVanillaTuplePat, hsPatType )
import Check            ( check, ExhaustivePat )
import CoreSyn
import Literal		( Literal )
import CoreUtils	( bindNonRec, exprType )
import DsMonad
import DsBinds		( dsLHsBinds, dsCoercion )
import DsGRHSs		( dsGRHSs )
import DsUtils
import Id		( idName, idType, Id )
import DataCon		( DataCon )
import MatchCon		( matchConFamily )
import MatchLit		( matchLiterals, matchNPlusKPats, matchNPats, 
			  tidyLitPat, tidyNPat, hsLitKey, hsOverLitKey )
import PrelInfo		( pAT_ERROR_ID )
import TcType		( Type )
import Type		( splitFunTysN, coreEqType )
import TysWiredIn	( consDataCon, mkListTy, unitTy,
			  tupleCon, parrFakeCon, mkPArrTy )
import BasicTypes	( Boxity(..) )
import ListSetOps	( equivClasses, runs )
import SrcLoc		( unLoc, Located(..) )
import Maybes		( isJust )
import Util             ( lengthExceeds, notNull )
import Name		( Name )
import Outputable
\end{code}

This function is a wrapper of @match@, it must be called from all the parts where 
it was called match, but only substitutes the firs call, ....
if the associated flags are declared, warnings will be issued.
It can not be called matchWrapper because this name already exists :-(

JJCQ 30-Nov-1997

\begin{code}
matchCheck ::  DsMatchContext
	    -> [Id]	        -- Vars rep'ing the exprs we're matching with
            -> Type             -- Type of the case expression
            -> [EquationInfo]   -- Info about patterns, etc. (type synonym below)
            -> DsM MatchResult  -- Desugared result!

matchCheck ctx vars ty qs
   = getDOptsDs 				`thenDs` \ dflags ->
     matchCheck_really dflags ctx vars ty qs

matchCheck_really dflags ctx vars ty qs
  | incomplete && shadow = 
      dsShadowWarn ctx eqns_shadow		`thenDs`   \ () ->
      dsIncompleteWarn ctx pats			`thenDs`   \ () ->
      match vars ty qs
  | incomplete            = 
      dsIncompleteWarn ctx pats			`thenDs`   \ () ->
      match vars ty qs
  | shadow                = 
      dsShadowWarn ctx eqns_shadow		`thenDs`   \ () ->
      match vars ty qs
  | otherwise             =
      match vars ty qs
  where (pats, eqns_shadow) = check qs
        incomplete    = want_incomplete && (notNull pats)
        want_incomplete = case ctx of
                              DsMatchContext RecUpd _ ->
                                  dopt Opt_WarnIncompletePatternsRecUpd dflags
                              _ ->
                                  dopt Opt_WarnIncompletePatterns       dflags
        shadow        = dopt Opt_WarnOverlappingPatterns dflags
			&& not (null eqns_shadow)
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
dsShadowWarn ctx@(DsMatchContext kind loc) qs
  = putSrcSpanDs loc (warnDs warn)
  where
    warn | qs `lengthExceeds` maximum_output
         = pp_context ctx (ptext SLIT("are overlapped"))
		      (\ f -> vcat (map (ppr_eqn f kind) (take maximum_output qs)) $$
		      ptext SLIT("..."))
	 | otherwise
         = pp_context ctx (ptext SLIT("are overlapped"))
	              (\ f -> vcat $ map (ppr_eqn f kind) qs)


dsIncompleteWarn :: DsMatchContext -> [ExhaustivePat] -> DsM ()
dsIncompleteWarn ctx@(DsMatchContext kind loc) pats 
  = putSrcSpanDs loc (warnDs warn)
	where
	  warn = pp_context ctx (ptext SLIT("are non-exhaustive"))
                    	    (\f -> hang (ptext SLIT("Patterns not matched:"))
		                   4 ((vcat $ map (ppr_incomplete_pats kind)
						  (take maximum_output pats))
		                      $$ dots))

	  dots | pats `lengthExceeds` maximum_output = ptext SLIT("...")
	       | otherwise                           = empty

pp_context (DsMatchContext kind _loc) msg rest_of_msg_fun
  = vcat [ptext SLIT("Pattern match(es)") <+> msg,
	  sep [ptext SLIT("In") <+> ppr_match <> char ':', nest 4 (rest_of_msg_fun pref)]]
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

ppr_eqn prefixF kind eqn = prefixF (ppr_shadow_pats kind (eqn_pats eqn))
\end{code}


%************************************************************************
%*									*
		The main matching function
%*									*
%************************************************************************

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

We are a little more paranoid about the ``empty rule'' (SLPJ, p.~87)
than the Wadler-chapter code for @match@ (p.~93, first @match@ clause).
And gluing the ``success expressions'' together isn't quite so pretty.

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
match :: [Id]		  -- Variables rep'ing the exprs we're matching with
      -> Type             -- Type of the case expression
      -> [EquationInfo]	  -- Info about patterns, etc. (type synonym below)
      -> DsM MatchResult  -- Desugared result!

match [] ty eqns
  = ASSERT( not (null eqns) )
    returnDs (foldr1 combineMatchResults match_results)
  where
    match_results = [ ASSERT( null (eqn_pats eqn) ) 
		      eqn_rhs eqn
		    | eqn <- eqns ]

match vars@(v:_) ty eqns
  = ASSERT( not (null eqns ) )
    do	{ 	-- Tidy the first pattern, generating
		-- auxiliary bindings if necessary
	  (aux_binds, tidy_eqns) <- mapAndUnzipM (tidyEqnInfo v) eqns

		-- Group the equations and match each group in turn
	; match_results <- mapM match_group (groupEquations tidy_eqns)

	; return (adjustMatchResult (foldr1 (.) aux_binds) $
		  foldr1 combineMatchResults match_results) }
  where
    dropGroup :: [(PatGroup,EquationInfo)] -> [EquationInfo]
    dropGroup = map snd

    match_group :: [(PatGroup,EquationInfo)] -> DsM MatchResult
    match_group eqns@((group,_) : _)
      = case group of
	  PgAny     -> matchVariables  vars ty (dropGroup eqns)
	  PgCon _   -> matchConFamily  vars ty (subGroups eqns)
	  PgLit _   -> matchLiterals   vars ty (subGroups eqns)
	  PgN lit   -> matchNPats      vars ty (subGroups eqns)
	  PgNpK lit -> matchNPlusKPats vars ty (dropGroup eqns)
	  PgBang    -> matchBangs      vars ty (dropGroup eqns)
	  PgCo _    -> matchCoercion   vars ty (dropGroup eqns)

matchVariables :: [Id] -> Type -> [EquationInfo] -> DsM MatchResult
-- Real true variables, just like in matchVar, SLPJ p 94
-- No binding to do: they'll all be wildcards by now (done in tidy)
matchVariables (var:vars) ty eqns = match vars ty (shiftEqns eqns)

matchBangs :: [Id] -> Type -> [EquationInfo] -> DsM MatchResult
matchBangs (var:vars) ty eqns
  = do	{ match_result <- match (var:vars) ty (map shift eqns)
	; return (mkEvalMatchResult var ty match_result) }
  where
    shift eqn@(EqnInfo { eqn_pats = BangPat pat : pats })
	= eqn { eqn_pats = unLoc pat : pats }

matchCoercion :: [Id] -> Type -> [EquationInfo] -> DsM MatchResult
-- Apply the coercion to the match variable and then match that
matchCoercion (var:vars) ty (eqn1:eqns)
  = do	{ let CoPat co pat _ = firstPat eqn1
	; var' <- newUniqueId (idName var) (hsPatType pat)
	; match_result <- match (var':vars) ty (map shift (eqn1:eqns))
	; rhs <- dsCoercion co (return (Var var))
	; return (mkCoLetMatchResult (NonRec var' rhs) match_result) }
  where
    shift eqn@(EqnInfo { eqn_pats = CoPat _ pat _ : pats })
	= eqn { eqn_pats = pat : pats }
\end{code}

%************************************************************************
%*									*
		Tidying patterns
%*									*
%************************************************************************

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
tidyEqnInfo :: Id -> EquationInfo
	    -> DsM (DsWrapper, EquationInfo)
	-- DsM'd because of internal call to dsLHsBinds
	-- 	and mkSelectorBinds.
	-- "tidy1" does the interesting stuff, looking at
	-- one pattern and fiddling the list of bindings.
	--
	-- POST CONDITION: head pattern in the EqnInfo is
	--	WildPat
	--	ConPat
	--	NPat
	--	LitPat
	--	NPlusKPat
	-- but no other

tidyEqnInfo v eqn@(EqnInfo { eqn_pats = pat : pats })
  = tidy1 v pat 	`thenDs` \ (wrap, pat') ->
    returnDs (wrap, eqn { eqn_pats = pat' : pats })

tidy1 :: Id 			-- The Id being scrutinised
      -> Pat Id 		-- The pattern against which it is to be matched
      -> DsM (DsWrapper,	-- Extra bindings to do before the match
	      Pat Id) 		-- Equivalent pattern

-------------------------------------------------------
--	(pat', mr') = tidy1 v pat mr
-- tidies the *outer level only* of pat, giving pat'
-- It eliminates many pattern forms (as-patterns, variable patterns,
-- list patterns, etc) yielding one of:
--	WildPat
--	ConPatOut
--	LitPat
--	NPat
--	NPlusKPat

tidy1 v (ParPat pat)      = tidy1 v (unLoc pat) 
tidy1 v (SigPatOut pat _) = tidy1 v (unLoc pat) 
tidy1 v (WildPat ty)      = returnDs (idWrapper, WildPat ty)

	-- case v of { x -> mr[] }
	-- = case v of { _ -> let x=v in mr[] }
tidy1 v (VarPat var)
  = returnDs (wrapBind var v, WildPat (idType var)) 

tidy1 v (VarPatOut var binds)
  = do	{ prs <- dsLHsBinds binds
	; return (wrapBind var v . mkDsLet (Rec prs),
		  WildPat (idType var)) }

	-- case v of { x@p -> mr[] }
	-- = case v of { p -> let x=v in mr[] }
tidy1 v (AsPat (L _ var) pat)
  = do	{ (wrap, pat') <- tidy1 v (unLoc pat)
	; return (wrapBind var v . wrap, pat') }

{- now, here we handle lazy patterns:
    tidy1 v ~p bs = (v, v1 = case v of p -> v1 :
			v2 = case v of p -> v2 : ... : bs )

    where the v_i's are the binders in the pattern.

    ToDo: in "v_i = ... -> v_i", are the v_i's really the same thing?

    The case expr for v_i is just: match [v] [(p, [], \ x -> Var v_i)] any_expr
-}

tidy1 v (LazyPat pat)
  = do	{ sel_prs <- mkSelectorBinds pat (Var v)
	; let sel_binds =  [NonRec b rhs | (b,rhs) <- sel_prs]
	; returnDs (mkDsLets sel_binds, WildPat (idType v)) }

tidy1 v (ListPat pats ty)
  = returnDs (idWrapper, unLoc list_ConPat)
  where
    list_ty     = mkListTy ty
    list_ConPat = foldr (\ x y -> mkPrefixConPat consDataCon [x, y] list_ty)
	      	  	(mkNilPat list_ty)
	      	  	pats

-- Introduce fake parallel array constructors to be able to handle parallel
-- arrays with the existing machinery for constructor pattern
tidy1 v (PArrPat pats ty)
  = returnDs (idWrapper, unLoc parrConPat)
  where
    arity      = length pats
    parrConPat = mkPrefixConPat (parrFakeCon arity) pats (mkPArrTy ty)

tidy1 v (TuplePat pats boxity ty)
  = returnDs (idWrapper, unLoc tuple_ConPat)
  where
    arity = length pats
    tuple_ConPat = mkPrefixConPat (tupleCon boxity arity) pats ty

tidy1 v (DictPat dicts methods)
  = case num_of_d_and_ms of
	0 -> tidy1 v (TuplePat [] Boxed unitTy) 
	1 -> tidy1 v (unLoc (head dict_and_method_pats))
	_ -> tidy1 v (mkVanillaTuplePat dict_and_method_pats Boxed)
  where
    num_of_d_and_ms	 = length dicts + length methods
    dict_and_method_pats = map nlVarPat (dicts ++ methods)

-- LitPats: we *might* be able to replace these w/ a simpler form
tidy1 v (LitPat lit)
  = returnDs (idWrapper, tidyLitPat lit)

-- NPats: we *might* be able to replace these w/ a simpler form
tidy1 v (NPat lit mb_neg eq lit_ty)
  = returnDs (idWrapper, tidyNPat lit mb_neg eq lit_ty)

-- Everything else goes through unchanged...

tidy1 v non_interesting_pat
  = returnDs (idWrapper, non_interesting_pat)
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
matchWrapper :: HsMatchContext Name	-- For shadowing warning messages
	     -> MatchGroup Id		-- Matches being desugared
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
matchWrapper ctxt (MatchGroup matches match_ty)
  = do	{ eqns_info   <- mapM mk_eqn_info matches
	; new_vars    <- selectMatchVars arg_pats
	; result_expr <- matchEquations ctxt new_vars eqns_info rhs_ty
	; return (new_vars, result_expr) }
  where
    arg_pats    = map unLoc (hsLMatchPats (head matches))
    n_pats	= length arg_pats
    (_, rhs_ty) = splitFunTysN n_pats match_ty

    mk_eqn_info (L _ (Match pats _ grhss))
      = do { let upats = map unLoc pats
	   ; match_result <- dsGRHSs ctxt upats grhss rhs_ty
	   ; return (EqnInfo { eqn_pats = upats, eqn_rhs  = match_result}) }


matchEquations  :: HsMatchContext Name
		-> [Id]	-> [EquationInfo] -> Type
		-> DsM CoreExpr
matchEquations ctxt vars eqns_info rhs_ty
  = do	{ dflags <- getDOptsDs
	; locn   <- getSrcSpanDs
	; let   ds_ctxt      = DsMatchContext ctxt locn
		error_string = matchContextErrString ctxt

	; match_result <- match_fun dflags ds_ctxt vars rhs_ty eqns_info

	; fail_expr <- mkErrorAppDs pAT_ERROR_ID rhs_ty error_string
	; extractMatchResult match_result fail_expr }
  where 
    match_fun dflags ds_ctxt
       = case ctxt of 
           LambdaExpr | dopt Opt_WarnSimplePatterns dflags -> matchCheck ds_ctxt
                      | otherwise                          -> match
           _                                               -> matchCheck ds_ctxt
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
	    -> HsMatchContext Name	-- Match kind
	    -> LPat Id			-- Pattern it should match
	    -> CoreExpr			-- Return this if it matches
	    -> CoreExpr			-- Return this if it doesn't
	    -> DsM CoreExpr

matchSimply scrut hs_ctx pat result_expr fail_expr
  = let
      match_result = cantFailMatchResult result_expr
      rhs_ty	   = exprType fail_expr
	-- Use exprType of fail_expr, because won't refine in the case of failure!
    in 
    matchSinglePat scrut hs_ctx pat rhs_ty match_result	`thenDs` \ match_result' ->
    extractMatchResult match_result' fail_expr


matchSinglePat :: CoreExpr -> HsMatchContext Name -> LPat Id
	       -> Type -> MatchResult -> DsM MatchResult
matchSinglePat (Var var) hs_ctx (L _ pat) ty match_result
  = getDOptsDs				`thenDs` \ dflags ->
    getSrcSpanDs			`thenDs` \ locn ->
    let
	match_fn dflags
           | dopt Opt_WarnSimplePatterns dflags = matchCheck ds_ctx
  	   | otherwise	       		        = match
	   where
	     ds_ctx = DsMatchContext hs_ctx locn
    in
    match_fn dflags [var] ty [EqnInfo { eqn_pats = [pat], eqn_rhs  = match_result }]

matchSinglePat scrut hs_ctx pat ty match_result
  = selectSimpleMatchVarL pat 		 		`thenDs` \ var ->
    matchSinglePat (Var var) hs_ctx pat ty match_result	`thenDs` \ match_result' ->
    returnDs (adjustMatchResult (bindNonRec var scrut) match_result')
\end{code}


%************************************************************************
%*									*
		Pattern classification
%*									*
%************************************************************************

\begin{code}
data PatGroup
  = PgAny		-- Immediate match: variables, wildcards, 
			--		    lazy patterns
  | PgCon DataCon	-- Constructor patterns (incl list, tuple)
  | PgLit Literal	-- Literal patterns
  | PgN   Literal	-- Overloaded literals
  | PgNpK Literal	-- n+k patterns
  | PgBang		-- Bang patterns
  | PgCo Type		-- Coercion patterns; the type is the type
			--	of the pattern *inside*


groupEquations :: [EquationInfo] -> [[(PatGroup, EquationInfo)]]
groupEquations eqns
  = runs same_gp [(patGroup (firstPat eqn), eqn) | eqn <- eqns]
  where
    same_gp :: (PatGroup,EquationInfo) -> (PatGroup,EquationInfo) -> Bool
    (pg1,_) `same_gp` (pg2,_) = pg1 `sameGroup` pg2

subGroups :: [(PatGroup, EquationInfo)] -> [[EquationInfo]]
-- Input is a particular group.  The result sub-groups the 
-- equations by with particular constructor, literal etc they match.
-- The order may be swizzled, so the matching should be order-independent
subGroups groups = map (map snd) (equivClasses cmp groups)
  where
    (pg1, _) `cmp` (pg2, _) = pg1 `cmp_pg` pg2
    (PgCon c1) `cmp_pg` (PgCon c2) = c1 `compare` c2
    (PgLit l1) `cmp_pg` (PgLit l2) = l1 `compare` l2
    (PgN   l1) `cmp_pg` (PgN   l2) = l1 `compare` l2
	-- These are the only cases that are every sub-grouped

sameGroup :: PatGroup -> PatGroup -> Bool
-- Same group means that a single case expression 
-- or test will suffice to match both, *and* the order
-- of testing within the group is insignificant.
sameGroup PgAny      PgAny      = True
sameGroup PgBang     PgBang     = True
sameGroup (PgCon _)  (PgCon _)  = True		-- One case expression
sameGroup (PgLit _)  (PgLit _)  = True		-- One case expression
sameGroup (PgN l1)   (PgN l2)   = True		-- Needs conditionals
sameGroup (PgNpK l1) (PgNpK l2) = l1==l2	-- Order is significant
						-- See Note [Order of n+k]
sameGroup (PgCo	t1)  (PgCo t2)  = t1 `coreEqType` t2
	-- CoPats are in the same goup only if the type of the
	-- enclosed pattern is the same. The patterns outside the CoPat
	-- always have the same type, so this boils down to saying that
	-- the two coercions are identical.
sameGroup _          _          = False
 
patGroup :: Pat Id -> PatGroup
patGroup (WildPat {})       	      = PgAny
patGroup (BangPat {})       	      = PgBang  
patGroup (ConPatOut { pat_con = dc }) = PgCon (unLoc dc)
patGroup (LitPat lit)		      = PgLit (hsLitKey lit)
patGroup (NPat olit mb_neg _ _)	      = PgN   (hsOverLitKey olit (isJust mb_neg))
patGroup (NPlusKPat _ olit _ _)	      = PgNpK (hsOverLitKey olit False)
patGroup (CoPat _ p _)		      = PgCo  (hsPatType p)	-- Type of inner pattern
patGroup pat = pprPanic "patGroup" (ppr pat)
\end{code}

Note [Order of n+k]
~~~~~~~~~~~~~~~~~~~
WATCH OUT!  Consider

	f (n+1) = ...
	f (n+2) = ...
	f (n+1) = ...

We can't group the first and third together, because the second may match 
the same thing as the first.  Contrast
	f 1 = ...
	f 2 = ...
	f 1 = ...
where we can group the first and third.  Hence we don't regard (n+1) and
(n+2) as part of the same group.
