%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

The @match@ function

\begin{code}
{-# LANGUAGE CPP #-}

module Match ( match, matchEquations, matchWrapper, matchSimply, matchSinglePat ) where

#include "HsVersions.h"

import {-#SOURCE#-} DsExpr (dsLExpr, dsExpr)

import DynFlags
import HsSyn
import TcHsSyn
import TcEvidence
import TcRnMonad
import Check
import CoreSyn
import Literal
import CoreUtils
import MkCore
import DsMonad
import DsBinds
import DsGRHSs
import DsUtils
import Id
import ConLike
import DataCon
import PatSyn
import MatchCon
import MatchLit
import Type
import TysWiredIn
import ListSetOps
import SrcLoc
import Maybes
import Util
import Name
import Outputable
import BasicTypes ( boxityNormalTupleSort, isGenerated )
import FastString

import Control.Monad( when )
import qualified Data.Map as Map
\end{code}

This function is a wrapper of @match@, it must be called from all the parts where
it was called match, but only substitutes the first call, ....
if the associated flags are declared, warnings will be issued.
It can not be called matchWrapper because this name already exists :-(

JJCQ 30-Nov-1997

\begin{code}
matchCheck ::  DsMatchContext
            -> [Id]             -- Vars rep'ing the exprs we're matching with
            -> Type             -- Type of the case expression
            -> [EquationInfo]   -- Info about patterns, etc. (type synonym below)
            -> DsM MatchResult  -- Desugared result!

matchCheck ctx vars ty qs
  = do { dflags <- getDynFlags
       ; matchCheck_really dflags ctx vars ty qs }

matchCheck_really :: DynFlags
                  -> DsMatchContext
                  -> [Id]
                  -> Type
                  -> [EquationInfo]
                  -> DsM MatchResult
matchCheck_really dflags ctx@(DsMatchContext hs_ctx _) vars ty qs
  = do { when shadow (dsShadowWarn ctx eqns_shadow)
       ; when incomplete (dsIncompleteWarn ctx pats)
       ; match vars ty qs }
  where
    (pats, eqns_shadow) = check qs
    incomplete = incomplete_flag hs_ctx && (notNull pats)
    shadow     = wopt Opt_WarnOverlappingPatterns dflags
              && notNull eqns_shadow

    incomplete_flag :: HsMatchContext id -> Bool
    incomplete_flag (FunRhs {})   = wopt Opt_WarnIncompletePatterns dflags
    incomplete_flag CaseAlt       = wopt Opt_WarnIncompletePatterns dflags
    incomplete_flag IfAlt         = False

    incomplete_flag LambdaExpr    = wopt Opt_WarnIncompleteUniPatterns dflags
    incomplete_flag PatBindRhs    = wopt Opt_WarnIncompleteUniPatterns dflags
    incomplete_flag ProcExpr      = wopt Opt_WarnIncompleteUniPatterns dflags

    incomplete_flag RecUpd        = wopt Opt_WarnIncompletePatternsRecUpd dflags

    incomplete_flag ThPatSplice   = False
    incomplete_flag PatSyn        = False
    incomplete_flag ThPatQuote    = False
    incomplete_flag (StmtCtxt {}) = False  -- Don't warn about incomplete patterns
                                           -- in list comprehensions, pattern guards
                                           -- etc.  They are often *supposed* to be
                                           -- incomplete
\end{code}

This variable shows the maximum number of lines of output generated for warnings.
It will limit the number of patterns/equations displayed to@ maximum_output@.

(ToDo: add command-line option?)

\begin{code}
maximum_output :: Int
maximum_output = 4
\end{code}

The next two functions create the warning message.

\begin{code}
dsShadowWarn :: DsMatchContext -> [EquationInfo] -> DsM ()
dsShadowWarn ctx@(DsMatchContext kind loc) qs
  = putSrcSpanDs loc (warnDs warn)
  where
    warn | qs `lengthExceeds` maximum_output
         = pp_context ctx (ptext (sLit "are overlapped"))
                      (\ f -> vcat (map (ppr_eqn f kind) (take maximum_output qs)) $$
                      ptext (sLit "..."))
         | otherwise
         = pp_context ctx (ptext (sLit "are overlapped"))
                      (\ f -> vcat $ map (ppr_eqn f kind) qs)


dsIncompleteWarn :: DsMatchContext -> [ExhaustivePat] -> DsM ()
dsIncompleteWarn ctx@(DsMatchContext kind loc) pats
  = putSrcSpanDs loc (warnDs warn)
        where
          warn = pp_context ctx (ptext (sLit "are non-exhaustive"))
                            (\_ -> hang (ptext (sLit "Patterns not matched:"))
                                   4 ((vcat $ map (ppr_incomplete_pats kind)
                                                  (take maximum_output pats))
                                      $$ dots))

          dots | pats `lengthExceeds` maximum_output = ptext (sLit "...")
               | otherwise                           = empty

pp_context :: DsMatchContext -> SDoc -> ((SDoc -> SDoc) -> SDoc) -> SDoc
pp_context (DsMatchContext kind _loc) msg rest_of_msg_fun
  = vcat [ptext (sLit "Pattern match(es)") <+> msg,
          sep [ptext (sLit "In") <+> ppr_match <> char ':', nest 4 (rest_of_msg_fun pref)]]
  where
    (ppr_match, pref)
        = case kind of
             FunRhs fun _ -> (pprMatchContext kind, \ pp -> ppr fun <+> pp)
             _            -> (pprMatchContext kind, \ pp -> pp)

ppr_pats :: Outputable a => [a] -> SDoc
ppr_pats pats = sep (map ppr pats)

ppr_shadow_pats :: HsMatchContext Name -> [Pat Id] -> SDoc
ppr_shadow_pats kind pats
  = sep [ppr_pats pats, matchSeparator kind, ptext (sLit "...")]

ppr_incomplete_pats :: HsMatchContext Name -> ExhaustivePat -> SDoc
ppr_incomplete_pats _ (pats,[]) = ppr_pats pats
ppr_incomplete_pats _ (pats,constraints) =
                         sep [ppr_pats pats, ptext (sLit "with"),
                              sep (map ppr_constraint constraints)]

ppr_constraint :: (Name,[HsLit]) -> SDoc
ppr_constraint (var,pats) = sep [ppr var, ptext (sLit "`notElem`"), ppr pats]

ppr_eqn :: (SDoc -> SDoc) -> HsMatchContext Name -> EquationInfo -> SDoc
ppr_eqn prefixF kind eqn = prefixF (ppr_shadow_pats kind (eqn_pats eqn))
\end{code}


%************************************************************************
%*                                                                      *
                The main matching function
%*                                                                      *
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
Now {\em unmix} the equations into {\em blocks} [w\/ local function
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
match :: [Id]             -- Variables rep\'ing the exprs we\'re matching with
      -> Type             -- Type of the case expression
      -> [EquationInfo]   -- Info about patterns, etc. (type synonym below)
      -> DsM MatchResult  -- Desugared result!

match [] ty eqns
  = ASSERT2( not (null eqns), ppr ty )
    return (foldr1 combineMatchResults match_results)
  where
    match_results = [ ASSERT( null (eqn_pats eqn) )
                      eqn_rhs eqn
                    | eqn <- eqns ]

match vars@(v:_) ty eqns    -- Eqns *can* be empty
  = do  { dflags <- getDynFlags
        ;       -- Tidy the first pattern, generating
                -- auxiliary bindings if necessary
          (aux_binds, tidy_eqns) <- mapAndUnzipM (tidyEqnInfo v) eqns

                -- Group the equations and match each group in turn
        ; let grouped = groupEquations dflags tidy_eqns

         -- print the view patterns that are commoned up to help debug
        ; whenDOptM Opt_D_dump_view_pattern_commoning (debug grouped)

        ; match_results <- match_groups grouped
        ; return (adjustMatchResult (foldr (.) id aux_binds) $
                  foldr1 combineMatchResults match_results) }
  where
    dropGroup :: [(PatGroup,EquationInfo)] -> [EquationInfo]
    dropGroup = map snd

    match_groups :: [[(PatGroup,EquationInfo)]] -> DsM [MatchResult]
    -- Result list of [MatchResult] is always non-empty
    match_groups [] = matchEmpty v ty
    match_groups gs = mapM match_group gs

    match_group :: [(PatGroup,EquationInfo)] -> DsM MatchResult
    match_group [] = panic "match_group"
    match_group eqns@((group,_) : _)
        = case group of
            PgCon _    -> matchConFamily  vars ty (subGroup [(c,e) | (PgCon c, e) <- eqns])
            PgSyn _    -> matchPatSyn     vars ty (dropGroup eqns)
            PgLit _    -> matchLiterals   vars ty (subGroup [(l,e) | (PgLit l, e) <- eqns])
            PgAny      -> matchVariables  vars ty (dropGroup eqns)
            PgN _      -> matchNPats      vars ty (dropGroup eqns)
            PgNpK _    -> matchNPlusKPats vars ty (dropGroup eqns)
            PgBang     -> matchBangs      vars ty (dropGroup eqns)
            PgCo _     -> matchCoercion   vars ty (dropGroup eqns)
            PgView _ _ -> matchView       vars ty (dropGroup eqns)
            PgOverloadedList -> matchOverloadedList vars ty (dropGroup eqns)

    -- FIXME: we should also warn about view patterns that should be
    -- commoned up but are not

    -- print some stuff to see what's getting grouped
    -- use -dppr-debug to see the resolution of overloaded literals
    debug eqns =
        let gs = map (\group -> foldr (\ (p,_) -> \acc ->
                                           case p of PgView e _ -> e:acc
                                                     _ -> acc) [] group) eqns
            maybeWarn [] = return ()
            maybeWarn l = warnDs (vcat l)
        in
          maybeWarn $ (map (\g -> text "Putting these view expressions into the same case:" <+> (ppr g))
                       (filter (not . null) gs))

matchEmpty :: Id -> Type -> DsM [MatchResult]
-- See Note [Empty case expressions]
matchEmpty var res_ty
  = return [MatchResult CanFail mk_seq]
  where
    mk_seq fail = return $ mkWildCase (Var var) (idType var) res_ty
                                      [(DEFAULT, [], fail)]

matchVariables :: [Id] -> Type -> [EquationInfo] -> DsM MatchResult
-- Real true variables, just like in matchVar, SLPJ p 94
-- No binding to do: they'll all be wildcards by now (done in tidy)
matchVariables (_:vars) ty eqns = match vars ty (shiftEqns eqns)
matchVariables [] _ _ = panic "matchVariables"

matchBangs :: [Id] -> Type -> [EquationInfo] -> DsM MatchResult
matchBangs (var:vars) ty eqns
  = do  { match_result <- match (var:vars) ty $
                          map (decomposeFirstPat getBangPat) eqns
        ; return (mkEvalMatchResult var ty match_result) }
matchBangs [] _ _ = panic "matchBangs"

matchCoercion :: [Id] -> Type -> [EquationInfo] -> DsM MatchResult
-- Apply the coercion to the match variable and then match that
matchCoercion (var:vars) ty (eqns@(eqn1:_))
  = do  { let CoPat co pat _ = firstPat eqn1
        ; var' <- newUniqueId var (hsPatType pat)
        ; match_result <- match (var':vars) ty $
                          map (decomposeFirstPat getCoPat) eqns
        ; rhs' <- dsHsWrapper co (Var var)
        ; return (mkCoLetMatchResult (NonRec var' rhs') match_result) }
matchCoercion _ _ _ = panic "matchCoercion"

matchView :: [Id] -> Type -> [EquationInfo] -> DsM MatchResult
-- Apply the view function to the match variable and then match that
matchView (var:vars) ty (eqns@(eqn1:_))
  = do  { -- we could pass in the expr from the PgView,
         -- but this needs to extract the pat anyway
         -- to figure out the type of the fresh variable
         let ViewPat viewExpr (L _ pat) _ = firstPat eqn1
         -- do the rest of the compilation
        ; var' <- newUniqueId var (hsPatType pat)
        ; match_result <- match (var':vars) ty $
                          map (decomposeFirstPat getViewPat) eqns
         -- compile the view expressions
        ; viewExpr' <- dsLExpr viewExpr
        ; return (mkViewMatchResult var' viewExpr' var match_result) }
matchView _ _ _ = panic "matchView"

matchOverloadedList :: [Id] -> Type -> [EquationInfo] -> DsM MatchResult
matchOverloadedList (var:vars) ty (eqns@(eqn1:_))
-- Since overloaded list patterns are treated as view patterns,
-- the code is roughly the same as for matchView
  = do { let ListPat _ elt_ty (Just (_,e)) = firstPat eqn1
       ; var' <- newUniqueId var (mkListTy elt_ty)  -- we construct the overall type by hand
       ; match_result <- match (var':vars) ty $
                            map (decomposeFirstPat getOLPat) eqns -- getOLPat builds the pattern inside as a non-overloaded version of the overloaded list pattern
       ; e' <- dsExpr e
       ; return (mkViewMatchResult var' e' var match_result) }
matchOverloadedList _ _ _ = panic "matchOverloadedList"

-- decompose the first pattern and leave the rest alone
decomposeFirstPat :: (Pat Id -> Pat Id) -> EquationInfo -> EquationInfo
decomposeFirstPat extractpat (eqn@(EqnInfo { eqn_pats = pat : pats }))
        = eqn { eqn_pats = extractpat pat : pats}
decomposeFirstPat _ _ = panic "decomposeFirstPat"

getCoPat, getBangPat, getViewPat, getOLPat :: Pat Id -> Pat Id
getCoPat (CoPat _ pat _)     = pat
getCoPat _                   = panic "getCoPat"
getBangPat (BangPat pat  )   = unLoc pat
getBangPat _                 = panic "getBangPat"
getViewPat (ViewPat _ pat _) = unLoc pat
getViewPat _                 = panic "getViewPat"
getOLPat (ListPat pats ty (Just _)) = ListPat pats ty Nothing
getOLPat _                   = panic "getOLPat"
\end{code}

Note [Empty case alternatives]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The list of EquationInfo can be empty, arising from
    case x of {}   or    \case {}
In that situation we desugar to
    case x of { _ -> error "pattern match failure" }
The *desugarer* isn't certain whether there really should be no
alternatives, so it adds a default case, as it always does.  A later
pass may remove it if it's inaccessible.  (See also Note [Empty case
alternatives] in CoreSyn.)

We do *not* desugar simply to
   error "empty case"
or some such, because 'x' might be bound to (error "hello"), in which
case we want to see that "hello" exception, not (error "empty case").
See also Note [Case elimination: lifted case] in Simplify.


%************************************************************************
%*                                                                      *
                Tidying patterns
%*                                                                      *
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
Float,  Double, at least) are converted to unboxed form; e.g.,
\tr{(NPat (HsInt i) _ _)} is converted to:
\begin{verbatim}
(ConPat I# _ _ [LitPat (HsIntPrim i)])
\end{verbatim}
\end{description}

\begin{code}
tidyEqnInfo :: Id -> EquationInfo
            -> DsM (DsWrapper, EquationInfo)
        -- DsM'd because of internal call to dsLHsBinds
        --      and mkSelectorBinds.
        -- "tidy1" does the interesting stuff, looking at
        -- one pattern and fiddling the list of bindings.
        --
        -- POST CONDITION: head pattern in the EqnInfo is
        --      WildPat
        --      ConPat
        --      NPat
        --      LitPat
        --      NPlusKPat
        -- but no other

tidyEqnInfo _ (EqnInfo { eqn_pats = [] })
  = panic "tidyEqnInfo"

tidyEqnInfo v eqn@(EqnInfo { eqn_pats = pat : pats })
  = do { (wrap, pat') <- tidy1 v pat
       ; return (wrap, eqn { eqn_pats = do pat' : pats }) }

tidy1 :: Id               -- The Id being scrutinised
      -> Pat Id           -- The pattern against which it is to be matched
      -> DsM (DsWrapper,  -- Extra bindings to do before the match
              Pat Id)     -- Equivalent pattern

-------------------------------------------------------
--      (pat', mr') = tidy1 v pat mr
-- tidies the *outer level only* of pat, giving pat'
-- It eliminates many pattern forms (as-patterns, variable patterns,
-- list patterns, etc) yielding one of:
--      WildPat
--      ConPatOut
--      LitPat
--      NPat
--      NPlusKPat

tidy1 v (ParPat pat)      = tidy1 v (unLoc pat)
tidy1 v (SigPatOut pat _) = tidy1 v (unLoc pat)
tidy1 _ (WildPat ty)      = return (idDsWrapper, WildPat ty)
tidy1 v (BangPat (L l p)) = tidy_bang_pat v l p

        -- case v of { x -> mr[] }
        -- = case v of { _ -> let x=v in mr[] }
tidy1 v (VarPat var)
  = return (wrapBind var v, WildPat (idType var))

        -- case v of { x@p -> mr[] }
        -- = case v of { p -> let x=v in mr[] }
tidy1 v (AsPat (L _ var) pat)
  = do  { (wrap, pat') <- tidy1 v (unLoc pat)
        ; return (wrapBind var v . wrap, pat') }

{- now, here we handle lazy patterns:
    tidy1 v ~p bs = (v, v1 = case v of p -> v1 :
                        v2 = case v of p -> v2 : ... : bs )

    where the v_i's are the binders in the pattern.

    ToDo: in "v_i = ... -> v_i", are the v_i's really the same thing?

    The case expr for v_i is just: match [v] [(p, [], \ x -> Var v_i)] any_expr
-}

tidy1 v (LazyPat pat)
  = do  { sel_prs <- mkSelectorBinds [] pat (Var v)
        ; let sel_binds =  [NonRec b rhs | (b,rhs) <- sel_prs]
        ; return (mkCoreLets sel_binds, WildPat (idType v)) }

tidy1 _ (ListPat pats ty Nothing)
  = return (idDsWrapper, unLoc list_ConPat)
  where
    list_ty     = mkListTy ty
    list_ConPat = foldr (\ x y -> mkPrefixConPat consDataCon [x, y] list_ty)
                        (mkNilPat list_ty)
                        pats

-- Introduce fake parallel array constructors to be able to handle parallel
-- arrays with the existing machinery for constructor pattern
tidy1 _ (PArrPat pats ty)
  = return (idDsWrapper, unLoc parrConPat)
  where
    arity      = length pats
    parrConPat = mkPrefixConPat (parrFakeCon arity) pats (mkPArrTy ty)

tidy1 _ (TuplePat pats boxity ty)
  = return (idDsWrapper, unLoc tuple_ConPat)
  where
    arity = length pats
    tuple_ConPat = mkPrefixConPat (tupleCon (boxityNormalTupleSort boxity) arity) pats ty

-- LitPats: we *might* be able to replace these w/ a simpler form
tidy1 _ (LitPat lit)
  = return (idDsWrapper, tidyLitPat lit)

-- NPats: we *might* be able to replace these w/ a simpler form
tidy1 _ (NPat lit mb_neg eq)
  = return (idDsWrapper, tidyNPat tidyLitPat lit mb_neg eq)

-- Everything else goes through unchanged...

tidy1 _ non_interesting_pat
  = return (idDsWrapper, non_interesting_pat)

--------------------
tidy_bang_pat :: Id -> SrcSpan -> Pat Id -> DsM (DsWrapper, Pat Id)

-- Discard bang around strict pattern
tidy_bang_pat v _ p@(ListPat {})   = tidy1 v p
tidy_bang_pat v _ p@(TuplePat {})  = tidy1 v p
tidy_bang_pat v _ p@(PArrPat {})   = tidy1 v p
tidy_bang_pat v _ p@(ConPatOut {}) = tidy1 v p
tidy_bang_pat v _ p@(LitPat {})    = tidy1 v p

-- Discard par/sig under a bang
tidy_bang_pat v _ (ParPat (L l p))      = tidy_bang_pat v l p
tidy_bang_pat v _ (SigPatOut (L l p) _) = tidy_bang_pat v l p

-- Push the bang-pattern inwards, in the hope that
-- it may disappear next time
tidy_bang_pat v l (AsPat v' p)  = tidy1 v (AsPat v' (L l (BangPat p)))
tidy_bang_pat v l (CoPat w p t) = tidy1 v (CoPat w (BangPat (L l p)) t)

-- Default case, leave the bang there:
-- VarPat, LazyPat, WildPat, ViewPat, NPat, NPlusKPat
-- For LazyPat, remember that it's semantically like a VarPat
--  i.e.  !(~p) is not like ~p, or p!  (Trac #8952)

tidy_bang_pat _ l p = return (idDsWrapper, BangPat (L l p))
  -- NB: SigPatIn, ConPatIn should not happen
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
%*                                                                      *
\subsubsection[improved-unmixing]{UNIMPLEMENTED idea for improved unmixing}
%*                                                                      *
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
%*                                                                      *
%*  matchWrapper: a convenient way to call @match@                      *
%*                                                                      *
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
matchWrapper :: HsMatchContext Name         -- For shadowing warning messages
             -> MatchGroup Id (LHsExpr Id)  -- Matches being desugared
             -> DsM ([Id], CoreExpr)        -- Results
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
matchWrapper ctxt (MG { mg_alts = matches
                      , mg_arg_tys = arg_tys
                      , mg_res_ty = rhs_ty
                      , mg_origin = origin })
  = do  { eqns_info   <- mapM mk_eqn_info matches
        ; new_vars    <- case matches of
                           []    -> mapM newSysLocalDs arg_tys
                           (m:_) -> selectMatchVars (map unLoc (hsLMatchPats m))
        ; result_expr <- handleWarnings $
                         matchEquations ctxt new_vars eqns_info rhs_ty
        ; return (new_vars, result_expr) }
  where
    mk_eqn_info (L _ (Match pats _ grhss))
      = do { let upats = map unLoc pats
           ; match_result <- dsGRHSs ctxt upats grhss rhs_ty
           ; return (EqnInfo { eqn_pats = upats, eqn_rhs  = match_result}) }

    handleWarnings = if isGenerated origin
                     then discardWarningsDs
                     else id


matchEquations  :: HsMatchContext Name
                -> [Id] -> [EquationInfo] -> Type
                -> DsM CoreExpr
matchEquations ctxt vars eqns_info rhs_ty
  = do  { locn <- getSrcSpanDs
        ; let   ds_ctxt   = DsMatchContext ctxt locn
                error_doc = matchContextErrString ctxt

        ; match_result <- matchCheck ds_ctxt vars rhs_ty eqns_info

        ; fail_expr <- mkErrorAppDs pAT_ERROR_ID rhs_ty error_doc
        ; extractMatchResult match_result fail_expr }
\end{code}

%************************************************************************
%*                                                                      *
\subsection[matchSimply]{@matchSimply@: match a single expression against a single pattern}
%*                                                                      *
%************************************************************************

@mkSimpleMatch@ is a wrapper for @match@ which deals with the
situation where we want to match a single expression against a single
pattern. It returns an expression.

\begin{code}
matchSimply :: CoreExpr                 -- Scrutinee
            -> HsMatchContext Name      -- Match kind
            -> LPat Id                  -- Pattern it should match
            -> CoreExpr                 -- Return this if it matches
            -> CoreExpr                 -- Return this if it doesn't
            -> DsM CoreExpr
-- Do not warn about incomplete patterns; see matchSinglePat comments
matchSimply scrut hs_ctx pat result_expr fail_expr = do
    let
      match_result = cantFailMatchResult result_expr
      rhs_ty       = exprType fail_expr
        -- Use exprType of fail_expr, because won't refine in the case of failure!
    match_result' <- matchSinglePat scrut hs_ctx pat rhs_ty match_result
    extractMatchResult match_result' fail_expr

matchSinglePat :: CoreExpr -> HsMatchContext Name -> LPat Id
               -> Type -> MatchResult -> DsM MatchResult
-- Do not warn about incomplete patterns
-- Used for things like [ e | pat <- stuff ], where
-- incomplete patterns are just fine
matchSinglePat (Var var) ctx (L _ pat) ty match_result
  = do { locn <- getSrcSpanDs
       ; matchCheck (DsMatchContext ctx locn)
                    [var] ty
                    [EqnInfo { eqn_pats = [pat], eqn_rhs  = match_result }] }

matchSinglePat scrut hs_ctx pat ty match_result
  = do { var <- selectSimpleMatchVarL pat
       ; match_result' <- matchSinglePat (Var var) hs_ctx pat ty match_result
       ; return (adjustMatchResult (bindNonRec var scrut) match_result') }
\end{code}


%************************************************************************
%*                                                                      *
                Pattern classification
%*                                                                      *
%************************************************************************

\begin{code}
data PatGroup
  = PgAny               -- Immediate match: variables, wildcards,
                        --                  lazy patterns
  | PgCon DataCon       -- Constructor patterns (incl list, tuple)
  | PgSyn PatSyn
  | PgLit Literal       -- Literal patterns
  | PgN   Literal       -- Overloaded literals
  | PgNpK Literal       -- n+k patterns
  | PgBang              -- Bang patterns
  | PgCo Type           -- Coercion patterns; the type is the type
                        --      of the pattern *inside*
  | PgView (LHsExpr Id) -- view pattern (e -> p):
                        -- the LHsExpr is the expression e
           Type         -- the Type is the type of p (equivalently, the result type of e)
  | PgOverloadedList

groupEquations :: DynFlags -> [EquationInfo] -> [[(PatGroup, EquationInfo)]]
-- If the result is of form [g1, g2, g3],
-- (a) all the (pg,eq) pairs in g1 have the same pg
-- (b) none of the gi are empty
-- The ordering of equations is unchanged
groupEquations dflags eqns
  = runs same_gp [(patGroup dflags (firstPat eqn), eqn) | eqn <- eqns]
  where
    same_gp :: (PatGroup,EquationInfo) -> (PatGroup,EquationInfo) -> Bool
    (pg1,_) `same_gp` (pg2,_) = pg1 `sameGroup` pg2

subGroup :: Ord a => [(a, EquationInfo)] -> [[EquationInfo]]
-- Input is a particular group.  The result sub-groups the
-- equations by with particular constructor, literal etc they match.
-- Each sub-list in the result has the same PatGroup
-- See Note [Take care with pattern order]
subGroup group
    = map reverse $ Map.elems $ foldl accumulate Map.empty group
  where
    accumulate pg_map (pg, eqn)
      = case Map.lookup pg pg_map of
          Just eqns -> Map.insert pg (eqn:eqns) pg_map
          Nothing   -> Map.insert pg [eqn]      pg_map

    -- pg_map :: Map a [EquationInfo]
    -- Equations seen so far in reverse order of appearance
\end{code}

Note [Take care with pattern order]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In the subGroup function we must be very careful about pattern re-ordering,
Consider the patterns [ (True, Nothing), (False, x), (True, y) ]
Then in bringing together the patterns for True, we must not
swap the Nothing and y!


\begin{code}
sameGroup :: PatGroup -> PatGroup -> Bool
-- Same group means that a single case expression
-- or test will suffice to match both, *and* the order
-- of testing within the group is insignificant.
sameGroup PgAny      PgAny      = True
sameGroup PgBang     PgBang     = True
sameGroup (PgCon _)  (PgCon _)  = True          -- One case expression
sameGroup (PgSyn p1) (PgSyn p2) = p1==p2
sameGroup (PgLit _)  (PgLit _)  = True          -- One case expression
sameGroup (PgN l1)   (PgN l2)   = l1==l2        -- Order is significant
sameGroup (PgNpK l1) (PgNpK l2) = l1==l2        -- See Note [Grouping overloaded literal patterns]
sameGroup (PgCo t1)  (PgCo t2)  = t1 `eqType` t2
        -- CoPats are in the same goup only if the type of the
        -- enclosed pattern is the same. The patterns outside the CoPat
        -- always have the same type, so this boils down to saying that
        -- the two coercions are identical.
sameGroup (PgView e1 t1) (PgView e2 t2) = viewLExprEq (e1,t1) (e2,t2)
       -- ViewPats are in the same group iff the expressions
       -- are "equal"---conservatively, we use syntactic equality
sameGroup _          _          = False

-- An approximation of syntactic equality used for determining when view
-- exprs are in the same group.
-- This function can always safely return false;
-- but doing so will result in the application of the view function being repeated.
--
-- Currently: compare applications of literals and variables
--            and anything else that we can do without involving other
--            HsSyn types in the recursion
--
-- NB we can't assume that the two view expressions have the same type.  Consider
--   f (e1 -> True) = ...
--   f (e2 -> "hi") = ...
viewLExprEq :: (LHsExpr Id,Type) -> (LHsExpr Id,Type) -> Bool
viewLExprEq (e1,_) (e2,_) = lexp e1 e2
  where
    lexp :: LHsExpr Id -> LHsExpr Id -> Bool
    lexp e e' = exp (unLoc e) (unLoc e')

    ---------
    exp :: HsExpr Id -> HsExpr Id -> Bool
    -- real comparison is on HsExpr's
    -- strip parens
    exp (HsPar (L _ e)) e'   = exp e e'
    exp e (HsPar (L _ e'))   = exp e e'
    -- because the expressions do not necessarily have the same type,
    -- we have to compare the wrappers
    exp (HsWrap h e) (HsWrap h' e') = wrap h h' && exp e e'
    exp (HsVar i) (HsVar i') =  i == i'
    -- the instance for IPName derives using the id, so this works if the
    -- above does
    exp (HsIPVar i) (HsIPVar i') = i == i'
    exp (HsOverLit l) (HsOverLit l') =
        -- Overloaded lits are equal if they have the same type
        -- and the data is the same.
        -- this is coarser than comparing the SyntaxExpr's in l and l',
        -- which resolve the overloading (e.g., fromInteger 1),
        -- because these expressions get written as a bunch of different variables
        -- (presumably to improve sharing)
        eqType (overLitType l) (overLitType l') && l == l'
    exp (HsApp e1 e2) (HsApp e1' e2') = lexp e1 e1' && lexp e2 e2'
    -- the fixities have been straightened out by now, so it's safe
    -- to ignore them?
    exp (OpApp l o _ ri) (OpApp l' o' _ ri') =
        lexp l l' && lexp o o' && lexp ri ri'
    exp (NegApp e n) (NegApp e' n') = lexp e e' && exp n n'
    exp (SectionL e1 e2) (SectionL e1' e2') =
        lexp e1 e1' && lexp e2 e2'
    exp (SectionR e1 e2) (SectionR e1' e2') =
        lexp e1 e1' && lexp e2 e2'
    exp (ExplicitTuple es1 _) (ExplicitTuple es2 _) =
        eq_list tup_arg es1 es2
    exp (HsIf _ e e1 e2) (HsIf _ e' e1' e2') =
        lexp e e' && lexp e1 e1' && lexp e2 e2'

    -- Enhancement: could implement equality for more expressions
    --   if it seems useful
    -- But no need for HsLit, ExplicitList, ExplicitTuple,
    -- because they cannot be functions
    exp _ _  = False

    ---------
    tup_arg (Present e1) (Present e2) = lexp e1 e2
    tup_arg (Missing t1) (Missing t2) = eqType t1 t2
    tup_arg _ _ = False

    ---------
    wrap :: HsWrapper -> HsWrapper -> Bool
    -- Conservative, in that it demands that wrappers be
    -- syntactically identical and doesn't look under binders
    --
    -- Coarser notions of equality are possible
    -- (e.g., reassociating compositions,
    --        equating different ways of writing a coercion)
    wrap WpHole WpHole = True
    wrap (WpCompose w1 w2) (WpCompose w1' w2') = wrap w1 w1' && wrap w2 w2'
    wrap (WpCast co)       (WpCast co')        = co `eq_co` co'
    wrap (WpEvApp et1)     (WpEvApp et2)       = et1 `ev_term` et2
    wrap (WpTyApp t)       (WpTyApp t')        = eqType t t'
    -- Enhancement: could implement equality for more wrappers
    --   if it seems useful (lams and lets)
    wrap _ _ = False

    ---------
    ev_term :: EvTerm -> EvTerm -> Bool
    ev_term (EvId a)       (EvId b)       = a==b
    ev_term (EvCoercion a) (EvCoercion b) = a `eq_co` b
    ev_term _ _ = False

    ---------
    eq_list :: (a->a->Bool) -> [a] -> [a] -> Bool
    eq_list _  []     []     = True
    eq_list _  []     (_:_)  = False
    eq_list _  (_:_)  []     = False
    eq_list eq (x:xs) (y:ys) = eq x y && eq_list eq xs ys

    ---------
    eq_co :: TcCoercion -> TcCoercion -> Bool
    -- Just some simple cases (should the r1 == r2 rather be an ASSERT?)
    eq_co (TcRefl r1 t1)             (TcRefl r2 t2)             = r1 == r2 && eqType t1 t2
    eq_co (TcCoVarCo v1)             (TcCoVarCo v2)             = v1==v2
    eq_co (TcSymCo co1)              (TcSymCo co2)              = co1 `eq_co` co2
    eq_co (TcTyConAppCo r1 tc1 cos1) (TcTyConAppCo r2 tc2 cos2) = r1 == r2 && tc1==tc2 && eq_list eq_co cos1 cos2
    eq_co _ _ = False

patGroup :: DynFlags -> Pat Id -> PatGroup
patGroup _      (WildPat {})                  = PgAny
patGroup _      (BangPat {})                  = PgBang
patGroup _      (ConPatOut { pat_con = con }) = case unLoc con of
    RealDataCon dcon -> PgCon dcon
    PatSynCon psyn -> PgSyn psyn
patGroup dflags (LitPat lit)                  = PgLit (hsLitKey dflags lit)
patGroup _      (NPat olit mb_neg _)          = PgN   (hsOverLitKey olit (isJust mb_neg))
patGroup _      (NPlusKPat _ olit _ _)        = PgNpK (hsOverLitKey olit False)
patGroup _      (CoPat _ p _)                 = PgCo  (hsPatType p) -- Type of innelexp pattern
patGroup _      (ViewPat expr p _)            = PgView expr (hsPatType (unLoc p))
patGroup _      (ListPat _ _ (Just _))        = PgOverloadedList
patGroup _      pat                           = pprPanic "patGroup" (ppr pat)
\end{code}

Note [Grouping overloaded literal patterns]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
WATCH OUT!  Consider

        f (n+1) = ...
        f (n+2) = ...
        f (n+1) = ...

We can't group the first and third together, because the second may match
the same thing as the first.  Same goes for *overloaded* literal patterns
        f 1 True = ...
        f 2 False = ...
        f 1 False = ...
If the first arg matches '1' but the second does not match 'True', we
cannot jump to the third equation!  Because the same argument might
match '2'!
Hence we don't regard 1 and 2, or (n+1) and (n+2), as part of the same group.

