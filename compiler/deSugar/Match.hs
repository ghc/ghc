{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


The @match@ function
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns   #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module Match ( match, matchEquations, matchWrapper, matchSimply
             , matchSinglePat, matchSinglePatVar ) where

#include "HsVersions.h"

import GhcPrelude

import {-#SOURCE#-} DsExpr (dsLExpr, dsSyntaxExpr)

import BasicTypes ( Origin(..) )
import DynFlags
import GHC.Hs
import TcHsSyn
import TcEvidence
import TcRnMonad
import GHC.HsToCore.PmCheck
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
import Coercion ( eqCoercion )
import TyCon( isNewTyCon )
import TysWiredIn
import SrcLoc
import Maybes
import Util
import Name
import Outputable
import BasicTypes ( isGenerated, il_value, fl_value )
import FastString
import Unique
import UniqDFM

import Control.Monad( when, unless )
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as Map

{-
************************************************************************
*                                                                      *
                The main matching function
*                                                                      *
************************************************************************

The function @match@ is basically the same as in the Wadler chapter
from "The Implementation of Functional Programming Languages",
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

There is a data type, @EquationInfo@, defined in module @DsMonad@.

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
\item
Now {\em unmix} the equations into {\em blocks} [w\/ local function
@match_groups@], in which the equations in a block all have the same
 match group.
(see ``the mixture rule'' in SLPJ).
\item
Call the right match variant on each block of equations; it will do the
appropriate thing for each kind of column-1 pattern.
\end{enumerate}

We are a little more paranoid about the ``empty rule'' (SLPJ, p.~87)
than the Wadler-chapter code for @match@ (p.~93, first @match@ clause).
And gluing the ``success expressions'' together isn't quite so pretty.

This  @match@ uses @tidyEqnInfo@
to get `as'- and `twiddle'-patterns out of the way (tidying), before
applying ``the mixture rule'' (SLPJ, p.~88) [which really {\em
un}mixes the equations], producing a list of equation-info
blocks, each block having as its first column patterns compatible with each other.

Note [Match Ids]
~~~~~~~~~~~~~~~~
Most of the matching functions take an Id or [Id] as argument.  This Id
is the scrutinee(s) of the match. The desugared expression may
sometimes use that Id in a local binding or as a case binder.  So it
should not have an External name; Lint rejects non-top-level binders
with External names (#13043).

See also Note [Localise pattern binders] in DsUtils
-}

type MatchId = Id   -- See Note [Match Ids]

match :: [MatchId] -- ^ Variables rep\'ing the exprs we\'re matching with. See Note [Match Ids]
      -> Type -- ^ Type of the case expression
      -> [EquationInfo] -- ^ Info about patterns, etc. (type synonym below)
      -> DsM MatchResult -- ^ Desugared result!

match [] ty eqns
  = ASSERT2( not (null eqns), ppr ty )
    return (foldr1 combineMatchResults match_results)
  where
    match_results = [ ASSERT( null (eqn_pats eqn) )
                      eqn_rhs eqn
                    | eqn <- eqns ]

match (v:vs) ty eqns    -- Eqns *can* be empty
  = ASSERT2( all (isInternalName . idName) vars, ppr vars )
    do  { dflags <- getDynFlags
                -- Tidy the first pattern, generating
                -- auxiliary bindings if necessary
        ; (aux_binds, tidy_eqns) <- mapAndUnzipM (tidyEqnInfo v) eqns
                -- Group the equations and match each group in turn
        ; let grouped = groupEquations dflags tidy_eqns

         -- print the view patterns that are commoned up to help debug
        ; whenDOptM Opt_D_dump_view_pattern_commoning (debug grouped)

        ; match_results <- match_groups grouped
        ; return (adjustMatchResult (foldr (.) id aux_binds) $
                  foldr1 combineMatchResults match_results) }
  where
    vars = v :| vs

    dropGroup :: Functor f => f (PatGroup,EquationInfo) -> f EquationInfo
    dropGroup = fmap snd

    match_groups :: [NonEmpty (PatGroup,EquationInfo)] -> DsM (NonEmpty MatchResult)
    -- Result list of [MatchResult] is always non-empty
    match_groups [] = matchEmpty v ty
    match_groups (g:gs) = mapM match_group $ g :| gs

    match_group :: NonEmpty (PatGroup,EquationInfo) -> DsM MatchResult
    match_group eqns@((group,_) :| _)
        = case group of
            PgCon {}  -> matchConFamily  vars ty (ne $ subGroupUniq [(c,e) | (PgCon c, e) <- eqns'])
            PgSyn {}  -> matchPatSyn     vars ty (dropGroup eqns)
            PgLit {}  -> matchLiterals   vars ty (ne $ subGroupOrd [(l,e) | (PgLit l, e) <- eqns'])
            PgAny     -> matchVariables  vars ty (dropGroup eqns)
            PgN {}    -> matchNPats      vars ty (dropGroup eqns)
            PgOverS {}-> matchNPats      vars ty (dropGroup eqns)
            PgNpK {}  -> matchNPlusKPats vars ty (dropGroup eqns)
            PgBang    -> matchBangs      vars ty (dropGroup eqns)
            PgCo {}   -> matchCoercion   vars ty (dropGroup eqns)
            PgView {} -> matchView       vars ty (dropGroup eqns)
            PgOverloadedList -> matchOverloadedList vars ty (dropGroup eqns)
      where eqns' = NEL.toList eqns
            ne l = case NEL.nonEmpty l of
              Just nel -> nel
              Nothing -> pprPanic "match match_group" $ text "Empty result should be impossible since input was non-empty"

    -- FIXME: we should also warn about view patterns that should be
    -- commoned up but are not

    -- print some stuff to see what's getting grouped
    -- use -dppr-debug to see the resolution of overloaded literals
    debug eqns =
        let gs = map (\group -> foldr (\ (p,_) -> \acc ->
                                           case p of PgView e _ -> e:acc
                                                     _ -> acc) [] group) eqns
            maybeWarn [] = return ()
            maybeWarn l = warnDs NoReason (vcat l)
        in
          maybeWarn $ (map (\g -> text "Putting these view expressions into the same case:" <+> (ppr g))
                       (filter (not . null) gs))

matchEmpty :: MatchId -> Type -> DsM (NonEmpty MatchResult)
-- See Note [Empty case expressions]
matchEmpty var res_ty
  = return [MatchResult CanFail mk_seq]
  where
    mk_seq fail = return $ mkWildCase (Var var) (idType var) res_ty
                                      [(DEFAULT, [], fail)]

matchVariables :: NonEmpty MatchId -> Type -> NonEmpty EquationInfo -> DsM MatchResult
-- Real true variables, just like in matchVar, SLPJ p 94
-- No binding to do: they'll all be wildcards by now (done in tidy)
matchVariables (_ :| vars) ty eqns = match vars ty $ NEL.toList $ shiftEqns eqns

matchBangs :: NonEmpty MatchId -> Type -> NonEmpty EquationInfo -> DsM MatchResult
matchBangs (var :| vars) ty eqns
  = do  { match_result <- match (var:vars) ty $ NEL.toList $
            decomposeFirstPat getBangPat <$> eqns
        ; return (mkEvalMatchResult var ty match_result) }

matchCoercion :: NonEmpty MatchId -> Type -> NonEmpty EquationInfo -> DsM MatchResult
-- Apply the coercion to the match variable and then match that
matchCoercion (var :| vars) ty (eqns@(eqn1 :| _))
  = do  { let XPat (CoPat co pat _) = firstPat eqn1
        ; let pat_ty' = hsPatType pat
        ; var' <- newUniqueId var pat_ty'
        ; match_result <- match (var':vars) ty $ NEL.toList $
            decomposeFirstPat getCoPat <$> eqns
        ; core_wrap <- dsHsWrapper co
        ; let bind = NonRec var' (core_wrap (Var var))
        ; return (mkCoLetMatchResult bind match_result) }

matchView :: NonEmpty MatchId -> Type -> NonEmpty EquationInfo -> DsM MatchResult
-- Apply the view function to the match variable and then match that
matchView (var :| vars) ty (eqns@(eqn1 :| _))
  = do  { -- we could pass in the expr from the PgView,
         -- but this needs to extract the pat anyway
         -- to figure out the type of the fresh variable
         let ViewPat _ viewExpr (L _ pat) = firstPat eqn1
         -- do the rest of the compilation
        ; let pat_ty' = hsPatType pat
        ; var' <- newUniqueId var pat_ty'
        ; match_result <- match (var':vars) ty $ NEL.toList $
            decomposeFirstPat getViewPat <$> eqns
         -- compile the view expressions
        ; viewExpr' <- dsLExpr viewExpr
        ; return (mkViewMatchResult var'
                    (mkCoreAppDs (text "matchView") viewExpr' (Var var))
                    match_result) }

matchOverloadedList :: NonEmpty MatchId -> Type -> NonEmpty EquationInfo -> DsM MatchResult
matchOverloadedList (var :| vars) ty (eqns@(eqn1 :| _))
-- Since overloaded list patterns are treated as view patterns,
-- the code is roughly the same as for matchView
  = do { let ListPat (ListPatTc elt_ty (Just (_,e))) _ = firstPat eqn1
       ; var' <- newUniqueId var (mkListTy elt_ty)  -- we construct the overall type by hand
       ; match_result <- match (var':vars) ty $ NEL.toList $
           decomposeFirstPat getOLPat <$> eqns -- getOLPat builds the pattern inside as a non-overloaded version of the overloaded list pattern
       ; e' <- dsSyntaxExpr e [Var var]
       ; return (mkViewMatchResult var' e' match_result)
       }

-- decompose the first pattern and leave the rest alone
decomposeFirstPat :: (Pat GhcTc -> Pat GhcTc) -> EquationInfo -> EquationInfo
decomposeFirstPat extractpat (eqn@(EqnInfo { eqn_pats = pat : pats }))
        = eqn { eqn_pats = extractpat pat : pats}
decomposeFirstPat _ _ = panic "decomposeFirstPat"

getCoPat, getBangPat, getViewPat, getOLPat :: Pat GhcTc -> Pat GhcTc
getCoPat (XPat (CoPat _ pat _)) = pat
getCoPat _                   = panic "getCoPat"
getBangPat (BangPat _ pat  ) = unLoc pat
getBangPat _                 = panic "getBangPat"
getViewPat (ViewPat _ _ pat) = unLoc pat
getViewPat _                 = panic "getViewPat"
getOLPat (ListPat (ListPatTc ty (Just _)) pats)
        = ListPat (ListPatTc ty Nothing)  pats
getOLPat _                   = panic "getOLPat"

{-
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


************************************************************************
*                                                                      *
                Tidying patterns
*                                                                      *
************************************************************************

Tidy up the leftmost pattern in an @EquationInfo@, given the variable @v@
which will be scrutinised.

This makes desugaring the pattern match simpler by transforming some of
the patterns to simpler forms. (Tuples to Constructor Patterns)

Among other things in the resulting Pattern:
* Variables and irrefutable(lazy) patterns are replaced by Wildcards
* As patterns are replaced by the patterns they wrap.

The bindings created by the above patterns are put into the returned wrapper
instead.

This means a definition of the form:
  f x = rhs
when called with v get's desugared to the equivalent of:
  let x = v
  in
  f _ = rhs

The same principle holds for as patterns (@) and
irrefutable/lazy patterns (~).
In the case of irrefutable patterns the irrefutable pattern is pushed into
the binding.

Pattern Constructors which only represent syntactic sugar are converted into
their desugared representation.
This usually means converting them to Constructor patterns but for some
depends on enabled extensions. (Eg OverloadedLists)

GHC also tries to convert overloaded Literals into regular ones.

The result of this tidying is that the column of patterns will include
only these which can be assigned a PatternGroup (see patGroup).

-}

tidyEqnInfo :: Id -> EquationInfo
            -> DsM (DsWrapper, EquationInfo)
        -- DsM'd because of internal call to dsLHsBinds
        --      and mkSelectorBinds.
        -- "tidy1" does the interesting stuff, looking at
        -- one pattern and fiddling the list of bindings.
        --
        -- POST CONDITION: head pattern in the EqnInfo is
        --      one of these for which patGroup is defined.

tidyEqnInfo _ (EqnInfo { eqn_pats = [] })
  = panic "tidyEqnInfo"

tidyEqnInfo v eqn@(EqnInfo { eqn_pats = pat : pats, eqn_orig = orig })
  = do { (wrap, pat') <- tidy1 v orig pat
       ; return (wrap, eqn { eqn_pats = do pat' : pats }) }

tidy1 :: Id                  -- The Id being scrutinised
      -> Origin              -- Was this a pattern the user wrote?
      -> Pat GhcTc           -- The pattern against which it is to be matched
      -> DsM (DsWrapper,     -- Extra bindings to do before the match
              Pat GhcTc)     -- Equivalent pattern

-------------------------------------------------------
--      (pat', mr') = tidy1 v pat mr
-- tidies the *outer level only* of pat, giving pat'
-- It eliminates many pattern forms (as-patterns, variable patterns,
-- list patterns, etc) and returns any created bindings in the wrapper.

tidy1 v o (ParPat _ pat)      = tidy1 v o (unLoc pat)
tidy1 v o (SigPat _ pat _)    = tidy1 v o (unLoc pat)
tidy1 _ _ (WildPat ty)        = return (idDsWrapper, WildPat ty)
tidy1 v o (BangPat _ (L l p)) = tidy_bang_pat v o l p

        -- case v of { x -> mr[] }
        -- = case v of { _ -> let x=v in mr[] }
tidy1 v _ (VarPat _ (L _ var))
  = return (wrapBind var v, WildPat (idType var))

        -- case v of { x@p -> mr[] }
        -- = case v of { p -> let x=v in mr[] }
tidy1 v o (AsPat _ (L _ var) pat)
  = do  { (wrap, pat') <- tidy1 v o (unLoc pat)
        ; return (wrapBind var v . wrap, pat') }

{- now, here we handle lazy patterns:
    tidy1 v ~p bs = (v, v1 = case v of p -> v1 :
                        v2 = case v of p -> v2 : ... : bs )

    where the v_i's are the binders in the pattern.

    ToDo: in "v_i = ... -> v_i", are the v_i's really the same thing?

    The case expr for v_i is just: match [v] [(p, [], \ x -> Var v_i)] any_expr
-}

tidy1 v _ (LazyPat _ pat)
    -- This is a convenient place to check for unlifted types under a lazy pattern.
    -- Doing this check during type-checking is unsatisfactory because we may
    -- not fully know the zonked types yet. We sure do here.
  = do  { let unlifted_bndrs = filter (isUnliftedType . idType) (collectPatBinders pat)
        ; unless (null unlifted_bndrs) $
          putSrcSpanDs (getLoc pat) $
          errDs (hang (text "A lazy (~) pattern cannot bind variables of unlifted type." $$
                       text "Unlifted variables:")
                    2 (vcat (map (\id -> ppr id <+> dcolon <+> ppr (idType id))
                                 unlifted_bndrs)))

        ; (_,sel_prs) <- mkSelectorBinds [] pat (Var v)
        ; let sel_binds =  [NonRec b rhs | (b,rhs) <- sel_prs]
        ; return (mkCoreLets sel_binds, WildPat (idType v)) }

tidy1 _ _ (ListPat (ListPatTc ty Nothing) pats )
  = return (idDsWrapper, unLoc list_ConPat)
  where
    list_ConPat = foldr (\ x y -> mkPrefixConPat consDataCon [x, y] [ty])
                        (mkNilPat ty)
                        pats

tidy1 _ _ (TuplePat tys pats boxity)
  = return (idDsWrapper, unLoc tuple_ConPat)
  where
    arity = length pats
    tuple_ConPat = mkPrefixConPat (tupleDataCon boxity arity) pats tys

tidy1 _ _ (SumPat tys pat alt arity)
  = return (idDsWrapper, unLoc sum_ConPat)
  where
    sum_ConPat = mkPrefixConPat (sumDataCon alt arity) [pat] tys

-- LitPats: we *might* be able to replace these w/ a simpler form
tidy1 _ o (LitPat _ lit)
  = do { unless (isGenerated o) $
           warnAboutOverflowedLit lit
       ; return (idDsWrapper, tidyLitPat lit) }

-- NPats: we *might* be able to replace these w/ a simpler form
tidy1 _ o (NPat ty (L _ lit@OverLit { ol_val = v }) mb_neg eq)
  = do { unless (isGenerated o) $
           let lit' | Just _ <- mb_neg = lit{ ol_val = negateOverLitVal v }
                    | otherwise = lit
           in warnAboutOverflowedOverLit lit'
       ; return (idDsWrapper, tidyNPat lit mb_neg eq ty) }

-- NPlusKPat: we may want to warn about the literals
tidy1 _ o n@(NPlusKPat _ _ (L _ lit1) lit2 _ _)
  = do { unless (isGenerated o) $ do
           warnAboutOverflowedOverLit lit1
           warnAboutOverflowedOverLit lit2
       ; return (idDsWrapper, n) }

-- Everything else goes through unchanged...
tidy1 _ _ non_interesting_pat
  = return (idDsWrapper, non_interesting_pat)

--------------------
tidy_bang_pat :: Id -> Origin -> SrcSpan -> Pat GhcTc
              -> DsM (DsWrapper, Pat GhcTc)

-- Discard par/sig under a bang
tidy_bang_pat v o _ (ParPat _ (L l p)) = tidy_bang_pat v o l p
tidy_bang_pat v o _ (SigPat _ (L l p) _) = tidy_bang_pat v o l p

-- Push the bang-pattern inwards, in the hope that
-- it may disappear next time
tidy_bang_pat v o l (AsPat x v' p)
  = tidy1 v o (AsPat x v' (L l (BangPat noExtField p)))
tidy_bang_pat v o l (XPat (CoPat w p t))
  = tidy1 v o (XPat $ CoPat w (BangPat noExtField (L l p)) t)

-- Discard bang around strict pattern
tidy_bang_pat v o _ p@(LitPat {})    = tidy1 v o p
tidy_bang_pat v o _ p@(ListPat {})   = tidy1 v o p
tidy_bang_pat v o _ p@(TuplePat {})  = tidy1 v o p
tidy_bang_pat v o _ p@(SumPat {})    = tidy1 v o p

-- Data/newtype constructors
tidy_bang_pat v o l p@(ConPat { pat_con = L _ (RealDataCon dc)
                              , pat_args = args
                              , pat_con_ext = ConPatTc
                                { pat_arg_tys = arg_tys
                                }
                              })
  -- Newtypes: push bang inwards (#9844)
  =
    if isNewTyCon (dataConTyCon dc)
      then tidy1 v o (p { pat_args = push_bang_into_newtype_arg l ty args })
      else tidy1 v o p  -- Data types: discard the bang
    where
      (ty:_) = dataConInstArgTys dc arg_tys

-------------------
-- Default case, leave the bang there:
--    VarPat,
--    LazyPat,
--    WildPat,
--    ViewPat,
--    pattern synonyms (ConPatOut with PatSynCon)
--    NPat,
--    NPlusKPat
--
-- For LazyPat, remember that it's semantically like a VarPat
--  i.e.  !(~p) is not like ~p, or p!  (#8952)
--
-- NB: SigPatIn, ConPatIn should not happen

tidy_bang_pat _ _ l p = return (idDsWrapper, BangPat noExtField (L l p))

-------------------
push_bang_into_newtype_arg :: SrcSpan
                           -> Type -- The type of the argument we are pushing
                                   -- onto
                           -> HsConPatDetails GhcTc -> HsConPatDetails GhcTc
-- See Note [Bang patterns and newtypes]
-- We are transforming   !(N p)   into   (N !p)
push_bang_into_newtype_arg l _ty (PrefixCon (arg:args))
  = ASSERT( null args)
    PrefixCon [L l (BangPat noExtField arg)]
push_bang_into_newtype_arg l _ty (RecCon rf)
  | HsRecFields { rec_flds = L lf fld : flds } <- rf
  , HsRecField { hsRecFieldArg = arg } <- fld
  = ASSERT( null flds)
    RecCon (rf { rec_flds = [L lf (fld { hsRecFieldArg
                                           = L l (BangPat noExtField arg) })] })
push_bang_into_newtype_arg l ty (RecCon rf) -- If a user writes !(T {})
  | HsRecFields { rec_flds = [] } <- rf
  = PrefixCon [L l (BangPat noExtField (noLoc (WildPat ty)))]
push_bang_into_newtype_arg _ _ cd
  = pprPanic "push_bang_into_newtype_arg" (pprConArgs cd)

{-
Note [Bang patterns and newtypes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For the pattern  !(Just pat)  we can discard the bang, because
the pattern is strict anyway. But for !(N pat), where
  newtype NT = N Int
we definitely can't discard the bang.  #9844.

So what we do is to push the bang inwards, in the hope that it will
get discarded there.  So we transform
   !(N pat)   into    (N !pat)

But what if there is nothing to push the bang onto? In at least one instance
a user has written !(N {}) which we translate into (N !_). See #13215


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

************************************************************************
*                                                                      *
\subsubsection[improved-unmixing]{UNIMPLEMENTED idea for improved unmixing}
*                                                                      *
************************************************************************

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

************************************************************************
*                                                                      *
*  matchWrapper: a convenient way to call @match@                      *
*                                                                      *
************************************************************************
\subsection[matchWrapper]{@matchWrapper@: a convenient interface to @match@}

Calls to @match@ often involve similar (non-trivial) work; that work
is collected here, in @matchWrapper@.  This function takes as
arguments:
\begin{itemize}
\item
Typechecked @Matches@ (of a function definition, or a case or lambda
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
-}

matchWrapper
  :: HsMatchContext GhcRn              -- ^ For shadowing warning messages
  -> Maybe (LHsExpr GhcTc)             -- ^ Scrutinee. (Just scrut) for a case expr
                                       --      case scrut of { p1 -> e1 ... }
                                       --   (and in this case the MatchGroup will
                                       --    have all singleton patterns)
                                       --   Nothing for a function definition
                                       --      f p1 q1 = ...  -- No "scrutinee"
                                       --      f p2 q2 = ...  -- in this case
  -> MatchGroup GhcTc (LHsExpr GhcTc)  -- ^ Matches being desugared
  -> DsM ([Id], CoreExpr)              -- ^ Results (usually passed to 'match')

{-
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
-}

matchWrapper ctxt mb_scr (MG { mg_alts = L _ matches
                             , mg_ext = MatchGroupTc arg_tys rhs_ty
                             , mg_origin = origin })
  = do  { dflags <- getDynFlags
        ; locn   <- getSrcSpanDs

        ; new_vars    <- case matches of
                           []    -> mapM newSysLocalDsNoLP arg_tys
                           (m:_) -> selectMatchVars (map unLoc (hsLMatchPats m))

        ; eqns_info   <- mapM (mk_eqn_info new_vars) matches

        -- Pattern match check warnings for /this match-group/
        ; when (isMatchContextPmChecked dflags origin ctxt) $
            addScrutTmCs mb_scr new_vars $
            -- See Note [Type and Term Equality Propagation]
            checkMatches dflags (DsMatchContext ctxt locn) new_vars matches

        ; result_expr <- handleWarnings $
                         matchEquations ctxt new_vars eqns_info rhs_ty
        ; return (new_vars, result_expr) }
  where
    -- Called once per equation in the match, or alternative in the case
    mk_eqn_info vars (L _ (Match { m_pats = pats, m_grhss = grhss }))
      = do { dflags <- getDynFlags
           ; let upats = map (unLoc . decideBangHood dflags) pats
                 dicts = collectEvVarsPats upats

           ; match_result <-
              -- Extend the environment with knowledge about
              -- the matches before desugaring the RHS
              -- See Note [Type and Term Equality Propagation]
              applyWhen (needToRunPmCheck dflags origin)
                        (addTyCsDs dicts . addScrutTmCs mb_scr vars . addPatTmCs upats vars)
                        (dsGRHSs ctxt grhss rhs_ty)

           ; return (EqnInfo { eqn_pats = upats
                             , eqn_orig = FromSource
                             , eqn_rhs = match_result }) }
    mk_eqn_info _ (L _ (XMatch nec)) = noExtCon nec

    handleWarnings = if isGenerated origin
                     then discardWarningsDs
                     else id
matchWrapper _ _ (XMatchGroup nec) = noExtCon nec

matchEquations  :: HsMatchContext GhcRn
                -> [MatchId] -> [EquationInfo] -> Type
                -> DsM CoreExpr
matchEquations ctxt vars eqns_info rhs_ty
  = do  { let error_doc = matchContextErrString ctxt

        ; match_result <- match vars rhs_ty eqns_info

        ; fail_expr <- mkErrorAppDs pAT_ERROR_ID rhs_ty error_doc
        ; extractMatchResult match_result fail_expr }

{-
************************************************************************
*                                                                      *
\subsection[matchSimply]{@matchSimply@: match a single expression against a single pattern}
*                                                                      *
************************************************************************

@mkSimpleMatch@ is a wrapper for @match@ which deals with the
situation where we want to match a single expression against a single
pattern. It returns an expression.
-}

matchSimply :: CoreExpr                 -- ^ Scrutinee
            -> HsMatchContext GhcRn     -- ^ Match kind
            -> LPat GhcTc               -- ^ Pattern it should match
            -> CoreExpr                 -- ^ Return this if it matches
            -> CoreExpr                 -- ^ Return this if it doesn't
            -> DsM CoreExpr
-- Do not warn about incomplete patterns; see matchSinglePat comments
matchSimply scrut hs_ctx pat result_expr fail_expr = do
    let
      match_result = cantFailMatchResult result_expr
      rhs_ty       = exprType fail_expr
        -- Use exprType of fail_expr, because won't refine in the case of failure!
    match_result' <- matchSinglePat scrut hs_ctx pat rhs_ty match_result
    extractMatchResult match_result' fail_expr

matchSinglePat :: CoreExpr -> HsMatchContext GhcRn -> LPat GhcTc
               -> Type -> MatchResult -> DsM MatchResult
-- matchSinglePat ensures that the scrutinee is a variable
-- and then calls matchSinglePatVar
--
-- matchSinglePat does not warn about incomplete patterns
-- Used for things like [ e | pat <- stuff ], where
-- incomplete patterns are just fine

matchSinglePat (Var var) ctx pat ty match_result
  | not (isExternalName (idName var))
  = matchSinglePatVar var ctx pat ty match_result

matchSinglePat scrut hs_ctx pat ty match_result
  = do { var           <- selectSimpleMatchVarL pat
       ; match_result' <- matchSinglePatVar var hs_ctx pat ty match_result
       ; return (adjustMatchResult (bindNonRec var scrut) match_result') }

matchSinglePatVar :: Id   -- See Note [Match Ids]
                  -> HsMatchContext GhcRn -> LPat GhcTc
                  -> Type -> MatchResult -> DsM MatchResult
matchSinglePatVar var ctx pat ty match_result
  = ASSERT2( isInternalName (idName var), ppr var )
    do { dflags <- getDynFlags
       ; locn   <- getSrcSpanDs

                    -- Pattern match check warnings
       ; checkSingle dflags (DsMatchContext ctx locn) var (unLoc pat)

       ; let eqn_info = EqnInfo { eqn_pats = [unLoc (decideBangHood dflags pat)]
                                , eqn_orig = FromSource
                                , eqn_rhs  = match_result }
       ; match [var] ty [eqn_info] }


{-
************************************************************************
*                                                                      *
                Pattern classification
*                                                                      *
************************************************************************
-}

data PatGroup
  = PgAny               -- Immediate match: variables, wildcards,
                        --                  lazy patterns
  | PgCon DataCon       -- Constructor patterns (incl list, tuple)
  | PgSyn PatSyn [Type] -- See Note [Pattern synonym groups]
  | PgLit Literal       -- Literal patterns
  | PgN   Rational      -- Overloaded numeric literals;
                        -- see Note [Don't use Literal for PgN]
  | PgOverS FastString  -- Overloaded string literals
  | PgNpK Integer       -- n+k patterns
  | PgBang              -- Bang patterns
  | PgCo Type           -- Coercion patterns; the type is the type
                        --      of the pattern *inside*
  | PgView (LHsExpr GhcTc) -- view pattern (e -> p):
                        -- the LHsExpr is the expression e
           Type         -- the Type is the type of p (equivalently, the result type of e)
  | PgOverloadedList

{- Note [Don't use Literal for PgN]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Previously we had, as PatGroup constructors

  | ...
  | PgN   Literal       -- Overloaded literals
  | PgNpK Literal       -- n+k patterns
  | ...

But Literal is really supposed to represent an *unboxed* literal, like Int#.
We were sticking the literal from, say, an overloaded numeric literal pattern
into a LitInt constructor. This didn't really make sense; and we now have
the invariant that value in a LitInt must be in the range of the target
machine's Int# type, and an overloaded literal could meaningfully be larger.

Solution: For pattern grouping purposes, just store the literal directly in
the PgN constructor as a Rational if numeric, and add a PgOverStr constructor
for overloaded strings.
-}

groupEquations :: DynFlags -> [EquationInfo] -> [NonEmpty (PatGroup, EquationInfo)]
-- If the result is of form [g1, g2, g3],
-- (a) all the (pg,eq) pairs in g1 have the same pg
-- (b) none of the gi are empty
-- The ordering of equations is unchanged
groupEquations dflags eqns
  = NEL.groupBy same_gp $ [(patGroup dflags (firstPat eqn), eqn) | eqn <- eqns]
  -- comprehension on NonEmpty
  where
    same_gp :: (PatGroup,EquationInfo) -> (PatGroup,EquationInfo) -> Bool
    (pg1,_) `same_gp` (pg2,_) = pg1 `sameGroup` pg2

-- TODO Make subGroup1 using a NonEmptyMap
subGroup :: (m -> [NonEmpty EquationInfo]) -- Map.elems
         -> m -- Map.empty
         -> (a -> m -> Maybe (NonEmpty EquationInfo)) -- Map.lookup
         -> (a -> NonEmpty EquationInfo -> m -> m) -- Map.insert
         -> [(a, EquationInfo)] -> [NonEmpty EquationInfo]
-- Input is a particular group.  The result sub-groups the
-- equations by with particular constructor, literal etc they match.
-- Each sub-list in the result has the same PatGroup
-- See Note [Take care with pattern order]
-- Parameterized by map operations to allow different implementations
-- and constraints, eg. types without Ord instance.
subGroup elems empty lookup insert group
    = fmap NEL.reverse $ elems $ foldl' accumulate empty group
  where
    accumulate pg_map (pg, eqn)
      = case lookup pg pg_map of
          Just eqns -> insert pg (NEL.cons eqn eqns) pg_map
          Nothing   -> insert pg [eqn] pg_map
    -- pg_map :: Map a [EquationInfo]
    -- Equations seen so far in reverse order of appearance

subGroupOrd :: Ord a => [(a, EquationInfo)] -> [NonEmpty EquationInfo]
subGroupOrd = subGroup Map.elems Map.empty Map.lookup Map.insert

subGroupUniq :: Uniquable a => [(a, EquationInfo)] -> [NonEmpty EquationInfo]
subGroupUniq =
  subGroup eltsUDFM emptyUDFM (flip lookupUDFM) (\k v m -> addToUDFM m k v)

{- Note [Pattern synonym groups]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we see
  f (P a) = e1
  f (P b) = e2
    ...
where P is a pattern synonym, can we put (P a -> e1) and (P b -> e2) in the
same group?  We can if P is a constructor, but /not/ if P is a pattern synonym.
Consider (#11224)
   -- readMaybe :: Read a => String -> Maybe a
   pattern PRead :: Read a => () => a -> String
   pattern PRead a <- (readMaybe -> Just a)

   f (PRead (x::Int))  = e1
   f (PRead (y::Bool)) = e2
This is all fine: we match the string by trying to read an Int; if that
fails we try to read a Bool. But clearly we can't combine the two into a single
match.

Conclusion: we can combine when we invoke PRead /at the same type/.  Hence
in PgSyn we record the instantiating types, and use them in sameGroup.

Note [Take care with pattern order]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In the subGroup function we must be very careful about pattern re-ordering,
Consider the patterns [ (True, Nothing), (False, x), (True, y) ]
Then in bringing together the patterns for True, we must not
swap the Nothing and y!
-}

sameGroup :: PatGroup -> PatGroup -> Bool
-- Same group means that a single case expression
-- or test will suffice to match both, *and* the order
-- of testing within the group is insignificant.
sameGroup PgAny         PgAny         = True
sameGroup PgBang        PgBang        = True
sameGroup (PgCon _)     (PgCon _)     = True    -- One case expression
sameGroup (PgSyn p1 t1) (PgSyn p2 t2) = p1==p2 && eqTypes t1 t2
                                                -- eqTypes: See Note [Pattern synonym groups]
sameGroup (PgLit _)     (PgLit _)     = True    -- One case expression
sameGroup (PgN l1)      (PgN l2)      = l1==l2  -- Order is significant
sameGroup (PgOverS s1)  (PgOverS s2)  = s1==s2
sameGroup (PgNpK l1)    (PgNpK l2)    = l1==l2  -- See Note [Grouping overloaded literal patterns]
sameGroup (PgCo t1)     (PgCo t2)     = t1 `eqType` t2
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
viewLExprEq :: (LHsExpr GhcTc,Type) -> (LHsExpr GhcTc,Type) -> Bool
viewLExprEq (e1,_) (e2,_) = lexp e1 e2
  where
    lexp :: LHsExpr GhcTc -> LHsExpr GhcTc -> Bool
    lexp e e' = exp (unLoc e) (unLoc e')

    ---------
    exp :: HsExpr GhcTc -> HsExpr GhcTc -> Bool
    -- real comparison is on HsExpr's
    -- strip parens
    exp (HsPar _ (L _ e)) e'   = exp e e'
    exp e (HsPar _ (L _ e'))   = exp e e'
    -- because the expressions do not necessarily have the same type,
    -- we have to compare the wrappers
    exp (XExpr (HsWrap h e)) (XExpr (HsWrap  h' e')) = wrap h h' && exp e e'
    exp (HsVar _ i) (HsVar _ i') =  i == i'
    exp (HsConLikeOut _ c) (HsConLikeOut _ c') = c == c'
    -- the instance for IPName derives using the id, so this works if the
    -- above does
    exp (HsIPVar _ i) (HsIPVar _ i') = i == i'
    exp (HsOverLabel _ l x) (HsOverLabel _ l' x') = l == l' && x == x'
    exp (HsOverLit _ l) (HsOverLit _ l') =
        -- Overloaded lits are equal if they have the same type
        -- and the data is the same.
        -- this is coarser than comparing the SyntaxExpr's in l and l',
        -- which resolve the overloading (e.g., fromInteger 1),
        -- because these expressions get written as a bunch of different variables
        -- (presumably to improve sharing)
        eqType (overLitType l) (overLitType l') && l == l'
    exp (HsApp _ e1 e2) (HsApp _ e1' e2') = lexp e1 e1' && lexp e2 e2'
    -- the fixities have been straightened out by now, so it's safe
    -- to ignore them?
    exp (OpApp _ l o ri) (OpApp _ l' o' ri') =
        lexp l l' && lexp o o' && lexp ri ri'
    exp (NegApp _ e n) (NegApp _ e' n') = lexp e e' && syn_exp n n'
    exp (SectionL _ e1 e2) (SectionL _ e1' e2') =
        lexp e1 e1' && lexp e2 e2'
    exp (SectionR _ e1 e2) (SectionR _ e1' e2') =
        lexp e1 e1' && lexp e2 e2'
    exp (ExplicitTuple _ es1 _) (ExplicitTuple _ es2 _) =
        eq_list tup_arg es1 es2
    exp (ExplicitSum _ _ _ e) (ExplicitSum _ _ _ e') = lexp e e'
    exp (HsIf _ _ e e1 e2) (HsIf _ _ e' e1' e2') =
        lexp e e' && lexp e1 e1' && lexp e2 e2'

    -- Enhancement: could implement equality for more expressions
    --   if it seems useful
    -- But no need for HsLit, ExplicitList, ExplicitTuple,
    -- because they cannot be functions
    exp _ _  = False

    ---------
    syn_exp :: SyntaxExpr GhcTc -> SyntaxExpr GhcTc -> Bool
    syn_exp (SyntaxExprTc { syn_expr      = expr1
                          , syn_arg_wraps = arg_wraps1
                          , syn_res_wrap  = res_wrap1 })
            (SyntaxExprTc { syn_expr      = expr2
                          , syn_arg_wraps = arg_wraps2
                          , syn_res_wrap  = res_wrap2 })
      = exp expr1 expr2 &&
        and (zipWithEqual "viewLExprEq" wrap arg_wraps1 arg_wraps2) &&
        wrap res_wrap1 res_wrap2
    syn_exp NoSyntaxExprTc NoSyntaxExprTc = True
    syn_exp _              _              = False

    ---------
    tup_arg (L _ (Present _ e1)) (L _ (Present _ e2)) = lexp e1 e2
    tup_arg (L _ (Missing t1))   (L _ (Missing t2))   = eqType t1 t2
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
    wrap (WpFun w1 w2 _ _) (WpFun w1' w2' _ _) = wrap w1 w1' && wrap w2 w2'
    wrap (WpCast co)       (WpCast co')        = co `eqCoercion` co'
    wrap (WpEvApp et1)     (WpEvApp et2)       = et1 `ev_term` et2
    wrap (WpTyApp t)       (WpTyApp t')        = eqType t t'
    -- Enhancement: could implement equality for more wrappers
    --   if it seems useful (lams and lets)
    wrap _ _ = False

    ---------
    ev_term :: EvTerm -> EvTerm -> Bool
    ev_term (EvExpr (Var a)) (EvExpr  (Var b)) = a==b
    ev_term (EvExpr (Coercion a)) (EvExpr (Coercion b)) = a `eqCoercion` b
    ev_term _ _ = False

    ---------
    eq_list :: (a->a->Bool) -> [a] -> [a] -> Bool
    eq_list _  []     []     = True
    eq_list _  []     (_:_)  = False
    eq_list _  (_:_)  []     = False
    eq_list eq (x:xs) (y:ys) = eq x y && eq_list eq xs ys

patGroup :: DynFlags -> Pat GhcTc -> PatGroup
patGroup _ (ConPat { pat_con = L _ con
                   , pat_con_ext = ConPatTc { pat_arg_tys = tys }
                   })
 | RealDataCon dcon <- con              = PgCon dcon
 | PatSynCon psyn <- con                = PgSyn psyn tys
patGroup _ (WildPat {})                 = PgAny
patGroup _ (BangPat {})                 = PgBang
patGroup _ (NPat _ (L _ (OverLit {ol_val=oval})) mb_neg _) =
  case (oval, isJust mb_neg) of
   (HsIntegral   i, False) -> PgN (fromInteger (il_value i))
   (HsIntegral   i, True ) -> PgN (-fromInteger (il_value i))
   (HsFractional r, False) -> PgN (fl_value r)
   (HsFractional r, True ) -> PgN (-fl_value r)
   (HsIsString _ s, _) -> ASSERT(isNothing mb_neg)
                          PgOverS s
patGroup _ (NPlusKPat _ _ (L _ (OverLit {ol_val=oval})) _ _ _) =
  case oval of
   HsIntegral i -> PgNpK (il_value i)
   _ -> pprPanic "patGroup NPlusKPat" (ppr oval)
patGroup _ (XPat (CoPat _ p _))         = PgCo  (hsPatType p)
                                                    -- Type of innelexp pattern
patGroup _ (ViewPat _ expr p)           = PgView expr (hsPatType (unLoc p))
patGroup _ (ListPat (ListPatTc _ (Just _)) _) = PgOverloadedList
patGroup dflags (LitPat _ lit)          = PgLit (hsLitKey dflags lit)
patGroup _ pat                          = pprPanic "patGroup" (ppr pat)

{-
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
-}
