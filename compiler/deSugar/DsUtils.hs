{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


Utilities for desugaring

This module exports some utility functions of no great interest.
-}

{-# LANGUAGE CPP #-}

-- | Utility functions for constructing Core syntax, principally for desugaring
module DsUtils (
        EquationInfo(..),
        firstPat, shiftEqns,

        MatchResult(..), CanItFail(..), CaseAlt(..),
        cantFailMatchResult, alwaysFailMatchResult,
        extractMatchResult, combineMatchResults,
        adjustMatchResult,  adjustMatchResultDs,
        mkCoLetMatchResult, mkViewMatchResult, mkGuardedMatchResult,
        matchCanFail, mkEvalMatchResult,
        mkCoPrimCaseMatchResult, mkCoAlgCaseMatchResult, mkCoSynCaseMatchResult,
        wrapBind, wrapBinds,

        mkErrorAppDs, mkCoreAppDs, mkCoreAppsDs, mkCastDs,

        seqVar,

        -- LHs tuples
        mkLHsVarPatTup, mkLHsPatTup, mkVanillaTuplePat,
        mkBigLHsVarTupId, mkBigLHsTupId, mkBigLHsVarPatTupId, mkBigLHsPatTupId,

        mkSelectorBinds,

        selectSimpleMatchVarL, selectMatchVars, selectMatchVar,
        mkOptTickBox, mkBinaryTickBox, decideBangHood
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} Match  ( matchSimply )
import {-# SOURCE #-} DsExpr ( dsLExpr )

import HsSyn
import TcHsSyn
import TcType( tcSplitTyConApp )
import CoreSyn
import DsMonad

import CoreUtils
import MkCore
import MkId
import Id
import Literal
import TyCon
import DataCon
import PatSyn
import Type
import Coercion
import TysPrim
import TysWiredIn
import BasicTypes
import ConLike
import UniqSet
import UniqSupply
import Module
import PrelNames
import Name( isInternalName )
import Outputable
import SrcLoc
import Util
import DynFlags
import FastString
import qualified GHC.LanguageExtensions as LangExt

import TcEvidence

import Control.Monad    ( zipWithM )

{-
************************************************************************
*                                                                      *
\subsection{ Selecting match variables}
*                                                                      *
************************************************************************

We're about to match against some patterns.  We want to make some
@Ids@ to use as match variables.  If a pattern has an @Id@ readily at
hand, which should indeed be bound to the pattern as a whole, then use it;
otherwise, make one up.
-}

selectSimpleMatchVarL :: LPat GhcTc -> DsM Id
selectSimpleMatchVarL pat = selectMatchVar (unLoc pat)

-- (selectMatchVars ps tys) chooses variables of type tys
-- to use for matching ps against.  If the pattern is a variable,
-- we try to use that, to save inventing lots of fresh variables.
--
-- OLD, but interesting note:
--    But even if it is a variable, its type might not match.  Consider
--      data T a where
--        T1 :: Int -> T Int
--        T2 :: a   -> T a
--
--      f :: T a -> a -> Int
--      f (T1 i) (x::Int) = x
--      f (T2 i) (y::a)   = 0
--    Then we must not choose (x::Int) as the matching variable!
-- And nowadays we won't, because the (x::Int) will be wrapped in a CoPat

selectMatchVars :: [Pat GhcTc] -> DsM [Id]
selectMatchVars ps = mapM selectMatchVar ps

selectMatchVar :: Pat GhcTc -> DsM Id
selectMatchVar (BangPat pat) = selectMatchVar (unLoc pat)
selectMatchVar (LazyPat pat) = selectMatchVar (unLoc pat)
selectMatchVar (ParPat pat)  = selectMatchVar (unLoc pat)
selectMatchVar (VarPat var)  = return (localiseId (unLoc var))
                                  -- Note [Localise pattern binders]
selectMatchVar (AsPat var _) = return (unLoc var)
selectMatchVar other_pat     = newSysLocalDsNoLP (hsPatType other_pat)
                                  -- OK, better make up one...

{-
Note [Localise pattern binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider     module M where
               [Just a] = e
After renaming it looks like
             module M where
               [Just M.a] = e

We don't generalise, since it's a pattern binding, monomorphic, etc,
so after desugaring we may get something like
             M.a = case e of (v:_) ->
                   case v of Just M.a -> M.a
Notice the "M.a" in the pattern; after all, it was in the original
pattern.  However, after optimisation those pattern binders can become
let-binders, and then end up floated to top level.  They have a
different *unique* by then (the simplifier is good about maintaining
proper scoping), but it's BAD to have two top-level bindings with the
External Name M.a, because that turns into two linker symbols for M.a.
It's quite rare for this to actually *happen* -- the only case I know
of is tc003 compiled with the 'hpc' way -- but that only makes it
all the more annoying.

To avoid this, we craftily call 'localiseId' in the desugarer, which
simply turns the External Name for the Id into an Internal one, but
doesn't change the unique.  So the desugarer produces this:
             M.a{r8} = case e of (v:_) ->
                       case v of Just a{r8} -> M.a{r8}
The unique is still 'r8', but the binding site in the pattern
is now an Internal Name.  Now the simplifier's usual mechanisms
will propagate that Name to all the occurrence sites, as well as
un-shadowing it, so we'll get
             M.a{r8} = case e of (v:_) ->
                       case v of Just a{s77} -> a{s77}
In fact, even CoreSubst.simplOptExpr will do this, and simpleOptExpr
runs on the output of the desugarer, so all is well by the end of
the desugaring pass.


************************************************************************
*                                                                      *
* type synonym EquationInfo and access functions for its pieces        *
*                                                                      *
************************************************************************
\subsection[EquationInfo-synonym]{@EquationInfo@: a useful synonym}

The ``equation info'' used by @match@ is relatively complicated and
worthy of a type synonym and a few handy functions.
-}

firstPat :: EquationInfo -> Pat GhcTc
firstPat eqn = ASSERT( notNull (eqn_pats eqn) ) head (eqn_pats eqn)

shiftEqns :: [EquationInfo] -> [EquationInfo]
-- Drop the first pattern in each equation
shiftEqns eqns = [ eqn { eqn_pats = tail (eqn_pats eqn) } | eqn <- eqns ]

-- Functions on MatchResults

matchCanFail :: MatchResult -> Bool
matchCanFail (MatchResult CanFail _)  = True
matchCanFail (MatchResult CantFail _) = False

alwaysFailMatchResult :: MatchResult
alwaysFailMatchResult = MatchResult CanFail (\fail -> return fail)

cantFailMatchResult :: CoreExpr -> MatchResult
cantFailMatchResult expr = MatchResult CantFail (\_ -> return expr)

extractMatchResult :: MatchResult -> CoreExpr -> DsM CoreExpr
extractMatchResult (MatchResult CantFail match_fn) _
  = match_fn (error "It can't fail!")

extractMatchResult (MatchResult CanFail match_fn) fail_expr = do
    (fail_bind, if_it_fails) <- mkFailurePair fail_expr
    body <- match_fn if_it_fails
    return (mkCoreLet fail_bind body)


combineMatchResults :: MatchResult -> MatchResult -> MatchResult
combineMatchResults (MatchResult CanFail      body_fn1)
                    (MatchResult can_it_fail2 body_fn2)
  = MatchResult can_it_fail2 body_fn
  where
    body_fn fail = do body2 <- body_fn2 fail
                      (fail_bind, duplicatable_expr) <- mkFailurePair body2
                      body1 <- body_fn1 duplicatable_expr
                      return (Let fail_bind body1)

combineMatchResults match_result1@(MatchResult CantFail _) _
  = match_result1

adjustMatchResult :: DsWrapper -> MatchResult -> MatchResult
adjustMatchResult encl_fn (MatchResult can_it_fail body_fn)
  = MatchResult can_it_fail (\fail -> encl_fn <$> body_fn fail)

adjustMatchResultDs :: (CoreExpr -> DsM CoreExpr) -> MatchResult -> MatchResult
adjustMatchResultDs encl_fn (MatchResult can_it_fail body_fn)
  = MatchResult can_it_fail (\fail -> encl_fn =<< body_fn fail)

wrapBinds :: [(Var,Var)] -> CoreExpr -> CoreExpr
wrapBinds [] e = e
wrapBinds ((new,old):prs) e = wrapBind new old (wrapBinds prs e)

wrapBind :: Var -> Var -> CoreExpr -> CoreExpr
wrapBind new old body   -- NB: this function must deal with term
  | new==old    = body  -- variables, type variables or coercion variables
  | otherwise   = Let (NonRec new (varToCoreExpr old)) body

seqVar :: Var -> CoreExpr -> CoreExpr
seqVar var body = Case (Var var) var (exprType body)
                        [(DEFAULT, [], body)]

mkCoLetMatchResult :: CoreBind -> MatchResult -> MatchResult
mkCoLetMatchResult bind = adjustMatchResult (mkCoreLet bind)

-- (mkViewMatchResult var' viewExpr mr) makes the expression
-- let var' = viewExpr in mr
mkViewMatchResult :: Id -> CoreExpr -> MatchResult -> MatchResult
mkViewMatchResult var' viewExpr =
    adjustMatchResult (mkCoreLet (NonRec var' viewExpr))

mkEvalMatchResult :: Id -> Type -> MatchResult -> MatchResult
mkEvalMatchResult var ty
  = adjustMatchResult (\e -> Case (Var var) var ty [(DEFAULT, [], e)])

mkGuardedMatchResult :: CoreExpr -> MatchResult -> MatchResult
mkGuardedMatchResult pred_expr (MatchResult _ body_fn)
  = MatchResult CanFail (\fail -> do body <- body_fn fail
                                     return (mkIfThenElse pred_expr body fail))

mkCoPrimCaseMatchResult :: Id                  -- Scrutinee
                        -> Type                      -- Type of the case
                        -> [(Literal, MatchResult)]  -- Alternatives
                        -> MatchResult               -- Literals are all unlifted
mkCoPrimCaseMatchResult var ty match_alts
  = MatchResult CanFail mk_case
  where
    mk_case fail = do
        alts <- mapM (mk_alt fail) sorted_alts
        return (Case (Var var) var ty ((DEFAULT, [], fail) : alts))

    sorted_alts = sortWith fst match_alts       -- Right order for a Case
    mk_alt fail (lit, MatchResult _ body_fn)
       = ASSERT( not (litIsLifted lit) )
         do body <- body_fn fail
            return (LitAlt lit, [], body)

data CaseAlt a = MkCaseAlt{ alt_pat :: a,
                            alt_bndrs :: [Var],
                            alt_wrapper :: HsWrapper,
                            alt_result :: MatchResult }

mkCoAlgCaseMatchResult
  :: DynFlags
  -> Id                 -- Scrutinee
  -> Type               -- Type of exp
  -> [CaseAlt DataCon]  -- Alternatives (bndrs *include* tyvars, dicts)
  -> MatchResult
mkCoAlgCaseMatchResult dflags var ty match_alts
  | isNewtype  -- Newtype case; use a let
  = ASSERT( null (tail match_alts) && null (tail arg_ids1) )
    mkCoLetMatchResult (NonRec arg_id1 newtype_rhs) match_result1

  | isPArrFakeAlts match_alts
  = MatchResult CanFail $ mkPArrCase dflags var ty (sort_alts match_alts)
  | otherwise
  = mkDataConCase var ty match_alts
  where
    isNewtype = isNewTyCon (dataConTyCon (alt_pat alt1))

        -- [Interesting: because of GADTs, we can't rely on the type of
        --  the scrutinised Id to be sufficiently refined to have a TyCon in it]

    alt1@MkCaseAlt{ alt_bndrs = arg_ids1, alt_result = match_result1 }
      = ASSERT( notNull match_alts ) head match_alts
    -- Stuff for newtype
    arg_id1       = ASSERT( notNull arg_ids1 ) head arg_ids1
    var_ty        = idType var
    (tc, ty_args) = tcSplitTyConApp var_ty      -- Don't look through newtypes
                                                -- (not that splitTyConApp does, these days)
    newtype_rhs = unwrapNewTypeBody tc ty_args (Var var)

        --- Stuff for parallel arrays
        --
        -- Concerning `isPArrFakeAlts':
        --
        --  * it is *not* sufficient to just check the type of the type
        --   constructor, as we have to be careful not to confuse the real
        --   representation of parallel arrays with the fake constructors;
        --   moreover, a list of alternatives must not mix fake and real
        --   constructors (this is checked earlier on)
        --
        -- FIXME: We actually go through the whole list and make sure that
        --        either all or none of the constructors are fake parallel
        --        array constructors.  This is to spot equations that mix fake
        --        constructors with the real representation defined in
        --        `PrelPArr'.  It would be nicer to spot this situation
        --        earlier and raise a proper error message, but it can really
        --        only happen in `PrelPArr' anyway.
        --

    isPArrFakeAlts :: [CaseAlt DataCon] -> Bool
    isPArrFakeAlts [alt] = isPArrFakeCon (alt_pat alt)
    isPArrFakeAlts (alt:alts) =
      case (isPArrFakeCon (alt_pat alt), isPArrFakeAlts alts) of
        (True , True ) -> True
        (False, False) -> False
        _              -> panic "DsUtils: you may not mix `[:...:]' with `PArr' patterns"
    isPArrFakeAlts [] = panic "DsUtils: unexpectedly found an empty list of PArr fake alternatives"

mkCoSynCaseMatchResult :: Id -> Type -> CaseAlt PatSyn -> MatchResult
mkCoSynCaseMatchResult var ty alt = MatchResult CanFail $ mkPatSynCase var ty alt

sort_alts :: [CaseAlt DataCon] -> [CaseAlt DataCon]
sort_alts = sortWith (dataConTag . alt_pat)

mkPatSynCase :: Id -> Type -> CaseAlt PatSyn -> CoreExpr -> DsM CoreExpr
mkPatSynCase var ty alt fail = do
    matcher <- dsLExpr $ mkLHsWrap wrapper $
                         nlHsTyApp matcher [getRuntimeRep "mkPatSynCase" ty, ty]
    let MatchResult _ mkCont = match_result
    cont <- mkCoreLams bndrs <$> mkCont fail
    return $ mkCoreAppsDs (text "patsyn" <+> ppr var) matcher [Var var, ensure_unstrict cont, Lam voidArgId fail]
  where
    MkCaseAlt{ alt_pat = psyn,
               alt_bndrs = bndrs,
               alt_wrapper = wrapper,
               alt_result = match_result} = alt
    (matcher, needs_void_lam) = patSynMatcher psyn

    -- See Note [Matchers and builders for pattern synonyms] in PatSyns
    -- on these extra Void# arguments
    ensure_unstrict cont | needs_void_lam = Lam voidArgId cont
                         | otherwise      = cont

mkDataConCase :: Id -> Type -> [CaseAlt DataCon] -> MatchResult
mkDataConCase _   _  []            = panic "mkDataConCase: no alternatives"
mkDataConCase var ty alts@(alt1:_) = MatchResult fail_flag mk_case
  where
    con1          = alt_pat alt1
    tycon         = dataConTyCon con1
    data_cons     = tyConDataCons tycon
    match_results = map alt_result alts

    sorted_alts :: [CaseAlt DataCon]
    sorted_alts  = sort_alts alts

    var_ty       = idType var
    (_, ty_args) = tcSplitTyConApp var_ty -- Don't look through newtypes
                                          -- (not that splitTyConApp does, these days)

    mk_case :: CoreExpr -> DsM CoreExpr
    mk_case fail = do
        alts <- mapM (mk_alt fail) sorted_alts
        return $ mkWildCase (Var var) (idType var) ty (mk_default fail ++ alts)

    mk_alt :: CoreExpr -> CaseAlt DataCon -> DsM CoreAlt
    mk_alt fail MkCaseAlt{ alt_pat = con,
                           alt_bndrs = args,
                           alt_result = MatchResult _ body_fn }
      = do { body <- body_fn fail
           ; case dataConBoxer con of {
                Nothing -> return (DataAlt con, args, body) ;
                Just (DCB boxer) ->
        do { us <- newUniqueSupply
           ; let (rep_ids, binds) = initUs_ us (boxer ty_args args)
           ; return (DataAlt con, rep_ids, mkLets binds body) } } }

    mk_default :: CoreExpr -> [CoreAlt]
    mk_default fail | exhaustive_case = []
                    | otherwise       = [(DEFAULT, [], fail)]

    fail_flag :: CanItFail
    fail_flag | exhaustive_case
              = foldr orFail CantFail [can_it_fail | MatchResult can_it_fail _ <- match_results]
              | otherwise
              = CanFail

    mentioned_constructors = mkUniqSet $ map alt_pat alts
    un_mentioned_constructors
        = mkUniqSet data_cons `minusUniqSet` mentioned_constructors
    exhaustive_case = isEmptyUniqSet un_mentioned_constructors

--- Stuff for parallel arrays
--
--  * the following is to desugar cases over fake constructors for
--   parallel arrays, which are introduced by `tidy1' in the `PArrPat'
--   case
--
mkPArrCase :: DynFlags -> Id -> Type -> [CaseAlt DataCon] -> CoreExpr
           -> DsM CoreExpr
mkPArrCase dflags var ty sorted_alts fail = do
    lengthP <- dsDPHBuiltin lengthPVar
    alt <- unboxAlt
    return (mkWildCase (len lengthP) intTy ty [alt])
  where
    elemTy      = case splitTyConApp (idType var) of
        (_, [elemTy]) -> elemTy
        _             -> panic panicMsg
    panicMsg    = "DsUtils.mkCoAlgCaseMatchResult: not a parallel array?"
    len lengthP = mkApps (Var lengthP) [Type elemTy, Var var]
    --
    unboxAlt = do
        l      <- newSysLocalDs intPrimTy
        indexP <- dsDPHBuiltin indexPVar
        alts   <- mapM (mkAlt indexP) sorted_alts
        return (DataAlt intDataCon, [l], mkWildCase (Var l) intPrimTy ty (dft : alts))
      where
        dft  = (DEFAULT, [], fail)

    --
    -- each alternative matches one array length (corresponding to one
    -- fake array constructor), so the match is on a literal; each
    -- alternative's body is extended by a local binding for each
    -- constructor argument, which are bound to array elements starting
    -- with the first
    --
    mkAlt indexP alt@MkCaseAlt{alt_result = MatchResult _ bodyFun} = do
        body <- bodyFun fail
        return (LitAlt lit, [], mkCoreLets binds body)
      where
        lit   = MachInt $ toInteger (dataConSourceArity (alt_pat alt))
        binds = [NonRec arg (indexExpr i) | (i, arg) <- zip [1..] (alt_bndrs alt)]
        --
        indexExpr i = mkApps (Var indexP) [Type elemTy, Var var, mkIntExpr dflags i]

{-
************************************************************************
*                                                                      *
\subsection{Desugarer's versions of some Core functions}
*                                                                      *
************************************************************************
-}

mkErrorAppDs :: Id              -- The error function
             -> Type            -- Type to which it should be applied
             -> SDoc            -- The error message string to pass
             -> DsM CoreExpr

mkErrorAppDs err_id ty msg = do
    src_loc <- getSrcSpanDs
    dflags <- getDynFlags
    let
        full_msg = showSDoc dflags (hcat [ppr src_loc, vbar, msg])
        core_msg = Lit (mkMachString full_msg)
        -- mkMachString returns a result of type String#
    return (mkApps (Var err_id) [Type (getRuntimeRep "mkErrorAppDs" ty), Type ty, core_msg])

{-
'mkCoreAppDs' and 'mkCoreAppsDs' hand the special-case desugaring of 'seq'.

Note [Desugaring seq (1)]  cf Trac #1031
~~~~~~~~~~~~~~~~~~~~~~~~~
   f x y = x `seq` (y `seq` (# x,y #))

The [CoreSyn let/app invariant] means that, other things being equal, because
the argument to the outer 'seq' has an unlifted type, we'll use call-by-value thus:

   f x y = case (y `seq` (# x,y #)) of v -> x `seq` v

But that is bad for two reasons:
  (a) we now evaluate y before x, and
  (b) we can't bind v to an unboxed pair

Seq is very, very special!  So we recognise it right here, and desugar to
        case x of _ -> case y of _ -> (# x,y #)

Note [Desugaring seq (2)]  cf Trac #2273
~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   let chp = case b of { True -> fst x; False -> 0 }
   in chp `seq` ...chp...
Here the seq is designed to plug the space leak of retaining (snd x)
for too long.

If we rely on the ordinary inlining of seq, we'll get
   let chp = case b of { True -> fst x; False -> 0 }
   case chp of _ { I# -> ...chp... }

But since chp is cheap, and the case is an alluring contet, we'll
inline chp into the case scrutinee.  Now there is only one use of chp,
so we'll inline a second copy.  Alas, we've now ruined the purpose of
the seq, by re-introducing the space leak:
    case (case b of {True -> fst x; False -> 0}) of
      I# _ -> ...case b of {True -> fst x; False -> 0}...

We can try to avoid doing this by ensuring that the binder-swap in the
case happens, so we get his at an early stage:
   case chp of chp2 { I# -> ...chp2... }
But this is fragile.  The real culprit is the source program.  Perhaps we
should have said explicitly
   let !chp2 = chp in ...chp2...

But that's painful.  So the code here does a little hack to make seq
more robust: a saturated application of 'seq' is turned *directly* into
the case expression, thus:
   x  `seq` e2 ==> case x of x -> e2    -- Note shadowing!
   e1 `seq` e2 ==> case x of _ -> e2

So we desugar our example to:
   let chp = case b of { True -> fst x; False -> 0 }
   case chp of chp { I# -> ...chp... }
And now all is well.

The reason it's a hack is because if you define mySeq=seq, the hack
won't work on mySeq.

Note [Desugaring seq (3)] cf Trac #2409
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The isLocalId ensures that we don't turn
        True `seq` e
into
        case True of True { ... }
which stupidly tries to bind the datacon 'True'.
-}

-- NB: Make sure the argument is not levity polymorphic
mkCoreAppDs  :: SDoc -> CoreExpr -> CoreExpr -> CoreExpr
mkCoreAppDs _ (Var f `App` Type ty1 `App` Type ty2 `App` arg1) arg2
  | f `hasKey` seqIdKey            -- Note [Desugaring seq (1), (2)]
  = Case arg1 case_bndr ty2 [(DEFAULT,[],arg2)]
  where
    case_bndr = case arg1 of
                   Var v1 | isInternalName (idName v1)
                          -> v1        -- Note [Desugaring seq (2) and (3)]
                   _      -> mkWildValBinder ty1

mkCoreAppDs s fun arg = mkCoreApp s fun arg  -- The rest is done in MkCore

-- NB: No argument can be levity polymorphic
mkCoreAppsDs :: SDoc -> CoreExpr -> [CoreExpr] -> CoreExpr
mkCoreAppsDs s fun args = foldl (mkCoreAppDs s) fun args

mkCastDs :: CoreExpr -> Coercion -> CoreExpr
-- We define a desugarer-specific version of CoreUtils.mkCast,
-- because in the immediate output of the desugarer, we can have
-- apparently-mis-matched coercions:  E.g.
--     let a = b
--     in (x :: a) |> (co :: b ~ Int)
-- Lint know about type-bindings for let and does not complain
-- So here we do not make the assertion checks that we make in
-- CoreUtils.mkCast; and we do less peephole optimisation too
mkCastDs e co | isReflCo co = e
              | otherwise   = Cast e co

{-
************************************************************************
*                                                                      *
               Tuples and selector bindings
*                                                                      *
************************************************************************

This is used in various places to do with lazy patterns.
For each binder $b$ in the pattern, we create a binding:
\begin{verbatim}
    b = case v of pat' -> b'
\end{verbatim}
where @pat'@ is @pat@ with each binder @b@ cloned into @b'@.

ToDo: making these bindings should really depend on whether there's
much work to be done per binding.  If the pattern is complex, it
should be de-mangled once, into a tuple (and then selected from).
Otherwise the demangling can be in-line in the bindings (as here).

Boring!  Boring!  One error message per binder.  The above ToDo is
even more helpful.  Something very similar happens for pattern-bound
expressions.

Note [mkSelectorBinds]
~~~~~~~~~~~~~~~~~~~~~~
mkSelectorBinds is used to desugar a pattern binding {p = e},
in a binding group:
  let { ...; p = e; ... } in body
where p binds x,y (this list of binders can be empty).
There are two cases.

------ Special case (A) -------
  For a pattern that is just a variable,
     let !x = e in body
  ==>
     let x = e in x `seq` body
  So we return the binding, with 'x' as the variable to seq.

------ Special case (B) -------
  For a pattern that is essentially just a tuple:
      * A product type, so cannot fail
      * Only one level, so that
          - generating multiple matches is fine
          - seq'ing it evaluates the same as matching it
  Then instead we generate
       { v = e
       ; x = case v of p -> x
       ; y = case v of p -> y }
  with 'v' as the variable to force

------ General case (C) -------
  In the general case we generate these bindings:
       let { ...; p = e; ... } in body
  ==>
       let { t = case e of p -> (x,y)
           ; x = case t of (x,y) -> x
           ; y = case t of (x,y) -> y }
       in t `seq` body

  Note that we return 't' as the variable to force if the pattern
  is strict (i.e. with -XStrict or an outermost-bang-pattern)

  Note that (A) /includes/ the situation where

   * The pattern binds exactly one variable
        let !(Just (Just x) = e in body
     ==>
       let { t = case e of Just (Just v) -> Unit v
           ; v = case t of Unit v -> v }
       in t `seq` body
    The 'Unit' is a one-tuple; see Note [One-tuples] in TysWiredIn
    Note that forcing 't' makes the pattern match happen,
    but does not force 'v'.

  * The pattern binds no variables
        let !(True,False) = e in body
    ==>
        let t = case e of (True,False) -> ()
        in t `seq` body


------ Examples ----------
  *   !(_, (_, a)) = e
    ==>
      t = case e of (_, (_, a)) -> Unit a
      a = case t of Unit a -> a

    Note that
     - Forcing 't' will force the pattern to match fully;
       e.g. will diverge if (snd e) is bottom
     - But 'a' itself is not forced; it is wrapped in a one-tuple
       (see Note [One-tuples] in TysWiredIn)

  *   !(Just x) = e
    ==>
      t = case e of Just x -> Unit x
      x = case t of Unit x -> x

    Again, forcing 't' will fail if 'e' yields Nothing.

Note that even though this is rather general, the special cases
work out well:

* One binder, not -XStrict:

    let Just (Just v) = e in body
  ==>
    let t = case e of Just (Just v) -> Unit v
        v = case t of Unit v -> v
    in body
  ==>
    let v = case (case e of Just (Just v) -> Unit v) of
              Unit v -> v
    in body
  ==>
    let v = case e of Just (Just v) -> v
    in body

* Non-recursive, -XStrict
     let p = e in body
  ==>
     let { t = case e of p -> (x,y)
         ; x = case t of (x,y) -> x
         ; y = case t of (x,y) -> x }
     in t `seq` body
  ==> {inline seq, float x,y bindings inwards}
     let t = case e of p -> (x,y) in
     case t of t' ->
     let { x = case t' of (x,y) -> x
         ; y = case t' of (x,y) -> x } in
     body
  ==> {inline t, do case of case}
     case e of p ->
     let t = (x,y) in
     let { x = case t' of (x,y) -> x
         ; y = case t' of (x,y) -> x } in
     body
  ==> {case-cancellation, drop dead code}
     case e of p -> body

* Special case (B) is there to avoid fruitlessly taking the tuple
  apart and rebuilding it. For example, consider
     { K x y = e }
  where K is a product constructor.  Then general case (A) does:
     { t = case e of K x y -> (x,y)
     ; x = case t of (x,y) -> x
     ; y = case t of (x,y) -> y }
  In the lazy case we can't optimise out this fruitless taking apart
  and rebuilding.  Instead (B) builds
     { v = e
     ; x = case v of K x y -> x
     ; y = case v of K x y -> y }
  which is better.
-}

mkSelectorBinds :: [[Tickish Id]] -- ^ ticks to add, possibly
                -> LPat GhcTc     -- ^ The pattern
                -> CoreExpr       -- ^ Expression to which the pattern is bound
                -> DsM (Id,[(Id,CoreExpr)])
                -- ^ Id the rhs is bound to, for desugaring strict
                -- binds (see Note [Desugar Strict binds] in DsBinds)
                -- and all the desugared binds

mkSelectorBinds ticks pat val_expr
  | L _ (VarPat (L _ v)) <- pat'     -- Special case (A)
  = return (v, [(v, val_expr)])

  | is_flat_prod_lpat pat'           -- Special case (B)
  = do { let pat_ty = hsLPatType pat'
       ; val_var <- newSysLocalDsNoLP pat_ty

       ; let mk_bind tick bndr_var
               -- (mk_bind sv bv)  generates  bv = case sv of { pat -> bv }
               -- Remember, 'pat' binds 'bv'
               = do { rhs_expr <- matchSimply (Var val_var) PatBindRhs pat'
                                       (Var bndr_var)
                                       (Var bndr_var)  -- Neat hack
                      -- Neat hack: since 'pat' can't fail, the
                      -- "fail-expr" passed to matchSimply is not
                      -- used. But it /is/ used for its type, and for
                      -- that bndr_var is just the ticket.
                    ; return (bndr_var, mkOptTickBox tick rhs_expr) }

       ; binds <- zipWithM mk_bind ticks' binders
       ; return ( val_var, (val_var, val_expr) : binds) }

  | otherwise                          -- General case (C)
  = do { tuple_var  <- newSysLocalDs tuple_ty
       ; error_expr <- mkErrorAppDs iRREFUT_PAT_ERROR_ID tuple_ty (ppr pat')
       ; tuple_expr <- matchSimply val_expr PatBindRhs pat
                                   local_tuple error_expr
       ; let mk_tup_bind tick binder
               = (binder, mkOptTickBox tick $
                          mkTupleSelector1 local_binders binder
                                           tuple_var (Var tuple_var))
             tup_binds = zipWith mk_tup_bind ticks' binders
       ; return (tuple_var, (tuple_var, tuple_expr) : tup_binds) }
  where
    pat' = strip_bangs pat
           -- Strip the bangs before looking for case (A) or (B)
           -- The incoming pattern may well have a bang on it

    binders = collectPatBinders pat'
    ticks'  = ticks ++ repeat []

    local_binders = map localiseId binders      -- See Note [Localise pattern binders]
    local_tuple   = mkBigCoreVarTup1 binders
    tuple_ty      = exprType local_tuple

strip_bangs :: LPat a -> LPat a
-- Remove outermost bangs and parens
strip_bangs (L _ (ParPat p))  = strip_bangs p
strip_bangs (L _ (BangPat p)) = strip_bangs p
strip_bangs lp                = lp

is_flat_prod_lpat :: LPat a -> Bool
is_flat_prod_lpat p = is_flat_prod_pat (unLoc p)

is_flat_prod_pat :: Pat a -> Bool
is_flat_prod_pat (ParPat p)            = is_flat_prod_lpat p
is_flat_prod_pat (TuplePat ps Boxed _) = all is_triv_lpat ps
is_flat_prod_pat (ConPatOut { pat_con = L _ pcon, pat_args = ps})
  | RealDataCon con <- pcon
  , isProductTyCon (dataConTyCon con)
  = all is_triv_lpat (hsConPatArgs ps)
is_flat_prod_pat _ = False

is_triv_lpat :: LPat a -> Bool
is_triv_lpat p = is_triv_pat (unLoc p)

is_triv_pat :: Pat a -> Bool
is_triv_pat (VarPat _)  = True
is_triv_pat (WildPat _) = True
is_triv_pat (ParPat p)  = is_triv_lpat p
is_triv_pat _           = False


{- *********************************************************************
*                                                                      *
  Creating big tuples and their types for full Haskell expressions.
  They work over *Ids*, and create tuples replete with their types,
  which is whey they are not in HsUtils.
*                                                                      *
********************************************************************* -}

mkLHsPatTup :: [LPat GhcTc] -> LPat GhcTc
mkLHsPatTup []     = noLoc $ mkVanillaTuplePat [] Boxed
mkLHsPatTup [lpat] = lpat
mkLHsPatTup lpats  = L (getLoc (head lpats)) $
                     mkVanillaTuplePat lpats Boxed

mkLHsVarPatTup :: [Id] -> LPat GhcTc
mkLHsVarPatTup bs  = mkLHsPatTup (map nlVarPat bs)

mkVanillaTuplePat :: [OutPat GhcTc] -> Boxity -> Pat GhcTc
-- A vanilla tuple pattern simply gets its type from its sub-patterns
mkVanillaTuplePat pats box = TuplePat pats box (map hsLPatType pats)

-- The Big equivalents for the source tuple expressions
mkBigLHsVarTupId :: [Id] -> LHsExpr GhcTc
mkBigLHsVarTupId ids = mkBigLHsTupId (map nlHsVar ids)

mkBigLHsTupId :: [LHsExpr GhcTc] -> LHsExpr GhcTc
mkBigLHsTupId = mkChunkified mkLHsTupleExpr

-- The Big equivalents for the source tuple patterns
mkBigLHsVarPatTupId :: [Id] -> LPat GhcTc
mkBigLHsVarPatTupId bs = mkBigLHsPatTupId (map nlVarPat bs)

mkBigLHsPatTupId :: [LPat GhcTc] -> LPat GhcTc
mkBigLHsPatTupId = mkChunkified mkLHsPatTup

{-
************************************************************************
*                                                                      *
        Code for pattern-matching and other failures
*                                                                      *
************************************************************************

Generally, we handle pattern matching failure like this: let-bind a
fail-variable, and use that variable if the thing fails:
\begin{verbatim}
        let fail.33 = error "Help"
        in
        case x of
                p1 -> ...
                p2 -> fail.33
                p3 -> fail.33
                p4 -> ...
\end{verbatim}
Then
\begin{itemize}
\item
If the case can't fail, then there'll be no mention of @fail.33@, and the
simplifier will later discard it.

\item
If it can fail in only one way, then the simplifier will inline it.

\item
Only if it is used more than once will the let-binding remain.
\end{itemize}

There's a problem when the result of the case expression is of
unboxed type.  Then the type of @fail.33@ is unboxed too, and
there is every chance that someone will change the let into a case:
\begin{verbatim}
        case error "Help" of
          fail.33 -> case ....
\end{verbatim}

which is of course utterly wrong.  Rather than drop the condition that
only boxed types can be let-bound, we just turn the fail into a function
for the primitive case:
\begin{verbatim}
        let fail.33 :: Void -> Int#
            fail.33 = \_ -> error "Help"
        in
        case x of
                p1 -> ...
                p2 -> fail.33 void
                p3 -> fail.33 void
                p4 -> ...
\end{verbatim}

Now @fail.33@ is a function, so it can be let-bound.

We would *like* to use join points here; in fact, these "fail variables" are
paradigmatic join points! Sadly, this breaks pattern synonyms, which desugar as
CPS functions - i.e. they take "join points" as parameters. It's not impossible
to imagine extending our type system to allow passing join points around (very
carefully), but we certainly don't support it now.

99.99% of the time, the fail variables wind up as join points in short order
anyway, and the Void# doesn't do much harm.
-}

mkFailurePair :: CoreExpr       -- Result type of the whole case expression
              -> DsM (CoreBind, -- Binds the newly-created fail variable
                                -- to \ _ -> expression
                      CoreExpr) -- Fail variable applied to realWorld#
-- See Note [Failure thunks and CPR]
mkFailurePair expr
  = do { fail_fun_var <- newFailLocalDs (voidPrimTy `mkFunTy` ty)
       ; fail_fun_arg <- newSysLocalDs voidPrimTy
       ; let real_arg = setOneShotLambda fail_fun_arg
       ; return (NonRec fail_fun_var (Lam real_arg expr),
                 App (Var fail_fun_var) (Var voidPrimId)) }
  where
    ty = exprType expr

{-
Note [Failure thunks and CPR]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(This note predates join points as formal entities (hence the quotation marks).
We can't use actual join points here (see above); if we did, this would also
solve the CPR problem, since join points don't get CPR'd. See Note [Don't CPR
join points] in WorkWrap.)

When we make a failure point we ensure that it
does not look like a thunk. Example:

   let fail = \rw -> error "urk"
   in case x of
        [] -> fail realWorld#
        (y:ys) -> case ys of
                    [] -> fail realWorld#
                    (z:zs) -> (y,z)

Reason: we know that a failure point is always a "join point" and is
entered at most once.  Adding a dummy 'realWorld' token argument makes
it clear that sharing is not an issue.  And that in turn makes it more
CPR-friendly.  This matters a lot: if you don't get it right, you lose
the tail call property.  For example, see Trac #3403.


************************************************************************
*                                                                      *
              Ticks
*                                                                      *
********************************************************************* -}

mkOptTickBox :: [Tickish Id] -> CoreExpr -> CoreExpr
mkOptTickBox = flip (foldr Tick)

mkBinaryTickBox :: Int -> Int -> CoreExpr -> DsM CoreExpr
mkBinaryTickBox ixT ixF e = do
       uq <- newUnique
       this_mod <- getModule
       let bndr1 = mkSysLocal (fsLit "t1") uq boolTy
       let
           falseBox = Tick (HpcTick this_mod ixF) (Var falseDataConId)
           trueBox  = Tick (HpcTick this_mod ixT) (Var trueDataConId)
       --
       return $ Case e bndr1 boolTy
                       [ (DataAlt falseDataCon, [], falseBox)
                       , (DataAlt trueDataCon,  [], trueBox)
                       ]



-- *******************************************************************

-- | Use -XStrict to add a ! or remove a ~
--
-- Examples:
-- ~pat    => pat    -- when -XStrict (even if pat = ~pat')
-- !pat    => !pat   -- always
-- pat     => !pat   -- when -XStrict
-- pat     => pat    -- otherwise
decideBangHood :: DynFlags
               -> LPat id  -- ^ Original pattern
               -> LPat id  -- Pattern with bang if necessary
decideBangHood dflags lpat
  | not (xopt LangExt.Strict dflags)
  = lpat
  | otherwise   --  -XStrict
  = go lpat
  where
    go lp@(L l p)
      = case p of
           ParPat p    -> L l (ParPat (go p))
           LazyPat lp' -> lp'
           BangPat _   -> lp
           _           -> L l (BangPat lp)
