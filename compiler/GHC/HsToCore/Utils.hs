{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


Utilities for desugaring

This module exports some utility functions of no great interest.
-}

-- | Utility functions for constructing Core syntax, principally for desugaring
module GHC.HsToCore.Utils (
        EquationInfo(..),
        firstPat, shiftEqns,

        MatchResult (..), CaseAlt(..),
        cantFailMatchResult, alwaysFailMatchResult,
        extractMatchResult, combineMatchResults,
        adjustMatchResultDs,
        shareFailureHandler,
        dsHandleMonadicFailure,
        mkCoLetMatchResult, mkViewMatchResult, mkGuardedMatchResult,
        matchCanFail, mkEvalMatchResult,
        mkCoPrimCaseMatchResult, mkCoAlgCaseMatchResult, mkCoSynCaseMatchResult,
        wrapBind, wrapBinds,

        mkErrorAppDs, mkCoreAppDs, mkCoreAppsDs, mkCastDs,

        seqVar,

        -- LHs tuples
        mkLHsPatTup, mkVanillaTuplePat,
        mkBigLHsVarTupId, mkBigLHsTupId, mkBigLHsVarPatTupId, mkBigLHsPatTupId,

        mkSelectorBinds,

        selectSimpleMatchVarL, selectMatchVars, selectMatchVar,
        mkOptTickBox, mkBinaryTickBox, decideBangHood,
        isTrueLHsExpr
    ) where

#include "HsVersions.h"

import GHC.Prelude

import {-# SOURCE #-} GHC.HsToCore.Match ( matchSimply )
import {-# SOURCE #-} GHC.HsToCore.Expr  ( dsLExpr, dsSyntaxExpr )

import GHC.Hs
import GHC.Tc.Utils.Zonk
import GHC.Tc.Utils.TcType( tcSplitTyConApp )
import GHC.Core
import GHC.HsToCore.Monad

import GHC.Core.Utils
import GHC.Core.Make
import GHC.Types.Id.Make
import GHC.Types.Id
import GHC.Types.Literal
import GHC.Core.TyCon
import GHC.Core.DataCon
import GHC.Core.PatSyn
import GHC.Core.Type
import GHC.Core.Coercion
import GHC.Builtin.Types
import GHC.Types.Basic
import GHC.Core.ConLike
import GHC.Types.Unique.Set
import GHC.Types.Unique.Supply
import GHC.Unit.Module
import GHC.Builtin.Names
import GHC.Types.Name( isInternalName )
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Types.SrcLoc
import GHC.Utils.Misc
import GHC.Driver.Session
import GHC.Driver.Ppr
import GHC.Data.FastString
import qualified GHC.LanguageExtensions as LangExt

import GHC.Tc.Types.Evidence

import Control.Monad    ( zipWithM )
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (maybeToList)
import qualified Data.List.NonEmpty as NEL

{-
************************************************************************
*                                                                      *
\subsection{ Selecting match variables}
*                                                                      *
************************************************************************

We're about to match against some patterns.  We want to make some
@Ids@ to use as match variables.  If a pattern has an @Id@ readily at
hand, which should indeed be bound to the pattern as a whole, then use it;
otherwise, make one up. The multiplicity argument is chosen as the multiplicity
of the variable if it is made up.
-}

selectSimpleMatchVarL :: Mult -> LPat GhcTc -> DsM Id
-- Postcondition: the returned Id has an Internal Name
selectSimpleMatchVarL w pat = selectMatchVar w (unLoc pat)

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

selectMatchVars :: [(Mult, Pat GhcTc)] -> DsM [Id]
-- Postcondition: the returned Ids have Internal Names
selectMatchVars ps = mapM (uncurry selectMatchVar) ps

selectMatchVar :: Mult -> Pat GhcTc -> DsM Id
-- Postcondition: the returned Id has an Internal Name
selectMatchVar w (BangPat _ pat) = selectMatchVar w (unLoc pat)
selectMatchVar w (LazyPat _ pat) = selectMatchVar w (unLoc pat)
selectMatchVar w (ParPat _ pat)  = selectMatchVar w (unLoc pat)
selectMatchVar _w (VarPat _ var)  = return (localiseId (unLoc var))
                                  -- Note [Localise pattern binders]
                                  --
                                  -- Remark: when the pattern is a variable (or
                                  -- an @-pattern), then w is the same as the
                                  -- multiplicity stored within the variable
                                  -- itself. It's easier to pull it from the
                                  -- variable, so we ignore the multiplicity.
selectMatchVar _w (AsPat _ var _) = ASSERT( isManyDataConTy _w ) (return (unLoc var))
selectMatchVar w other_pat     = newSysLocalDsNoLP w (hsPatType other_pat)

{- Note [Localise pattern binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
In fact, even GHC.Core.Subst.simplOptExpr will do this, and simpleOptExpr
runs on the output of the desugarer, so all is well by the end of
the desugaring pass.

See also Note [MatchIds] in GHC.HsToCore.Match

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

shiftEqns :: Functor f => f EquationInfo -> f EquationInfo
-- Drop the first pattern in each equation
shiftEqns = fmap $ \eqn -> eqn { eqn_pats = tail (eqn_pats eqn) }

-- Functions on MatchResult CoreExprs

matchCanFail :: MatchResult a -> Bool
matchCanFail (MR_Fallible {})  = True
matchCanFail (MR_Infallible {}) = False

alwaysFailMatchResult :: MatchResult CoreExpr
alwaysFailMatchResult = MR_Fallible $ \fail -> return fail

cantFailMatchResult :: CoreExpr -> MatchResult CoreExpr
cantFailMatchResult expr = MR_Infallible $ return expr

extractMatchResult :: MatchResult CoreExpr -> CoreExpr -> DsM CoreExpr
extractMatchResult match_result failure_expr =
  runMatchResult
    failure_expr
    (shareFailureHandler match_result)

combineMatchResults :: MatchResult CoreExpr -> MatchResult CoreExpr -> MatchResult CoreExpr
combineMatchResults match_result1@(MR_Infallible _) _
  = match_result1
combineMatchResults match_result1 match_result2 =
  -- if the first pattern needs a failure handler (i.e. if it is fallible),
  -- make it let-bind it bind it with `shareFailureHandler`.
  case shareFailureHandler match_result1 of
    MR_Infallible _ -> match_result1
    MR_Fallible body_fn1 -> MR_Fallible $ \fail_expr ->
      -- Before actually failing, try the next match arm.
      body_fn1 =<< runMatchResult fail_expr match_result2

adjustMatchResultDs :: (a -> DsM b) -> MatchResult a -> MatchResult b
adjustMatchResultDs encl_fn = \case
  MR_Infallible body_fn -> MR_Infallible $
    encl_fn =<< body_fn
  MR_Fallible body_fn -> MR_Fallible $ \fail ->
    encl_fn =<< body_fn fail

wrapBinds :: [(Var,Var)] -> CoreExpr -> CoreExpr
wrapBinds [] e = e
wrapBinds ((new,old):prs) e = wrapBind new old (wrapBinds prs e)

wrapBind :: Var -> Var -> CoreExpr -> CoreExpr
wrapBind new old body   -- NB: this function must deal with term
  | new==old    = body  -- variables, type variables or coercion variables
  | otherwise   = Let (NonRec new (varToCoreExpr old)) body

seqVar :: Var -> CoreExpr -> CoreExpr
seqVar var body = mkDefaultCase (Var var) var body

mkCoLetMatchResult :: CoreBind -> MatchResult CoreExpr -> MatchResult CoreExpr
mkCoLetMatchResult bind = fmap (mkCoreLet bind)

-- (mkViewMatchResult var' viewExpr mr) makes the expression
-- let var' = viewExpr in mr
mkViewMatchResult :: Id -> CoreExpr -> MatchResult CoreExpr -> MatchResult CoreExpr
mkViewMatchResult var' viewExpr = fmap $ mkCoreLet $ NonRec var' viewExpr

mkEvalMatchResult :: Id -> Type -> MatchResult CoreExpr -> MatchResult CoreExpr
mkEvalMatchResult var ty = fmap $ \e ->
  Case (Var var) var ty [(DEFAULT, [], e)]

mkGuardedMatchResult :: CoreExpr -> MatchResult CoreExpr -> MatchResult CoreExpr
mkGuardedMatchResult pred_expr mr = MR_Fallible $ \fail -> do
  body <- runMatchResult fail mr
  return (mkIfThenElse pred_expr body fail)

mkCoPrimCaseMatchResult :: Id                  -- Scrutinee
                        -> Type                      -- Type of the case
                        -> [(Literal, MatchResult CoreExpr)]  -- Alternatives
                        -> MatchResult CoreExpr               -- Literals are all unlifted
mkCoPrimCaseMatchResult var ty match_alts
  = MR_Fallible mk_case
  where
    mk_case fail = do
        alts <- mapM (mk_alt fail) sorted_alts
        return (Case (Var var) var ty ((DEFAULT, [], fail) : alts))

    sorted_alts = sortWith fst match_alts       -- Right order for a Case
    mk_alt fail (lit, mr)
       = ASSERT( not (litIsLifted lit) )
         do body <- runMatchResult fail mr
            return (LitAlt lit, [], body)

data CaseAlt a = MkCaseAlt{ alt_pat :: a,
                            alt_bndrs :: [Var],
                            alt_wrapper :: HsWrapper,
                            alt_result :: MatchResult CoreExpr }

mkCoAlgCaseMatchResult
  :: Id -- ^ Scrutinee
  -> Type -- ^ Type of exp
  -> NonEmpty (CaseAlt DataCon) -- ^ Alternatives (bndrs *include* tyvars, dicts)
  -> MatchResult CoreExpr
mkCoAlgCaseMatchResult var ty match_alts
  | isNewtype  -- Newtype case; use a let
  = ASSERT( null match_alts_tail && null (tail arg_ids1) )
    mkCoLetMatchResult (NonRec arg_id1 newtype_rhs) match_result1

  | otherwise
  = mkDataConCase var ty match_alts
  where
    isNewtype = isNewTyCon (dataConTyCon (alt_pat alt1))

        -- [Interesting: because of GADTs, we can't rely on the type of
        --  the scrutinised Id to be sufficiently refined to have a TyCon in it]

    alt1@MkCaseAlt{ alt_bndrs = arg_ids1, alt_result = match_result1 } :| match_alts_tail
      = match_alts
    -- Stuff for newtype
    arg_id1       = ASSERT( notNull arg_ids1 ) head arg_ids1
    var_ty        = idType var
    (tc, ty_args) = tcSplitTyConApp var_ty      -- Don't look through newtypes
                                                -- (not that splitTyConApp does, these days)
    newtype_rhs = unwrapNewTypeBody tc ty_args (Var var)

mkCoSynCaseMatchResult :: Id -> Type -> CaseAlt PatSyn -> MatchResult CoreExpr
mkCoSynCaseMatchResult var ty alt = MR_Fallible $ mkPatSynCase var ty alt

mkPatSynCase :: Id -> Type -> CaseAlt PatSyn -> CoreExpr -> DsM CoreExpr
mkPatSynCase var ty alt fail = do
    matcher_id <- dsLookupGlobalId matcher_name
    matcher <- dsLExpr $ mkLHsWrap wrapper $
                         nlHsTyApp matcher_id [getRuntimeRep ty, ty]
    cont <- mkCoreLams bndrs <$> runMatchResult fail match_result
    return $ mkCoreAppsDs (text "patsyn" <+> ppr var) matcher [Var var, ensure_unstrict cont, Lam voidArgId fail]
  where
    MkCaseAlt{ alt_pat = psyn,
               alt_bndrs = bndrs,
               alt_wrapper = wrapper,
               alt_result = match_result} = alt
    (matcher_name, _, needs_void_lam) = patSynMatcher psyn

    -- See Note [Matchers and builders for pattern synonyms] in GHC.Core.PatSyn
    -- on these extra Void# arguments
    ensure_unstrict cont | needs_void_lam = Lam voidArgId cont
                         | otherwise      = cont

mkDataConCase :: Id -> Type -> NonEmpty (CaseAlt DataCon) -> MatchResult CoreExpr
mkDataConCase var ty alts@(alt1 :| _)
    = liftA2 mk_case mk_default mk_alts
    -- The liftA2 combines the failability of all the alternatives and the default
  where
    con1          = alt_pat alt1
    tycon         = dataConTyCon con1
    data_cons     = tyConDataCons tycon

    sorted_alts :: [ CaseAlt DataCon ]
    sorted_alts  = sortWith (dataConTag . alt_pat) $ NEL.toList alts

    var_ty       = idType var
    (_, ty_args) = tcSplitTyConApp var_ty -- Don't look through newtypes
                                          -- (not that splitTyConApp does, these days)

    mk_case :: Maybe CoreAlt -> [CoreAlt] -> CoreExpr
    mk_case def alts = mkWildCase (Var var) (idScaledType var) ty $
      maybeToList def ++ alts

    mk_alts :: MatchResult [CoreAlt]
    mk_alts = traverse mk_alt sorted_alts

    mk_alt :: CaseAlt DataCon -> MatchResult CoreAlt
    mk_alt MkCaseAlt { alt_pat = con
                     , alt_bndrs = args
                     , alt_result = match_result } =
      flip adjustMatchResultDs match_result $ \body -> do
        case dataConBoxer con of
          Nothing -> return (DataAlt con, args, body)
          Just (DCB boxer) -> do
            us <- newUniqueSupply
            let (rep_ids, binds) = initUs_ us (boxer ty_args args)
            let rep_ids' = map (scaleVarBy (idMult var)) rep_ids
              -- Upholds the invariant that the binders of a case expression
              -- must be scaled by the case multiplicity. See Note [Case
              -- expression invariants] in CoreSyn.
            return (DataAlt con, rep_ids', mkLets binds body)

    mk_default :: MatchResult (Maybe CoreAlt)
    mk_default
      | exhaustive_case = MR_Infallible $ return Nothing
      | otherwise       = MR_Fallible $ \fail -> return $ Just (DEFAULT, [], fail)

    mentioned_constructors = mkUniqSet $ map alt_pat sorted_alts
    un_mentioned_constructors
        = mkUniqSet data_cons `minusUniqSet` mentioned_constructors
    exhaustive_case = isEmptyUniqSet un_mentioned_constructors

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
        core_msg = Lit (mkLitString full_msg)
        -- mkLitString returns a result of type String#
    return (mkApps (Var err_id) [Type (getRuntimeRep ty), Type ty, core_msg])

{-
'mkCoreAppDs' and 'mkCoreAppsDs' handle the special-case desugaring of 'seq'.

Note [Desugaring seq]
~~~~~~~~~~~~~~~~~~~~~

There are a few subtleties in the desugaring of `seq`:

 1. (as described in #1031)

    Consider,
       f x y = x `seq` (y `seq` (# x,y #))

    The [Core let/app invariant] means that, other things being equal, because
    the argument to the outer 'seq' has an unlifted type, we'll use call-by-value thus:

       f x y = case (y `seq` (# x,y #)) of v -> x `seq` v

    But that is bad for two reasons:
      (a) we now evaluate y before x, and
      (b) we can't bind v to an unboxed pair

    Seq is very, very special!  So we recognise it right here, and desugar to
            case x of _ -> case y of _ -> (# x,y #)

 2. (as described in #2273)

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
    case happens, so we get this at an early stage:
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

 3. (as described in #2409)

    The isLocalId ensures that we don't turn
            True `seq` e
    into
            case True of True { ... }
    which stupidly tries to bind the datacon 'True'.
-}

-- NB: Make sure the argument is not levity polymorphic
mkCoreAppDs  :: SDoc -> CoreExpr -> CoreExpr -> CoreExpr
mkCoreAppDs _ (Var f `App` Type _r `App` Type ty1 `App` Type ty2 `App` arg1) arg2
  | f `hasKey` seqIdKey            -- Note [Desugaring seq], points (1) and (2)
  = Case arg1 case_bndr ty2 [(DEFAULT,[],arg2)]
  where
    case_bndr = case arg1 of
                   Var v1 | isInternalName (idName v1)
                          -> v1        -- Note [Desugaring seq], points (2) and (3)
                   _      -> mkWildValBinder Many ty1

mkCoreAppDs _ (Var f `App` Type _r) arg
  | f `hasKey` noinlineIdKey   -- See Note [noinlineId magic] in GHC.Types.Id.Make
  , (fun, args) <- collectArgs arg
  , not (null args)
  = (Var f `App` Type (exprType fun) `App` fun)
    `mkCoreApps` args

mkCoreAppDs s fun arg = mkCoreApp s fun arg  -- The rest is done in GHC.Core.Make

-- NB: No argument can be levity polymorphic
mkCoreAppsDs :: SDoc -> CoreExpr -> [CoreExpr] -> CoreExpr
mkCoreAppsDs s fun args = foldl' (mkCoreAppDs s) fun args

mkCastDs :: CoreExpr -> Coercion -> CoreExpr
-- We define a desugarer-specific version of GHC.Core.Utils.mkCast,
-- because in the immediate output of the desugarer, we can have
-- apparently-mis-matched coercions:  E.g.
--     let a = b
--     in (x :: a) |> (co :: b ~ Int)
-- Lint know about type-bindings for let and does not complain
-- So here we do not make the assertion checks that we make in
-- GHC.Core.Utils.mkCast; and we do less peephole optimisation too
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
       let { t = case e of Just (Just v) -> Solo v
           ; v = case t of Solo v -> v }
       in t `seq` body
    The 'Solo' is a one-tuple; see Note [One-tuples] in GHC.Builtin.Types
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
      t = case e of (_, (_, a)) -> Solo a
      a = case t of Solo a -> a

    Note that
     - Forcing 't' will force the pattern to match fully;
       e.g. will diverge if (snd e) is bottom
     - But 'a' itself is not forced; it is wrapped in a one-tuple
       (see Note [One-tuples] in GHC.Builtin.Types)

  *   !(Just x) = e
    ==>
      t = case e of Just x -> Solo x
      x = case t of Solo x -> x

    Again, forcing 't' will fail if 'e' yields Nothing.

Note that even though this is rather general, the special cases
work out well:

* One binder, not -XStrict:

    let Just (Just v) = e in body
  ==>
    let t = case e of Just (Just v) -> Solo v
        v = case t of Solo v -> v
    in body
  ==>
    let v = case (case e of Just (Just v) -> Solo v) of
              Solo v -> v
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
-- Remark: pattern selectors only occur in unrestricted patterns so we are free
-- to select Many as the multiplicity of every let-expression introduced.
mkSelectorBinds :: [[Tickish Id]] -- ^ ticks to add, possibly
                -> LPat GhcTc     -- ^ The pattern
                -> CoreExpr       -- ^ Expression to which the pattern is bound
                -> DsM (Id,[(Id,CoreExpr)])
                -- ^ Id the rhs is bound to, for desugaring strict
                -- binds (see Note [Desugar Strict binds] in "GHC.HsToCore.Binds")
                -- and all the desugared binds

mkSelectorBinds ticks pat val_expr
  | L _ (VarPat _ (L _ v)) <- pat'     -- Special case (A)
  = return (v, [(v, val_expr)])

  | is_flat_prod_lpat pat'           -- Special case (B)
  = do { let pat_ty = hsLPatType pat'
       ; val_var <- newSysLocalDsNoLP Many pat_ty

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
  = do { tuple_var  <- newSysLocalDs Many tuple_ty
       ; error_expr <- mkErrorAppDs pAT_ERROR_ID tuple_ty (ppr pat')
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

strip_bangs :: LPat (GhcPass p) -> LPat (GhcPass p)
-- Remove outermost bangs and parens
strip_bangs (L _ (ParPat _ p))  = strip_bangs p
strip_bangs (L _ (BangPat _ p)) = strip_bangs p
strip_bangs lp                  = lp

is_flat_prod_lpat :: LPat GhcTc -> Bool
is_flat_prod_lpat = is_flat_prod_pat . unLoc

is_flat_prod_pat :: Pat GhcTc -> Bool
is_flat_prod_pat (ParPat _ p)          = is_flat_prod_lpat p
is_flat_prod_pat (TuplePat _ ps Boxed) = all is_triv_lpat ps
is_flat_prod_pat (ConPat { pat_con  = L _ pcon
                         , pat_args = ps})
  | RealDataCon con <- pcon
  , isProductTyCon (dataConTyCon con)
  = all is_triv_lpat (hsConPatArgs ps)
is_flat_prod_pat _ = False

is_triv_lpat :: LPat (GhcPass p) -> Bool
is_triv_lpat = is_triv_pat . unLoc

is_triv_pat :: Pat (GhcPass p) -> Bool
is_triv_pat (VarPat {})  = True
is_triv_pat (WildPat{})  = True
is_triv_pat (ParPat _ p) = is_triv_lpat p
is_triv_pat _            = False


{- *********************************************************************
*                                                                      *
  Creating big tuples and their types for full Haskell expressions.
  They work over *Ids*, and create tuples replete with their types,
  which is whey they are not in GHC.Hs.Utils.
*                                                                      *
********************************************************************* -}

mkLHsPatTup :: [LPat GhcTc] -> LPat GhcTc
mkLHsPatTup []     = noLoc $ mkVanillaTuplePat [] Boxed
mkLHsPatTup [lpat] = lpat
mkLHsPatTup lpats  = L (getLoc (head lpats)) $
                     mkVanillaTuplePat lpats Boxed

mkVanillaTuplePat :: [LPat GhcTc] -> Boxity -> Pat GhcTc
-- A vanilla tuple pattern simply gets its type from its sub-patterns
mkVanillaTuplePat pats box = TuplePat (map hsLPatType pats) pats box

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
  = do { fail_fun_var <- newFailLocalDs Many (unboxedUnitTy `mkVisFunTyMany` ty)
       ; fail_fun_arg <- newSysLocalDs Many unboxedUnitTy
       ; let real_arg = setOneShotLambda fail_fun_arg
       ; return (NonRec fail_fun_var (Lam real_arg expr),
                 App (Var fail_fun_var) (Var voidPrimId)) }
  where
    ty = exprType expr

-- Uses '@mkFailurePair@' to bind the failure case. Infallible matches have
-- neither a failure arg or failure "hole", so nothing is let-bound, and no
-- extraneous Core is produced.
shareFailureHandler :: MatchResult CoreExpr -> MatchResult CoreExpr
shareFailureHandler = \case
  mr@(MR_Infallible _) -> mr
  MR_Fallible match_fn -> MR_Fallible $ \fail_expr -> do
    (fail_bind, shared_failure_handler) <- mkFailurePair fail_expr
    body <- match_fn shared_failure_handler
    -- Never unboxed, per the above, so always OK for `let` not `case`.
    return $ Let fail_bind body

{-
Note [Failure thunks and CPR]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(This note predates join points as formal entities (hence the quotation marks).
We can't use actual join points here (see above); if we did, this would also
solve the CPR problem, since join points don't get CPR'd. See Note [Don't CPR
join points] in GHC.Core.Opt.WorkWrap.)

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
the tail call property.  For example, see #3403.
-}

dsHandleMonadicFailure :: HsStmtContext GhcRn -> LPat GhcTc -> MatchResult CoreExpr -> FailOperator GhcTc -> DsM CoreExpr
    -- In a do expression, pattern-match failure just calls
    -- the monadic 'fail' rather than throwing an exception
dsHandleMonadicFailure ctx pat match m_fail_op =
  case shareFailureHandler match of
    MR_Infallible body -> body
    MR_Fallible body -> do
      fail_op <- case m_fail_op of
        -- Note that (non-monadic) list comprehension, pattern guards, etc could
        -- have fallible bindings without an explicit failure op, but this is
        -- handled elsewhere. See Note [Failing pattern matches in Stmts] the
        -- breakdown of regular and special binds.
        Nothing -> pprPanic "missing fail op" $
          text "Pattern match:" <+> ppr pat <+>
          text "is failable, and fail_expr was left unset"
        Just fail_op -> pure fail_op
      dflags <- getDynFlags
      fail_msg <- mkStringExpr (mk_fail_msg dflags ctx pat)
      fail_expr <- dsSyntaxExpr fail_op [fail_msg]
      body fail_expr

mk_fail_msg :: DynFlags -> HsStmtContext GhcRn -> Located e -> String
mk_fail_msg dflags ctx pat
  = showPpr dflags $ text "Pattern match failure in" <+> pprStmtContext ctx <+> text "at" <+> ppr (getLoc pat)

{- *********************************************************************
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
       let bndr1 = mkSysLocal (fsLit "t1") uq One boolTy
         -- It's always sufficient to pattern-match on a boolean with
         -- multiplicity 'One'.
       let
           falseBox = Tick (HpcTick this_mod ixF) (Var falseDataConId)
           trueBox  = Tick (HpcTick this_mod ixT) (Var trueDataConId)
       --
       return $ Case e bndr1 boolTy
                       [ (DataAlt falseDataCon, [], falseBox)
                       , (DataAlt trueDataCon,  [], trueBox)
                       ]



-- *******************************************************************

{- Note [decideBangHood]
~~~~~~~~~~~~~~~~~~~~~~~~
With -XStrict we may make /outermost/ patterns more strict.
E.g.
       let (Just x) = e in ...
          ==>
       let !(Just x) = e in ...
and
       f x = e
          ==>
       f !x = e

This adjustment is done by decideBangHood,

  * Just before constructing an EqnInfo, in GHC.HsToCore.Match
      (matchWrapper and matchSinglePat)

  * When desugaring a pattern-binding in GHC.HsToCore.Binds.dsHsBind

Note that it is /not/ done recursively.  See the -XStrict
spec in the user manual.

Specifically:
   ~pat    => pat    -- when -XStrict (even if pat = ~pat')
   !pat    => !pat   -- always
   pat     => !pat   -- when -XStrict
   pat     => pat    -- otherwise
-}


-- | Use -XStrict to add a ! or remove a ~
-- See Note [decideBangHood]
decideBangHood :: DynFlags
               -> LPat GhcTc  -- ^ Original pattern
               -> LPat GhcTc  -- Pattern with bang if necessary
decideBangHood dflags lpat
  | not (xopt LangExt.Strict dflags)
  = lpat
  | otherwise   --  -XStrict
  = go lpat
  where
    go lp@(L l p)
      = case p of
           ParPat x p    -> L l (ParPat x (go p))
           LazyPat _ lp' -> lp'
           BangPat _ _   -> lp
           _             -> L l (BangPat noExtField lp)

isTrueLHsExpr :: LHsExpr GhcTc -> Maybe (CoreExpr -> DsM CoreExpr)

-- Returns Just {..} if we're sure that the expression is True
-- I.e.   * 'True' datacon
--        * 'otherwise' Id
--        * Trivial wappings of these
-- The arguments to Just are any HsTicks that we have found,
-- because we still want to tick then, even it they are always evaluated.
isTrueLHsExpr (L _ (HsVar _ (L _ v)))
  |  v `hasKey` otherwiseIdKey
     || v `hasKey` getUnique trueDataConId
                                              = Just return
        -- trueDataConId doesn't have the same unique as trueDataCon
isTrueLHsExpr (L _ (HsConLikeOut _ con))
  | con `hasKey` getUnique trueDataCon = Just return
isTrueLHsExpr (L _ (HsTick _ tickish e))
    | Just ticks <- isTrueLHsExpr e
    = Just (\x -> do wrapped <- ticks x
                     return (Tick tickish wrapped))
   -- This encodes that the result is constant True for Hpc tick purposes;
   -- which is specifically what isTrueLHsExpr is trying to find out.
isTrueLHsExpr (L _ (HsBinTick _ ixT _ e))
    | Just ticks <- isTrueLHsExpr e
    = Just (\x -> do e <- ticks x
                     this_mod <- getModule
                     return (Tick (HpcTick this_mod ixT) e))

isTrueLHsExpr (L _ (HsPar _ e))   = isTrueLHsExpr e
isTrueLHsExpr _                   = Nothing
