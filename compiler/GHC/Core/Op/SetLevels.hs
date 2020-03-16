{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section{GHC.Core.Op.SetLevels}

                ***************************
                        Overview
                ***************************

1. We attach binding levels to Core bindings, in preparation for floating
   outwards (@FloatOut@).

2. We also let-ify many expressions (notably case scrutinees), so they
   will have a fighting chance of being floated sensible.

3. Note [Need for cloning during float-out]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   We clone the binders of any floatable let-binding, so that when it is
   floated out it will be unique. Example
      (let x=2 in x) + (let x=3 in x)
   we must clone before floating so we get
      let x1=2 in
      let x2=3 in
      x1+x2

   NOTE: this can't be done using the uniqAway idea, because the variable
         must be unique in the whole program, not just its current scope,
         because two variables in different scopes may float out to the
         same top level place

   NOTE: Very tiresomely, we must apply this substitution to
         the rules stored inside a variable too.

   We do *not* clone top-level bindings, because some of them must not change,
   but we *do* clone bindings that are heading for the top level

4. Note [Binder-swap during float-out]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   In the expression
        case x of wild { p -> ...wild... }
   we substitute x for wild in the RHS of the case alternatives:
        case x of wild { p -> ...x... }
   This means that a sub-expression involving x is not "trapped" inside the RHS.
   And it's not inconvenient because we already have a substitution.

  Note that this is EXACTLY BACKWARDS from the what the simplifier does.
  The simplifier tries to get rid of occurrences of x, in favour of wild,
  in the hope that there will only be one remaining occurrence of x, namely
  the scrutinee of the case, and we can inline it.
-}

{-# LANGUAGE CPP, MultiWayIf #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module GHC.Core.Op.SetLevels (
        setLevels,

        Level(..), LevelType(..), tOP_LEVEL, isJoinCeilLvl, asJoinCeilLvl,
        LevelledBind, LevelledExpr, LevelledBndr,
        FloatSpec(..), floatSpecLevel,

        incMinorLvl, ltMajLvl, ltLvl, isTopLvl
    ) where

#include "HsVersions.h"

import GhcPrelude

import GHC.Core
import GHC.Core.Op.Monad ( FloatOutSwitches(..) )
import GHC.Core.Utils   ( exprType, exprIsHNF
                        , exprOkForSpeculation
                        , exprIsTopLevelBindable
                        , isExprLevPoly
                        , collectMakeStaticArgs
                        )
import GHC.Core.Arity   ( exprBotStrictness_maybe )
import GHC.Core.FVs     -- all of it
import GHC.Core.Subst
import GHC.Core.Make    ( sortQuantVars )

import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.Var
import GHC.Types.Var.Set
import GHC.Types.Unique.Set   ( nonDetFoldUniqSet )
import GHC.Types.Unique.DSet  ( getUniqDSet )
import GHC.Types.Var.Env
import GHC.Types.Literal      ( litIsTrivial )
import GHC.Types.Demand       ( StrictSig, Demand, isStrictDmd, splitStrictSig, prependArgsStrictSig )
import GHC.Types.Cpr          ( mkCprSig, botCpr )
import GHC.Types.Name         ( getOccName, mkSystemVarName )
import GHC.Types.Name.Occurrence ( occNameString )
import GHC.Core.Type    ( Type, mkLamTypes, splitTyConApp_maybe, tyCoVarsOfType
                        , mightBeUnliftedType, closeOverKindsDSet )
import GHC.Types.Basic  ( Arity, RecFlag(..), isRec )
import GHC.Core.DataCon ( dataConOrigResTy )
import TysWiredIn
import GHC.Types.Unique.Supply
import Util
import Outputable
import FastString
import GHC.Types.Unique.DFM
import FV
import Data.Maybe
import MonadUtils       ( mapAccumLM )

{-
************************************************************************
*                                                                      *
\subsection{Level numbers}
*                                                                      *
************************************************************************
-}

type LevelledExpr = TaggedExpr FloatSpec
type LevelledBind = TaggedBind FloatSpec
type LevelledBndr = TaggedBndr FloatSpec

data Level = Level Int  -- Level number of enclosing lambdas
                   Int  -- Number of big-lambda and/or case expressions and/or
                        -- context boundaries between
                        -- here and the nearest enclosing lambda
                   LevelType -- Binder or join ceiling?
data LevelType = BndrLvl | JoinCeilLvl deriving (Eq)

data FloatSpec
  = FloatMe Level       -- Float to just inside the binding
                        --    tagged with this level
  | StayPut Level       -- Stay where it is; binding is
                        --     tagged with this level

floatSpecLevel :: FloatSpec -> Level
floatSpecLevel (FloatMe l) = l
floatSpecLevel (StayPut l) = l

{-
The {\em level number} on a (type-)lambda-bound variable is the
nesting depth of the (type-)lambda which binds it.  The outermost lambda
has level 1, so (Level 0 0) means that the variable is bound outside any lambda.

On an expression, it's the maximum level number of its free
(type-)variables.  On a let(rec)-bound variable, it's the level of its
RHS.  On a case-bound variable, it's the number of enclosing lambdas.

Top-level variables: level~0.  Those bound on the RHS of a top-level
definition but ``before'' a lambda; e.g., the \tr{x} in (levels shown
as ``subscripts'')...
\begin{verbatim}
a_0 = let  b_? = ...  in
           x_1 = ... b ... in ...
\end{verbatim}

The main function @lvlExpr@ carries a ``context level'' (@le_ctxt_lvl@).
That's meant to be the level number of the enclosing binder in the
final (floated) program.  If the level number of a sub-expression is
less than that of the context, then it might be worth let-binding the
sub-expression so that it will indeed float.

If you can float to level @Level 0 0@ worth doing so because then your
allocation becomes static instead of dynamic.  We always start with
context @Level 0 0@.


Note [FloatOut inside INLINE]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@InlineCtxt@ very similar to @Level 0 0@, but is used for one purpose:
to say "don't float anything out of here".  That's exactly what we
want for the body of an INLINE, where we don't want to float anything
out at all.  See notes with lvlMFE below.

But, check this out:

-- At one time I tried the effect of not floating anything out of an InlineMe,
-- but it sometimes works badly.  For example, consider PrelArr.done.  It
-- has the form         __inline (\d. e)
-- where e doesn't mention d.  If we float this to
--      __inline (let x = e in \d. x)
-- things are bad.  The inliner doesn't even inline it because it doesn't look
-- like a head-normal form.  So it seems a lesser evil to let things float.
-- In GHC.Core.Op.SetLevels we do set the context to (Level 0 0) when we get to an InlineMe
-- which discourages floating out.

So the conclusion is: don't do any floating at all inside an InlineMe.
(In the above example, don't float the {x=e} out of the \d.)

One particular case is that of workers: we don't want to float the
call to the worker outside the wrapper, otherwise the worker might get
inlined into the floated expression, and an importing module won't see
the worker at all.

Note [Join ceiling]
~~~~~~~~~~~~~~~~~~~
Join points can't float very far; too far, and they can't remain join points
So, suppose we have:

  f x = (joinrec j y = ... x ... in jump j x) + 1

One may be tempted to float j out to the top of f's RHS, but then the jump
would not be a tail call. Thus we keep track of a level called the *join
ceiling* past which join points are not allowed to float.

The troublesome thing is that, unlike most levels to which something might
float, there is not necessarily an identifier to which the join ceiling is
attached. Fortunately, if something is to be floated to a join ceiling, it must
be dropped at the *nearest* join ceiling. Thus each level is marked as to
whether it is a join ceiling, so that FloatOut can tell which binders are being
floated to the nearest join ceiling and which to a particular binder (or set of
binders).
-}

instance Outputable FloatSpec where
  ppr (FloatMe l) = char 'F' <> ppr l
  ppr (StayPut l) = ppr l

tOP_LEVEL :: Level
tOP_LEVEL   = Level 0 0 BndrLvl

incMajorLvl :: Level -> Level
incMajorLvl (Level major _ _) = Level (major + 1) 0 BndrLvl

incMinorLvl :: Level -> Level
incMinorLvl (Level major minor _) = Level major (minor+1) BndrLvl

asJoinCeilLvl :: Level -> Level
asJoinCeilLvl (Level major minor _) = Level major minor JoinCeilLvl

maxLvl :: Level -> Level -> Level
maxLvl l1@(Level maj1 min1 _) l2@(Level maj2 min2 _)
  | (maj1 > maj2) || (maj1 == maj2 && min1 > min2) = l1
  | otherwise                                      = l2

ltLvl :: Level -> Level -> Bool
ltLvl (Level maj1 min1 _) (Level maj2 min2 _)
  = (maj1 < maj2) || (maj1 == maj2 && min1 < min2)

ltMajLvl :: Level -> Level -> Bool
    -- Tells if one level belongs to a difft *lambda* level to another
ltMajLvl (Level maj1 _ _) (Level maj2 _ _) = maj1 < maj2

isTopLvl :: Level -> Bool
isTopLvl (Level 0 0 _) = True
isTopLvl _             = False

isJoinCeilLvl :: Level -> Bool
isJoinCeilLvl (Level _ _ t) = t == JoinCeilLvl

instance Outputable Level where
  ppr (Level maj min typ)
    = hcat [ char '<', int maj, char ',', int min, char '>'
           , ppWhen (typ == JoinCeilLvl) (char 'C') ]

instance Eq Level where
  (Level maj1 min1 _) == (Level maj2 min2 _) = maj1 == maj2 && min1 == min2

{-
************************************************************************
*                                                                      *
\subsection{Main level-setting code}
*                                                                      *
************************************************************************
-}

setLevels :: FloatOutSwitches
          -> CoreProgram
          -> UniqSupply
          -> [LevelledBind]

setLevels float_lams binds us
  = initLvl us (do_them init_env binds)
  where
    init_env = initialEnv float_lams

    do_them :: LevelEnv -> [CoreBind] -> LvlM [LevelledBind]
    do_them _ [] = return []
    do_them env (b:bs)
      = do { (lvld_bind, env') <- lvlTopBind env b
           ; lvld_binds <- do_them env' bs
           ; return (lvld_bind : lvld_binds) }

lvlTopBind :: LevelEnv -> Bind Id -> LvlM (LevelledBind, LevelEnv)
lvlTopBind env (NonRec bndr rhs)
  = do { rhs' <- lvl_top env NonRecursive bndr rhs
       ; let (env', [bndr']) = substAndLvlBndrs NonRecursive env tOP_LEVEL [bndr]
       ; return (NonRec bndr' rhs', env') }

lvlTopBind env (Rec pairs)
  = do { let (env', bndrs') = substAndLvlBndrs Recursive env tOP_LEVEL
                                               (map fst pairs)
       ; rhss' <- mapM (\(b,r) -> lvl_top env' Recursive b r) pairs
       ; return (Rec (bndrs' `zip` rhss'), env') }

lvl_top :: LevelEnv -> RecFlag -> Id -> CoreExpr -> LvlM LevelledExpr
lvl_top env is_rec bndr rhs
  = lvlRhs env is_rec
           (isDeadEndId bndr)
           Nothing  -- Not a join point
           (freeVars rhs)

{-
************************************************************************
*                                                                      *
\subsection{Setting expression levels}
*                                                                      *
************************************************************************

Note [Floating over-saturated applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we see (f x y), and (f x) is a redex (ie f's arity is 1),
we call (f x) an "over-saturated application"

Should we float out an over-sat app, if can escape a value lambda?
It is sometimes very beneficial (-7% runtime -4% alloc over nofib -O2).
But we don't want to do it for class selectors, because the work saved
is minimal, and the extra local thunks allocated cost money.

Arguably we could float even class-op applications if they were going to
top level -- but then they must be applied to a constant dictionary and
will almost certainly be optimised away anyway.
-}

lvlExpr :: LevelEnv             -- Context
        -> CoreExprWithFVs      -- Input expression
        -> LvlM LevelledExpr    -- Result expression

{-
The @le_ctxt_lvl@ is, roughly, the level of the innermost enclosing
binder.  Here's an example

        v = \x -> ...\y -> let r = case (..x..) of
                                        ..x..
                           in ..

When looking at the rhs of @r@, @le_ctxt_lvl@ will be 1 because that's
the level of @r@, even though it's inside a level-2 @\y@.  It's
important that @le_ctxt_lvl@ is 1 and not 2 in @r@'s rhs, because we
don't want @lvlExpr@ to turn the scrutinee of the @case@ into an MFE
--- because it isn't a *maximal* free expression.

If there were another lambda in @r@'s rhs, it would get level-2 as well.
-}

lvlExpr env (_, AnnType ty)     = return (Type (GHC.Core.Subst.substTy (le_subst env) ty))
lvlExpr env (_, AnnCoercion co) = return (Coercion (substCo (le_subst env) co))
lvlExpr env (_, AnnVar v)       = return (lookupVar env v)
lvlExpr _   (_, AnnLit lit)     = return (Lit lit)

lvlExpr env (_, AnnCast expr (_, co)) = do
    expr' <- lvlNonTailExpr env expr
    return (Cast expr' (substCo (le_subst env) co))

lvlExpr env (_, AnnTick tickish expr) = do
    expr' <- lvlNonTailExpr env expr
    let tickish' = substTickish (le_subst env) tickish
    return (Tick tickish' expr')

lvlExpr env expr@(_, AnnApp _ _) = lvlApp env expr (collectAnnArgs expr)

-- We don't split adjacent lambdas.  That is, given
--      \x y -> (x+1,y)
-- we don't float to give
--      \x -> let v = x+1 in \y -> (v,y)
-- Why not?  Because partial applications are fairly rare, and splitting
-- lambdas makes them more expensive.

lvlExpr env expr@(_, AnnLam {})
  = do { new_body <- lvlNonTailMFE new_env True body
       ; return (mkLams new_bndrs new_body) }
  where
    (bndrs, body)        = collectAnnBndrs expr
    (env1, bndrs1)       = substBndrsSL NonRecursive env bndrs
    (new_env, new_bndrs) = lvlLamBndrs env1 (le_ctxt_lvl env) bndrs1
        -- At one time we called a special version of collectBinders,
        -- which ignored coercions, because we don't want to split
        -- a lambda like this (\x -> coerce t (\s -> ...))
        -- This used to happen quite a bit in state-transformer programs,
        -- but not nearly so much now non-recursive newtypes are transparent.
        -- [See GHC.Core.Op.SetLevels rev 1.50 for a version with this approach.]

lvlExpr env (_, AnnLet bind body)
  = do { (bind', new_env) <- lvlBind env bind
       ; body' <- lvlExpr new_env body
           -- No point in going via lvlMFE here.  If the binding is alive
           -- (mentioned in body), and the whole let-expression doesn't
           -- float, then neither will the body
       ; return (Let bind' body') }

lvlExpr env (_, AnnCase scrut case_bndr ty alts)
  = do { scrut' <- lvlNonTailMFE env True scrut
       ; lvlCase env (freeVarsOf scrut) scrut' case_bndr ty alts }

lvlNonTailExpr :: LevelEnv             -- Context
               -> CoreExprWithFVs      -- Input expression
               -> LvlM LevelledExpr    -- Result expression
lvlNonTailExpr env expr
  = lvlExpr (placeJoinCeiling env) expr

-------------------------------------------
lvlApp :: LevelEnv
       -> CoreExprWithFVs
       -> (CoreExprWithFVs, [CoreExprWithFVs]) -- Input application
        -> LvlM LevelledExpr                   -- Result expression
lvlApp env orig_expr ((_,AnnVar fn), args)
  | floatOverSat env   -- See Note [Floating over-saturated applications]
  , arity > 0
  , arity < n_val_args
  , Nothing <- isClassOpId_maybe fn
  =  do { rargs' <- mapM (lvlNonTailMFE env False) rargs
        ; lapp'  <- lvlNonTailMFE env False lapp
        ; return (foldl' App lapp' rargs') }

  | otherwise
  = do { (_, args') <- mapAccumLM lvl_arg stricts args
            -- Take account of argument strictness; see
            -- Note [Floating to the top]
       ; return (foldl' App (lookupVar env fn) args') }
  where
    n_val_args = count (isValArg . deAnnotate) args
    arity      = idArity fn

    stricts :: [Demand]   -- True for strict /value/ arguments
    stricts = case splitStrictSig (idStrictness fn) of
                (arg_ds, _) | arg_ds `lengthExceeds` n_val_args
                            -> []
                            | otherwise
                            -> arg_ds

    -- Separate out the PAP that we are floating from the extra
    -- arguments, by traversing the spine until we have collected
    -- (n_val_args - arity) value arguments.
    (lapp, rargs) = left (n_val_args - arity) orig_expr []

    left 0 e               rargs = (e, rargs)
    left n (_, AnnApp f a) rargs
       | isValArg (deAnnotate a) = left (n-1) f (a:rargs)
       | otherwise               = left n     f (a:rargs)
    left _ _ _                   = panic "GHC.Core.Op.SetLevels.lvlExpr.left"

    is_val_arg :: CoreExprWithFVs -> Bool
    is_val_arg (_, AnnType {}) = False
    is_val_arg _               = True

    lvl_arg :: [Demand] -> CoreExprWithFVs -> LvlM ([Demand], LevelledExpr)
    lvl_arg strs arg | (str1 : strs') <- strs
                     , is_val_arg arg
                     = do { arg' <- lvlMFE env (isStrictDmd str1) arg
                          ; return (strs', arg') }
                     | otherwise
                     = do { arg' <- lvlMFE env False arg
                          ; return (strs, arg') }

lvlApp env _ (fun, args)
  =  -- No PAPs that we can float: just carry on with the
     -- arguments and the function.
     do { args' <- mapM (lvlNonTailMFE env False) args
        ; fun'  <- lvlNonTailExpr env fun
        ; return (foldl' App fun' args') }

-------------------------------------------
lvlCase :: LevelEnv             -- Level of in-scope names/tyvars
        -> DVarSet              -- Free vars of input scrutinee
        -> LevelledExpr         -- Processed scrutinee
        -> Id -> Type           -- Case binder and result type
        -> [CoreAltWithFVs]     -- Input alternatives
        -> LvlM LevelledExpr    -- Result expression
lvlCase env scrut_fvs scrut' case_bndr ty alts
  -- See Note [Floating single-alternative cases]
  | [(con@(DataAlt {}), bs, body)] <- alts
  , exprIsHNF (deTagExpr scrut')  -- See Note [Check the output scrutinee for exprIsHNF]
  , not (isTopLvl dest_lvl)       -- Can't have top-level cases
  , not (floatTopLvlOnly env)     -- Can float anywhere
  =     -- Always float the case if possible
        -- Unlike lets we don't insist that it escapes a value lambda
    do { (env1, (case_bndr' : bs')) <- cloneCaseBndrs env dest_lvl (case_bndr : bs)
       ; let rhs_env = extendCaseBndrEnv env1 case_bndr scrut'
       ; body' <- lvlMFE rhs_env True body
       ; let alt' = (con, map (stayPut dest_lvl) bs', body')
       ; return (Case scrut' (TB case_bndr' (FloatMe dest_lvl)) ty' [alt']) }

  | otherwise     -- Stays put
  = do { let (alts_env1, [case_bndr']) = substAndLvlBndrs NonRecursive env incd_lvl [case_bndr]
             alts_env = extendCaseBndrEnv alts_env1 case_bndr scrut'
       ; alts' <- mapM (lvl_alt alts_env) alts
       ; return (Case scrut' case_bndr' ty' alts') }
  where
    ty' = substTy (le_subst env) ty

    incd_lvl = incMinorLvl (le_ctxt_lvl env)
    dest_lvl = maxFvLevel (const True) env scrut_fvs
            -- Don't abstract over type variables, hence const True

    lvl_alt alts_env (con, bs, rhs)
      = do { rhs' <- lvlMFE new_env True rhs
           ; return (con, bs', rhs') }
      where
        (new_env, bs') = substAndLvlBndrs NonRecursive alts_env incd_lvl bs

{- Note [Floating single-alternative cases]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this:
  data T a = MkT !a
  f :: T Int -> blah
  f x vs = case x of { MkT y ->
             let f vs = ...(case y of I# w -> e)...f..
             in f vs

Here we can float the (case y ...) out, because y is sure
to be evaluated, to give
  f x vs = case x of { MkT y ->
           case y of I# w ->
             let f vs = ...(e)...f..
             in f vs

That saves unboxing it every time round the loop.  It's important in
some DPH stuff where we really want to avoid that repeated unboxing in
the inner loop.

Things to note:

 * The test we perform is exprIsHNF, and /not/ exprOkForSpeculation.

     - exrpIsHNF catches the key case of an evaluated variable

     - exprOkForSpeculation is /false/ of an evaluated variable;
       See Note [exprOkForSpeculation and evaluated variables] in GHC.Core.Utils
       So we'd actually miss the key case!

     - Nothing is gained from the extra generality of exprOkForSpeculation
       since we only consider floating a case whose single alternative
       is a DataAlt   K a b -> rhs

 * We can't float a case to top level

 * It's worth doing this float even if we don't float
   the case outside a value lambda.  Example
     case x of {
       MkT y -> (case y of I# w2 -> ..., case y of I# w2 -> ...)
   If we floated the cases out we could eliminate one of them.

 * We only do this with a single-alternative case


Note [Setting levels when floating single-alternative cases]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Handling level-setting when floating a single-alternative case binding
is a bit subtle, as evidenced by #16978.  In particular, we must keep
in mind that we are merely moving the case and its binders, not the
body. For example, suppose 'a' is known to be evaluated and we have

  \z -> case a of
          (x,_) -> <body involving x and z>

After floating we may have:

  case a of
    (x,_) -> \z -> <body involving x and z>
      {- some expression involving x and z -}

When analysing <body involving...> we want to use the /ambient/ level,
and /not/ the destination level of the 'case a of (x,-) ->' binding.

#16978 was caused by us setting the context level to the destination
level of `x` when analysing <body>. This led us to conclude that we
needed to quantify over some of its free variables (e.g. z), resulting
in shadowing and very confusing Core Lint failures.


Note [Check the output scrutinee for exprIsHNF]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this:
  case x of y {
    A -> ....(case y of alts)....
  }

Because of the binder-swap, the inner case will get substituted to
(case x of ..).  So when testing whether the scrutinee is in HNF we
must be careful to test the *result* scrutinee ('x' in this case), not
the *input* one 'y'.  The latter *is* in HNF here (because y is
evaluated), but the former is not -- and indeed we can't float the
inner case out, at least not unless x is also evaluated at its binding
site.  See #5453.

That's why we apply exprIsHNF to scrut' and not to scrut.

See Note [Floating single-alternative cases] for why
we use exprIsHNF in the first place.
-}

lvlNonTailMFE :: LevelEnv             -- Level of in-scope names/tyvars
              -> Bool                 -- True <=> strict context [body of case
                                      --   or let]
              -> CoreExprWithFVs      -- input expression
              -> LvlM LevelledExpr    -- Result expression
lvlNonTailMFE env strict_ctxt ann_expr
  = lvlMFE (placeJoinCeiling env) strict_ctxt ann_expr

lvlMFE ::  LevelEnv             -- Level of in-scope names/tyvars
        -> Bool                 -- True <=> strict context [body of case or let]
        -> CoreExprWithFVs      -- input expression
        -> LvlM LevelledExpr    -- Result expression
-- lvlMFE is just like lvlExpr, except that it might let-bind
-- the expression, so that it can itself be floated.

lvlMFE env _ (_, AnnType ty)
  = return (Type (GHC.Core.Subst.substTy (le_subst env) ty))

-- No point in floating out an expression wrapped in a coercion or note
-- If we do we'll transform  lvl = e |> co
--                       to  lvl' = e; lvl = lvl' |> co
-- and then inline lvl.  Better just to float out the payload.
lvlMFE env strict_ctxt (_, AnnTick t e)
  = do { e' <- lvlMFE env strict_ctxt e
       ; let t' = substTickish (le_subst env) t
       ; return (Tick t' e') }

lvlMFE env strict_ctxt (_, AnnCast e (_, co))
  = do  { e' <- lvlMFE env strict_ctxt e
        ; return (Cast e' (substCo (le_subst env) co)) }

lvlMFE env strict_ctxt e@(_, AnnCase {})
  | strict_ctxt       -- Don't share cases in a strict context
  = lvlExpr env e     -- See Note [Case MFEs]

lvlMFE env strict_ctxt ann_expr
  |  floatTopLvlOnly env && not (isTopLvl dest_lvl)
         -- Only floating to the top level is allowed.
  || anyDVarSet isJoinId fvs   -- If there is a free join, don't float
                               -- See Note [Free join points]
  || isExprLevPoly expr
         -- We can't let-bind levity polymorphic expressions
         -- See Note [Levity polymorphism invariants] in GHC.Core
  || notWorthFloating expr abs_vars
  || not float_me
  =     -- Don't float it out
    lvlExpr env ann_expr

  |  float_is_new_lam || exprIsTopLevelBindable expr expr_ty
         -- No wrapping needed if the type is lifted, or is a literal string
         -- or if we are wrapping it in one or more value lambdas
  = do { expr1 <- lvlFloatRhs abs_vars dest_lvl rhs_env NonRecursive
                              (isJust mb_bot_str)
                              join_arity_maybe
                              ann_expr
                  -- Treat the expr just like a right-hand side
       ; var <- newLvlVar expr1 join_arity_maybe is_mk_static
       ; let var2 = annotateBotStr var float_n_lams mb_bot_str
       ; return (Let (NonRec (TB var2 (FloatMe dest_lvl)) expr1)
                     (mkVarApps (Var var2) abs_vars)) }

  -- OK, so the float has an unlifted type (not top-level bindable)
  --     and no new value lambdas (float_is_new_lam is False)
  -- Try for the boxing strategy
  -- See Note [Floating MFEs of unlifted type]
  | escapes_value_lam
  , not expr_ok_for_spec -- Boxing/unboxing isn't worth it for cheap expressions
                         -- See Note [Test cheapness with exprOkForSpeculation]
  , Just (tc, _) <- splitTyConApp_maybe expr_ty
  , Just dc <- boxingDataCon_maybe tc
  , let dc_res_ty = dataConOrigResTy dc  -- No free type variables
        [bx_bndr, ubx_bndr] = mkTemplateLocals [dc_res_ty, expr_ty]
  = do { expr1 <- lvlExpr rhs_env ann_expr
       ; let l1r       = incMinorLvlFrom rhs_env
             float_rhs = mkLams abs_vars_w_lvls $
                         Case expr1 (stayPut l1r ubx_bndr) dc_res_ty
                             [(DEFAULT, [], mkConApp dc [Var ubx_bndr])]

       ; var <- newLvlVar float_rhs Nothing is_mk_static
       ; let l1u      = incMinorLvlFrom env
             use_expr = Case (mkVarApps (Var var) abs_vars)
                             (stayPut l1u bx_bndr) expr_ty
                             [(DataAlt dc, [stayPut l1u ubx_bndr], Var ubx_bndr)]
       ; return (Let (NonRec (TB var (FloatMe dest_lvl)) float_rhs)
                     use_expr) }

  | otherwise          -- e.g. do not float unboxed tuples
  = lvlExpr env ann_expr

  where
    expr         = deAnnotate ann_expr
    expr_ty      = exprType expr
    fvs          = freeVarsOf ann_expr
    fvs_ty       = tyCoVarsOfType expr_ty
    is_bot       = isBottomThunk mb_bot_str
    is_function  = isFunction ann_expr
    mb_bot_str   = exprBotStrictness_maybe expr
                           -- See Note [Bottoming floats]
                           -- esp Bottoming floats (2)
    expr_ok_for_spec = exprOkForSpeculation expr
    dest_lvl     = destLevel env fvs fvs_ty is_function is_bot False
    abs_vars     = abstractVars dest_lvl env fvs

    -- float_is_new_lam: the floated thing will be a new value lambda
    -- replacing, say (g (x+4)) by (lvl x).  No work is saved, nor is
    -- allocation saved.  The benefit is to get it to the top level
    -- and hence out of the body of this function altogether, making
    -- it smaller and more inlinable
    float_is_new_lam = float_n_lams > 0
    float_n_lams     = count isId abs_vars

    (rhs_env, abs_vars_w_lvls) = lvlLamBndrs env dest_lvl abs_vars

    join_arity_maybe = Nothing

    is_mk_static = isJust (collectMakeStaticArgs expr)
        -- Yuk: See Note [Grand plan for static forms] in main/StaticPtrTable

        -- A decision to float entails let-binding this thing, and we only do
        -- that if we'll escape a value lambda, or will go to the top level.
    float_me = saves_work || saves_alloc || is_mk_static

    -- We can save work if we can move a redex outside a value lambda
    -- But if float_is_new_lam is True, then the redex is wrapped in a
    -- a new lambda, so no work is saved
    saves_work = escapes_value_lam && not float_is_new_lam

    escapes_value_lam = dest_lvl `ltMajLvl` (le_ctxt_lvl env)
                  -- See Note [Escaping a value lambda]

    -- See Note [Floating to the top]
    saves_alloc =  isTopLvl dest_lvl
                && floatConsts env
                && (not strict_ctxt || is_bot || exprIsHNF expr)

isBottomThunk :: Maybe (Arity, s) -> Bool
-- See Note [Bottoming floats] (2)
isBottomThunk (Just (0, _)) = True   -- Zero arity
isBottomThunk _             = False

{- Note [Floating to the top]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We are keen to float something to the top level, even if it does not
escape a value lambda (and hence save work), for two reasons:

  * Doing so makes the function smaller, by floating out
    bottoming expressions, or integer or string literals.  That in
    turn makes it easier to inline, with less duplication.

  * (Minor) Doing so may turn a dynamic allocation (done by machine
    instructions) into a static one. Minor because we are assuming
    we are not escaping a value lambda.

But do not so if:
     - the context is a strict, and
     - the expression is not a HNF, and
     - the expression is not bottoming

Exammples:

* Bottoming
      f x = case x of
              0 -> error <big thing>
              _ -> x+1
  Here we want to float (error <big thing>) to top level, abstracting
  over 'x', so as to make f's RHS smaller.

* HNF
      f = case y of
            True  -> p:q
            False -> blah
  We may as well float the (p:q) so it becomes a static data structure.

* Case scrutinee
      f = case g True of ....
  Don't float (g True) to top level; then we have the admin of a
  top-level thunk to worry about, with zero gain.

* Case alternative
      h = case y of
             True  -> g True
             False -> False
  Don't float (g True) to the top level

* Arguments
     t = f (g True)
  If f is lazy, we /do/ float (g True) because then we can allocate
  the thunk statically rather than dynamically.  But if f is strict
  we don't (see the use of idStrictness in lvlApp).  It's not clear
  if this test is worth the bother: it's only about CAFs!

It's controlled by a flag (floatConsts), because doing this too
early loses opportunities for RULES which (needless to say) are
important in some nofib programs (gcd is an example).  [SPJ note:
I think this is obsolete; the flag seems always on.]

Note [Floating join point bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Mostly we only float a join point if it can /stay/ a join point.  But
there is one exception: if it can go to the top level (#13286).
Consider
  f x = joinrec j y n = <...j y' n'...>
        in jump j x 0

Here we may just as well produce
  j y n = <....j y' n'...>
  f x = j x 0

and now there is a chance that 'f' will be inlined at its call sites.
It shouldn't make a lot of difference, but these tests
  perf/should_run/MethSharing
  simplCore/should_compile/spec-inline
and one nofib program, all improve if you do float to top, because
of the resulting inlining of f.  So ok, let's do it.

Note [Free join points]
~~~~~~~~~~~~~~~~~~~~~~~
We never float a MFE that has a free join-point variable.  You might think
this can never occur.  After all, consider
     join j x = ...
     in ....(jump j x)....
How might we ever want to float that (jump j x)?
  * If it would escape a value lambda, thus
        join j x = ... in (\y. ...(jump j x)... )
    then 'j' isn't a valid join point in the first place.

But consider
     join j x = .... in
     joinrec j2 y =  ...(jump j x)...(a+b)....

Since j2 is recursive, it /is/ worth floating (a+b) out of the joinrec.
But it is emphatically /not/ good to float the (jump j x) out:
 (a) 'j' will stop being a join point
 (b) In any case, jumping to 'j' must be an exit of the j2 loop, so no
     work would be saved by floating it out of the \y.

Even if we floated 'j' to top level, (b) would still hold.

Bottom line: never float a MFE that has a free JoinId.

Note [Floating MFEs of unlifted type]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have
   case f x of (r::Int#) -> blah
we'd like to float (f x). But it's not trivial because it has type
Int#, and we don't want to evaluate it too early.  But we can instead
float a boxed version
   y = case f x of r -> I# r
and replace the original (f x) with
   case (case y of I# r -> r) of r -> blah

Being able to float unboxed expressions is sometimes important; see
#12603.  I'm not sure how /often/ it is important, but it's
not hard to achieve.

We only do it for a fixed collection of types for which we have a
convenient boxing constructor (see boxingDataCon_maybe).  In
particular we /don't/ do it for unboxed tuples; it's better to float
the components of the tuple individually.

I did experiment with a form of boxing that works for any type, namely
wrapping in a function.  In our example

   let y = case f x of r -> \v. f x
   in case y void of r -> blah

It works fine, but it's 50% slower (based on some crude benchmarking).
I suppose we could do it for types not covered by boxingDataCon_maybe,
but it's more code and I'll wait to see if anyone wants it.

Note [Test cheapness with exprOkForSpeculation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We don't want to float very cheap expressions by boxing and unboxing.
But we use exprOkForSpeculation for the test, not exprIsCheap.
Why?  Because it's important /not/ to transform
     f (a /# 3)
to
     f (case bx of I# a -> a /# 3)
and float bx = I# (a /# 3), because the application of f no
longer obeys the let/app invariant.  But (a /# 3) is ok-for-spec
due to a special hack that says division operators can't fail
when the denominator is definitely non-zero.  And yet that
same expression says False to exprIsCheap.  Simplest way to
guarantee the let/app invariant is to use the same function!

If an expression is okay for speculation, we could also float it out
*without* boxing and unboxing, since evaluating it early is okay.
However, it turned out to usually be better not to float such expressions,
since they tend to be extremely cheap things like (x +# 1#). Even the
cost of spilling the let-bound variable to the stack across a call may
exceed the cost of recomputing such an expression. (And we can't float
unlifted bindings to top-level.)

We could try to do something smarter here, and float out expensive yet
okay-for-speculation things, such as division by non-zero constants.
But I suspect it's a narrow target.

Note [Bottoming floats]
~~~~~~~~~~~~~~~~~~~~~~~
If we see
        f = \x. g (error "urk")
we'd like to float the call to error, to get
        lvl = error "urk"
        f = \x. g lvl

But, as ever, we need to be careful:

(1) We want to float a bottoming
    expression even if it has free variables:
        f = \x. g (let v = h x in error ("urk" ++ v))
    Then we'd like to abstract over 'x' can float the whole arg of g:
        lvl = \x. let v = h x in error ("urk" ++ v)
        f = \x. g (lvl x)
    To achieve this we pass is_bot to destLevel

(2) We do not do this for lambdas that return
    bottom.  Instead we treat the /body/ of such a function specially,
    via point (1).  For example:
        f = \x. ....(\y z. if x then error y else error z)....
    ===>
        lvl = \x z y. if b then error y else error z
        f = \x. ...(\y z. lvl x z y)...
    (There is no guarantee that we'll choose the perfect argument order.)

(3) If we have a /binding/ that returns bottom, we want to float it to top
    level, even if it has free vars (point (1)), and even it has lambdas.
    Example:
       ... let { v = \y. error (show x ++ show y) } in ...
    We want to abstract over x and float the whole thing to top:
       lvl = \xy. errror (show x ++ show y)
       ...let {v = lvl x} in ...

    Then of course we don't want to separately float the body (error ...)
    as /another/ MFE, so we tell lvlFloatRhs not to do that, via the is_bot
    argument.

See Maessen's paper 1999 "Bottom extraction: factoring error handling out
of functional programs" (unpublished I think).

When we do this, we set the strictness and arity of the new bottoming
Id, *immediately*, for three reasons:

  * To prevent the abstracted thing being immediately inlined back in again
    via preInlineUnconditionally.  The latter has a test for bottoming Ids
    to stop inlining them, so we'd better make sure it *is* a bottoming Id!

  * So that it's properly exposed as such in the interface file, even if
    this is all happening after strictness analysis.

  * In case we do CSE with the same expression that *is* marked bottom
        lvl          = error "urk"
          x{str=bot) = error "urk"
    Here we don't want to replace 'x' with 'lvl', else we may get Lint
    errors, e.g. via a case with empty alternatives:  (case x of {})
    Lint complains unless the scrutinee of such a case is clearly bottom.

    This was reported in #11290.   But since the whole bottoming-float
    thing is based on the cheap-and-cheerful exprIsDeadEnd, I'm not sure
    that it'll nail all such cases.

Note [Bottoming floats: eta expansion] c.f Note [Bottoming floats]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Tiresomely, though, the simplifier has an invariant that the manifest
arity of the RHS should be the same as the arity; but we can't call
etaExpand during GHC.Core.Op.SetLevels because it works over a decorated form of
CoreExpr.  So we do the eta expansion later, in GHC.Core.Op.FloatOut.

Note [Case MFEs]
~~~~~~~~~~~~~~~~
We don't float a case expression as an MFE from a strict context.  Why not?
Because in doing so we share a tiny bit of computation (the switch) but
in exchange we build a thunk, which is bad.  This case reduces allocation
by 7% in spectral/puzzle (a rather strange benchmark) and 1.2% in real/fem.
Doesn't change any other allocation at all.

We will make a separate decision for the scrutinee and alternatives.

However this can have a knock-on effect for fusion: consider
    \v -> foldr k z (case x of I# y -> build ..y..)
Perhaps we can float the entire (case x of ...) out of the \v.  Then
fusion will not happen, but we will get more sharing.  But if we don't
float the case (as advocated here) we won't float the (build ...y..)
either, so fusion will happen.  It can be a big effect, esp in some
artificial benchmarks (e.g. integer, queens), but there is no perfect
answer.

-}

annotateBotStr :: Id -> Arity -> Maybe (Arity, StrictSig) -> Id
-- See Note [Bottoming floats] for why we want to add
-- bottoming information right now
--
-- n_extra are the number of extra value arguments added during floating
annotateBotStr id n_extra mb_str
  = case mb_str of
      Nothing           -> id
      Just (arity, sig) -> id `setIdArity`      (arity + n_extra)
                              `setIdStrictness` (prependArgsStrictSig n_extra sig)
                              `setIdCprInfo`    mkCprSig (arity + n_extra) botCpr

notWorthFloating :: CoreExpr -> [Var] -> Bool
-- Returns True if the expression would be replaced by
-- something bigger than it is now.  For example:
--   abs_vars = tvars only:  return True if e is trivial,
--                           but False for anything bigger
--   abs_vars = [x] (an Id): return True for trivial, or an application (f x)
--                           but False for (f x x)
--
-- One big goal is that floating should be idempotent.  Eg if
-- we replace e with (lvl79 x y) and then run FloatOut again, don't want
-- to replace (lvl79 x y) with (lvl83 x y)!

notWorthFloating e abs_vars
  = go e (count isId abs_vars)
  where
    go (Var {}) n    = n >= 0
    go (Lit lit) n   = ASSERT( n==0 )
                       litIsTrivial lit   -- Note [Floating literals]
    go (Tick t e) n  = not (tickishIsCode t) && go e n
    go (Cast e _)  n = go e n
    go (App e arg) n
       -- See Note [Floating applications to coercions]
       | Type {} <- arg = go e n
       | n==0           = False
       | is_triv arg    = go e (n-1)
       | otherwise      = False
    go _ _              = False

    is_triv (Lit {})              = True        -- Treat all literals as trivial
    is_triv (Var {})              = True        -- (ie not worth floating)
    is_triv (Cast e _)            = is_triv e
    is_triv (App e (Type {}))     = is_triv e   -- See Note [Floating applications to coercions]
    is_triv (Tick t e)            = not (tickishIsCode t) && is_triv e
    is_triv _                     = False

{-
Note [Floating literals]
~~~~~~~~~~~~~~~~~~~~~~~~
It's important to float Integer literals, so that they get shared,
rather than being allocated every time round the loop.
Hence the litIsTrivial.

Ditto literal strings (LitString), which we'd like to float to top
level, which is now possible.

Note [Floating applications to coercions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We don’t float out variables applied only to type arguments, since the
extra binding would be pointless: type arguments are completely erased.
But *coercion* arguments aren’t (see Note [Coercion tokens] in
CoreToStg.hs and Note [Count coercion arguments in boring contexts] in
CoreUnfold.hs), so we still want to float out variables applied only to
coercion arguments.

Note [Escaping a value lambda]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We want to float even cheap expressions out of value lambdas,
because that saves allocation.  Consider
        f = \x.  .. (\y.e) ...
Then we'd like to avoid allocating the (\y.e) every time we call f,
(assuming e does not mention x). An example where this really makes a
difference is simplrun009.

Another reason it's good is because it makes SpecContr fire on functions.
Consider
        f = \x. ....(f (\y.e))....
After floating we get
        lvl = \y.e
        f = \x. ....(f lvl)...
and that is much easier for SpecConstr to generate a robust
specialisation for.

However, if we are wrapping the thing in extra value lambdas (in
abs_vars), then nothing is saved.  E.g.
        f = \xyz. ...(e1[y],e2)....
If we float
        lvl = \y. (e1[y],e2)
        f = \xyz. ...(lvl y)...
we have saved nothing: one pair will still be allocated for each
call of 'f'.  Hence the (not float_is_lam) in float_me.


************************************************************************
*                                                                      *
\subsection{Bindings}
*                                                                      *
************************************************************************

The binding stuff works for top level too.
-}

lvlBind :: LevelEnv
        -> CoreBindWithFVs
        -> LvlM (LevelledBind, LevelEnv)

lvlBind env (AnnNonRec bndr rhs)
  | isTyVar bndr    -- Don't do anything for TyVar binders
                    --   (simplifier gets rid of them pronto)
  || isCoVar bndr   -- Difficult to fix up CoVar occurrences (see extendPolyLvlEnv)
                    -- so we will ignore this case for now
  || not (profitableFloat env dest_lvl)
  || (isTopLvl dest_lvl && not (exprIsTopLevelBindable deann_rhs bndr_ty))
          -- We can't float an unlifted binding to top level (except
          -- literal strings), so we don't float it at all.  It's a
          -- bit brutal, but unlifted bindings aren't expensive either

  = -- No float
    do { rhs' <- lvlRhs env NonRecursive is_bot mb_join_arity rhs
       ; let  bind_lvl        = incMinorLvl (le_ctxt_lvl env)
              (env', [bndr']) = substAndLvlBndrs NonRecursive env bind_lvl [bndr]
       ; return (NonRec bndr' rhs', env') }

  -- Otherwise we are going to float
  | null abs_vars
  = do {  -- No type abstraction; clone existing binder
         rhs' <- lvlFloatRhs [] dest_lvl env NonRecursive
                             is_bot mb_join_arity rhs
       ; (env', [bndr']) <- cloneLetVars NonRecursive env dest_lvl [bndr]
       ; let bndr2 = annotateBotStr bndr' 0 mb_bot_str
       ; return (NonRec (TB bndr2 (FloatMe dest_lvl)) rhs', env') }

  | otherwise
  = do {  -- Yes, type abstraction; create a new binder, extend substitution, etc
         rhs' <- lvlFloatRhs abs_vars dest_lvl env NonRecursive
                             is_bot mb_join_arity rhs
       ; (env', [bndr']) <- newPolyBndrs dest_lvl env abs_vars [bndr]
       ; let bndr2 = annotateBotStr bndr' n_extra mb_bot_str
       ; return (NonRec (TB bndr2 (FloatMe dest_lvl)) rhs', env') }

  where
    bndr_ty    = idType bndr
    ty_fvs     = tyCoVarsOfType bndr_ty
    rhs_fvs    = freeVarsOf rhs
    bind_fvs   = rhs_fvs `unionDVarSet` dIdFreeVars bndr
    abs_vars   = abstractVars dest_lvl env bind_fvs
    dest_lvl   = destLevel env bind_fvs ty_fvs (isFunction rhs) is_bot is_join

    deann_rhs  = deAnnotate rhs
    mb_bot_str = exprBotStrictness_maybe deann_rhs
    is_bot     = isJust mb_bot_str
        -- NB: not isBottomThunk!  See Note [Bottoming floats] point (3)

    n_extra    = count isId abs_vars
    mb_join_arity = isJoinId_maybe bndr
    is_join       = isJust mb_join_arity

lvlBind env (AnnRec pairs)
  |  floatTopLvlOnly env && not (isTopLvl dest_lvl)
         -- Only floating to the top level is allowed.
  || not (profitableFloat env dest_lvl)
  || (isTopLvl dest_lvl && any (mightBeUnliftedType . idType) bndrs)
       -- This mightBeUnliftedType stuff is the same test as in the non-rec case
       -- You might wonder whether we can have a recursive binding for
       -- an unlifted value -- but we can if it's a /join binding/ (#16978)
       -- (Ultimately I think we should not use GHC.Core.Op.SetLevels to
       -- float join bindings at all, but that's another story.)
  =    -- No float
    do { let bind_lvl       = incMinorLvl (le_ctxt_lvl env)
             (env', bndrs') = substAndLvlBndrs Recursive env bind_lvl bndrs
             lvl_rhs (b,r)  = lvlRhs env' Recursive is_bot (isJoinId_maybe b) r
       ; rhss' <- mapM lvl_rhs pairs
       ; return (Rec (bndrs' `zip` rhss'), env') }

  -- Otherwise we are going to float
  | null abs_vars
  = do { (new_env, new_bndrs) <- cloneLetVars Recursive env dest_lvl bndrs
       ; new_rhss <- mapM (do_rhs new_env) pairs
       ; return ( Rec ([TB b (FloatMe dest_lvl) | b <- new_bndrs] `zip` new_rhss)
                , new_env) }

-- ToDo: when enabling the floatLambda stuff,
--       I think we want to stop doing this
  | [(bndr,rhs)] <- pairs
  , count isId abs_vars > 1
  = do  -- Special case for self recursion where there are
        -- several variables carried around: build a local loop:
        --      poly_f = \abs_vars. \lam_vars . letrec f = \lam_vars. rhs in f lam_vars
        -- This just makes the closures a bit smaller.  If we don't do
        -- this, allocation rises significantly on some programs
        --
        -- We could elaborate it for the case where there are several
        -- mutually recursive functions, but it's quite a bit more complicated
        --
        -- This all seems a bit ad hoc -- sigh
    let (rhs_env, abs_vars_w_lvls) = lvlLamBndrs env dest_lvl abs_vars
        rhs_lvl = le_ctxt_lvl rhs_env

    (rhs_env', [new_bndr]) <- cloneLetVars Recursive rhs_env rhs_lvl [bndr]
    let
        (lam_bndrs, rhs_body)   = collectAnnBndrs rhs
        (body_env1, lam_bndrs1) = substBndrsSL NonRecursive rhs_env' lam_bndrs
        (body_env2, lam_bndrs2) = lvlLamBndrs body_env1 rhs_lvl lam_bndrs1
    new_rhs_body <- lvlRhs body_env2 Recursive is_bot (get_join bndr) rhs_body
    (poly_env, [poly_bndr]) <- newPolyBndrs dest_lvl env abs_vars [bndr]
    return (Rec [(TB poly_bndr (FloatMe dest_lvl)
                 , mkLams abs_vars_w_lvls $
                   mkLams lam_bndrs2 $
                   Let (Rec [( TB new_bndr (StayPut rhs_lvl)
                             , mkLams lam_bndrs2 new_rhs_body)])
                       (mkVarApps (Var new_bndr) lam_bndrs1))]
           , poly_env)

  | otherwise  -- Non-null abs_vars
  = do { (new_env, new_bndrs) <- newPolyBndrs dest_lvl env abs_vars bndrs
       ; new_rhss <- mapM (do_rhs new_env) pairs
       ; return ( Rec ([TB b (FloatMe dest_lvl) | b <- new_bndrs] `zip` new_rhss)
                , new_env) }

  where
    (bndrs,rhss) = unzip pairs
    is_join  = isJoinId (head bndrs)
                -- bndrs is always non-empty and if one is a join they all are
                -- Both are checked by Lint
    is_fun   = all isFunction rhss
    is_bot   = False  -- It's odd to have an unconditionally divergent
                      -- function in a Rec, and we don't much care what
                      -- happens to it.  False is simple!

    do_rhs env (bndr,rhs) = lvlFloatRhs abs_vars dest_lvl env Recursive
                                        is_bot (get_join bndr)
                                        rhs

    get_join bndr | need_zap  = Nothing
                  | otherwise = isJoinId_maybe bndr
    need_zap = dest_lvl `ltLvl` joinCeilingLevel env

        -- Finding the free vars of the binding group is annoying
    bind_fvs = ((unionDVarSets [ freeVarsOf rhs | (_, rhs) <- pairs])
                `unionDVarSet`
                (fvDVarSet $ unionsFV [ idFVs bndr
                                      | (bndr, (_,_)) <- pairs]))
               `delDVarSetList`
                bndrs

    ty_fvs   = foldr (unionVarSet . tyCoVarsOfType . idType) emptyVarSet bndrs
    dest_lvl = destLevel env bind_fvs ty_fvs is_fun is_bot is_join
    abs_vars = abstractVars dest_lvl env bind_fvs

profitableFloat :: LevelEnv -> Level -> Bool
profitableFloat env dest_lvl
  =  (dest_lvl `ltMajLvl` le_ctxt_lvl env)  -- Escapes a value lambda
  || isTopLvl dest_lvl                      -- Going all the way to top level


----------------------------------------------------
-- Three help functions for the type-abstraction case

lvlRhs :: LevelEnv
       -> RecFlag
       -> Bool               -- Is this a bottoming function
       -> Maybe JoinArity
       -> CoreExprWithFVs
       -> LvlM LevelledExpr
lvlRhs env rec_flag is_bot mb_join_arity expr
  = lvlFloatRhs [] (le_ctxt_lvl env) env
                rec_flag is_bot mb_join_arity expr

lvlFloatRhs :: [OutVar] -> Level -> LevelEnv -> RecFlag
            -> Bool   -- Binding is for a bottoming function
            -> Maybe JoinArity
            -> CoreExprWithFVs
            -> LvlM (Expr LevelledBndr)
-- Ignores the le_ctxt_lvl in env; treats dest_lvl as the baseline
lvlFloatRhs abs_vars dest_lvl env rec is_bot mb_join_arity rhs
  = do { body' <- if not is_bot  -- See Note [Floating from a RHS]
                     && any isId bndrs
                  then lvlMFE  body_env True body
                  else lvlExpr body_env      body
       ; return (mkLams bndrs' body') }
  where
    (bndrs, body)     | Just join_arity <- mb_join_arity
                      = collectNAnnBndrs join_arity rhs
                      | otherwise
                      = collectAnnBndrs rhs
    (env1, bndrs1)    = substBndrsSL NonRecursive env bndrs
    all_bndrs         = abs_vars ++ bndrs1
    (body_env, bndrs') | Just _ <- mb_join_arity
                      = lvlJoinBndrs env1 dest_lvl rec all_bndrs
                      | otherwise
                      = case lvlLamBndrs env1 dest_lvl all_bndrs of
                          (env2, bndrs') -> (placeJoinCeiling env2, bndrs')
        -- The important thing here is that we call lvlLamBndrs on
        -- all these binders at once (abs_vars and bndrs), so they
        -- all get the same major level.  Otherwise we create stupid
        -- let-bindings inside, joyfully thinking they can float; but
        -- in the end they don't because we never float bindings in
        -- between lambdas

{- Note [Floating from a RHS]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When floating the RHS of a let-binding, we don't always want to apply
lvlMFE to the body of a lambda, as we usually do, because the entire
binding body is already going to the right place (dest_lvl).

A particular example is the top level.  Consider
   concat = /\ a -> foldr ..a.. (++) []
We don't want to float the body of the lambda to get
   lvl    = /\ a -> foldr ..a.. (++) []
   concat = /\ a -> lvl a
That would be stupid.

Previously this was avoided in a much nastier way, by testing strict_ctxt
in float_me in lvlMFE.  But that wasn't even right because it would fail
to float out the error sub-expression in
    f = \x. case x of
              True  -> error ("blah" ++ show x)
              False -> ...

But we must be careful:

* If we had
    f = \x -> factorial 20
  we /would/ want to float that (factorial 20) out!  Functions are treated
  differently: see the use of isFunction in the calls to destLevel. If
  there are only type lambdas, then destLevel will say "go to top, and
  abstract over the free tyvars" and we don't want that here.

* But if we had
    f = \x -> error (...x....)
  we would NOT want to float the bottoming expression out to give
    lvl = \x -> error (...x...)
    f = \x -> lvl x

Conclusion: use lvlMFE if there are
  * any value lambdas in the original function, and
  * this is not a bottoming function (the is_bot argument)
Use lvlExpr otherwise.  A little subtle, and I got it wrong at least twice
(e.g. #13369).
-}

{-
************************************************************************
*                                                                      *
\subsection{Deciding floatability}
*                                                                      *
************************************************************************
-}

substAndLvlBndrs :: RecFlag -> LevelEnv -> Level -> [InVar] -> (LevelEnv, [LevelledBndr])
substAndLvlBndrs is_rec env lvl bndrs
  = lvlBndrs subst_env lvl subst_bndrs
  where
    (subst_env, subst_bndrs) = substBndrsSL is_rec env bndrs

substBndrsSL :: RecFlag -> LevelEnv -> [InVar] -> (LevelEnv, [OutVar])
-- So named only to avoid the name clash with GHC.Core.Subst.substBndrs
substBndrsSL is_rec env@(LE { le_subst = subst, le_env = id_env }) bndrs
  = ( env { le_subst    = subst'
          , le_env      = foldl' add_id  id_env (bndrs `zip` bndrs') }
    , bndrs')
  where
    (subst', bndrs') = case is_rec of
                         NonRecursive -> substBndrs    subst bndrs
                         Recursive    -> substRecBndrs subst bndrs

lvlLamBndrs :: LevelEnv -> Level -> [OutVar] -> (LevelEnv, [LevelledBndr])
-- Compute the levels for the binders of a lambda group
lvlLamBndrs env lvl bndrs
  = lvlBndrs env new_lvl bndrs
  where
    new_lvl | any is_major bndrs = incMajorLvl lvl
            | otherwise          = incMinorLvl lvl

    is_major bndr = isId bndr && not (isProbablyOneShotLambda bndr)
       -- The "probably" part says "don't float things out of a
       -- probable one-shot lambda"
       -- See Note [Computing one-shot info] in GHC.Types.Demand

lvlJoinBndrs :: LevelEnv -> Level -> RecFlag -> [OutVar]
             -> (LevelEnv, [LevelledBndr])
lvlJoinBndrs env lvl rec bndrs
  = lvlBndrs env new_lvl bndrs
  where
    new_lvl | isRec rec = incMajorLvl lvl
            | otherwise = incMinorLvl lvl
      -- Non-recursive join points are one-shot; recursive ones are not

lvlBndrs :: LevelEnv -> Level -> [CoreBndr] -> (LevelEnv, [LevelledBndr])
-- The binders returned are exactly the same as the ones passed,
-- apart from applying the substitution, but they are now paired
-- with a (StayPut level)
--
-- The returned envt has le_ctxt_lvl updated to the new_lvl
--
-- All the new binders get the same level, because
-- any floating binding is either going to float past
-- all or none.  We never separate binders.
lvlBndrs env@(LE { le_lvl_env = lvl_env }) new_lvl bndrs
  = ( env { le_ctxt_lvl = new_lvl
          , le_join_ceil = new_lvl
          , le_lvl_env  = addLvls new_lvl lvl_env bndrs }
    , map (stayPut new_lvl) bndrs)

stayPut :: Level -> OutVar -> LevelledBndr
stayPut new_lvl bndr = TB bndr (StayPut new_lvl)

  -- Destination level is the max Id level of the expression
  -- (We'll abstract the type variables, if any.)
destLevel :: LevelEnv
          -> DVarSet    -- Free vars of the term
          -> TyCoVarSet -- Free in the /type/ of the term
                        -- (a subset of the previous argument)
          -> Bool   -- True <=> is function
          -> Bool   -- True <=> is bottom
          -> Bool   -- True <=> is a join point
          -> Level
-- INVARIANT: if is_join=True then result >= join_ceiling
destLevel env fvs fvs_ty is_function is_bot is_join
  | isTopLvl max_fv_id_level  -- Float even joins if they get to top level
                              -- See Note [Floating join point bindings]
  = tOP_LEVEL

  | is_join  -- Never float a join point past the join ceiling
             -- See Note [Join points] in GHC.Core.Op.FloatOut
  = if max_fv_id_level `ltLvl` join_ceiling
    then join_ceiling
    else max_fv_id_level

  | is_bot              -- Send bottoming bindings to the top
  = as_far_as_poss      -- regardless; see Note [Bottoming floats]
                        -- Esp Bottoming floats (1)

  | Just n_args <- floatLams env
  , n_args > 0  -- n=0 case handled uniformly by the 'otherwise' case
  , is_function
  , countFreeIds fvs <= n_args
  = as_far_as_poss  -- Send functions to top level; see
                    -- the comments with isFunction

  | otherwise = max_fv_id_level
  where
    join_ceiling    = joinCeilingLevel env
    max_fv_id_level = maxFvLevel isId env fvs -- Max over Ids only; the
                                              -- tyvars will be abstracted

    as_far_as_poss = maxFvLevel' isId env fvs_ty
                     -- See Note [Floating and kind casts]

{- Note [Floating and kind casts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this
   case x of
     K (co :: * ~# k) -> let v :: Int |> co
                             v = e
                         in blah

Then, even if we are abstracting over Ids, or if e is bottom, we can't
float v outside the 'co' binding.  Reason: if we did we'd get
    v' :: forall k. (Int ~# Age) => Int |> co
and now 'co' isn't in scope in that type. The underlying reason is
that 'co' is a value-level thing and we can't abstract over that in a
type (else we'd get a dependent type).  So if v's /type/ mentions 'co'
we can't float it out beyond the binding site of 'co'.

That's why we have this as_far_as_poss stuff.  Usually as_far_as_poss
is just tOP_LEVEL; but occasionally a coercion variable (which is an
Id) mentioned in type prevents this.

Example #14270 comment:15.
-}


isFunction :: CoreExprWithFVs -> Bool
-- The idea here is that we want to float *functions* to
-- the top level.  This saves no work, but
--      (a) it can make the host function body a lot smaller,
--              and hence inlinable.
--      (b) it can also save allocation when the function is recursive:
--          h = \x -> letrec f = \y -> ...f...y...x...
--                    in f x
--     becomes
--          f = \x y -> ...(f x)...y...x...
--          h = \x -> f x x
--     No allocation for f now.
-- We may only want to do this if there are sufficiently few free
-- variables.  We certainly only want to do it for values, and not for
-- constructors.  So the simple thing is just to look for lambdas
isFunction (_, AnnLam b e) | isId b    = True
                           | otherwise = isFunction e
-- isFunction (_, AnnTick _ e)         = isFunction e  -- dubious
isFunction _                           = False

countFreeIds :: DVarSet -> Int
countFreeIds = nonDetFoldUDFM add 0 . getUniqDSet
  -- It's OK to use nonDetFoldUDFM here because we're just counting things.
  where
    add :: Var -> Int -> Int
    add v n | isId v    = n+1
            | otherwise = n

{-
************************************************************************
*                                                                      *
\subsection{Free-To-Level Monad}
*                                                                      *
************************************************************************
-}

data LevelEnv
  = LE { le_switches :: FloatOutSwitches
       , le_ctxt_lvl :: Level           -- The current level
       , le_lvl_env  :: VarEnv Level    -- Domain is *post-cloned* TyVars and Ids
       , le_join_ceil:: Level           -- Highest level to which joins float
                                        -- Invariant: always >= le_ctxt_lvl

       -- See Note [le_subst and le_env]
       , le_subst    :: Subst           -- Domain is pre-cloned TyVars and Ids
                                        -- The Id -> CoreExpr in the Subst is ignored
                                        -- (since we want to substitute a LevelledExpr for
                                        -- an Id via le_env) but we do use the Co/TyVar substs
       , le_env      :: IdEnv ([OutVar], LevelledExpr)  -- Domain is pre-cloned Ids
    }

{- Note [le_subst and le_env]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We clone let- and case-bound variables so that they are still distinct
when floated out; hence the le_subst/le_env.  (see point 3 of the
module overview comment).  We also use these envs when making a
variable polymorphic because we want to float it out past a big
lambda.

The le_subst and le_env always implement the same mapping,
     in_x :->  out_x a b
where out_x is an OutVar, and a,b are its arguments (when
we perform abstraction at the same time as floating).

  le_subst maps to CoreExpr
  le_env   maps to LevelledExpr

Since the range is always a variable or application, there is never
any difference between the two, but sadly the types differ.  The
le_subst is used when substituting in a variable's IdInfo; the le_env
when we find a Var.

In addition the le_env records a [OutVar] of variables free in the
OutExpr/LevelledExpr, just so we don't have to call freeVars
repeatedly.  This list is always non-empty, and the first element is
out_x

The domain of the both envs is *pre-cloned* Ids, though

The domain of the le_lvl_env is the *post-cloned* Ids
-}

initialEnv :: FloatOutSwitches -> LevelEnv
initialEnv float_lams
  = LE { le_switches = float_lams
       , le_ctxt_lvl = tOP_LEVEL
       , le_join_ceil = panic "initialEnv"
       , le_lvl_env = emptyVarEnv
       , le_subst = emptySubst
       , le_env = emptyVarEnv }

addLvl :: Level -> VarEnv Level -> OutVar -> VarEnv Level
addLvl dest_lvl env v' = extendVarEnv env v' dest_lvl

addLvls :: Level -> VarEnv Level -> [OutVar] -> VarEnv Level
addLvls dest_lvl env vs = foldl' (addLvl dest_lvl) env vs

floatLams :: LevelEnv -> Maybe Int
floatLams le = floatOutLambdas (le_switches le)

floatConsts :: LevelEnv -> Bool
floatConsts le = floatOutConstants (le_switches le)

floatOverSat :: LevelEnv -> Bool
floatOverSat le = floatOutOverSatApps (le_switches le)

floatTopLvlOnly :: LevelEnv -> Bool
floatTopLvlOnly le = floatToTopLevelOnly (le_switches le)

incMinorLvlFrom :: LevelEnv -> Level
incMinorLvlFrom env = incMinorLvl (le_ctxt_lvl env)

-- extendCaseBndrEnv adds the mapping case-bndr->scrut-var if it can
-- See Note [Binder-swap during float-out]
extendCaseBndrEnv :: LevelEnv
                  -> Id                 -- Pre-cloned case binder
                  -> Expr LevelledBndr  -- Post-cloned scrutinee
                  -> LevelEnv
extendCaseBndrEnv le@(LE { le_subst = subst, le_env = id_env })
                  case_bndr (Var scrut_var)
  = le { le_subst   = extendSubstWithVar subst case_bndr scrut_var
       , le_env     = add_id id_env (case_bndr, scrut_var) }
extendCaseBndrEnv env _ _ = env

-- See Note [Join ceiling]
placeJoinCeiling :: LevelEnv -> LevelEnv
placeJoinCeiling le@(LE { le_ctxt_lvl = lvl })
  = le { le_ctxt_lvl = lvl', le_join_ceil = lvl' }
  where
    lvl' = asJoinCeilLvl (incMinorLvl lvl)

maxFvLevel :: (Var -> Bool) -> LevelEnv -> DVarSet -> Level
maxFvLevel max_me env var_set
  = foldDVarSet (maxIn max_me env) tOP_LEVEL var_set

maxFvLevel' :: (Var -> Bool) -> LevelEnv -> TyCoVarSet -> Level
-- Same but for TyCoVarSet
maxFvLevel' max_me env var_set
  = nonDetFoldUniqSet (maxIn max_me env) tOP_LEVEL var_set

maxIn :: (Var -> Bool) -> LevelEnv -> InVar -> Level -> Level
maxIn max_me (LE { le_lvl_env = lvl_env, le_env = id_env }) in_var lvl
  = case lookupVarEnv id_env in_var of
      Just (abs_vars, _) -> foldr max_out lvl abs_vars
      Nothing            -> max_out in_var lvl
  where
    max_out out_var lvl
        | max_me out_var = case lookupVarEnv lvl_env out_var of
                                Just lvl' -> maxLvl lvl' lvl
                                Nothing   -> lvl
        | otherwise = lvl       -- Ignore some vars depending on max_me

lookupVar :: LevelEnv -> Id -> LevelledExpr
lookupVar le v = case lookupVarEnv (le_env le) v of
                    Just (_, expr) -> expr
                    _              -> Var v

-- Level to which join points are allowed to float (boundary of current tail
-- context). See Note [Join ceiling]
joinCeilingLevel :: LevelEnv -> Level
joinCeilingLevel = le_join_ceil

abstractVars :: Level -> LevelEnv -> DVarSet -> [OutVar]
        -- Find the variables in fvs, free vars of the target expression,
        -- whose level is greater than the destination level
        -- These are the ones we are going to abstract out
        --
        -- Note that to get reproducible builds, the variables need to be
        -- abstracted in deterministic order, not dependent on the values of
        -- Uniques. This is achieved by using DVarSets, deterministic free
        -- variable computation and deterministic sort.
        -- See Note [Unique Determinism] in GHC.Types.Unique for explanation of why
        -- Uniques are not deterministic.
abstractVars dest_lvl (LE { le_subst = subst, le_lvl_env = lvl_env }) in_fvs
  =  -- NB: sortQuantVars might not put duplicates next to each other
    map zap $ sortQuantVars $
    filter abstract_me      $
    dVarSetElems            $
    closeOverKindsDSet      $
    substDVarSet subst in_fvs
        -- NB: it's important to call abstract_me only on the OutIds the
        -- come from substDVarSet (not on fv, which is an InId)
  where
    abstract_me v = case lookupVarEnv lvl_env v of
                        Just lvl -> dest_lvl `ltLvl` lvl
                        Nothing  -> False

        -- We are going to lambda-abstract, so nuke any IdInfo,
        -- and add the tyvars of the Id (if necessary)
    zap v | isId v = WARN( isStableUnfolding (idUnfolding v) ||
                           not (isEmptyRuleInfo (idSpecialisation v)),
                           text "absVarsOf: discarding info on" <+> ppr v )
                     setIdInfo v vanillaIdInfo
          | otherwise = v

type LvlM result = UniqSM result

initLvl :: UniqSupply -> UniqSM a -> a
initLvl = initUs_

newPolyBndrs :: Level -> LevelEnv -> [OutVar] -> [InId]
             -> LvlM (LevelEnv, [OutId])
-- The envt is extended to bind the new bndrs to dest_lvl, but
-- the le_ctxt_lvl is unaffected
newPolyBndrs dest_lvl
             env@(LE { le_lvl_env = lvl_env, le_subst = subst, le_env = id_env })
             abs_vars bndrs
 = ASSERT( all (not . isCoVar) bndrs )   -- What would we add to the CoSubst in this case. No easy answer.
   do { uniqs <- getUniquesM
      ; let new_bndrs = zipWith mk_poly_bndr bndrs uniqs
            bndr_prs  = bndrs `zip` new_bndrs
            env' = env { le_lvl_env = addLvls dest_lvl lvl_env new_bndrs
                       , le_subst   = foldl' add_subst subst   bndr_prs
                       , le_env     = foldl' add_id    id_env  bndr_prs }
      ; return (env', new_bndrs) }
  where
    add_subst env (v, v') = extendIdSubst env v (mkVarApps (Var v') abs_vars)
    add_id    env (v, v') = extendVarEnv env v ((v':abs_vars), mkVarApps (Var v') abs_vars)

    mk_poly_bndr bndr uniq = transferPolyIdInfo bndr abs_vars $ -- Note [transferPolyIdInfo] in GHC.Types.Id
                             transfer_join_info bndr $
                             mkSysLocal (mkFastString str) uniq poly_ty
                           where
                             str     = "poly_" ++ occNameString (getOccName bndr)
                             poly_ty = mkLamTypes abs_vars (GHC.Core.Subst.substTy subst (idType bndr))

    -- If we are floating a join point to top level, it stops being
    -- a join point.  Otherwise it continues to be a join point,
    -- but we may need to adjust its arity
    dest_is_top = isTopLvl dest_lvl
    transfer_join_info bndr new_bndr
      | Just join_arity <- isJoinId_maybe bndr
      , not dest_is_top
      = new_bndr `asJoinId` join_arity + length abs_vars
      | otherwise
      = new_bndr

newLvlVar :: LevelledExpr        -- The RHS of the new binding
          -> Maybe JoinArity     -- Its join arity, if it is a join point
          -> Bool                -- True <=> the RHS looks like (makeStatic ...)
          -> LvlM Id
newLvlVar lvld_rhs join_arity_maybe is_mk_static
  = do { uniq <- getUniqueM
       ; return (add_join_info (mk_id uniq rhs_ty))
       }
  where
    add_join_info var = var `asJoinId_maybe` join_arity_maybe
    de_tagged_rhs = deTagExpr lvld_rhs
    rhs_ty        = exprType de_tagged_rhs

    mk_id uniq rhs_ty
      -- See Note [Grand plan for static forms] in StaticPtrTable.
      | is_mk_static
      = mkExportedVanillaId (mkSystemVarName uniq (mkFastString "static_ptr"))
                            rhs_ty
      | otherwise
      = mkSysLocal (mkFastString "lvl") uniq rhs_ty

-- | Clone the binders bound by a single-alternative case.
cloneCaseBndrs :: LevelEnv -> Level -> [Var] -> LvlM (LevelEnv, [Var])
cloneCaseBndrs env@(LE { le_subst = subst, le_lvl_env = lvl_env, le_env = id_env })
               new_lvl vs
  = do { us <- getUniqueSupplyM
       ; let (subst', vs') = cloneBndrs subst us vs
             -- N.B. We are not moving the body of the case, merely its case
             -- binders.  Consequently we should *not* set le_ctxt_lvl and
             -- le_join_ceil.  See Note [Setting levels when floating
             -- single-alternative cases].
             env' = env { le_lvl_env   = addLvls new_lvl lvl_env vs'
                        , le_subst     = subst'
                        , le_env       = foldl' add_id id_env (vs `zip` vs') }

       ; return (env', vs') }

cloneLetVars :: RecFlag -> LevelEnv -> Level -> [InVar]
             -> LvlM (LevelEnv, [OutVar])
-- See Note [Need for cloning during float-out]
-- Works for Ids bound by let(rec)
-- The dest_lvl is attributed to the binders in the new env,
-- but cloneVars doesn't affect the le_ctxt_lvl of the incoming env
cloneLetVars is_rec
          env@(LE { le_subst = subst, le_lvl_env = lvl_env, le_env = id_env })
          dest_lvl vs
  = do { us <- getUniqueSupplyM
       ; let vs1  = map zap vs
                      -- See Note [Zapping the demand info]
             (subst', vs2) = case is_rec of
                               NonRecursive -> cloneBndrs      subst us vs1
                               Recursive    -> cloneRecIdBndrs subst us vs1
             prs  = vs `zip` vs2
             env' = env { le_lvl_env = addLvls dest_lvl lvl_env vs2
                        , le_subst   = subst'
                        , le_env     = foldl' add_id id_env prs }

       ; return (env', vs2) }
  where
    zap :: Var -> Var
    zap v | isId v    = zap_join (zapIdDemandInfo v)
          | otherwise = v

    zap_join | isTopLvl dest_lvl = zapJoinId
             | otherwise         = id

add_id :: IdEnv ([Var], LevelledExpr) -> (Var, Var) -> IdEnv ([Var], LevelledExpr)
add_id id_env (v, v1)
  | isTyVar v = delVarEnv    id_env v
  | otherwise = extendVarEnv id_env v ([v1], ASSERT(not (isCoVar v1)) Var v1)

{-
Note [Zapping the demand info]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
VERY IMPORTANT: we must zap the demand info if the thing is going to
float out, because it may be less demanded than at its original
binding site.  Eg
   f :: Int -> Int
   f x = let v = 3*4 in v+x
Here v is strict; but if we float v to top level, it isn't any more.

Similarly, if we're floating a join point, it won't be one anymore, so we zap
join point information as well.
-}
