{-# LANGUAGE PatternSynonyms #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section{GHC.Core.Opt.SetLevels}

                ***************************
                        Overview
                ***************************

1. We attach binding levels to Core bindings, in preparation for floating
   outwards (@FloatOut@).

2. We also let-ify many expressions (notably case scrutinees), so they
   will have a fighting chance of being floated sensibly.

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
   This means that a sub-expression involving x is not "trapped" inside the RHS
   (i.e. it can now be floated out, whereas if it mentioned wild it could not).
   And it's not inconvenient because we already have a substitution.

   For example, consider:

      f x = letrec go y = case x of z { (a,b) -> ...(expensive z)... }
              in ...

   If we do the reverse binder-swap we get

      f x = letrec go y = case x of z { (a,b) -> ...(expensive x)... }
              in ...

   and now we can float out:

      f x = let t = expensive x
              in letrec go y = case x of z { (a,b) -> ...(t)... }
              in ...

   Now (expensive x) is computed once, rather than once each time around the 'go' loop.

   Note that this is EXACTLY BACKWARDS from the what the simplifier does.
   The simplifier tries to get rid of occurrences of x, in favour of wild,
   in the hope that there will only be one remaining occurrence of x, namely
   the scrutinee of the case, and we can inline it.

-}

module GHC.Core.Opt.SetLevels (
        setLevels,

        Level(..), tOP_LEVEL,
        LevelledBind, LevelledExpr, LevelledBndr,
        FloatSpec(..), floatSpecLevel,

        incMinorLvl, ltMajLvl, ltLvl, isTopLvl
    ) where

import GHC.Prelude

import GHC.Core
import GHC.Core.Opt.Monad ( FloatOutSwitches(..) )
import GHC.Core.Utils
import GHC.Core.Opt.Arity   ( exprBotStrictness_maybe, isOneShotBndr )
import GHC.Core.FVs     -- all of it
import GHC.Core.Subst
import GHC.Core.Make    ( sortQuantVars )
import GHC.Core.Type    ( Type, tyCoVarsOfType
                        , mightBeUnliftedType, closeOverKindsDSet
                        , typeHasFixedRuntimeRep
                        )
import GHC.Core.Multiplicity     ( pattern ManyTy )

import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.Var
import GHC.Types.Var.Set
import GHC.Types.Unique.Set   ( nonDetStrictFoldUniqSet )
import GHC.Types.Unique.DSet  ( getUniqDSet )
import GHC.Types.Var.Env
import GHC.Types.Literal      ( litIsTrivial )
import GHC.Types.Demand       ( DmdSig, prependArgsDmdSig )
import GHC.Types.Cpr          ( CprSig, prependArgsCprSig )
import GHC.Types.Name         ( getOccName, mkSystemVarName )
import GHC.Types.Name.Occurrence ( occNameFS )
import GHC.Types.Unique       ( hasKey )
import GHC.Types.Tickish      ( tickishIsCode )
import GHC.Types.Unique.Supply
import GHC.Types.Unique.DFM
import GHC.Types.Basic  ( Arity, RecFlag(..), isRec )

import GHC.Builtin.Types
import GHC.Builtin.Names      ( runRWKey )

import GHC.Data.FastString

import GHC.Utils.FV
import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic

import Data.Maybe

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
-- In GHC.Core.Opt.SetLevels we do set the context to (Level 0 0) when we get to an InlineMe
-- which discourages floating out.

So the conclusion is: don't do any floating at all inside an InlineMe.
(In the above example, don't float the {x=e} out of the \d.)

One particular case is that of workers: we don't want to float the
call to the worker outside the wrapper, otherwise the worker might get
inlined into the floated expression, and an importing module won't see
the worker at all.
-}

instance Outputable FloatSpec where
  ppr (FloatMe l) = char 'F' <> ppr l
  ppr (StayPut l) = ppr l

tOP_LEVEL :: Level
tOP_LEVEL   = Level 0 0

incMajorLvl :: Level -> Level
incMajorLvl (Level major _) = Level (major + 1) 0

incMinorLvl :: Level -> Level
incMinorLvl (Level major minor) = Level major (minor+1)

maxLvl :: Level -> Level -> Level
maxLvl l1@(Level maj1 min1) l2@(Level maj2 min2)
  | (maj1 > maj2) || (maj1 == maj2 && min1 > min2) = l1
  | otherwise                                      = l2

ltLvl :: Level -> Level -> Bool
ltLvl (Level maj1 min1) (Level maj2 min2)
  = (maj1 < maj2) || (maj1 == maj2 && min1 < min2)

ltMajLvl :: Level -> Level -> Bool
    -- Tells if one level belongs to a difft *lambda* level to another
ltMajLvl (Level maj1 _) (Level maj2 _) = maj1 < maj2

isTopLvl :: Level -> Bool
isTopLvl (Level 0 0) = True
isTopLvl _           = False

instance Outputable Level where
  ppr (Level maj min)
    = hcat [ char '<', int maj, char ',', int min, char '>' ]

instance Eq Level where
  (Level maj1 min1) == (Level maj2 min2) = maj1 == maj2 && min1 == min2

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
  = initLvl us (do_them binds)
  where
    env = initialEnv float_lams binds

    do_them :: [CoreBind] -> LvlM [LevelledBind]
    do_them [] = return []
    do_them (b:bs)
      = do { lvld_bind <- lvlTopBind env b
           ; lvld_binds <- do_them bs
           ; return (lvld_bind : lvld_binds) }

lvlTopBind :: LevelEnv -> Bind Id -> LvlM LevelledBind
lvlTopBind env (NonRec bndr rhs)
  = do { (bndr', rhs') <- lvl_top env NonRecursive bndr rhs
       ; return (NonRec bndr' rhs') }

lvlTopBind env (Rec pairs)
  = do { prs' <- mapM (\(b,r) -> lvl_top env Recursive b r) pairs
       ; return (Rec prs') }

lvl_top :: LevelEnv -> RecFlag -> Id -> CoreExpr
        -> LvlM (LevelledBndr, LevelledExpr)
-- NB: 'env' has all the top-level binders in scope, so
--     there is no need call substAndLvlBndrs here
lvl_top env is_rec bndr rhs
  = do { rhs' <- lvlRhs env is_rec (isDeadEndId bndr)
                                   NotJoinPoint
                                   (freeVars rhs)
       ; return (stayPut tOP_LEVEL bndr, rhs') }

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

lvlExpr env (_, AnnType ty)     = return (Type (substTyUnchecked (le_subst env) ty))
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
        -- [See GHC.Core.Opt.SetLevels rev 1.50 for a version with this approach.]

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
  = lvlExpr env expr

-------------------------------------------
lvlApp :: LevelEnv
       -> CoreExprWithFVs
       -> (CoreExprWithFVs, [CoreExprWithFVs]) -- Input application
       -> LvlM LevelledExpr                    -- Result expression
lvlApp env orig_expr ((_,AnnVar fn), args)
  -- Try to ensure that runRW#'s continuation isn't floated out.
  -- See Note [Simplification of runRW#].
  | fn `hasKey` runRWKey
  = do { args' <- mapM (lvlExpr env) args
       ; return (foldl' App (lookupVar env fn) args') }

  | floatOverSat env   -- See Note [Floating over-saturated applications]
  , arity > 0
  , arity < n_val_args
  , Nothing <- isClassOpId_maybe fn
  =  do { rargs' <- mapM (lvlNonTailMFE env False) rargs
        ; lapp'  <- lvlNonTailMFE env False lapp
        ; return (foldl' App lapp' rargs') }

  | otherwise
  = do { args' <- mapM (lvlMFE env False) args
                  -- False: see "Arguments" in Note [Floating to the top]
       ; return (foldl' App (lookupVar env fn) args') }
  where
    n_val_args = count (isValArg . deAnnotate) args
    arity      = idArity fn

    -- Separate out the PAP that we are floating from the extra
    -- arguments, by traversing the spine until we have collected
    -- (n_val_args - arity) value arguments.
    (lapp, rargs) = left (n_val_args - arity) orig_expr []

    left 0 e               rargs = (e, rargs)
    left n (_, AnnApp f a) rargs
       | isValArg (deAnnotate a) = left (n-1) f (a:rargs)
       | otherwise               = left n     f (a:rargs)
    left _ _ _                   = panic "GHC.Core.Opt.SetLevels.lvlExpr.left"

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
  | [AnnAlt con@(DataAlt {}) bs body] <- alts
  , exprIsHNF (deTagExpr scrut')  -- See Note [Check the output scrutinee for exprIsHNF]
  , not (isTopLvl dest_lvl)       -- Can't have top-level cases
  , not (floatTopLvlOnly env)     -- Can float anywhere
  , ManyTy <- idMult case_bndr     -- See Note [Floating linear case]
  =     -- Always float the case if possible
        -- Unlike lets we don't insist that it escapes a value lambda
    do { (env1, (case_bndr' : bs')) <- cloneCaseBndrs env dest_lvl (case_bndr : bs)
       ; let rhs_env = extendCaseBndrEnv env1 case_bndr scrut'
       ; body' <- lvlMFE rhs_env True body
       ; let alt' = Alt con (map (stayPut dest_lvl) bs') body'
       ; return (Case scrut' (TB case_bndr' (FloatMe dest_lvl)) ty' [alt']) }

  | otherwise     -- Stays put
  = do { let (alts_env1, [case_bndr']) = substAndLvlBndrs NonRecursive env incd_lvl [case_bndr]
             alts_env = extendCaseBndrEnv alts_env1 case_bndr scrut'
       ; alts' <- mapM (lvl_alt alts_env) alts
       ; return (Case scrut' case_bndr' ty' alts') }
  where
    ty' = substTyUnchecked (le_subst env) ty

    incd_lvl = incMinorLvl (le_ctxt_lvl env)
    dest_lvl = maxFvLevel (const True) env scrut_fvs
            -- Don't abstract over type variables, hence const True

    lvl_alt alts_env (AnnAlt con bs rhs)
      = do { rhs' <- lvlMFE new_env True rhs
           ; return (Alt con bs' rhs') }
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

     - exprIsHNF catches the key case of an evaluated variable

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


Note [Floating linear case]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Linear case can't be floated past case branches:
    case u of { p1 -> case[1] v of { C x -> ...x...}; p2 -> ... }
Is well typed, but
    case[1] v of { C x -> case u of { p1 -> ...x...; p2 -> ... }}
Will not be, because of how `x` is used in one alternative but not the other.

It is not easy to float this linear cases precisely, so, instead, we elect, for
the moment, to simply not float linear case.


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
  = lvlMFE env strict_ctxt ann_expr

lvlMFE ::  LevelEnv             -- Level of in-scope names/tyvars
        -> Bool                 -- True <=> strict context [body of case or let]
        -> CoreExprWithFVs      -- input expression
        -> LvlM LevelledExpr    -- Result expression
-- lvlMFE is just like lvlExpr, except that it might let-bind
-- the expression, so that it can itself be floated.

lvlMFE env _ (_, AnnType ty)
  = return (Type (substTyUnchecked (le_subst env) ty))

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
  | not float_me
  || floatTopLvlOnly env && not (isTopLvl dest_lvl)
         -- Only floating to the top level is allowed.
  || hasFreeJoin env fvs   -- If there is a free join, don't float
                           -- See Note [Free join points]
  || not (typeHasFixedRuntimeRep (exprType expr))
         -- We can't let-bind an expression if we don't know
         -- how it will be represented at runtime.
         -- See Note [Representation polymorphism invariants] in GHC.Core
  || notWorthFloating expr abs_vars
         -- Test notWorhtFloating last;
         -- See Note [Large free-variable sets]
  = -- Don't float it out
    lvlExpr env ann_expr

  |  float_is_new_lam || exprIsTopLevelBindable expr expr_ty
         -- No wrapping needed if the type is lifted, or is a literal string
         -- or if we are wrapping it in one or more value lambdas
  = do { expr1 <- lvlFloatRhs abs_vars dest_lvl rhs_env NonRecursive
                              is_bot_lam NotJoinPoint ann_expr
                  -- Treat the expr just like a right-hand side
       ; var <- newLvlVar expr1 NotJoinPoint is_mk_static
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
  , BI_Box { bi_data_con = box_dc, bi_inst_con = boxing_expr
           , bi_boxed_type = box_ty } <- boxingDataCon expr_ty
  , let [bx_bndr, ubx_bndr] = mkTemplateLocals [box_ty, expr_ty]
  = do { expr1 <- lvlExpr rhs_env ann_expr
       ; let l1r       = incMinorLvlFrom rhs_env
             float_rhs = mkLams abs_vars_w_lvls $
                         Case expr1 (stayPut l1r ubx_bndr) box_ty
                             [Alt DEFAULT [] (App boxing_expr (Var ubx_bndr))]

       ; var <- newLvlVar float_rhs NotJoinPoint is_mk_static
       ; let l1u      = incMinorLvlFrom env
             use_expr = Case (mkVarApps (Var var) abs_vars)
                             (stayPut l1u bx_bndr) expr_ty
                             [Alt (DataAlt box_dc) [stayPut l1u ubx_bndr] (Var ubx_bndr)]
       ; return (Let (NonRec (TB var (FloatMe dest_lvl)) float_rhs)
                     use_expr) }

  | otherwise          -- e.g. do not float unboxed tuples
  = lvlExpr env ann_expr

  where
    expr         = deAnnotate ann_expr
    expr_ty      = exprType expr
    fvs          = freeVarsOf ann_expr
    fvs_ty       = tyCoVarsOfType expr_ty
    is_bot_lam   = isJust mb_bot_str   -- True of bottoming thunks too!
    is_function  = isFunction ann_expr
    mb_bot_str   = exprBotStrictness_maybe expr
                           -- See Note [Bottoming floats]
                           -- esp Bottoming floats (2)
    expr_ok_for_spec = exprOkForSpeculation expr
    abs_vars = abstractVars dest_lvl env fvs
    dest_lvl = destLevel env fvs fvs_ty is_function is_bot_lam
               -- NB: is_bot_lam not is_bot; see (3) in
               --     Note [Bottoming floats]

    -- float_is_new_lam: the floated thing will be a new value lambda
    -- replacing, say (g (x+4)) by (lvl x).  No work is saved, nor is
    -- allocation saved.  The benefit is to get it to the top level
    -- and hence out of the body of this function altogether, making
    -- it smaller and more inlinable
    float_is_new_lam = float_n_lams > 0
    float_n_lams     = count isId abs_vars

    (rhs_env, abs_vars_w_lvls) = lvlLamBndrs env dest_lvl abs_vars

    is_mk_static = isJust (collectMakeStaticArgs expr)
        -- Yuk: See Note [Grand plan for static forms] in GHC.Iface.Tidy.StaticPtrTable

        -- A decision to float entails let-binding this thing, and we only do
        -- that if we'll escape a value lambda, or will go to the top level.
    float_me = saves_work || saves_alloc || is_mk_static

    -- See Note [Saving work]
    saves_work = escapes_value_lam        -- (a)
                 && not (exprIsHNF expr)  -- (b)
                 && not float_is_new_lam  -- (c)
    escapes_value_lam = dest_lvl `ltMajLvl` (le_ctxt_lvl env)

    -- See Note [Saving allocation] and Note [Floating to the top]
    saves_alloc =  isTopLvl dest_lvl
                && floatConsts env
                && (   not strict_ctxt                     -- (a)
                    || exprIsHNF expr                      -- (b)
                    || (is_bot_lam && escapes_value_lam))  -- (c)

hasFreeJoin :: LevelEnv -> DVarSet -> Bool
-- Has a free join point which is not being floated to top level.
-- (In the latter case it won't be a join point any more.)
-- Not treating top-level ones specially had a massive effect
-- on nofib/minimax/Prog.prog
hasFreeJoin env fvs
  = not (maxFvLevel isJoinId env fvs == tOP_LEVEL)

{- Note [Saving work]
~~~~~~~~~~~~~~~~~~~~~
The key idea in let-floating is to
  * float a redex out of a (value) lambda
Doing so can save an unbounded amount of work.
But see also Note [Saving allocation].

So we definitely float an expression out if
(a) It will escape a value lambda (escapes_value_lam)
(b) The expression is not a head-normal form (exprIsHNF); see (SW1, SW2).
(c) Floating does not require wrapping it in value lambdas (float_is_new_lam).
    See (SW3) below

Wrinkles:

(SW1) Concerning (b) I experimented with using `exprIsCheap` rather than
      `exprIsHNF` but the latter seems better, according to nofib
      (`spectral/mate` got 10% worse with exprIsCheap).  It's really a bit of a
      heuristic.

(SW2) What about omitting (b), and hence floating HNFs as well?  The danger of
      doing so is that we end up floating out a HNF from a cold path (where it
      might never get allocated at all) and allocating it all the time
      regardless.  Example
          f xs = case xs of
                   [x] | x>3       -> (y,y)
                       | otherwise -> (x,y)
                   (x:xs) -> ...f xs...
      We can float (y,y) out, but in a particular call to `f` that path might
      not be taken, so allocating it before the definition of `f` is a waste.

      See !12410 for some data comparing the effect of omitting (b) altogether,
      This doesn't apply, though, if we float the thing to the top level; see
      Note [Floating to the top].  Bottom line (data from !12410): adding the
      not.exprIsHNF test to `saves_work`:
       - Decreases compiler allocations by 0.5%
       - Occasionally decreases runtime allocation (T12996 -2.5%)
       - Slightly mixed effect on nofib: (puzzle -10%, mate -5%, cichelli +5%)
         but geometric mean is -0.09%.
      Overall, a win.

(SW3) Concerning (c), if we are wrapping the thing in extra value lambdas (in
      abs_vars), then nothing is saved.  E.g.
        f = \xyz. ...(e1[y],e2)....
      If we float
        lvl = \y. (e1[y],e2)
        f = \xyz. ...(lvl y)...
      we have saved nothing: one pair will still be allocated for each
      call of `f`.  Hence the (not float_is_new_lam) in saves_work.

Note [Saving allocation]
~~~~~~~~~~~~~~~~~~~~~~~~
Even if `saves_work` is false, we we may want to float even cheap/HNF
expressions out of value lambdas, for several reasons:

* Doing so may save allocation. Consider
        f = \x.  .. (\y.e) ...
  Then we'd like to avoid allocating the (\y.e) every time we call f,
  (assuming e does not mention x). An example where this really makes a
  difference is simplrun009.

* It may allow SpecContr to fire on functions. Consider
        f = \x. ....(f (\y.e))....
  After floating we get
        lvl = \y.e
        f = \x. ....(f lvl)...
  Now it's easier for SpecConstr to generate a robust specialisation for f.

* It makes the function smaller, and hence more likely to inline.  This can make
  a big difference for string literals and bottoming expressions: see Note
  [Floating to the top]

Data suggests, however, that it is better /only/ to float HNFS, /if/ they can go
to top level. See (SW2) of Note [Saving work].  If the expression goes to top
level we don't pay the cost of allocating cold-path thunks described in (SW2).

Hence `isTopLvl dest_lvl` in `saves_alloc`.

Note [Floating to the top]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Even though Note [Saving allocation] suggests that we should not, in
general, float HNFs, the balance change if it goes to the top:

* We don't pay an allocation cost for the floated expression; it
  just becomes static data.

* Floating string literal is valuable -- no point in duplicating the
  at each call site!

* Floating bottoming expressions is valuable: they are always cold
  paths; we don't want to duplicate them at each call site; and they
  can be quite big, inhibiting inlining. See Note [Bottoming floats]

So we float an expression to the top if:
  (a) the context is lazy (so we get allocation), or
  (b) the expression is a HNF (so we get allocation), or
  (c) the expression is bottoming and floating would escape a
      value lambda (NB: if the expression itself is a lambda, (b)
      will apply; so this case only catches bottoming thunks)

Examples:

* (a) Strict.  Case scrutinee
      f = case g True of ....
  Don't float (g True) to top level; then we have the admin of a
  top-level thunk to worry about, with zero gain.

* (a) Strict.  Case alternative
      h = case y of
             True  -> g True
             False -> False
  Don't float (g True) to the top level

* (b) HNF
      f = case y of
            True  -> p:q
            False -> blah
  We may as well float the (p:q) so it becomes a static data structure.

* (c) Bottoming expressions; see also Note [Bottoming floats]
      f x = case x of
              0 -> error <big thing>
              _ -> x+1
  Here we want to float (error <big thing>) to top level, abstracting
  over 'x', so as to make f's RHS smaller.

  But (#22494) if it's more like
       foo = case error <thing> of { ... }
  then there is no point in floating; we are never going to inline
  'foo' anyway.  So float bottoming things only if they escape
  a lambda.

* Arguments
     t = f (g True)
  Prior to Apr 22 we didn't float (g True) to the top if f was strict.
  But (a) this only affected CAFs, because if it escapes a value lambda
          we'll definitely float it; so the complication of working out
          argument strictness doesn't seem worth it.
      (b) floating to the top helps SpecContr; see GHC.Core.Opt.SpecConstr
          Note [Specialising on dictionaries].
  So now we don't use strictness to affect argument floating.

It's controlled by a flag (floatConsts), because doing this too
early loses opportunities for RULES which (needless to say) are
important in some nofib programs (gcd is an example).  [SPJ note:
I think this is obsolete; the flag seems always on.]

Note [Large free-variable sets]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In #24471 we had something like
     x1 = I# 1
     ...
     x1000 = I# 1000
     foo = f x1 (f x2 (f x3 ....))
So every sub-expression in `foo` has lots and lots of free variables.  But
none of these sub-expressions float anywhere; the entire float-out pass is a
no-op.

In lvlMFE, we want to find out quickly if the MFE is not-floatable; that is
the common case.  In #24471 it turned out that we were testing `abs_vars` (a
relatively complicated calculation that takes at least O(n-free-vars) time to
compute) for every sub-expression.

Better instead to test `float_me` early. That still involves looking at
dest_lvl, which means looking at every free variable, but the constant factor
is a lot better.

ToDo: find a way to fix the bad asymptotic complexity.

Note [Floating join point bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Mostly we don't float join points at all -- we want them to /stay/ join points.
This decision is made in `wantToFloat`.

But there is one exception: if it can go to the top level (#13286).
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
of the resulting inlining of f.

Another reason for floating join points to the top. spectral/minimax has:
    prog input = join $j y = <expensive> in
                 case (input == "doesnt happen") of
                   True  -> $j (testBoard + testBoard)
                   False -> $j testBoard
Now, iff we float $j to the top, we can /also/ float ($j (tb+tb)) and ($j tb).
Result: asymptotic improvement in perf, if `prof` is called many times.

However there are also bad consequences of floating join points to the top:

* If a continuation consumes (let $j x = Just x in case y of {...})
  we may get much less duplication of the continuation if we don't
  float $j to the top, because the contination goes into $j's RHS

* See #21392 for an example of how demand analysis can get worse if you
  float a join point to the top level.

Compromise (determined experimentally):

* Always float /recursive/ join points to the top.

* For /non-recursive/ join points, float them to the top in the second
  invocation of FloatOut, near the end of the pipeline.  This is controlled by
  the FloatOutSwitch floatJoinsToTop.

Missed opportunity
------------------
There is another benfit of floating local join points.  Stream fusion
has been known to produce nested loops like this:

  joinrec j1 x1 =
    joinrec j2 x2 =
      joinrec j3 x3 = ... jump j1 (x3 + 1) ... jump j2 (x3 + 1) ...
      in jump j3 x2
    in jump j2 x1
  in jump j1 x

(Assume x1 and x2 do *not* occur free in j3.)

Here j1 and j2 are wholly superfluous---each of them merely forwards its
argument to j3. Since j3 only refers to x3, we can float j2 and j3 to make
everything one big mutual recursion:

  joinrec j1 x1 = jump j2 x1
          j2 x2 = jump j3 x2
          j3 x3 = ... jump j1 (x3 + 1) ... jump j2 (x3 + 1) ...
  in jump j1 x

Now the simplifier will happily inline the trivial j1 and j2, leaving only j3.
Without floating, we're stuck with three loops instead of one.

Currently we don't do this -- a missed opportunity.

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

Being able to float unboxed expressions is sometimes important; see #12603.
I'm not sure how /often/ it is important, but it's not hard to achieve.

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
     let x = a /# 3
to
     let x = case bx of I# a -> a /# 3
because the let binding no
longer obeys the let-can-float invariant.  But (a /# 3) is ok-for-spec
due to a special hack that says division operators can't fail
when the denominator is definitely non-zero.  And yet that
same expression says False to exprIsCheap.  Simplest way to
guarantee the let-can-float invariant is to use the same function!

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
    Then we'd like to abstract over 'x', and float the whole arg of g:
        lvl = \x. let v = h x in error ("urk" ++ v)
        f = \x. g (lvl x)
    To achieve this we pass is_bot to destLevel

(2) We do not do this for lambdas that return
    bottom.  Instead we treat the /body/ of such a function specially,
    via point (1).  For example:
        f = \x. ....(\y z. if x then error y else error z)....
    If we float the whole lambda thus
        lvl = \x. \y z. if x then error y else error z
        f = \x. ...(lvl x)...
    we may well end up eta-expanding that PAP to
        f = \x. ...(\y z. lvl x y z)...

    ===>
        lvl = \x z y. if b then error y else error z
        f = \x. ...(\y z. lvl x z y)...
    (There is no guarantee that we'll choose the perfect argument order.)

(3) If we have a /binding/ that returns bottom, we want to float it to top
    level, even if it has free vars (point (1)), and even it has lambdas.
    Example:
       ... let { v = \y. error (show x ++ show y) } in ...
    We want to abstract over x and float the whole thing to top:
       lvl = \xy. error (show x ++ show y)
       ...let {v = lvl x} in ...

    Then of course we don't want to separately float the body (error ...)
    as /another/ MFE, so we tell lvlFloatRhs not to do that, via the is_bot
    argument.

    Do /not/ do this for bottoming /join-point/ bindings.   They may call other
    join points (#24768), and floating to the top would abstract over those join
    points, which we should never do.


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

annotateBotStr :: Id -> Arity -> Maybe (Arity, DmdSig, CprSig) -> Id
-- See Note [Bottoming floats] for why we want to add
-- bottoming information right now
--
-- n_extra are the number of extra value arguments added during floating
annotateBotStr id n_extra mb_bot_str
  | Just (arity, str_sig, cpr_sig) <- mb_bot_str
  = id `setIdArity`  (arity + n_extra)
       `setIdDmdSig` prependArgsDmdSig n_extra str_sig
       `setIdCprSig` prependArgsCprSig n_extra cpr_sig
  | otherwise
  = id

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
    go (Var {}) n               = n >= 0
    go (Lit lit) n              = assert (n==0) $
                                  litIsTrivial lit   -- Note [Floating literals]
    go (Type {}) _              = True
    go (Coercion {}) _          = True
    go (App e arg) n
       -- See Note [Floating applications to coercions]
       | not (isRuntimeArg arg) = go e n
       | n==0                   = False
       | exprIsTrivial arg      = go e (n-1) -- NB: exprIsTrivial arg = go arg 0
       | otherwise              = False
    go (Tick t e) n             = not (tickishIsCode t) && go e n
    go (Cast e _)  n            = go e n
    go (Case e b _ as) n
      | null as
      = go e n     -- See Note [Empty case is trivial]
      | Just rhs <- isUnsafeEqualityCase e b as
      = go rhs n   -- See (U2) of Note [Implementing unsafeCoerce] in base:Unsafe.Coerce
    go _ _                      = False

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
"GHC.CoreToStg" and Note [Count coercion arguments in boring contexts] in
"GHC.Core.Unfold"), so we still want to float out variables applied only to
coercion arguments.


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
  |  isTyVar bndr  -- Don't float TyVar binders (simplifier gets rid of them pronto)
  || isCoVar bndr  -- Don't float CoVars: difficult to fix up CoVar occurrences
                   --                     (see extendPolyLvlEnv)
  || not (wantToFloat env NonRecursive dest_lvl is_join is_top_bindable)
  = -- No float
    do { rhs' <- lvlRhs env NonRecursive is_bot_lam mb_join_arity rhs
       ; let  bind_lvl        = incMinorLvl (le_ctxt_lvl env)
              (env', [bndr']) = substAndLvlBndrs NonRecursive env bind_lvl [bndr]
       ; return (NonRec bndr' rhs', env') }

  -- Otherwise we are going to float
  | null abs_vars
  = do {  -- No type abstraction; clone existing binder
         rhs' <- lvlFloatRhs [] dest_lvl env NonRecursive
                             is_bot_lam NotJoinPoint rhs
       ; (env', [bndr']) <- cloneLetVars NonRecursive env dest_lvl [bndr]
       ; let bndr2 = annotateBotStr bndr' 0 mb_bot_str
       ; return (NonRec (TB bndr2 (FloatMe dest_lvl)) rhs', env') }

  | otherwise
  = do {  -- Yes, type abstraction; create a new binder, extend substitution, etc
         rhs' <- lvlFloatRhs abs_vars dest_lvl env NonRecursive
                             is_bot_lam NotJoinPoint rhs
       ; (env', [bndr']) <- newPolyBndrs dest_lvl env abs_vars [bndr]
       ; let bndr2 = annotateBotStr bndr' n_extra mb_bot_str
       ; return (NonRec (TB bndr2 (FloatMe dest_lvl)) rhs', env') }

  where
    bndr_ty    = idType bndr
    ty_fvs     = tyCoVarsOfType bndr_ty
    rhs_fvs    = freeVarsOf rhs
    bind_fvs   = rhs_fvs `unionDVarSet` dIdFreeVars bndr
    abs_vars   = abstractVars dest_lvl env bind_fvs
    dest_lvl   = destLevel env bind_fvs ty_fvs (isFunction rhs) is_bot_lam

    deann_rhs  = deAnnotate rhs
    mb_bot_str = exprBotStrictness_maybe deann_rhs
    is_bot_lam = not is_join && isJust mb_bot_str
        -- is_bot_lam: looks like (\xy. bot), maybe zero lams
        -- NB: not isBottomThunk!
        -- NB: not is_join: don't send bottoming join points to the top.
        -- See Note [Bottoming floats] point (3)

    is_top_bindable = exprIsTopLevelBindable deann_rhs bndr_ty
    n_extra       = count isId abs_vars
    mb_join_arity = idJoinPointHood bndr
    is_join       = isJoinPoint mb_join_arity

lvlBind env (AnnRec pairs)
  |  not (wantToFloat env Recursive dest_lvl is_join is_top_bindable)
  = -- No float
    do { let bind_lvl       = incMinorLvl (le_ctxt_lvl env)
             (env', bndrs') = substAndLvlBndrs Recursive env bind_lvl bndrs
             lvl_rhs (b,r)  = lvlRhs env' Recursive is_bot (idJoinPointHood b) r
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
    new_rhs_body <- lvlRhs body_env2 Recursive is_bot NotJoinPoint rhs_body
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

    do_rhs env (_,rhs) = lvlFloatRhs abs_vars dest_lvl env Recursive
                                     is_bot NotJoinPoint
                                     rhs

        -- Finding the free vars of the binding group is annoying
    bind_fvs = ((unionDVarSets [ freeVarsOf rhs | (_, rhs) <- pairs])
                `unionDVarSet`
                (fvDVarSet $ unionsFV [ idFVs bndr
                                      | (bndr, (_,_)) <- pairs]))
               `delDVarSetList`
                bndrs

    ty_fvs   = foldr (unionVarSet . tyCoVarsOfType . idType) emptyVarSet bndrs
    dest_lvl = destLevel env bind_fvs ty_fvs is_fun is_bot
    abs_vars = abstractVars dest_lvl env bind_fvs

    is_top_bindable = not (any (mightBeUnliftedType . idType) bndrs)
       -- This mightBeUnliftedType stuff is the same test as in the non-rec case
       -- You might wonder whether we can have a recursive binding for
       -- an unlifted value -- but we can if it's a /join binding/ (#16978)

wantToFloat :: LevelEnv
            -> RecFlag
            -> Level    -- This is how far it could float
            -> Bool     -- Join point
            -> Bool     -- True <=> top-level-bindadable
            -> Bool     -- True <=> Yes! Float me

wantToFloat env is_rec dest_lvl is_join is_top_bindable
  | not (profitableFloat env dest_lvl)
  = False

  | floatTopLvlOnly env && not (isTopLvl dest_lvl)
  = False

  | isTopLvl dest_lvl, not is_top_bindable
  = False    -- We can't float an unlifted binding to top level (except
             -- literal strings), so we don't float it at all.  It's a
             -- bit brutal, but unlifted bindings aren't expensive either

  | is_join  -- Join points either stay put, or float to top
             -- See Note [Floating join point bindings]
  = isTopLvl dest_lvl && (isRec is_rec || floatJoinsToTop (le_switches env))

  | otherwise
  = True     -- Yes!  Float me


profitableFloat :: LevelEnv -> Level -> Bool
profitableFloat env dest_lvl
  =  (dest_lvl `ltMajLvl` le_ctxt_lvl env)  -- Escapes a value lambda
  || (isTopLvl dest_lvl && floatConsts env) -- Going all the way to top level


----------------------------------------------------
-- Three help functions for the type-abstraction case

lvlRhs :: LevelEnv
       -> RecFlag
       -> Bool               -- Is this a bottoming function
       -> JoinPointHood
       -> CoreExprWithFVs
       -> LvlM LevelledExpr
lvlRhs env rec_flag is_bot mb_join_arity expr
  = lvlFloatRhs [] (le_ctxt_lvl env) env
                rec_flag is_bot mb_join_arity expr

lvlFloatRhs :: [OutVar] -> Level -> LevelEnv -> RecFlag
            -> Bool   -- Binding is for a bottoming function
            -> JoinPointHood
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
    (bndrs, body)     | JoinPoint join_arity <- mb_join_arity
                      = collectNAnnBndrs join_arity rhs
                      | otherwise
                      = collectAnnBndrs rhs
    (env1, bndrs1)    = substBndrsSL NonRecursive env bndrs
    all_bndrs         = abs_vars ++ bndrs1
    (body_env, bndrs') | JoinPoint {} <- mb_join_arity
                      = lvlJoinBndrs env1 dest_lvl rec all_bndrs
                      | otherwise
                      = lvlLamBndrs env1 dest_lvl all_bndrs
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

    is_major bndr = not (isOneShotBndr bndr)
       -- Only non-one-shot lambdas bump a major level, which in
       -- turn triggers floating.  NB: isOneShotBndr is always
       -- true of a type variable -- there is no point in floating
       -- out of a big lambda.
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
          -> Bool   -- True <=> looks like \x1..xn.bottom (n>=0)
          -> Level
destLevel env fvs fvs_ty is_function is_bot
  | isTopLvl max_fv_id_level  -- Float even joins if they get to top level
                              -- See Note [Floating join point bindings]
  = tOP_LEVEL

  | is_bot              -- Send bottoming bindings to the top
  = as_far_as_poss      -- regardless; see Note [Bottoming floats]
                        -- Esp Bottoming floats (1) and (3)

  | Just n_args <- floatLams env
  , n_args > 0  -- n=0 case handled uniformly by the 'otherwise' case
  , is_function
  , countFreeIds fvs <= n_args
  = as_far_as_poss  -- Send functions to top level; see
                    -- the comments with isFunction

  | otherwise = max_fv_id_level
  where
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
countFreeIds = nonDetStrictFoldUDFM add 0 . getUniqDSet
  -- It's OK to use nonDetStrictFoldUDFM here because we're just counting things.
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

       -- See Note [le_subst and le_env]
       , le_subst    :: Subst           -- Domain is pre-cloned TyVars and Ids
                                        -- The Id -> CoreExpr in the Subst is ignored
                                        -- (since we want to substitute a LevelledExpr for
                                        -- an Id via le_env) but we do use the Co/TyVar substs
       , le_env      :: IdEnv ([OutVar], LevelledExpr)  -- Domain is pre-cloned Ids
    }

{- Note [le_subst and le_env]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We clone nested let- and case-bound variables so that they are still
distinct when floated out; hence the le_subst/le_env.  (see point 3 of
the module overview comment).  We also use these envs when making a
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

initialEnv :: FloatOutSwitches -> CoreProgram -> LevelEnv
initialEnv float_lams binds
  = LE { le_switches  = float_lams
       , le_ctxt_lvl  = tOP_LEVEL
       , le_lvl_env   = emptyVarEnv
       , le_subst     = mkEmptySubst in_scope_toplvl
       , le_env       = emptyVarEnv }
  where
    in_scope_toplvl = emptyInScopeSet `extendInScopeSetBndrs` binds
      -- The Simplifier (see Note [Glomming] in GHC.Core.Opt.OccurAnal) and
      -- the specialiser (see Note [Top level scope] in GHC.Core.Opt.Specialise)
      -- may both produce top-level bindings where an early binding refers
      -- to a later one.  So here we put all the top-level binders in scope before
      -- we start, to satisfy the lookupIdSubst invariants (#20200 and #20294)

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
  -- We could use OccurAnal. scrutOkForBinderSwap here, and perhaps
  -- get a bit more floating.  But we didn't in the past and it's
  -- an unforced change, so I'm leaving it.
  = le { le_subst   = extendSubstWithVar subst case_bndr scrut_var
       , le_env     = add_id id_env (case_bndr, scrut_var) }
extendCaseBndrEnv env _ _ = env

maxFvLevel :: (Var -> Bool) -> LevelEnv -> DVarSet -> Level
maxFvLevel max_me env var_set
  = nonDetStrictFoldDVarSet (maxIn max_me env) tOP_LEVEL var_set
    -- It's OK to use a non-deterministic fold here because maxIn commutes.

maxFvLevel' :: (Var -> Bool) -> LevelEnv -> TyCoVarSet -> Level
-- Same but for TyCoVarSet
maxFvLevel' max_me env var_set
  = nonDetStrictFoldUniqSet (maxIn max_me env) tOP_LEVEL var_set
    -- It's OK to use a non-deterministic fold here because maxIn commutes.

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
    zap v | isId v = warnPprTrace (isStableUnfolding (idUnfolding v) ||
                           not (isEmptyRuleInfo (idSpecialisation v)))
                           "absVarsOf: discarding info on" (ppr v) $
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
 = assert (all (not . isCoVar) bndrs) $   -- What would we add to the CoSubst in this case. No easy answer.
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
                             mkSysLocal str uniq (idMult bndr) poly_ty
                           where
                             str     = fsLit "poly_" `appendFS` occNameFS (getOccName bndr)
                             poly_ty = mkLamTypes abs_vars (substTyUnchecked subst (idType bndr))

    -- If we are floating a join point to top level, it stops being
    -- a join point.  Otherwise it continues to be a join point,
    -- but we may need to adjust its arity
    dest_is_top = isTopLvl dest_lvl
    transfer_join_info bndr new_bndr
      | JoinPoint join_arity <- idJoinPointHood bndr
      , not dest_is_top
      = new_bndr `asJoinId` join_arity + length abs_vars
      | otherwise
      = new_bndr

newLvlVar :: LevelledExpr        -- The RHS of the new binding
          -> JoinPointHood       -- Its join arity, if it is a join point
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
      -- See Note [Grand plan for static forms] in GHC.Iface.Tidy.StaticPtrTable.
      | is_mk_static
      = mkExportedVanillaId (mkSystemVarName uniq (mkFastString "static_ptr"))
                            rhs_ty
      | otherwise
      = mkSysLocal (mkFastString "lvl") uniq ManyTy rhs_ty

-- | Clone the binders bound by a single-alternative case.
cloneCaseBndrs :: LevelEnv -> Level -> [Var] -> LvlM (LevelEnv, [Var])
cloneCaseBndrs env@(LE { le_subst = subst, le_lvl_env = lvl_env, le_env = id_env })
               new_lvl vs
  = do { (subst', vs') <- cloneBndrs subst vs
             -- N.B. We are not moving the body of the case, merely its case
             -- binders.  Consequently we should *not* set le_ctxt_lvl.
             -- See Note [Setting levels when floating single-alternative cases].
       ; let env' = env { le_lvl_env   = addLvls new_lvl lvl_env vs'
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
  = do { let vs1  = map zap vs
                      -- See Note [Zapping the demand info]
       ; (subst', vs2) <- case is_rec of
                            NonRecursive -> cloneBndrs      subst vs1
                            Recursive    -> cloneRecIdBndrs subst vs1

       ; let prs  = vs `zip` vs2
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
  | otherwise = extendVarEnv id_env v ([v1], assert (not (isCoVar v1)) $ Var v1)

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
