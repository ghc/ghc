{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


Utility functions on @Core@ syntax
-}

{-# LANGUAGE CPP #-}

-- | Commonly useful utilites for manipulating the Core language
module CoreUtils (
        -- * Constructing expressions
        mkCast,
        mkTick, mkTicks, mkTickNoHNF, tickHNFArgs,
        bindNonRec, needsCaseBinding,
        mkAltExpr,

        -- * Taking expressions apart
        findDefault, addDefault, findAlt, isDefaultAlt,
        mergeAlts, trimConArgs,
        filterAlts, combineIdenticalAlts, refineDefaultAlt,

        -- * Properties of expressions
        exprType, coreAltType, coreAltsType, isExprLevPoly,
        exprIsDupable, exprIsTrivial, getIdFromTrivialExpr, exprIsBottom,
        getIdFromTrivialExpr_maybe,
        exprIsCheap, exprIsExpandable, exprIsCheapX, CheapAppFun,
        exprIsHNF, exprOkForSpeculation, exprOkForSideEffects, exprIsWorkFree,
        exprIsBig, exprIsConLike,
        rhsIsStatic, isCheapApp, isExpandableApp,
        exprIsLiteralString, exprIsTopLevelBindable,
        altsAreExhaustive,

        -- * Equality
        cheapEqExpr, cheapEqExpr', eqExpr,
        diffExpr, diffBinds,

        -- * Eta reduction
        tryEtaReduce,

        -- * Manipulating data constructors and types
        exprToType, exprToCoercion_maybe,
        applyTypeToArgs, applyTypeToArg,
        dataConRepInstPat, dataConRepFSInstPat,
        isEmptyTy,

        -- * Working with ticks
        stripTicksTop, stripTicksTopE, stripTicksTopT,
        stripTicksE, stripTicksT,

        -- * StaticPtr
        collectMakeStaticArgs,

        -- * Join points
        isJoinBind
    ) where

#include "HsVersions.h"

import CoreSyn
import PrelNames ( makeStaticName )
import PprCore
import CoreFVs( exprFreeVars )
import Var
import SrcLoc
import VarEnv
import VarSet
import Name
import Literal
import DataCon
import PrimOp
import Id
import IdInfo
import Type
import TyCoRep( TyBinder(..) )
import Coercion
import TyCon
import Unique
import Outputable
import TysPrim
import DynFlags
import FastString
import Maybes
import ListSetOps       ( minusList )
import BasicTypes       ( Arity, isConLike )
import Platform
import Util
import Pair
import Data.Function       ( on )
import Data.List
import Data.Ord            ( comparing )
import OrdList

{-
************************************************************************
*                                                                      *
\subsection{Find the type of a Core atom/expression}
*                                                                      *
************************************************************************
-}

exprType :: CoreExpr -> Type
-- ^ Recover the type of a well-typed Core expression. Fails when
-- applied to the actual 'CoreSyn.Type' expression as it cannot
-- really be said to have a type
exprType (Var var)           = idType var
exprType (Lit lit)           = literalType lit
exprType (Coercion co)       = coercionType co
exprType (Let bind body)
  | NonRec tv rhs <- bind    -- See Note [Type bindings]
  , Type ty <- rhs           = substTyWithUnchecked [tv] [ty] (exprType body)
  | otherwise                = exprType body
exprType (Case _ _ ty _)     = ty
exprType (Cast _ co)         = pSnd (coercionKind co)
exprType (Tick _ e)          = exprType e
exprType (Lam binder expr)   = mkLamType binder (exprType expr)
exprType e@(App _ _)
  = case collectArgs e of
        (fun, args) -> applyTypeToArgs e (exprType fun) args

exprType other = pprTrace "exprType" (pprCoreExpr other) alphaTy

coreAltType :: CoreAlt -> Type
-- ^ Returns the type of the alternatives right hand side
coreAltType (_,bs,rhs)
  | any bad_binder bs = expandTypeSynonyms ty
  | otherwise         = ty    -- Note [Existential variables and silly type synonyms]
  where
    ty           = exprType rhs
    free_tvs     = tyCoVarsOfType ty
    bad_binder b = b `elemVarSet` free_tvs

coreAltsType :: [CoreAlt] -> Type
-- ^ Returns the type of the first alternative, which should be the same as for all alternatives
coreAltsType (alt:_) = coreAltType alt
coreAltsType []      = panic "corAltsType"

-- | Is this expression levity polymorphic? This should be the
-- same as saying (isKindLevPoly . typeKind . exprType) but
-- much faster.
isExprLevPoly :: CoreExpr -> Bool
isExprLevPoly = go
  where
   go (Var _)                      = False  -- no levity-polymorphic binders
   go (Lit _)                      = False  -- no levity-polymorphic literals
   go e@(App f _) | not (go_app f) = False
                  | otherwise      = check_type e
   go (Lam _ _)                    = False
   go (Let _ e)                    = go e
   go e@(Case {})                  = check_type e -- checking type is fast
   go e@(Cast {})                  = check_type e
   go (Tick _ e)                   = go e
   go e@(Type {})                  = pprPanic "isExprLevPoly ty" (ppr e)
   go (Coercion {})                = False  -- this case can happen in SetLevels

   check_type = isTypeLevPoly . exprType  -- slow approach

      -- if the function is a variable (common case), check its
      -- levityInfo. This might mean we don't need to look up and compute
      -- on the type. Spec of these functions: return False if there is
      -- no possibility, ever, of this expression becoming levity polymorphic,
      -- no matter what it's applied to; return True otherwise.
      -- returning True is always safe. See also Note [Levity info] in
      -- IdInfo
   go_app (Var id)        = not (isNeverLevPolyId id)
   go_app (Lit _)         = False
   go_app (App f _)       = go_app f
   go_app (Lam _ e)       = go_app e
   go_app (Let _ e)       = go_app e
   go_app (Case _ _ ty _) = resultIsLevPoly ty
   go_app (Cast _ co)     = resultIsLevPoly (pSnd $ coercionKind co)
   go_app (Tick _ e)      = go_app e
   go_app e@(Type {})     = pprPanic "isExprLevPoly app ty" (ppr e)
   go_app e@(Coercion {}) = pprPanic "isExprLevPoly app co" (ppr e)


{-
Note [Type bindings]
~~~~~~~~~~~~~~~~~~~~
Core does allow type bindings, although such bindings are
not much used, except in the output of the desuguarer.
Example:
     let a = Int in (\x:a. x)
Given this, exprType must be careful to substitute 'a' in the
result type (Trac #8522).

Note [Existential variables and silly type synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
        data T = forall a. T (Funny a)
        type Funny a = Bool
        f :: T -> Bool
        f (T x) = x

Now, the type of 'x' is (Funny a), where 'a' is existentially quantified.
That means that 'exprType' and 'coreAltsType' may give a result that *appears*
to mention an out-of-scope type variable.  See Trac #3409 for a more real-world
example.

Various possibilities suggest themselves:

 - Ignore the problem, and make Lint not complain about such variables

 - Expand all type synonyms (or at least all those that discard arguments)
      This is tricky, because at least for top-level things we want to
      retain the type the user originally specified.

 - Expand synonyms on the fly, when the problem arises. That is what
   we are doing here.  It's not too expensive, I think.

Note that there might be existentially quantified coercion variables, too.
-}

-- Not defined with applyTypeToArg because you can't print from CoreSyn.
applyTypeToArgs :: CoreExpr -> Type -> [CoreExpr] -> Type
-- ^ A more efficient version of 'applyTypeToArg' when we have several arguments.
-- The first argument is just for debugging, and gives some context
applyTypeToArgs e op_ty args
  = go op_ty args
  where
    go op_ty []                   = op_ty
    go op_ty (Type ty : args)     = go_ty_args op_ty [ty] args
    go op_ty (Coercion co : args) = go_ty_args op_ty [mkCoercionTy co] args
    go op_ty (_ : args)           | Just (_, res_ty) <- splitFunTy_maybe op_ty
                                  = go res_ty args
    go _ _ = pprPanic "applyTypeToArgs" panic_msg

    -- go_ty_args: accumulate type arguments so we can
    -- instantiate all at once with piResultTys
    go_ty_args op_ty rev_tys (Type ty : args)
       = go_ty_args op_ty (ty:rev_tys) args
    go_ty_args op_ty rev_tys (Coercion co : args)
       = go_ty_args op_ty (mkCoercionTy co : rev_tys) args
    go_ty_args op_ty rev_tys args
       = go (piResultTys op_ty (reverse rev_tys)) args

    panic_msg = vcat [ text "Expression:" <+> pprCoreExpr e
                     , text "Type:" <+> ppr op_ty
                     , text "Args:" <+> ppr args ]


{-
************************************************************************
*                                                                      *
\subsection{Attaching notes}
*                                                                      *
************************************************************************
-}

-- | Wrap the given expression in the coercion safely, dropping
-- identity coercions and coalescing nested coercions
mkCast :: CoreExpr -> Coercion -> CoreExpr
mkCast e co
  | ASSERT2( coercionRole co == Representational
           , text "coercion" <+> ppr co <+> ptext (sLit "passed to mkCast")
             <+> ppr e <+> text "has wrong role" <+> ppr (coercionRole co) )
    isReflCo co
  = e

mkCast (Coercion e_co) co
  | isCoercionType (pSnd (coercionKind co))
       -- The guard here checks that g has a (~#) on both sides,
       -- otherwise decomposeCo fails.  Can in principle happen
       -- with unsafeCoerce
  = Coercion (mkCoCast e_co co)

mkCast (Cast expr co2) co
  = WARN(let { Pair  from_ty  _to_ty  = coercionKind co;
               Pair _from_ty2  to_ty2 = coercionKind co2} in
            not (from_ty `eqType` to_ty2),
             vcat ([ text "expr:" <+> ppr expr
                   , text "co2:" <+> ppr co2
                   , text "co:" <+> ppr co ]) )
    mkCast expr (mkTransCo co2 co)

mkCast (Tick t expr) co
   = Tick t (mkCast expr co)

mkCast expr co
  = let Pair from_ty _to_ty = coercionKind co in
    WARN( not (from_ty `eqType` exprType expr),
          text "Trying to coerce" <+> text "(" <> ppr expr
          $$ text "::" <+> ppr (exprType expr) <> text ")"
          $$ ppr co $$ ppr (coercionType co) )
    (Cast expr co)

-- | Wraps the given expression in the source annotation, dropping the
-- annotation if possible.
mkTick :: Tickish Id -> CoreExpr -> CoreExpr
mkTick t orig_expr = mkTick' id id orig_expr
 where
  -- Some ticks (cost-centres) can be split in two, with the
  -- non-counting part having laxer placement properties.
  canSplit = tickishCanSplit t && tickishPlace (mkNoCount t) /= tickishPlace t

  mkTick' :: (CoreExpr -> CoreExpr) -- ^ apply after adding tick (float through)
          -> (CoreExpr -> CoreExpr) -- ^ apply before adding tick (float with)
          -> CoreExpr               -- ^ current expression
          -> CoreExpr
  mkTick' top rest expr = case expr of

    -- Cost centre ticks should never be reordered relative to each
    -- other. Therefore we can stop whenever two collide.
    Tick t2 e
      | ProfNote{} <- t2, ProfNote{} <- t -> top $ Tick t $ rest expr

    -- Otherwise we assume that ticks of different placements float
    -- through each other.
      | tickishPlace t2 /= tickishPlace t -> mkTick' (top . Tick t2) rest e

    -- For annotations this is where we make sure to not introduce
    -- redundant ticks.
      | tickishContains t t2              -> mkTick' top rest e
      | tickishContains t2 t              -> orig_expr
      | otherwise                         -> mkTick' top (rest . Tick t2) e

    -- Ticks don't care about types, so we just float all ticks
    -- through them. Note that it's not enough to check for these
    -- cases top-level. While mkTick will never produce Core with type
    -- expressions below ticks, such constructs can be the result of
    -- unfoldings. We therefore make an effort to put everything into
    -- the right place no matter what we start with.
    Cast e co   -> mkTick' (top . flip Cast co) rest e
    Coercion co -> Coercion co

    Lam x e
      -- Always float through type lambdas. Even for non-type lambdas,
      -- floating is allowed for all but the most strict placement rule.
      | not (isRuntimeVar x) || tickishPlace t /= PlaceRuntime
      -> mkTick' (top . Lam x) rest e

      -- If it is both counting and scoped, we split the tick into its
      -- two components, often allowing us to keep the counting tick on
      -- the outside of the lambda and push the scoped tick inside.
      -- The point of this is that the counting tick can probably be
      -- floated, and the lambda may then be in a position to be
      -- beta-reduced.
      | canSplit
      -> top $ Tick (mkNoScope t) $ rest $ Lam x $ mkTick (mkNoCount t) e

    App f arg
      -- Always float through type applications.
      | not (isRuntimeArg arg)
      -> mkTick' (top . flip App arg) rest f

      -- We can also float through constructor applications, placement
      -- permitting. Again we can split.
      | isSaturatedConApp expr && (tickishPlace t==PlaceCostCentre || canSplit)
      -> if tickishPlace t == PlaceCostCentre
         then top $ rest $ tickHNFArgs t expr
         else top $ Tick (mkNoScope t) $ rest $ tickHNFArgs (mkNoCount t) expr

    Var x
      | notFunction && tickishPlace t == PlaceCostCentre
      -> orig_expr
      | notFunction && canSplit
      -> top $ Tick (mkNoScope t) $ rest expr
      where
        -- SCCs can be eliminated on variables provided the variable
        -- is not a function.  In these cases the SCC makes no difference:
        -- the cost of evaluating the variable will be attributed to its
        -- definition site.  When the variable refers to a function, however,
        -- an SCC annotation on the variable affects the cost-centre stack
        -- when the function is called, so we must retain those.
        notFunction = not (isFunTy (idType x))

    Lit{}
      | tickishPlace t == PlaceCostCentre
      -> orig_expr

    -- Catch-all: Annotate where we stand
    _any -> top $ Tick t $ rest expr

mkTicks :: [Tickish Id] -> CoreExpr -> CoreExpr
mkTicks ticks expr = foldr mkTick expr ticks

isSaturatedConApp :: CoreExpr -> Bool
isSaturatedConApp e = go e []
  where go (App f a) as = go f (a:as)
        go (Var fun) args
           = isConLikeId fun && idArity fun == valArgCount args
        go (Cast f _) as = go f as
        go _ _ = False

mkTickNoHNF :: Tickish Id -> CoreExpr -> CoreExpr
mkTickNoHNF t e
  | exprIsHNF e = tickHNFArgs t e
  | otherwise   = mkTick t e

-- push a tick into the arguments of a HNF (call or constructor app)
tickHNFArgs :: Tickish Id -> CoreExpr -> CoreExpr
tickHNFArgs t e = push t e
 where
  push t (App f (Type u)) = App (push t f) (Type u)
  push t (App f arg) = App (push t f) (mkTick t arg)
  push _t e = e

-- | Strip ticks satisfying a predicate from top of an expression
stripTicksTop :: (Tickish Id -> Bool) -> Expr b -> ([Tickish Id], Expr b)
stripTicksTop p = go []
  where go ts (Tick t e) | p t = go (t:ts) e
        go ts other            = (reverse ts, other)

-- | Strip ticks satisfying a predicate from top of an expression,
-- returning the remaining expression
stripTicksTopE :: (Tickish Id -> Bool) -> Expr b -> Expr b
stripTicksTopE p = go
  where go (Tick t e) | p t = go e
        go other            = other

-- | Strip ticks satisfying a predicate from top of an expression,
-- returning the ticks
stripTicksTopT :: (Tickish Id -> Bool) -> Expr b -> [Tickish Id]
stripTicksTopT p = go []
  where go ts (Tick t e) | p t = go (t:ts) e
        go ts _                = ts

-- | Completely strip ticks satisfying a predicate from an
-- expression. Note this is O(n) in the size of the expression!
stripTicksE :: (Tickish Id -> Bool) -> Expr b -> Expr b
stripTicksE p expr = go expr
  where go (App e a)        = App (go e) (go a)
        go (Lam b e)        = Lam b (go e)
        go (Let b e)        = Let (go_bs b) (go e)
        go (Case e b t as)  = Case (go e) b t (map go_a as)
        go (Cast e c)       = Cast (go e) c
        go (Tick t e)
          | p t             = go e
          | otherwise       = Tick t (go e)
        go other            = other
        go_bs (NonRec b e)  = NonRec b (go e)
        go_bs (Rec bs)      = Rec (map go_b bs)
        go_b (b, e)         = (b, go e)
        go_a (c,bs,e)       = (c,bs, go e)

stripTicksT :: (Tickish Id -> Bool) -> Expr b -> [Tickish Id]
stripTicksT p expr = fromOL $ go expr
  where go (App e a)        = go e `appOL` go a
        go (Lam _ e)        = go e
        go (Let b e)        = go_bs b `appOL` go e
        go (Case e _ _ as)  = go e `appOL` concatOL (map go_a as)
        go (Cast e _)       = go e
        go (Tick t e)
          | p t             = t `consOL` go e
          | otherwise       = go e
        go _                = nilOL
        go_bs (NonRec _ e)  = go e
        go_bs (Rec bs)      = concatOL (map go_b bs)
        go_b (_, e)         = go e
        go_a (_, _, e)      = go e

{-
************************************************************************
*                                                                      *
\subsection{Other expression construction}
*                                                                      *
************************************************************************
-}

bindNonRec :: Id -> CoreExpr -> CoreExpr -> CoreExpr
-- ^ @bindNonRec x r b@ produces either:
--
-- > let x = r in b
--
-- or:
--
-- > case r of x { _DEFAULT_ -> b }
--
-- depending on whether we have to use a @case@ or @let@
-- binding for the expression (see 'needsCaseBinding').
-- It's used by the desugarer to avoid building bindings
-- that give Core Lint a heart attack, although actually
-- the simplifier deals with them perfectly well. See
-- also 'MkCore.mkCoreLet'
bindNonRec bndr rhs body
  | needsCaseBinding (idType bndr) rhs = Case rhs bndr (exprType body) [(DEFAULT, [], body)]
  | otherwise                          = Let (NonRec bndr rhs) body

-- | Tests whether we have to use a @case@ rather than @let@ binding for this expression
-- as per the invariants of 'CoreExpr': see "CoreSyn#let_app_invariant"
needsCaseBinding :: Type -> CoreExpr -> Bool
needsCaseBinding ty rhs = isUnliftedType ty && not (exprOkForSpeculation rhs)
        -- Make a case expression instead of a let
        -- These can arise either from the desugarer,
        -- or from beta reductions: (\x.e) (x +# y)

mkAltExpr :: AltCon     -- ^ Case alternative constructor
          -> [CoreBndr] -- ^ Things bound by the pattern match
          -> [Type]     -- ^ The type arguments to the case alternative
          -> CoreExpr
-- ^ This guy constructs the value that the scrutinee must have
-- given that you are in one particular branch of a case
mkAltExpr (DataAlt con) args inst_tys
  = mkConApp con (map Type inst_tys ++ varsToCoreExprs args)
mkAltExpr (LitAlt lit) [] []
  = Lit lit
mkAltExpr (LitAlt _) _ _ = panic "mkAltExpr LitAlt"
mkAltExpr DEFAULT _ _ = panic "mkAltExpr DEFAULT"

{-
************************************************************************
*                                                                      *
               Operations oer case alternatives
*                                                                      *
************************************************************************

The default alternative must be first, if it exists at all.
This makes it easy to find, though it makes matching marginally harder.
-}

-- | Extract the default case alternative
findDefault :: [(AltCon, [a], b)] -> ([(AltCon, [a], b)], Maybe b)
findDefault ((DEFAULT,args,rhs) : alts) = ASSERT( null args ) (alts, Just rhs)
findDefault alts                        =                     (alts, Nothing)

addDefault :: [(AltCon, [a], b)] -> Maybe b -> [(AltCon, [a], b)]
addDefault alts Nothing    = alts
addDefault alts (Just rhs) = (DEFAULT, [], rhs) : alts

isDefaultAlt :: (AltCon, a, b) -> Bool
isDefaultAlt (DEFAULT, _, _) = True
isDefaultAlt _               = False

-- | Find the case alternative corresponding to a particular
-- constructor: panics if no such constructor exists
findAlt :: AltCon -> [(AltCon, a, b)] -> Maybe (AltCon, a, b)
    -- A "Nothing" result *is* legitmiate
    -- See Note [Unreachable code]
findAlt con alts
  = case alts of
        (deflt@(DEFAULT,_,_):alts) -> go alts (Just deflt)
        _                          -> go alts Nothing
  where
    go []                     deflt = deflt
    go (alt@(con1,_,_) : alts) deflt
      = case con `cmpAltCon` con1 of
          LT -> deflt   -- Missed it already; the alts are in increasing order
          EQ -> Just alt
          GT -> ASSERT( not (con1 == DEFAULT) ) go alts deflt

{- Note [Unreachable code]
~~~~~~~~~~~~~~~~~~~~~~~~~~
It is possible (although unusual) for GHC to find a case expression
that cannot match.  For example:

     data Col = Red | Green | Blue
     x = Red
     f v = case x of
              Red -> ...
              _ -> ...(case x of { Green -> e1; Blue -> e2 })...

Suppose that for some silly reason, x isn't substituted in the case
expression.  (Perhaps there's a NOINLINE on it, or profiling SCC stuff
gets in the way; cf Trac #3118.)  Then the full-lazines pass might produce
this

     x = Red
     lvl = case x of { Green -> e1; Blue -> e2 })
     f v = case x of
             Red -> ...
             _ -> ...lvl...

Now if x gets inlined, we won't be able to find a matching alternative
for 'Red'.  That's because 'lvl' is unreachable.  So rather than crashing
we generate (error "Inaccessible alternative").

Similar things can happen (augmented by GADTs) when the Simplifier
filters down the matching alternatives in Simplify.rebuildCase.
-}

---------------------------------
mergeAlts :: [(AltCon, a, b)] -> [(AltCon, a, b)] -> [(AltCon, a, b)]
-- ^ Merge alternatives preserving order; alternatives in
-- the first argument shadow ones in the second
mergeAlts [] as2 = as2
mergeAlts as1 [] = as1
mergeAlts (a1:as1) (a2:as2)
  = case a1 `cmpAlt` a2 of
        LT -> a1 : mergeAlts as1      (a2:as2)
        EQ -> a1 : mergeAlts as1      as2       -- Discard a2
        GT -> a2 : mergeAlts (a1:as1) as2


---------------------------------
trimConArgs :: AltCon -> [CoreArg] -> [CoreArg]
-- ^ Given:
--
-- > case (C a b x y) of
-- >        C b x y -> ...
--
-- We want to drop the leading type argument of the scrutinee
-- leaving the arguments to match against the pattern

trimConArgs DEFAULT      args = ASSERT( null args ) []
trimConArgs (LitAlt _)   args = ASSERT( null args ) []
trimConArgs (DataAlt dc) args = dropList (dataConUnivTyVars dc) args

filterAlts :: TyCon                -- ^ Type constructor of scrutinee's type (used to prune possibilities)
           -> [Type]               -- ^ And its type arguments
           -> [AltCon]             -- ^ 'imposs_cons': constructors known to be impossible due to the form of the scrutinee
           -> [(AltCon, [Var], a)] -- ^ Alternatives
           -> ([AltCon], [(AltCon, [Var], a)])
             -- Returns:
             --  1. Constructors that will never be encountered by the
             --     *default* case (if any).  A superset of imposs_cons
             --  2. The new alternatives, trimmed by
             --        a) remove imposs_cons
             --        b) remove constructors which can't match because of GADTs
             --      and with the DEFAULT expanded to a DataAlt if there is exactly
             --      remaining constructor that can match
             --
             -- NB: the final list of alternatives may be empty:
             -- This is a tricky corner case.  If the data type has no constructors,
             -- which GHC allows, or if the imposs_cons covers all constructors (after taking
             -- account of GADTs), then no alternatives can match.
             --
             -- If callers need to preserve the invariant that there is always at least one branch
             -- in a "case" statement then they will need to manually add a dummy case branch that just
             -- calls "error" or similar.
filterAlts _tycon inst_tys imposs_cons alts
  = (imposs_deflt_cons, addDefault trimmed_alts maybe_deflt)
  where
    (alts_wo_default, maybe_deflt) = findDefault alts
    alt_cons = [con | (con,_,_) <- alts_wo_default]

    trimmed_alts = filterOut (impossible_alt inst_tys) alts_wo_default

    imposs_deflt_cons = nub (imposs_cons ++ alt_cons)
         -- "imposs_deflt_cons" are handled
         --   EITHER by the context,
         --   OR by a non-DEFAULT branch in this case expression.

    impossible_alt :: [Type] -> (AltCon, a, b) -> Bool
    impossible_alt _ (con, _, _) | con `elem` imposs_cons = True
    impossible_alt inst_tys (DataAlt con, _, _) = dataConCannotMatch inst_tys con
    impossible_alt _  _                         = False

refineDefaultAlt :: [Unique] -> TyCon -> [Type]
                 -> [AltCon]  -- Constructors that cannot match the DEFAULT (if any)
                 -> [CoreAlt]
                 -> (Bool, [CoreAlt])
-- Refine the default alternative to a DataAlt,
-- if there is a unique way to do so
refineDefaultAlt us tycon tys imposs_deflt_cons all_alts
  | (DEFAULT,_,rhs) : rest_alts <- all_alts
  , isAlgTyCon tycon            -- It's a data type, tuple, or unboxed tuples.
  , not (isNewTyCon tycon)      -- We can have a newtype, if we are just doing an eval:
                                --      case x of { DEFAULT -> e }
                                -- and we don't want to fill in a default for them!
  , Just all_cons <- tyConDataCons_maybe tycon
  , let imposs_data_cons = [con | DataAlt con <- imposs_deflt_cons]   -- We now know it's a data type
        impossible con   = con `elem` imposs_data_cons || dataConCannotMatch tys con
  = case filterOut impossible all_cons of
       -- Eliminate the default alternative
       -- altogether if it can't match:
       []    -> (False, rest_alts)

       -- It matches exactly one constructor, so fill it in:
       [con] -> (True, mergeAlts rest_alts [(DataAlt con, ex_tvs ++ arg_ids, rhs)])
                       -- We need the mergeAlts to keep the alternatives in the right order
             where
                (ex_tvs, arg_ids) = dataConRepInstPat us con tys

       -- It matches more than one, so do nothing
       _  -> (False, all_alts)

  | debugIsOn, isAlgTyCon tycon, null (tyConDataCons tycon)
  , not (isFamilyTyCon tycon || isAbstractTyCon tycon)
        -- Check for no data constructors
        -- This can legitimately happen for abstract types and type families,
        -- so don't report that
  = (False, all_alts)

  | otherwise      -- The common case
  = (False, all_alts)

{- Note [Combine identical alternatives]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If several alternatives are identical, merge them into a single
DEFAULT alternative.  I've occasionally seen this making a big
difference:

     case e of               =====>     case e of
       C _ -> f x                         D v -> ....v....
       D v -> ....v....                   DEFAULT -> f x
       DEFAULT -> f x

The point is that we merge common RHSs, at least for the DEFAULT case.
[One could do something more elaborate but I've never seen it needed.]
To avoid an expensive test, we just merge branches equal to the *first*
alternative; this picks up the common cases
     a) all branches equal
     b) some branches equal to the DEFAULT (which occurs first)

The case where Combine Identical Alternatives transformation showed up
was like this (base/Foreign/C/Err/Error.hs):

        x | p `is` 1 -> e1
          | p `is` 2 -> e2
        ...etc...

where @is@ was something like

        p `is` n = p /= (-1) && p == n

This gave rise to a horrible sequence of cases

        case p of
          (-1) -> $j p
          1    -> e1
          DEFAULT -> $j p

and similarly in cascade for all the join points!

NB: it's important that all this is done in [InAlt], *before* we work
on the alternatives themselves, because Simplify.simplAlt may zap the
occurrence info on the binders in the alternatives, which in turn
defeats combineIdenticalAlts (see Trac #7360).

Note [Care with impossible-constructors when combining alternatives]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have (Trac #10538)
   data T = A | B | C | D

      case x::T of   (Imposs-default-cons {A,B})
         DEFAULT -> e1
         A -> e2
         B -> e1

When calling combineIdentialAlts, we'll have computed that the
"impossible constructors" for the DEFAULT alt is {A,B}, since if x is
A or B we'll take the other alternatives.  But suppose we combine B
into the DEFAULT, to get

      case x::T of   (Imposs-default-cons {A})
         DEFAULT -> e1
         A -> e2

Then we must be careful to trim the impossible constructors to just {A},
else we risk compiling 'e1' wrong!

Not only that, but we take care when there is no DEFAULT beforehand,
because we are introducing one.  Consider

   case x of   (Imposs-default-cons {A,B,C})
     A -> e1
     B -> e2
     C -> e1

Then when combining the A and C alternatives we get

   case x of   (Imposs-default-cons {B})
     DEFAULT -> e1
     B -> e2

Note that we have a new DEFAULT branch that we didn't have before.  So
we need delete from the "impossible-default-constructors" all the
known-con alternatives that we have eliminated. (In Trac #11172 we
missed the first one.)

-}

combineIdenticalAlts :: [AltCon]    -- Constructors that cannot match DEFAULT
                     -> [CoreAlt]
                     -> (Bool,      -- True <=> something happened
                         [AltCon],  -- New constructors that cannot match DEFAULT
                         [CoreAlt]) -- New alternatives
-- See Note [Combine identical alternatives]
-- True <=> we did some combining, result is a single DEFAULT alternative
combineIdenticalAlts imposs_deflt_cons ((con1,bndrs1,rhs1) : rest_alts)
  | all isDeadBinder bndrs1    -- Remember the default
  , not (null elim_rest) -- alternative comes first
  = (True, imposs_deflt_cons', deflt_alt : filtered_rest)
  where
    (elim_rest, filtered_rest) = partition identical_to_alt1 rest_alts
    deflt_alt = (DEFAULT, [], mkTicks (concat tickss) rhs1)

     -- See Note [Care with impossible-constructors when combining alternatives]
    imposs_deflt_cons' = imposs_deflt_cons `minusList` elim_cons
    elim_cons = elim_con1 ++ map fstOf3 elim_rest
    elim_con1 = case con1 of     -- Don't forget con1!
                  DEFAULT -> []  -- See Note [
                  _       -> [con1]

    cheapEqTicked e1 e2 = cheapEqExpr' tickishFloatable e1 e2
    identical_to_alt1 (_con,bndrs,rhs)
      = all isDeadBinder bndrs && rhs `cheapEqTicked` rhs1
    tickss = map (stripTicksT tickishFloatable . thdOf3) elim_rest

combineIdenticalAlts imposs_cons alts
  = (False, imposs_cons, alts)

{- *********************************************************************
*                                                                      *
             exprIsTrivial
*                                                                      *
************************************************************************

Note [exprIsTrivial]
~~~~~~~~~~~~~~~~~~~~
@exprIsTrivial@ is true of expressions we are unconditionally happy to
                duplicate; simple variables and constants, and type
                applications.  Note that primop Ids aren't considered
                trivial unless

Note [Variables are trivial]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There used to be a gruesome test for (hasNoBinding v) in the
Var case:
        exprIsTrivial (Var v) | hasNoBinding v = idArity v == 0
The idea here is that a constructor worker, like \$wJust, is
really short for (\x -> \$wJust x), because \$wJust has no binding.
So it should be treated like a lambda.  Ditto unsaturated primops.
But now constructor workers are not "have-no-binding" Ids.  And
completely un-applied primops and foreign-call Ids are sufficiently
rare that I plan to allow them to be duplicated and put up with
saturating them.

Note [Tick trivial]
~~~~~~~~~~~~~~~~~~~
Ticks are only trivial if they are pure annotations. If we treat
"tick<n> x" as trivial, it will be inlined inside lambdas and the
entry count will be skewed, for example.  Furthermore "scc<n> x" will
turn into just "x" in mkTick.

Note [Empty case is trivial]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The expression (case (x::Int) Bool of {}) is just a type-changing
case used when we are sure that 'x' will not return.  See
Note [Empty case alternatives] in CoreSyn.

If the scrutinee is trivial, then so is the whole expression; and the
CoreToSTG pass in fact drops the case expression leaving only the
scrutinee.

Having more trivial expressions is good.  Moreover, if we don't treat
it as trivial we may land up with let-bindings like
   let v = case x of {} in ...
and after CoreToSTG that gives
   let v = x in ...
and that confuses the code generator (Trac #11155). So best to kill
it off at source.
-}

exprIsTrivial :: CoreExpr -> Bool
exprIsTrivial (Var _)          = True        -- See Note [Variables are trivial]
exprIsTrivial (Type _)         = True
exprIsTrivial (Coercion _)     = True
exprIsTrivial (Lit lit)        = litIsTrivial lit
exprIsTrivial (App e arg)      = not (isRuntimeArg arg) && exprIsTrivial e
exprIsTrivial (Lam b e)        = not (isRuntimeVar b) && exprIsTrivial e
exprIsTrivial (Tick t e)       = not (tickishIsCode t) && exprIsTrivial e
                                 -- See Note [Tick trivial]
exprIsTrivial (Cast e _)       = exprIsTrivial e
exprIsTrivial (Case e _ _ [])  = exprIsTrivial e  -- See Note [Empty case is trivial]
exprIsTrivial _                = False

{-
Note [getIdFromTrivialExpr]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
When substituting in a breakpoint we need to strip away the type cruft
from a trivial expression and get back to the Id.  The invariant is
that the expression we're substituting was originally trivial
according to exprIsTrivial, AND the expression is not a literal.
See Note [substTickish] for how breakpoint substitution preserves
this extra invariant.

We also need this functionality in CorePrep to extract out Id of a
function which we are saturating.  However, in this case we don't know
if the variable actually refers to a literal; thus we use
'getIdFromTrivialExpr_maybe' to handle this case.  See test
T12076lit for an example where this matters.
-}

getIdFromTrivialExpr :: CoreExpr -> Id
getIdFromTrivialExpr e
    = fromMaybe (pprPanic "getIdFromTrivialExpr" (ppr e))
                (getIdFromTrivialExpr_maybe e)

getIdFromTrivialExpr_maybe :: CoreExpr -> Maybe Id
-- See Note [getIdFromTrivialExpr]
getIdFromTrivialExpr_maybe e = go e
  where go (Var v) = Just v
        go (App f t) | not (isRuntimeArg t) = go f
        go (Tick t e) | not (tickishIsCode t) = go e
        go (Cast e _) = go e
        go (Lam b e) | not (isRuntimeVar b) = go e
        go _ = Nothing

{-
exprIsBottom is a very cheap and cheerful function; it may return
False for bottoming expressions, but it never costs much to ask.  See
also CoreArity.exprBotStrictness_maybe, but that's a bit more
expensive.
-}

exprIsBottom :: CoreExpr -> Bool
-- See Note [Bottoming expressions]
exprIsBottom e
  | isEmptyTy (exprType e)
  = True
  | otherwise
  = go 0 e
  where
    go n (Var v) = isBottomingId v &&  n >= idArity v
    go n (App e a) | isTypeArg a = go n e
                   | otherwise   = go (n+1) e
    go n (Tick _ e)              = go n e
    go n (Cast e _)              = go n e
    go n (Let _ e)               = go n e
    go n (Lam v e) | isTyVar v   = go n e
    go _ (Case _ _ _ alts)       = null alts
       -- See Note [Empty case alternatives] in CoreSyn
    go _ _                       = False

{- Note [Bottoming expressions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A bottoming expression is guaranteed to diverge, or raise an
exception.  We can test for it in two different ways, and exprIsBottom
checks for both of these situations:

* Visibly-bottom computations.  For example
      (error Int "Hello")
  is visibly bottom.  The strictness analyser also finds out if
  a function diverges or raises an exception, and puts that info
  in its strictness signature.

* Empty types.  If a type is empty, its only inhabitant is bottom.
  For example:
      data T
      f :: T -> Bool
      f = \(x:t). case x of Bool {}
  Since T has no data constructors, the case alternatives are of course
  empty.  However note that 'x' is not bound to a visibly-bottom value;
  it's the *type* that tells us it's going to diverge.

A GADT may also be empty even though it has constructors:
        data T a where
          T1 :: a -> T Bool
          T2 :: T Int
        ...(case (x::T Char) of {})...
Here (T Char) is uninhabited.  A more realistic case is (Int ~ Bool),
which is likewise uninhabited.


************************************************************************
*                                                                      *
             exprIsDupable
*                                                                      *
************************************************************************

Note [exprIsDupable]
~~~~~~~~~~~~~~~~~~~~
@exprIsDupable@ is true of expressions that can be duplicated at a modest
                cost in code size.  This will only happen in different case
                branches, so there's no issue about duplicating work.

                That is, exprIsDupable returns True of (f x) even if
                f is very very expensive to call.

                Its only purpose is to avoid fruitless let-binding
                and then inlining of case join points
-}

exprIsDupable :: DynFlags -> CoreExpr -> Bool
exprIsDupable dflags e
  = isJust (go dupAppSize e)
  where
    go :: Int -> CoreExpr -> Maybe Int
    go n (Type {})     = Just n
    go n (Coercion {}) = Just n
    go n (Var {})      = decrement n
    go n (Tick _ e)    = go n e
    go n (Cast e _)    = go n e
    go n (App f a) | Just n' <- go n a = go n' f
    go n (Lit lit) | litIsDupable dflags lit = decrement n
    go _ _ = Nothing

    decrement :: Int -> Maybe Int
    decrement 0 = Nothing
    decrement n = Just (n-1)

dupAppSize :: Int
dupAppSize = 8   -- Size of term we are prepared to duplicate
                 -- This is *just* big enough to make test MethSharing
                 -- inline enough join points.  Really it should be
                 -- smaller, and could be if we fixed Trac #4960.

{-
************************************************************************
*                                                                      *
             exprIsCheap, exprIsExpandable
*                                                                      *
************************************************************************

Note [exprIsWorkFree]
~~~~~~~~~~~~~~~~~~~~~
exprIsWorkFree is used when deciding whether to inline something; we
don't inline it if doing so might duplicate work, by peeling off a
complete copy of the expression.  Here we do not want even to
duplicate a primop (Trac #5623):
   eg   let x = a #+ b in x +# x
   we do not want to inline/duplicate x

Previously we were a bit more liberal, which led to the primop-duplicating
problem.  However, being more conservative did lead to a big regression in
one nofib benchmark, wheel-sieve1.  The situation looks like this:

   let noFactor_sZ3 :: GHC.Types.Int -> GHC.Types.Bool
       noFactor_sZ3 = case s_adJ of _ { GHC.Types.I# x_aRs ->
         case GHC.Prim.<=# x_aRs 2 of _ {
           GHC.Types.False -> notDivBy ps_adM qs_adN;
           GHC.Types.True -> lvl_r2Eb }}
       go = \x. ...(noFactor (I# y))....(go x')...

The function 'noFactor' is heap-allocated and then called.  Turns out
that 'notDivBy' is strict in its THIRD arg, but that is invisible to
the caller of noFactor, which therefore cannot do w/w and
heap-allocates noFactor's argument.  At the moment (May 12) we are just
going to put up with this, because the previous more aggressive inlining
(which treated 'noFactor' as work-free) was duplicating primops, which
in turn was making inner loops of array calculations runs slow (#5623)

Note [Case expressions are work-free]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Are case-expressions work-free?  Consider
    let v = case x of (p,q) -> p
        go = \y -> ...case v of ...
Should we inline 'v' at its use site inside the loop?  At the moment
we do.  I experimented with saying that case are *not* work-free, but
that increased allocation slightly.  It's a fairly small effect, and at
the moment we go for the slightly more aggressive version which treats
(case x of ....) as work-free if the alternatives are.

Moreover it improves arities of overloaded functions where
there is only dictionary selection (no construction) involved

Note [exprIsCheap]   See also Note [Interaction of exprIsCheap and lone variables]
~~~~~~~~~~~~~~~~~~   in CoreUnfold.hs
@exprIsCheap@ looks at a Core expression and returns \tr{True} if
it is obviously in weak head normal form, or is cheap to get to WHNF.
[Note that that's not the same as exprIsDupable; an expression might be
big, and hence not dupable, but still cheap.]

By ``cheap'' we mean a computation we're willing to:
        push inside a lambda, or
        inline at more than one place
That might mean it gets evaluated more than once, instead of being
shared.  The main examples of things which aren't WHNF but are
``cheap'' are:

  *     case e of
          pi -> ei
        (where e, and all the ei are cheap)

  *     let x = e in b
        (where e and b are cheap)

  *     op x1 ... xn
        (where op is a cheap primitive operator)

  *     error "foo"
        (because we are happy to substitute it inside a lambda)

Notice that a variable is considered 'cheap': we can push it inside a lambda,
because sharing will make sure it is only evaluated once.

Note [exprIsCheap and exprIsHNF]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Note that exprIsHNF does not imply exprIsCheap.  Eg
        let x = fac 20 in Just x
This responds True to exprIsHNF (you can discard a seq), but
False to exprIsCheap.

Note [exprIsExpandable]
~~~~~~~~~~~~~~~~~~~~~~~
An expression is "expandable" if we are willing to dupicate it, if doing
so might make a RULE or case-of-constructor fire.  Mainly this means
data-constructor applications, but it's a bit more generous than exprIsCheap
because it is true of "CONLIKE" Ids: see Note [CONLIKE pragma] in BasicTypes.

It is used to set the uf_expandable field of an Unfolding, and that
in turn is used
  * In RULE matching
  * In exprIsConApp_maybe, exprIsLiteral_maybe, exprIsLambda_maybe

But take care: exprIsExpandable should /not/ be true of primops.  I
found this in test T5623a:
    let q = /\a. Ptr a (a +# b)
    in case q @ Float of Ptr v -> ...q...

q's inlining should not be expandable, else exprIsConApp_maybe will
say that (q @ Float) expands to (Ptr a (a +# b)), and that will
duplicate the (a +# b) primop, which we should not do lightly.
(It's quite hard to trigger this bug, but T13155 does so for GHC 8.0.)


Note [Arguments and let-bindings exprIsCheapX]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
What predicate should we apply to the argument of an application, or the
RHS of a let-binding?

We used to say "exprIsTrivial arg" due to concerns about duplicating
nested constructor applications, but see #4978.  So now we just recursively
use exprIsCheapX.

We definitely want to treat let and app the same.  The principle here is
that
   let x = blah in f x
should behave equivalently to
   f blah

This in turn means that the 'letrec g' does not prevent eta expansion
in this (which it previously was):
    f = \x. let v = case x of
                      True -> letrec g = \w. blah
                              in g
                      False -> \x. x
            in \w. v True
-}

--------------------
exprIsCheap :: CoreExpr -> Bool
exprIsCheap = exprIsCheapX isCheapApp

exprIsExpandable :: CoreExpr -> Bool -- See Note [exprIsExpandable]
exprIsExpandable = exprIsCheapX isExpandableApp

exprIsWorkFree :: CoreExpr -> Bool   -- See Note [exprIsWorkFree]
exprIsWorkFree = exprIsCheapX isWorkFreeApp

--------------------
exprIsCheapX :: CheapAppFun -> CoreExpr -> Bool
exprIsCheapX ok_app e
  = ok e
  where
    ok e = go 0 e

    -- n is the number of value arguments
    go n (Var v)                      = ok_app v n
    go _ (Lit {})                     = True
    go _ (Type {})                    = True
    go _ (Coercion {})                = True
    go n (Cast e _)                   = go n e
    go n (Case scrut _ _ alts)        = ok scrut &&
                                        and [ go n rhs | (_,_,rhs) <- alts ]
    go n (Tick t e) | tickishCounts t = False
                    | otherwise       = go n e
    go n (Lam x e)  | isRuntimeVar x  = n==0 || go (n-1) e
                    | otherwise       = go n e
    go n (App f e)  | isRuntimeArg e  = go (n+1) f && ok e
                    | otherwise       = go n f
    go n (Let (NonRec _ r) e)         = go n e && ok r
    go n (Let (Rec prs) e)            = go n e && all (ok . snd) prs

      -- Case: see Note [Case expressions are work-free]
      -- App, Let: see Note [Arguments and let-bindings exprIsCheapX]


-------------------------------------
type CheapAppFun = Id -> Arity -> Bool
  -- Is an application of this function to n *value* args
  -- always cheap, assuming the arguments are cheap?
  -- True mainly of data constructors, partial applications;
  -- but with minor variations:
  --    isWorkFreeApp
  --    isCheapApp
  --    isExpandableApp

isWorkFreeApp :: CheapAppFun
isWorkFreeApp fn n_val_args
  | n_val_args == 0           -- No value args
  = True
  | n_val_args < idArity fn   -- Partial application
  = True
  | otherwise
  = case idDetails fn of
      DataConWorkId {} -> True
      _                -> False

isCheapApp :: CheapAppFun
isCheapApp fn n_val_args
  | isWorkFreeApp fn n_val_args = True
  | isBottomingId fn            = True  -- See Note [isCheapApp: bottoming functions]
  | otherwise
  = case idDetails fn of
      DataConWorkId {} -> True  -- Actually handled by isWorkFreeApp
      RecSelId {}      -> n_val_args == 1  -- See Note [Record selection]
      ClassOpId {}     -> n_val_args == 1
      PrimOpId op      -> primOpIsCheap op
      _                -> False
        -- In principle we should worry about primops
        -- that return a type variable, since the result
        -- might be applied to something, but I'm not going
        -- to bother to check the number of args

isExpandableApp :: CheapAppFun
isExpandableApp fn n_val_args
  | isWorkFreeApp fn n_val_args = True
  | otherwise
  = case idDetails fn of
      DataConWorkId {} -> True  -- Actually handled by isWorkFreeApp
      RecSelId {}      -> n_val_args == 1  -- See Note [Record selection]
      ClassOpId {}     -> n_val_args == 1
      PrimOpId {}      -> False
      _ | isBottomingId fn               -> False
          -- See Note [isExpandableApp: bottoming functions]
        | isConLike (idRuleMatchInfo fn) -> True
        | all_args_are_preds             -> True
        | otherwise                      -> False

  where
     -- See if all the arguments are PredTys (implicit params or classes)
     -- If so we'll regard it as expandable; see Note [Expandable overloadings]
     all_args_are_preds = all_pred_args n_val_args (idType fn)

     all_pred_args n_val_args ty
       | n_val_args == 0
       = True

       | Just (bndr, ty) <- splitPiTy_maybe ty
       = caseBinder bndr
           (\_tv -> all_pred_args n_val_args ty)
           (\bndr_ty -> isPredTy bndr_ty && all_pred_args (n_val_args-1) ty)

       | otherwise
       = False

{- Note [isCheapApp: bottoming functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
I'm not sure why we have a special case for bottoming
functions in isCheapApp.  Maybe we don't need it.

Note [isExpandableApp: bottoming functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's important that isExpandableApp does not respond True to bottoming
functions.  Recall  undefined :: HasCallStack => a
Suppose isExpandableApp responded True to (undefined d), and we had:

  x = undefined <dict-expr>

Then Simplify.prepareRhs would ANF the RHS:

  d = <dict-expr>
  x = undefined d

This is already bad: we gain nothing from having x bound to (undefined
var), unlike the case for data constructors.  Worse, we get the
simplifier loop described in OccurAnal Note [Cascading inlines].
Suppose x occurs just once; OccurAnal.occAnalNonRecRhs decides x will
certainly_inline; so we end up inlining d right back into x; but in
the end x doesn't inline because it is bottom (preInlineUnconditionally);
so the process repeats.. We could elaborate the certainly_inline logic
some more, but it's better just to treat bottoming bindings as
non-expandable, because ANFing them is a bad idea in the first place.

Note [Record selection]
~~~~~~~~~~~~~~~~~~~~~~~~~~
I'm experimenting with making record selection
look cheap, so we will substitute it inside a
lambda.  Particularly for dictionary field selection.

BUT: Take care with (sel d x)!  The (sel d) might be cheap, but
there's no guarantee that (sel d x) will be too.  Hence (n_val_args == 1)

Note [Expandable overloadings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose the user wrote this
   {-# RULE  forall x. foo (negate x) = h x #-}
   f x = ....(foo (negate x))....
He'd expect the rule to fire. But since negate is overloaded, we might
get this:
    f = \d -> let n = negate d in \x -> ...foo (n x)...
So we treat the application of a function (negate in this case) to a
*dictionary* as expandable.  In effect, every function is CONLIKE when
it's applied only to dictionaries.


************************************************************************
*                                                                      *
             exprOkForSpeculation
*                                                                      *
************************************************************************
-}

-----------------------------
-- | 'exprOkForSpeculation' returns True of an expression that is:
--
--  * Safe to evaluate even if normal order eval might not
--    evaluate the expression at all, or
--
--  * Safe /not/ to evaluate even if normal order would do so
--
-- It is usually called on arguments of unlifted type, but not always
-- In particular, Simplify.rebuildCase calls it on lifted types
-- when a 'case' is a plain 'seq'. See the example in
-- Note [exprOkForSpeculation: case expressions] below
--
-- Precisely, it returns @True@ iff:
--  a) The expression guarantees to terminate,
--  b) soon,
--  c) without causing a write side effect (e.g. writing a mutable variable)
--  d) without throwing a Haskell exception
--  e) without risking an unchecked runtime exception (array out of bounds,
--     divide by zero)
--
-- For @exprOkForSideEffects@ the list is the same, but omitting (e).
--
-- Note that
--    exprIsHNF            implies exprOkForSpeculation
--    exprOkForSpeculation implies exprOkForSideEffects
--
-- See Note [PrimOp can_fail and has_side_effects] in PrimOp
-- and Note [Transformations affected by can_fail and has_side_effects]
--
-- As an example of the considerations in this test, consider:
--
-- > let x = case y# +# 1# of { r# -> I# r# }
-- > in E
--
-- being translated to:
--
-- > case y# +# 1# of { r# ->
-- >    let x = I# r#
-- >    in E
-- > }
--
-- We can only do this if the @y + 1@ is ok for speculation: it has no
-- side effects, and can't diverge or raise an exception.

exprOkForSpeculation, exprOkForSideEffects :: CoreExpr -> Bool
exprOkForSpeculation = expr_ok primOpOkForSpeculation
exprOkForSideEffects = expr_ok primOpOkForSideEffects

expr_ok :: (PrimOp -> Bool) -> CoreExpr -> Bool
expr_ok _ (Lit _)      = True
expr_ok _ (Type _)     = True
expr_ok _ (Coercion _) = True

expr_ok primop_ok (Var v)      = app_ok primop_ok v []
expr_ok primop_ok (Cast e _)   = expr_ok primop_ok e

-- Tick annotations that *tick* cannot be speculated, because these
-- are meant to identify whether or not (and how often) the particular
-- source expression was evaluated at runtime.
expr_ok primop_ok (Tick tickish e)
   | tickishCounts tickish = False
   | otherwise             = expr_ok primop_ok e

expr_ok _ (Let {}) = False
  -- Lets can be stacked deeply, so just give up.
  -- In any case, the argument of exprOkForSpeculation is
  -- usually in a strict context, so any lets will have been
  -- floated away.

expr_ok primop_ok (Case scrut bndr _ alts)
  =  -- See Note [exprOkForSpeculation: case expressions]
     expr_ok primop_ok scrut
  && isUnliftedType (idType bndr)
  && all (\(_,_,rhs) -> expr_ok primop_ok rhs) alts
  && altsAreExhaustive alts

expr_ok primop_ok other_expr
  = case collectArgs other_expr of
        (expr, args) | Var f <- stripTicksTopE (not . tickishCounts) expr
                     -> app_ok primop_ok f args
        _            -> False

-----------------------------
app_ok :: (PrimOp -> Bool) -> Id -> [CoreExpr] -> Bool
app_ok primop_ok fun args
  = case idDetails fun of
      DFunId new_type ->  not new_type
         -- DFuns terminate, unless the dict is implemented
         -- with a newtype in which case they may not

      DataConWorkId {} -> True
                -- The strictness of the constructor has already
                -- been expressed by its "wrapper", so we don't need
                -- to take the arguments into account

      PrimOpId op
        | isDivOp op
        , [arg1, Lit lit] <- args
        -> not (isZeroLit lit) && expr_ok primop_ok arg1
              -- Special case for dividing operations that fail
              -- In general they are NOT ok-for-speculation
              -- (which primop_ok will catch), but they ARE OK
              -- if the divisor is definitely non-zero.
              -- Often there is a literal divisor, and this
              -- can get rid of a thunk in an inner loop

        | otherwise
        -> primop_ok op     -- Check the primop itself
        && and (zipWith arg_ok arg_tys args)  -- Check the arguments

      _other -> isUnliftedType (idType fun)          -- c.f. the Var case of exprIsHNF
             || idArity fun > n_val_args             -- Partial apps
             || (n_val_args == 0 &&
                 isEvaldUnfolding (idUnfolding fun)) -- Let-bound values
             where
               n_val_args = valArgCount args
  where
    (arg_tys, _) = splitPiTys (idType fun)

    arg_ok :: TyBinder -> CoreExpr -> Bool
    arg_ok (Named _) _ = True   -- A type argument
    arg_ok (Anon ty) arg        -- A term argument
       | isUnliftedType ty = expr_ok primop_ok arg
       | otherwise         = True  -- See Note [Primops with lifted arguments]

-----------------------------
altsAreExhaustive :: [Alt b] -> Bool
-- True  <=> the case alternatives are definiely exhaustive
-- False <=> they may or may not be
altsAreExhaustive []
  = False    -- Should not happen
altsAreExhaustive ((con1,_,_) : alts)
  = case con1 of
      DEFAULT   -> True
      LitAlt {} -> False
      DataAlt c -> alts `lengthIs` (tyConFamilySize (dataConTyCon c) - 1)
      -- It is possible to have an exhaustive case that does not
      -- enumerate all constructors, notably in a GADT match, but
      -- we behave conservatively here -- I don't think it's important
      -- enough to deserve special treatment

-- | True of dyadic operators that can fail only if the second arg is zero!
isDivOp :: PrimOp -> Bool
-- This function probably belongs in PrimOp, or even in
-- an automagically generated file.. but it's such a
-- special case I thought I'd leave it here for now.
isDivOp IntQuotOp        = True
isDivOp IntRemOp         = True
isDivOp WordQuotOp       = True
isDivOp WordRemOp        = True
isDivOp FloatDivOp       = True
isDivOp DoubleDivOp      = True
isDivOp _                = False

{- Note [exprOkForSpeculation: case expressions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
exprOkForSpeculation accepts very special case exprsssions.
Reason: (a ==# b) is ok-for-speculation, but the litEq rules
in PrelRules convert it (a ==# 3#) to
   case a of { DEAFULT -> 0#; 3# -> 1# }
for excellent reasons described in
  PrelRules Note [The litEq rule: converting equality to case].
So, annoyingly, we want that case expression to be
ok-for-speculation too. Bother.

But we restrict it sharply:

* We restrict it to unlifted scrutinees. Consider this:
     case x of y {
       DEFAULT -> ... (let v::Int# = case y of { True  -> e1
                                               ; False -> e2 }
                       in ...) ...

  Does the RHS of v satisfy the let/app invariant?  Previously we said
  yes, on the grounds that y is evaluated.  But the binder-swap done
  by SetLevels would transform the inner alternative to
     DEFAULT -> ... (let v::Int# = case x of { ... }
                     in ...) ....
  which does /not/ satisfy the let/app invariant, because x is
  not evaluated. See Note [Binder-swap during float-out]
  in SetLevels.  To avoid this awkwardness it seems simpler
  to stick to unlifted scrutinees where the issue does not
  arise.

* We restrict it to exhaustive alternatives. A non-exhaustive
  case manifestly isn't ok-for-speculation. Consider
    case e of x { DEAFULT ->
      ...(case x of y
            A -> ...
            _ -> ...(case (case x of { B -> p; C -> p }) of
                       I# r -> blah)...
  If SetLevesls considers the inner nested case as ok-for-speculation
  it can do case-floating (see Note [Floating cases] in SetLevels).
  So we'd float to:
    case e of x { DEAFULT ->
    case (case x of { B -> p; C -> p }) of I# r ->
    ...(case x of y
            A -> ...
            _ -> ...blah...)...
  which is utterly bogus (seg fault); see Trac #5453.

  Similarly, this is a valid program (albeit a slightly dodgy one)
    let v = case x of { B -> ...; C -> ... }
    in case x of
         A -> ...
         _ ->  ...v...v....
  Should v be considered ok-for-speculation?  Its scrutinee may be
  evaluated, but the alternatives are incomplete so we should not
  evalutate it strictly.

  Now, all this is for lifted types, but it'd be the same for any
  finite unlifted type. We don't have many of them, but we might
  add unlifted algebraic types in due course.

----- Historical note: Trac #3717: --------
    foo :: Int -> Int
    foo 0 = 0
    foo n = (if n < 5 then 1 else 2) `seq` foo (n-1)

In earlier GHCs, we got this:
    T.$wfoo =
      \ (ww :: GHC.Prim.Int#) ->
        case ww of ds {
          __DEFAULT -> case (case <# ds 5 of _ {
                          GHC.Types.False -> lvl1;
                          GHC.Types.True -> lvl})
                       of _ { __DEFAULT ->
                       T.$wfoo (GHC.Prim.-# ds_XkE 1) };
          0 -> 0 }

Before join-points etc we could only get rid of two cases (which are
redundant) by recognising that th e(case <# ds 5 of { ... }) is
ok-for-speculation, even though it has /lifted/ tyupe.  But now join
points do the job nicely.
------- End of historical note ------------


Note [Primops with lifted arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Is this ok-for-speculation (see Trac #13027)?
   reallyUnsafePtrEq# a b
Well, yes.  The primop accepts lifted arguments and does not
evaluate them.  Indeed, in general primops are, well, primitive
and do not perform evaluation.

There is one primop, dataToTag#, which does /require/ a lifted
argument to be evaluted.  To ensure this, CorePrep adds an
eval if it can't see the the argument is definitely evaluated
(see [dataToTag magic] in CorePrep).

We make no attempt to guarantee that dataToTag#'s argument is
evaluated here.  Main reason: it's very fragile to test for the
evaluatedness of a lifted argument.  Consider
    case x of y -> let v = dataToTag# y in ...

where x/y have type Int, say.  'y' looks evaluated (by the enclosing
case) so all is well.  Now the FloatOut pass does a binder-swap (for
very good reasons), changing to
   case x of y -> let v = dataToTag# x in ...

See also Note [dataToTag#] in primops.txt.pp.

Bottom line:
  * in exprOkForSpeculation we simply ignore all lifted arguments.


************************************************************************
*                                                                      *
             exprIsHNF, exprIsConLike
*                                                                      *
************************************************************************
-}

-- Note [exprIsHNF]             See also Note [exprIsCheap and exprIsHNF]
-- ~~~~~~~~~~~~~~~~
-- | exprIsHNF returns true for expressions that are certainly /already/
-- evaluated to /head/ normal form.  This is used to decide whether it's ok
-- to change:
--
-- > case x of _ -> e
--
--    into:
--
-- > e
--
-- and to decide whether it's safe to discard a 'seq'.
--
-- So, it does /not/ treat variables as evaluated, unless they say they are.
-- However, it /does/ treat partial applications and constructor applications
-- as values, even if their arguments are non-trivial, provided the argument
-- type is lifted. For example, both of these are values:
--
-- > (:) (f x) (map f xs)
-- > map (...redex...)
--
-- because 'seq' on such things completes immediately.
--
-- For unlifted argument types, we have to be careful:
--
-- > C (f x :: Int#)
--
-- Suppose @f x@ diverges; then @C (f x)@ is not a value. However this can't
-- happen: see "CoreSyn#let_app_invariant". This invariant states that arguments of
-- unboxed type must be ok-for-speculation (or trivial).
exprIsHNF :: CoreExpr -> Bool           -- True => Value-lambda, constructor, PAP
exprIsHNF = exprIsHNFlike isDataConWorkId isEvaldUnfolding

-- | Similar to 'exprIsHNF' but includes CONLIKE functions as well as
-- data constructors. Conlike arguments are considered interesting by the
-- inliner.
exprIsConLike :: CoreExpr -> Bool       -- True => lambda, conlike, PAP
exprIsConLike = exprIsHNFlike isConLikeId isConLikeUnfolding

-- | Returns true for values or value-like expressions. These are lambdas,
-- constructors / CONLIKE functions (as determined by the function argument)
-- or PAPs.
--
exprIsHNFlike :: (Var -> Bool) -> (Unfolding -> Bool) -> CoreExpr -> Bool
exprIsHNFlike is_con is_con_unf = is_hnf_like
  where
    is_hnf_like (Var v) -- NB: There are no value args at this point
      =  is_con v       -- Catches nullary constructors,
                        --      so that [] and () are values, for example
      || idArity v > 0  -- Catches (e.g.) primops that don't have unfoldings
      || is_con_unf (idUnfolding v)
        -- Check the thing's unfolding; it might be bound to a value
        -- We don't look through loop breakers here, which is a bit conservative
        -- but otherwise I worry that if an Id's unfolding is just itself,
        -- we could get an infinite loop

    is_hnf_like (Lit _)          = True
    is_hnf_like (Type _)         = True       -- Types are honorary Values;
                                              -- we don't mind copying them
    is_hnf_like (Coercion _)     = True       -- Same for coercions
    is_hnf_like (Lam b e)        = isRuntimeVar b || is_hnf_like e
    is_hnf_like (Tick tickish e) = not (tickishCounts tickish)
                                      && is_hnf_like e
                                      -- See Note [exprIsHNF Tick]
    is_hnf_like (Cast e _)       = is_hnf_like e
    is_hnf_like (App e a)
      | isValArg a               = app_is_value e 1
      | otherwise                = is_hnf_like e
    is_hnf_like (Let _ e)        = is_hnf_like e  -- Lazy let(rec)s don't affect us
    is_hnf_like _                = False

    -- There is at least one value argument
    -- 'n' is number of value args to which the expression is applied
    app_is_value :: CoreExpr -> Int -> Bool
    app_is_value (Var fun) n_val_args
      = idArity fun > n_val_args    -- Under-applied function
        || is_con fun               --  or constructor-like
    app_is_value (Tick _ f) nva = app_is_value f nva
    app_is_value (Cast f _) nva = app_is_value f nva
    app_is_value (App f a)  nva
      | isValArg a              = app_is_value f (nva + 1)
      | otherwise               = app_is_value f nva
    app_is_value _ _ = False

{-
Note [exprIsHNF Tick]

We can discard source annotations on HNFs as long as they aren't
tick-like:

  scc c (\x . e)    =>  \x . e
  scc c (C x1..xn)  =>  C x1..xn

So we regard these as HNFs.  Tick annotations that tick are not
regarded as HNF if the expression they surround is HNF, because the
tick is there to tell us that the expression was evaluated, so we
don't want to discard a seq on it.
-}

-- | Can we bind this 'CoreExpr' at the top level?
exprIsTopLevelBindable :: CoreExpr -> Type -> Bool
-- See Note [CoreSyn top-level string literals]
-- Precondition: exprType expr = ty
exprIsTopLevelBindable expr ty
  = exprIsLiteralString expr
  || not (isUnliftedType ty)

exprIsLiteralString :: CoreExpr -> Bool
exprIsLiteralString (Lit (MachStr _)) = True
exprIsLiteralString _ = False

{-
************************************************************************
*                                                                      *
             Instantiating data constructors
*                                                                      *
************************************************************************

These InstPat functions go here to avoid circularity between DataCon and Id
-}

dataConRepInstPat   ::                 [Unique] -> DataCon -> [Type] -> ([TyVar], [Id])
dataConRepFSInstPat :: [FastString] -> [Unique] -> DataCon -> [Type] -> ([TyVar], [Id])

dataConRepInstPat   = dataConInstPat (repeat ((fsLit "ipv")))
dataConRepFSInstPat = dataConInstPat

dataConInstPat :: [FastString]          -- A long enough list of FSs to use for names
               -> [Unique]              -- An equally long list of uniques, at least one for each binder
               -> DataCon
               -> [Type]                -- Types to instantiate the universally quantified tyvars
               -> ([TyVar], [Id])       -- Return instantiated variables
-- dataConInstPat arg_fun fss us con inst_tys returns a tuple
-- (ex_tvs, arg_ids),
--
--   ex_tvs are intended to be used as binders for existential type args
--
--   arg_ids are indended to be used as binders for value arguments,
--     and their types have been instantiated with inst_tys and ex_tys
--     The arg_ids include both evidence and
--     programmer-specified arguments (both after rep-ing)
--
-- Example.
--  The following constructor T1
--
--  data T a where
--    T1 :: forall b. Int -> b -> T(a,b)
--    ...
--
--  has representation type
--   forall a. forall a1. forall b. (a ~ (a1,b)) =>
--     Int -> b -> T a
--
--  dataConInstPat fss us T1 (a1',b') will return
--
--  ([a1'', b''], [c :: (a1', b')~(a1'', b''), x :: Int, y :: b''])
--
--  where the double-primed variables are created with the FastStrings and
--  Uniques given as fss and us
dataConInstPat fss uniqs con inst_tys
  = ASSERT( univ_tvs `equalLength` inst_tys )
    (ex_bndrs, arg_ids)
  where
    univ_tvs = dataConUnivTyVars con
    ex_tvs   = dataConExTyVars con
    arg_tys  = dataConRepArgTys con
    arg_strs = dataConRepStrictness con  -- 1-1 with arg_tys
    n_ex = length ex_tvs

      -- split the Uniques and FastStrings
    (ex_uniqs, id_uniqs) = splitAt n_ex uniqs
    (ex_fss,   id_fss)   = splitAt n_ex fss

      -- Make the instantiating substitution for universals
    univ_subst = zipTvSubst univ_tvs inst_tys

      -- Make existential type variables, applying and extending the substitution
    (full_subst, ex_bndrs) = mapAccumL mk_ex_var univ_subst
                                       (zip3 ex_tvs ex_fss ex_uniqs)

    mk_ex_var :: TCvSubst -> (TyVar, FastString, Unique) -> (TCvSubst, TyVar)
    mk_ex_var subst (tv, fs, uniq) = (Type.extendTvSubstWithClone subst tv
                                       new_tv
                                     , new_tv)
      where
        new_tv = mkTyVar (mkSysTvName uniq fs) kind
        kind   = Type.substTyUnchecked subst (tyVarKind tv)

      -- Make value vars, instantiating types
    arg_ids = zipWith4 mk_id_var id_uniqs id_fss arg_tys arg_strs
    mk_id_var uniq fs ty str
      = setCaseBndrEvald str $  -- See Note [Mark evaluated arguments]
        mkLocalIdOrCoVar name (Type.substTy full_subst ty)
      where
        name = mkInternalName uniq (mkVarOccFS fs) noSrcSpan

{-
Note [Mark evaluated arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When pattern matching on a constructor with strict fields, the binder
can have an 'evaldUnfolding'.  Moreover, it *should* have one, so that
when loading an interface file unfolding like:
  data T = MkT !Int
  f x = case x of { MkT y -> let v::Int# = case y of I# n -> n+1
                             in ... }
we don't want Lint to complain.  The 'y' is evaluated, so the
case in the RHS of the binding for 'v' is fine.  But only if we
*know* that 'y' is evaluated.

c.f. add_evals in Simplify.simplAlt

************************************************************************
*                                                                      *
         Equality
*                                                                      *
************************************************************************
-}

-- | A cheap equality test which bales out fast!
--      If it returns @True@ the arguments are definitely equal,
--      otherwise, they may or may not be equal.
--
-- See also 'exprIsBig'
cheapEqExpr :: Expr b -> Expr b -> Bool
cheapEqExpr = cheapEqExpr' (const False)

-- | Cheap expression equality test, can ignore ticks by type.
cheapEqExpr' :: (Tickish Id -> Bool) -> Expr b -> Expr b -> Bool
cheapEqExpr' ignoreTick = go_s
  where go_s = go `on` stripTicksTopE ignoreTick
        go (Var v1)   (Var v2)   = v1 == v2
        go (Lit lit1) (Lit lit2) = lit1 == lit2
        go (Type t1)  (Type t2)  = t1 `eqType` t2
        go (Coercion c1) (Coercion c2) = c1 `eqCoercion` c2

        go (App f1 a1) (App f2 a2)
          = f1 `go_s` f2 && a1 `go_s` a2

        go (Cast e1 t1) (Cast e2 t2)
          = e1 `go_s` e2 && t1 `eqCoercion` t2

        go (Tick t1 e1) (Tick t2 e2)
          = t1 == t2 && e1 `go_s` e2

        go _ _ = False
        {-# INLINE go #-}
{-# INLINE cheapEqExpr' #-}

exprIsBig :: Expr b -> Bool
-- ^ Returns @True@ of expressions that are too big to be compared by 'cheapEqExpr'
exprIsBig (Lit _)      = False
exprIsBig (Var _)      = False
exprIsBig (Type _)     = False
exprIsBig (Coercion _) = False
exprIsBig (Lam _ e)    = exprIsBig e
exprIsBig (App f a)    = exprIsBig f || exprIsBig a
exprIsBig (Cast e _)   = exprIsBig e    -- Hopefully coercions are not too big!
exprIsBig (Tick _ e)   = exprIsBig e
exprIsBig _            = True

eqExpr :: InScopeSet -> CoreExpr -> CoreExpr -> Bool
-- Compares for equality, modulo alpha
eqExpr in_scope e1 e2
  = go (mkRnEnv2 in_scope) e1 e2
  where
    go env (Var v1) (Var v2)
      | rnOccL env v1 == rnOccR env v2
      = True

    go _   (Lit lit1)    (Lit lit2)      = lit1 == lit2
    go env (Type t1)    (Type t2)        = eqTypeX env t1 t2
    go env (Coercion co1) (Coercion co2) = eqCoercionX env co1 co2
    go env (Cast e1 co1) (Cast e2 co2) = eqCoercionX env co1 co2 && go env e1 e2
    go env (App f1 a1)   (App f2 a2)   = go env f1 f2 && go env a1 a2
    go env (Tick n1 e1)  (Tick n2 e2)  = eqTickish env n1 n2 && go env e1 e2

    go env (Lam b1 e1)  (Lam b2 e2)
      =  eqTypeX env (varType b1) (varType b2)   -- False for Id/TyVar combination
      && go (rnBndr2 env b1 b2) e1 e2

    go env (Let (NonRec v1 r1) e1) (Let (NonRec v2 r2) e2)
      =  go env r1 r2  -- No need to check binder types, since RHSs match
      && go (rnBndr2 env v1 v2) e1 e2

    go env (Let (Rec ps1) e1) (Let (Rec ps2) e2)
      = equalLength ps1 ps2
      && all2 (go env') rs1 rs2 && go env' e1 e2
      where
        (bs1,rs1) = unzip ps1
        (bs2,rs2) = unzip ps2
        env' = rnBndrs2 env bs1 bs2

    go env (Case e1 b1 t1 a1) (Case e2 b2 t2 a2)
      | null a1   -- See Note [Empty case alternatives] in TrieMap
      = null a2 && go env e1 e2 && eqTypeX env t1 t2
      | otherwise
      =  go env e1 e2 && all2 (go_alt (rnBndr2 env b1 b2)) a1 a2

    go _ _ _ = False

    -----------
    go_alt env (c1, bs1, e1) (c2, bs2, e2)
      = c1 == c2 && go (rnBndrs2 env bs1 bs2) e1 e2

eqTickish :: RnEnv2 -> Tickish Id -> Tickish Id -> Bool
eqTickish env (Breakpoint lid lids) (Breakpoint rid rids)
      = lid == rid  &&  map (rnOccL env) lids == map (rnOccR env) rids
eqTickish _ l r = l == r

-- | Finds differences between core expressions, modulo alpha and
-- renaming. Setting @top@ means that the @IdInfo@ of bindings will be
-- checked for differences as well.
diffExpr :: Bool -> RnEnv2 -> CoreExpr -> CoreExpr -> [SDoc]
diffExpr _   env (Var v1)   (Var v2)   | rnOccL env v1 == rnOccR env v2 = []
diffExpr _   _   (Lit lit1) (Lit lit2) | lit1 == lit2                   = []
diffExpr _   env (Type t1)  (Type t2)  | eqTypeX env t1 t2              = []
diffExpr _   env (Coercion co1) (Coercion co2)
                                       | eqCoercionX env co1 co2        = []
diffExpr top env (Cast e1 co1)  (Cast e2 co2)
  | eqCoercionX env co1 co2                = diffExpr top env e1 e2
diffExpr top env (Tick n1 e1)   e2
  | not (tickishIsCode n1)                 = diffExpr top env e1 e2
diffExpr top env e1             (Tick n2 e2)
  | not (tickishIsCode n2)                 = diffExpr top env e1 e2
diffExpr top env (Tick n1 e1)   (Tick n2 e2)
  | eqTickish env n1 n2                    = diffExpr top env e1 e2
 -- The error message of failed pattern matches will contain
 -- generated names, which are allowed to differ.
diffExpr _   _   (App (App (Var absent) _) _)
                 (App (App (Var absent2) _) _)
  | isBottomingId absent && isBottomingId absent2 = []
diffExpr top env (App f1 a1)    (App f2 a2)
  = diffExpr top env f1 f2 ++ diffExpr top env a1 a2
diffExpr top env (Lam b1 e1)  (Lam b2 e2)
  | eqTypeX env (varType b1) (varType b2)   -- False for Id/TyVar combination
  = diffExpr top (rnBndr2 env b1 b2) e1 e2
diffExpr top env (Let bs1 e1) (Let bs2 e2)
  = let (ds, env') = diffBinds top env (flattenBinds [bs1]) (flattenBinds [bs2])
    in ds ++ diffExpr top env' e1 e2
diffExpr top env (Case e1 b1 t1 a1) (Case e2 b2 t2 a2)
  | equalLength a1 a2 && not (null a1) || eqTypeX env t1 t2
    -- See Note [Empty case alternatives] in TrieMap
  = diffExpr top env e1 e2 ++ concat (zipWith diffAlt a1 a2)
  where env' = rnBndr2 env b1 b2
        diffAlt (c1, bs1, e1) (c2, bs2, e2)
          | c1 /= c2  = [text "alt-cons " <> ppr c1 <> text " /= " <> ppr c2]
          | otherwise = diffExpr top (rnBndrs2 env' bs1 bs2) e1 e2
diffExpr _  _ e1 e2
  = [fsep [ppr e1, text "/=", ppr e2]]

-- | Finds differences between core bindings, see @diffExpr@.
--
-- The main problem here is that while we expect the binds to have the
-- same order in both lists, this is not guaranteed. To do this
-- properly we'd either have to do some sort of unification or check
-- all possible mappings, which would be seriously expensive. So
-- instead we simply match single bindings as far as we can. This
-- leaves us just with mutually recursive and/or mismatching bindings,
-- which we then speculatively match by ordering them. It's by no means
-- perfect, but gets the job done well enough.
diffBinds :: Bool -> RnEnv2 -> [(Var, CoreExpr)] -> [(Var, CoreExpr)]
          -> ([SDoc], RnEnv2)
diffBinds top env binds1 = go (length binds1) env binds1
 where go _    env []     []
          = ([], env)
       go fuel env binds1 binds2
          -- No binds left to compare? Bail out early.
          | null binds1 || null binds2
          = (warn env binds1 binds2, env)
          -- Iterated over all binds without finding a match? Then
          -- try speculatively matching binders by order.
          | fuel == 0
          = if not $ env `inRnEnvL` fst (head binds1)
            then let env' = uncurry (rnBndrs2 env) $ unzip $
                            zip (sort $ map fst binds1) (sort $ map fst binds2)
                 in go (length binds1) env' binds1 binds2
            -- If we have already tried that, give up
            else (warn env binds1 binds2, env)
       go fuel env ((bndr1,expr1):binds1) binds2
          | let matchExpr (bndr,expr) =
                  (not top || null (diffIdInfo env bndr bndr1)) &&
                  null (diffExpr top (rnBndr2 env bndr1 bndr) expr1 expr)
          , (binds2l, (bndr2,_):binds2r) <- break matchExpr binds2
          = go (length binds1) (rnBndr2 env bndr1 bndr2)
                binds1 (binds2l ++ binds2r)
          | otherwise -- No match, so push back (FIXME O(n^2))
          = go (fuel-1) env (binds1++[(bndr1,expr1)]) binds2
       go _ _ _ _ = panic "diffBinds: impossible" -- GHC isn't smart enough

       -- We have tried everything, but couldn't find a good match. So
       -- now we just return the comparison results when we pair up
       -- the binds in a pseudo-random order.
       warn env binds1 binds2 =
         concatMap (uncurry (diffBind env)) (zip binds1' binds2') ++
         unmatched "unmatched left-hand:" (drop l binds1') ++
         unmatched "unmatched right-hand:" (drop l binds2')
        where binds1' = sortBy (comparing fst) binds1
              binds2' = sortBy (comparing fst) binds2
              l = min (length binds1') (length binds2')
       unmatched _   [] = []
       unmatched txt bs = [text txt $$ ppr (Rec bs)]
       diffBind env (bndr1,expr1) (bndr2,expr2)
         | ds@(_:_) <- diffExpr top env expr1 expr2
         = locBind "in binding" bndr1 bndr2 ds
         | otherwise
         = diffIdInfo env bndr1 bndr2

-- | Find differences in @IdInfo@. We will especially check whether
-- the unfoldings match, if present (see @diffUnfold@).
diffIdInfo :: RnEnv2 -> Var -> Var -> [SDoc]
diffIdInfo env bndr1 bndr2
  | arityInfo info1 == arityInfo info2
    && cafInfo info1 == cafInfo info2
    && oneShotInfo info1 == oneShotInfo info2
    && inlinePragInfo info1 == inlinePragInfo info2
    && occInfo info1 == occInfo info2
    && demandInfo info1 == demandInfo info2
    && callArityInfo info1 == callArityInfo info2
    && levityInfo info1 == levityInfo info2
  = locBind "in unfolding of" bndr1 bndr2 $
    diffUnfold env (unfoldingInfo info1) (unfoldingInfo info2)
  | otherwise
  = locBind "in Id info of" bndr1 bndr2
    [fsep [pprBndr LetBind bndr1, text "/=", pprBndr LetBind bndr2]]
  where info1 = idInfo bndr1; info2 = idInfo bndr2

-- | Find differences in unfoldings. Note that we will not check for
-- differences of @IdInfo@ in unfoldings, as this is generally
-- redundant, and can lead to an exponential blow-up in complexity.
diffUnfold :: RnEnv2 -> Unfolding -> Unfolding -> [SDoc]
diffUnfold _   NoUnfolding    NoUnfolding                 = []
diffUnfold _   BootUnfolding  BootUnfolding               = []
diffUnfold _   (OtherCon cs1) (OtherCon cs2) | cs1 == cs2 = []
diffUnfold env (DFunUnfolding bs1 c1 a1)
               (DFunUnfolding bs2 c2 a2)
  | c1 == c2 && equalLength bs1 bs2
  = concatMap (uncurry (diffExpr False env')) (zip a1 a2)
  where env' = rnBndrs2 env bs1 bs2
diffUnfold env (CoreUnfolding t1 _ _ v1 cl1 wf1 x1 g1)
               (CoreUnfolding t2 _ _ v2 cl2 wf2 x2 g2)
  | v1 == v2 && cl1 == cl2
    && wf1 == wf2 && x1 == x2 && g1 == g2
  = diffExpr False env t1 t2
diffUnfold _   uf1 uf2
  = [fsep [ppr uf1, text "/=", ppr uf2]]

-- | Add location information to diff messages
locBind :: String -> Var -> Var -> [SDoc] -> [SDoc]
locBind loc b1 b2 diffs = map addLoc diffs
  where addLoc d            = d $$ nest 2 (parens (text loc <+> bindLoc))
        bindLoc | b1 == b2  = ppr b1
                | otherwise = ppr b1 <> char '/' <> ppr b2

{-
************************************************************************
*                                                                      *
                Eta reduction
*                                                                      *
************************************************************************

Note [Eta reduction conditions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We try for eta reduction here, but *only* if we get all the way to an
trivial expression.  We don't want to remove extra lambdas unless we
are going to avoid allocating this thing altogether.

There are some particularly delicate points here:

* We want to eta-reduce if doing so leaves a trivial expression,
  *including* a cast.  For example
       \x. f |> co  -->  f |> co
  (provided co doesn't mention x)

* Eta reduction is not valid in general:
        \x. bot  /=  bot
  This matters, partly for old-fashioned correctness reasons but,
  worse, getting it wrong can yield a seg fault. Consider
        f = \x.f x
        h y = case (case y of { True -> f `seq` True; False -> False }) of
                True -> ...; False -> ...

  If we (unsoundly) eta-reduce f to get f=f, the strictness analyser
  says f=bottom, and replaces the (f `seq` True) with just
  (f `cast` unsafe-co).  BUT, as thing stand, 'f' got arity 1, and it
  *keeps* arity 1 (perhaps also wrongly).  So CorePrep eta-expands
  the definition again, so that it does not termninate after all.
  Result: seg-fault because the boolean case actually gets a function value.
  See Trac #1947.

  So it's important to do the right thing.

* Note [Arity care]: we need to be careful if we just look at f's
  arity. Currently (Dec07), f's arity is visible in its own RHS (see
  Note [Arity robustness] in SimplEnv) so we must *not* trust the
  arity when checking that 'f' is a value.  Otherwise we will
  eta-reduce
      f = \x. f x
  to
      f = f
  Which might change a terminating program (think (f `seq` e)) to a
  non-terminating one.  So we check for being a loop breaker first.

  However for GlobalIds we can look at the arity; and for primops we
  must, since they have no unfolding.

* Regardless of whether 'f' is a value, we always want to
  reduce (/\a -> f a) to f
  This came up in a RULE: foldr (build (/\a -> g a))
  did not match           foldr (build (/\b -> ...something complex...))
  The type checker can insert these eta-expanded versions,
  with both type and dictionary lambdas; hence the slightly
  ad-hoc isDictId

* Never *reduce* arity. For example
      f = \xy. g x y
  Then if h has arity 1 we don't want to eta-reduce because then
  f's arity would decrease, and that is bad

These delicacies are why we don't use exprIsTrivial and exprIsHNF here.
Alas.

Note [Eta reduction with casted arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
    (\(x:t3). f (x |> g)) :: t3 -> t2
  where
    f :: t1 -> t2
    g :: t3 ~ t1
This should be eta-reduced to

    f |> (sym g -> t2)

So we need to accumulate a coercion, pushing it inward (past
variable arguments only) thus:
   f (x |> co_arg) |> co  -->  (f |> (sym co_arg -> co)) x
   f (x:t)         |> co  -->  (f |> (t -> co)) x
   f @ a           |> co  -->  (f |> (forall a.co)) @ a
   f @ (g:t1~t2)   |> co  -->  (f |> (t1~t2 => co)) @ (g:t1~t2)
These are the equations for ok_arg.

It's true that we could also hope to eta reduce these:
    (\xy. (f x |> g) y)
    (\xy. (f x y) |> g)
But the simplifier pushes those casts outwards, so we don't
need to address that here.
-}

tryEtaReduce :: [Var] -> CoreExpr -> Maybe CoreExpr
tryEtaReduce bndrs body
  = go (reverse bndrs) body (mkRepReflCo (exprType body))
  where
    incoming_arity = count isId bndrs

    go :: [Var]            -- Binders, innermost first, types [a3,a2,a1]
       -> CoreExpr         -- Of type tr
       -> Coercion         -- Of type tr ~ ts
       -> Maybe CoreExpr   -- Of type a1 -> a2 -> a3 -> ts
    -- See Note [Eta reduction with casted arguments]
    -- for why we have an accumulating coercion
    go [] fun co
      | ok_fun fun
      , let used_vars = exprFreeVars fun `unionVarSet` tyCoVarsOfCo co
      , not (any (`elemVarSet` used_vars) bndrs)
      = Just (mkCast fun co)   -- Check for any of the binders free in the result
                               -- including the accumulated coercion

    go bs (Tick t e) co
      | tickishFloatable t
      = fmap (Tick t) $ go bs e co
      -- Float app ticks: \x -> Tick t (e x) ==> Tick t e

    go (b : bs) (App fun arg) co
      | Just (co', ticks) <- ok_arg b arg co
      = fmap (flip (foldr mkTick) ticks) $ go bs fun co'
            -- Float arg ticks: \x -> e (Tick t x) ==> Tick t e

    go _ _ _  = Nothing         -- Failure!

    ---------------
    -- Note [Eta reduction conditions]
    ok_fun (App fun (Type {})) = ok_fun fun
    ok_fun (Cast fun _)        = ok_fun fun
    ok_fun (Tick _ expr)       = ok_fun expr
    ok_fun (Var fun_id)        = ok_fun_id fun_id || all ok_lam bndrs
    ok_fun _fun                = False

    ---------------
    ok_fun_id fun = fun_arity fun >= incoming_arity

    ---------------
    fun_arity fun             -- See Note [Arity care]
       | isLocalId fun
       , isStrongLoopBreaker (idOccInfo fun) = 0
       | arity > 0                           = arity
       | isEvaldUnfolding (idUnfolding fun)  = 1
            -- See Note [Eta reduction of an eval'd function]
       | otherwise                           = 0
       where
         arity = idArity fun

    ---------------
    ok_lam v = isTyVar v || isEvVar v

    ---------------
    ok_arg :: Var              -- Of type bndr_t
           -> CoreExpr         -- Of type arg_t
           -> Coercion         -- Of kind (t1~t2)
           -> Maybe (Coercion  -- Of type (arg_t -> t1 ~  bndr_t -> t2)
                               --   (and similarly for tyvars, coercion args)
                    , [Tickish Var])
    -- See Note [Eta reduction with casted arguments]
    ok_arg bndr (Type ty) co
       | Just tv <- getTyVar_maybe ty
       , bndr == tv  = Just (mkHomoForAllCos [tv] co, [])
    ok_arg bndr (Var v) co
       | bndr == v   = let reflCo = mkRepReflCo (idType bndr)
                       in Just (mkFunCo Representational reflCo co, [])
    ok_arg bndr (Cast e co_arg) co
       | (ticks, Var v) <- stripTicksTop tickishFloatable e
       , bndr == v
       = Just (mkFunCo Representational (mkSymCo co_arg) co, ticks)
       -- The simplifier combines multiple casts into one,
       -- so we can have a simple-minded pattern match here
    ok_arg bndr (Tick t arg) co
       | tickishFloatable t, Just (co', ticks) <- ok_arg bndr arg co
       = Just (co', t:ticks)

    ok_arg _ _ _ = Nothing

{-
Note [Eta reduction of an eval'd function]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In Haskell it is not true that    f = \x. f x
because f might be bottom, and 'seq' can distinguish them.

But it *is* true that   f = f `seq` \x. f x
and we'd like to simplify the latter to the former.  This amounts
to the rule that
  * when there is just *one* value argument,
  * f is not bottom
we can eta-reduce    \x. f x  ===>  f

This turned up in Trac #7542.


************************************************************************
*                                                                      *
\subsection{Determining non-updatable right-hand-sides}
*                                                                      *
************************************************************************

Top-level constructor applications can usually be allocated
statically, but they can't if the constructor, or any of the
arguments, come from another DLL (because we can't refer to static
labels in other DLLs).

If this happens we simply make the RHS into an updatable thunk,
and 'execute' it rather than allocating it statically.
-}

-- | This function is called only on *top-level* right-hand sides.
-- Returns @True@ if the RHS can be allocated statically in the output,
-- with no thunks involved at all.
rhsIsStatic :: Platform
            -> (Name -> Bool)         -- Which names are dynamic
            -> (Integer -> CoreExpr)  -- Desugaring for integer literals (disgusting)
                                      -- C.f. Note [Disgusting computation of CafRefs]
                                      --      in TidyPgm
            -> CoreExpr -> Bool
-- It's called (i) in TidyPgm.hasCafRefs to decide if the rhs is, or
-- refers to, CAFs; (ii) in CoreToStg to decide whether to put an
-- update flag on it and (iii) in DsExpr to decide how to expand
-- list literals
--
-- The basic idea is that rhsIsStatic returns True only if the RHS is
--      (a) a value lambda
--      (b) a saturated constructor application with static args
--
-- BUT watch out for
--  (i) Any cross-DLL references kill static-ness completely
--      because they must be 'executed' not statically allocated
--      ("DLL" here really only refers to Windows DLLs, on other platforms,
--      this is not necessary)
--
-- (ii) We treat partial applications as redexes, because in fact we
--      make a thunk for them that runs and builds a PAP
--      at run-time.  The only applications that are treated as
--      static are *saturated* applications of constructors.

-- We used to try to be clever with nested structures like this:
--              ys = (:) w ((:) w [])
-- on the grounds that CorePrep will flatten ANF-ise it later.
-- But supporting this special case made the function much more
-- complicated, because the special case only applies if there are no
-- enclosing type lambdas:
--              ys = /\ a -> Foo (Baz ([] a))
-- Here the nested (Baz []) won't float out to top level in CorePrep.
--
-- But in fact, even without -O, nested structures at top level are
-- flattened by the simplifier, so we don't need to be super-clever here.
--
-- Examples
--
--      f = \x::Int. x+7        TRUE
--      p = (True,False)        TRUE
--
--      d = (fst p, False)      FALSE because there's a redex inside
--                              (this particular one doesn't happen but...)
--
--      h = D# (1.0## /## 2.0##)        FALSE (redex again)
--      n = /\a. Nil a                  TRUE
--
--      t = /\a. (:) (case w a of ...) (Nil a)  FALSE (redex)
--
--
-- This is a bit like CoreUtils.exprIsHNF, with the following differences:
--    a) scc "foo" (\x -> ...) is updatable (so we catch the right SCC)
--
--    b) (C x xs), where C is a constructor is updatable if the application is
--         dynamic
--
--    c) don't look through unfolding of f in (f x).

rhsIsStatic platform is_dynamic_name cvt_integer rhs = is_static False rhs
  where
  is_static :: Bool     -- True <=> in a constructor argument; must be atomic
            -> CoreExpr -> Bool

  is_static False  (Lam b e)              = isRuntimeVar b || is_static False e
  is_static in_arg (Tick n e)             = not (tickishIsCode n)
                                              && is_static in_arg e
  is_static in_arg (Cast e _)             = is_static in_arg e
  is_static _      (Coercion {})          = True   -- Behaves just like a literal
  is_static in_arg (Lit (LitInteger i _)) = is_static in_arg (cvt_integer i)
  is_static _      (Lit (MachLabel {}))   = False
  is_static _      (Lit _)                = True
        -- A MachLabel (foreign import "&foo") in an argument
        -- prevents a constructor application from being static.  The
        -- reason is that it might give rise to unresolvable symbols
        -- in the object file: under Linux, references to "weak"
        -- symbols from the data segment give rise to "unresolvable
        -- relocation" errors at link time This might be due to a bug
        -- in the linker, but we'll work around it here anyway.
        -- SDM 24/2/2004

  is_static in_arg other_expr = go other_expr 0
   where
    go (Var f) n_val_args
        | (platformOS platform /= OSMinGW32) ||
          not (is_dynamic_name (idName f))
        =  saturated_data_con f n_val_args
        || (in_arg && n_val_args == 0)
                -- A naked un-applied variable is *not* deemed a static RHS
                -- E.g.         f = g
                -- Reason: better to update so that the indirection gets shorted
                --         out, and the true value will be seen
                -- NB: if you change this, you'll break the invariant that THUNK_STATICs
                --     are always updatable.  If you do so, make sure that non-updatable
                --     ones have enough space for their static link field!

    go (App f a) n_val_args
        | isTypeArg a                    = go f n_val_args
        | not in_arg && is_static True a = go f (n_val_args + 1)
        -- The (not in_arg) checks that we aren't in a constructor argument;
        -- if we are, we don't allow (value) applications of any sort
        --
        -- NB. In case you wonder, args are sometimes not atomic.  eg.
        --   x = D# (1.0## /## 2.0##)
        -- can't float because /## can fail.

    go (Tick n f) n_val_args = not (tickishIsCode n) && go f n_val_args
    go (Cast e _) n_val_args = go e n_val_args
    go _          _          = False

    saturated_data_con f n_val_args
        = case isDataConWorkId_maybe f of
            Just dc -> n_val_args == dataConRepArity dc
            Nothing -> False

{-
************************************************************************
*                                                                      *
\subsection{Type utilities}
*                                                                      *
************************************************************************
-}

-- | True if the type has no non-bottom elements, e.g. when it is an empty
-- datatype, or a GADT with non-satisfiable type parameters, e.g. Int :~: Bool.
-- See Note [Bottoming expressions]
--
-- See Note [No alternatives lint check] for another use of this function.
isEmptyTy :: Type -> Bool
isEmptyTy ty
    -- Data types where, given the particular type parameters, no data
    -- constructor matches, are empty.
    -- This includes data types with no constructors, e.g. Data.Void.Void.
    | Just (tc, inst_tys) <- splitTyConApp_maybe ty
    , Just dcs <- tyConDataCons_maybe tc
    , all (dataConCannotMatch inst_tys) dcs
    = True
    | otherwise
    = False

{-
*****************************************************
*
* StaticPtr
*
*****************************************************
-}

-- | @collectMakeStaticArgs (makeStatic t srcLoc e)@ yields
-- @Just (makeStatic, t, srcLoc, e)@.
--
-- Returns @Nothing@ for every other expression.
collectMakeStaticArgs
  :: CoreExpr -> Maybe (CoreExpr, Type, CoreExpr, CoreExpr)
collectMakeStaticArgs e
    | (fun@(Var b), [Type t, loc, arg], _) <- collectArgsTicks (const True) e
    , idName b == makeStaticName = Just (fun, t, loc, arg)
collectMakeStaticArgs _          = Nothing

{-
************************************************************************
*                                                                      *
\subsection{Join points}
*                                                                      *
************************************************************************
-}

-- | Does this binding bind a join point (or a recursive group of join points)?
isJoinBind :: CoreBind -> Bool
isJoinBind (NonRec b _)       = isJoinId b
isJoinBind (Rec ((b, _) : _)) = isJoinId b
isJoinBind _                  = False
