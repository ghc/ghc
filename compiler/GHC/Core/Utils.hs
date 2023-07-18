{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


Utility functions on @Core@ syntax
-}

-- | Commonly useful utilities for manipulating the Core language
module GHC.Core.Utils (
        -- * Constructing expressions
        mkCast, mkCastMCo, mkPiMCo,
        mkTick, mkTicks, mkTickNoHNF, tickHNFArgs,
        bindNonRec, needsCaseBinding, needsCaseBindingL,
        mkAltExpr, mkDefaultCase, mkSingleAltCase,

        -- * Taking expressions apart
        findDefault, addDefault, findAlt, isDefaultAlt,
        mergeAlts, mergeCaseAlts, trimConArgs,
        filterAlts, combineIdenticalAlts, refineDefaultAlt,
        scaleAltsBy,

        -- * Properties of expressions
        exprType, coreAltType, coreAltsType,
        mkLamType, mkLamTypes,
        mkFunctionType,
        exprIsTrivial, getIdFromTrivialExpr, getIdFromTrivialExpr_maybe,
        trivial_expr_fold,
        exprIsDupable, exprIsCheap, exprIsExpandable, exprIsCheapX, CheapAppFun,
        exprIsHNF, exprOkForSpeculation, exprOkToDiscard, exprOkForSpecEval,
        exprIsWorkFree, exprIsConLike,
        isCheapApp, isExpandableApp, isSaturatedConApp,
        exprIsTickedString, exprIsTickedString_maybe,
        exprIsTopLevelBindable,
        altsAreExhaustive, etaExpansionTick,

        -- * Equality
        cheapEqExpr, cheapEqExpr', diffBinds,

        -- * Manipulating data constructors and types
        exprToType,
        applyTypeToArgs,
        dataConRepInstPat, dataConRepFSInstPat,
        isEmptyTy, normSplitTyConApp_maybe,

        -- * Working with ticks
        stripTicksTop, stripTicksTopE, stripTicksTopT,
        stripTicksE, stripTicksT,

        -- * InScopeSet things which work over CoreBinds
        mkInScopeSetBndrs, extendInScopeSetBind, extendInScopeSetBndrs,

        -- * StaticPtr
        collectMakeStaticArgs,

        -- * Join points
        isJoinBind,

        -- * Tag inference
        mkStrictFieldSeqs, shouldStrictifyIdForCbv, shouldUseCbvForId,

        -- * unsafeEqualityProof
        isUnsafeEqualityCase,

        -- * Dumping stuff
        dumpIdInfoOfProgram
    ) where

import GHC.Prelude
import GHC.Platform

import GHC.Core
import GHC.Core.Ppr
import GHC.Core.FVs( bindFreeVars )
import GHC.Core.DataCon
import GHC.Core.Type as Type
import GHC.Core.Predicate( isCoVarType )
import GHC.Core.FamInstEnv
import GHC.Core.TyCo.Compare( eqType, eqTypeX )
import GHC.Core.Coercion
import GHC.Core.Reduction
import GHC.Core.TyCon
import GHC.Core.Multiplicity

import GHC.Builtin.Names ( makeStaticName, unsafeEqualityProofIdKey, unsafeReflDataConKey )
import GHC.Builtin.PrimOps

import GHC.Types.Var
import GHC.Types.SrcLoc
import GHC.Types.Var.Env
import GHC.Types.Var.Set
import GHC.Types.Name
import GHC.Types.Literal
import GHC.Types.Tickish
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.Basic( Arity )
import GHC.Types.Unique
import GHC.Types.Unique.Set
import GHC.Types.Demand
import GHC.Types.RepType (isZeroBitTy)

import GHC.Data.FastString
import GHC.Data.Maybe
import GHC.Data.List.SetOps( minusList )
import GHC.Data.OrdList

import GHC.Utils.Constants (debugIsOn)
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Misc

import Data.ByteString     ( ByteString )
import Data.Function       ( on )
import Data.List           ( sort, sortBy, partition, zipWith4, mapAccumL )
import qualified Data.List as Partial ( init, last )
import Data.Ord            ( comparing )
import Control.Monad       ( guard )
import qualified Data.Set as Set

{-
************************************************************************
*                                                                      *
\subsection{Find the type of a Core atom/expression}
*                                                                      *
************************************************************************
-}

exprType :: HasDebugCallStack => CoreExpr -> Type
-- ^ Recover the type of a well-typed Core expression. Fails when
-- applied to the actual 'GHC.Core.Type' expression as it cannot
-- really be said to have a type
exprType (Var var)           = idType var
exprType (Lit lit)           = literalType lit
exprType (Coercion co)       = coercionType co
exprType (Let bind body)
  | NonRec tv rhs <- bind    -- See Note [Type bindings]
  , Type ty <- rhs           = substTyWithUnchecked [tv] [ty] (exprType body)
  | otherwise                = exprType body
exprType (Case _ _ ty _)     = ty
exprType (Cast _ co)         = coercionRKind co
exprType (Tick _ e)          = exprType e
exprType (Lam binder expr)   = mkLamType binder (exprType expr)
exprType e@(App _ _)
  = case collectArgs e of
        (fun, args) -> applyTypeToArgs (exprType fun) args
exprType (Type ty) = pprPanic "exprType" (ppr ty)

coreAltType :: CoreAlt -> Type
-- ^ Returns the type of the alternatives right hand side
coreAltType alt@(Alt _ bs rhs)
  = case occCheckExpand bs rhs_ty of
      -- Note [Existential variables and silly type synonyms]
      Just ty -> ty
      Nothing -> pprPanic "coreAltType" (pprCoreAlt alt $$ ppr rhs_ty)
  where
    rhs_ty = exprType rhs

coreAltsType :: [CoreAlt] -> Type
-- ^ Returns the type of the first alternative, which should be the same as for all alternatives
coreAltsType (alt:_) = coreAltType alt
coreAltsType []      = panic "coreAltsType"

mkLamType  :: HasDebugCallStack => Var -> Type -> Type
-- ^ Makes a @(->)@ type or an implicit forall type, depending
-- on whether it is given a type variable or a term variable.
-- This is used, for example, when producing the type of a lambda.
--
mkLamTypes :: [Var] -> Type -> Type
-- ^ 'mkLamType' for multiple type or value arguments

mkLamType v body_ty
   | isTyVar v
   = mkForAllTy (Bndr v coreTyLamForAllTyFlag) body_ty
     -- coreTyLamForAllTyFlag: see Note [Required foralls in Core]
     --                        in GHC.Core.TyCo.Rep

   | isCoVar v
   , v `elemVarSet` tyCoVarsOfType body_ty
     -- See Note [Unused coercion variable in ForAllTy] in GHC.Core.TyCo.Rep
   = mkForAllTy (Bndr v coreTyLamForAllTyFlag) body_ty

   | otherwise
   = mkFunctionType (idMult v) (idType v) body_ty

mkLamTypes vs ty = foldr mkLamType ty vs

{-
Note [Type bindings]
~~~~~~~~~~~~~~~~~~~~
Core does allow type bindings, although such bindings are
not much used, except in the output of the desugarer.
Example:
     let a = Int in (\x:a. x)
Given this, exprType must be careful to substitute 'a' in the
result type (#8522).

Note [Existential variables and silly type synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
        data T = forall a. T (Funny a)
        type Funny a = Bool
        f :: T -> Bool
        f (T x) = x

Now, the type of 'x' is (Funny a), where 'a' is existentially quantified.
That means that 'exprType' and 'coreAltsType' may give a result that *appears*
to mention an out-of-scope type variable.  See #3409 for a more real-world
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

applyTypeToArgs :: HasDebugCallStack => Type -> [CoreExpr] -> Type
-- ^ Determines the type resulting from applying an expression with given type
--- to given argument expressions.
applyTypeToArgs op_ty args
  = go op_ty args
  where
    go op_ty []                   = op_ty
    go op_ty (Type ty : args)     = go_ty_args op_ty [ty] args
    go op_ty (Coercion co : args) = go_ty_args op_ty [mkCoercionTy co] args
    go op_ty (_ : args)           | Just (_, _, _, res_ty) <- splitFunTy_maybe op_ty
                                  = go res_ty args
    go _ args = pprPanic "applyTypeToArgs" (panic_msg args)

    -- go_ty_args: accumulate type arguments so we can
    -- instantiate all at once with piResultTys
    go_ty_args op_ty rev_tys (Type ty : args)
       = go_ty_args op_ty (ty:rev_tys) args
    go_ty_args op_ty rev_tys (Coercion co : args)
       = go_ty_args op_ty (mkCoercionTy co : rev_tys) args
    go_ty_args op_ty rev_tys args
       = go (piResultTys op_ty (reverse rev_tys)) args

    panic_msg as = vcat [ text "Type:" <+> ppr op_ty
                        , text "Args:" <+> ppr args
                        , text "Args':" <+> ppr as ]

mkCastMCo :: CoreExpr -> MCoercionR -> CoreExpr
mkCastMCo e MRefl    = e
mkCastMCo e (MCo co) = Cast e co
  -- We are careful to use (MCo co) only when co is not reflexive
  -- Hence (Cast e co) rather than (mkCast e co)

mkPiMCo :: Var -> MCoercionR -> MCoercionR
mkPiMCo _  MRefl   = MRefl
mkPiMCo v (MCo co) = MCo (mkPiCo Representational v co)


{- *********************************************************************
*                                                                      *
             Casts
*                                                                      *
********************************************************************* -}

-- | Wrap the given expression in the coercion safely, dropping
-- identity coercions and coalescing nested coercions
mkCast :: HasDebugCallStack => CoreExpr -> CoercionR -> CoreExpr

mkCast expr co
  = assertPpr (coercionRole co == Representational)
              (text "coercion" <+> ppr co <+> text "passed to mkCast"
               <+> ppr expr <+> text "has wrong role" <+> ppr (coercionRole co)) $
    warnPprTrace (not (coercionLKind co `eqType` exprType expr))
          "Trying to coerce" (text "(" <> ppr expr
          $$ text "::" <+> ppr (exprType expr) <> text ")"
          $$ ppr co $$ ppr (coercionType co)
          $$ callStackDoc) $
    case expr of
      Cast expr co2 -> mkCast expr (mkTransCo co2 co)
      Tick t expr   -> Tick t (mkCast expr co)

      Coercion e_co | isCoVarType (coercionRKind co)
         -- The guard here checks that g has a (~#) on both sides,
         -- otherwise decomposeCo fails.  Can in principle happen
         -- with unsafeCoerce
                      -> Coercion (mkCoCast e_co co)

      _ | isReflCo co -> expr
        | otherwise   -> Cast expr co


{- *********************************************************************
*                                                                      *
             Attaching ticks
*                                                                      *
********************************************************************* -}

-- | Wraps the given expression in the source annotation, dropping the
-- annotation if possible.
mkTick :: CoreTickish -> CoreExpr -> CoreExpr
mkTick t orig_expr = mkTick' id id orig_expr
 where
  -- Some ticks (cost-centres) can be split in two, with the
  -- non-counting part having laxer placement properties.
  canSplit = tickishCanSplit t && tickishPlace (mkNoCount t) /= tickishPlace t
  -- mkTick' handles floating of ticks *into* the expression.
  -- In this function, `top` is applied after adding the tick, and `rest` before.
  -- This will result in applications that look like (top $ Tick t $ rest expr).
  -- If we want to push the tick deeper, we pre-compose `top` with a function
  -- adding the tick.
  mkTick' :: (CoreExpr -> CoreExpr) -- apply after adding tick (float through)
          -> (CoreExpr -> CoreExpr) -- apply before adding tick (float with)
          -> CoreExpr               -- current expression
          -> CoreExpr
  mkTick' top rest expr = case expr of
    -- Float ticks into unsafe coerce the same way we would do with a cast.
    Case scrut bndr ty alts@[Alt ac abs _rhs]
      | Just rhs <- isUnsafeEqualityCase scrut bndr alts
      -> top $ mkTick' (\e -> Case scrut bndr ty [Alt ac abs e]) rest rhs

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

mkTicks :: [CoreTickish] -> CoreExpr -> CoreExpr
mkTicks ticks expr = foldr mkTick expr ticks

isSaturatedConApp :: CoreExpr -> Bool
isSaturatedConApp e = go e []
  where go (App f a) as = go f (a:as)
        go (Var fun) args
           = isConLikeId fun && idArity fun == valArgCount args
        go (Cast f _) as = go f as
        go _ _ = False

mkTickNoHNF :: CoreTickish -> CoreExpr -> CoreExpr
mkTickNoHNF t e
  | exprIsHNF e = tickHNFArgs t e
  | otherwise   = mkTick t e

-- push a tick into the arguments of a HNF (call or constructor app)
tickHNFArgs :: CoreTickish -> CoreExpr -> CoreExpr
tickHNFArgs t e = push t e
 where
  push t (App f (Type u)) = App (push t f) (Type u)
  push t (App f arg) = App (push t f) (mkTick t arg)
  push _t e = e

-- | Strip ticks satisfying a predicate from top of an expression
stripTicksTop :: (CoreTickish -> Bool) -> Expr b -> ([CoreTickish], Expr b)
stripTicksTop p = go []
  where go ts (Tick t e) | p t = go (t:ts) e
        go ts other            = (reverse ts, other)

-- | Strip ticks satisfying a predicate from top of an expression,
-- returning the remaining expression
stripTicksTopE :: (CoreTickish -> Bool) -> Expr b -> Expr b
stripTicksTopE p = go
  where go (Tick t e) | p t = go e
        go other            = other

-- | Strip ticks satisfying a predicate from top of an expression,
-- returning the ticks
stripTicksTopT :: (CoreTickish -> Bool) -> Expr b -> [CoreTickish]
stripTicksTopT p = go []
  where go ts (Tick t e) | p t = go (t:ts) e
        go ts _                = ts

-- | Completely strip ticks satisfying a predicate from an
-- expression. Note this is O(n) in the size of the expression!
stripTicksE :: (CoreTickish -> Bool) -> Expr b -> Expr b
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
        go_a (Alt c bs e)   = Alt c bs (go e)

stripTicksT :: (CoreTickish -> Bool) -> Expr b -> [CoreTickish]
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
        go_a (Alt _ _ e)    = go e

{-
************************************************************************
*                                                                      *
\subsection{Other expression construction}
*                                                                      *
************************************************************************
-}

bindNonRec :: HasDebugCallStack => Id -> CoreExpr -> CoreExpr -> CoreExpr
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
-- also 'GHC.Core.Make.mkCoreLet'
bindNonRec bndr rhs body
  | isTyVar bndr                       = let_bind
  | isCoVar bndr                       = if isCoArg rhs then let_bind
    {- See Note [Binding coercions] -}                  else case_bind
  | isJoinId bndr                      = let_bind
  | needsCaseBinding (idType bndr) rhs = case_bind
  | otherwise                          = let_bind
  where
    case_bind = mkDefaultCase rhs bndr body
    let_bind  = Let (NonRec bndr rhs) body

-- | `needsCaseBinding` tests whether we have to use a @case@ rather than @let@
-- binding for this expression as per the invariants of 'CoreExpr': see
-- "GHC.Core#let_can_float_invariant"
-- (needsCaseBinding ty rhs) requires that `ty` has a well-defined levity, else
-- `typeLevity ty` will fail; but that should be the case because
-- `needsCaseBinding` is only called once typechecking is complete
needsCaseBinding :: HasDebugCallStack => Type -> CoreExpr -> Bool
needsCaseBinding ty rhs = needsCaseBindingL (typeLevity ty) rhs

needsCaseBindingL :: Levity -> CoreExpr -> Bool
-- True <=> make a case expression instead of a let
-- These can arise either from the desugarer,
-- or from beta reductions: (\x.e) (x +# y)
needsCaseBindingL Lifted   _rhs = False
needsCaseBindingL Unlifted rhs = not (exprOkForSpeculation rhs)

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

mkDefaultCase :: CoreExpr -> Id -> CoreExpr -> CoreExpr
-- Make (case x of y { DEFAULT -> e }
mkDefaultCase scrut case_bndr body
  = Case scrut case_bndr (exprType body) [Alt DEFAULT [] body]

mkSingleAltCase :: CoreExpr -> Id -> AltCon -> [Var] -> CoreExpr -> CoreExpr
-- Use this function if possible, when building a case,
-- because it ensures that the type on the Case itself
-- doesn't mention variables bound by the case
-- See Note [Care with the type of a case expression]
mkSingleAltCase scrut case_bndr con bndrs body
  = Case scrut case_bndr case_ty [Alt con bndrs body]
  where
    body_ty = exprType body

    case_ty -- See Note [Care with the type of a case expression]
      | Just body_ty' <- occCheckExpand bndrs body_ty
      = body_ty'

      | otherwise
      = pprPanic "mkSingleAltCase" (ppr scrut $$ ppr bndrs $$ ppr body_ty)

{- Note [Care with the type of a case expression]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider a phantom type synonym
   type S a = Int
and we want to form the case expression
   case x of K (a::*) -> (e :: S a)

We must not make the type field of the case-expression (S a) because
'a' isn't in scope.  Hence the call to occCheckExpand.  This caused
issue #17056.

NB: this situation can only arise with type synonyms, which can
falsely "mention" type variables that aren't "really there", and which
can be eliminated by expanding the synonym.

Note [Binding coercions]
~~~~~~~~~~~~~~~~~~~~~~~~
Consider binding a CoVar, c = e.  Then, we must satisfy
Note [Core type and coercion invariant] in GHC.Core,
which allows only (Coercion co) on the RHS.

************************************************************************
*                                                                      *
               Operations over case alternatives
*                                                                      *
************************************************************************

The default alternative must be first, if it exists at all.
This makes it easy to find, though it makes matching marginally harder.
-}

-- | Extract the default case alternative
findDefault :: [Alt b] -> ([Alt b], Maybe (Expr b))
findDefault (Alt DEFAULT args rhs : alts) = assert (null args) (alts, Just rhs)
findDefault alts                          =                    (alts, Nothing)

addDefault :: [Alt b] -> Maybe (Expr b) -> [Alt b]
addDefault alts Nothing    = alts
addDefault alts (Just rhs) = Alt DEFAULT [] rhs : alts

isDefaultAlt :: Alt b -> Bool
isDefaultAlt (Alt DEFAULT _ _) = True
isDefaultAlt _                 = False

-- | Find the case alternative corresponding to a particular
-- constructor: panics if no such constructor exists
findAlt :: AltCon -> [Alt b] -> Maybe (Alt b)
    -- A "Nothing" result *is* legitimate
    -- See Note [Unreachable code]
findAlt con alts
  = case alts of
        (deflt@(Alt DEFAULT _ _):alts) -> go alts (Just deflt)
        _                              -> go alts Nothing
  where
    go []                     deflt = deflt
    go (alt@(Alt con1 _ _) : alts) deflt
      = case con `cmpAltCon` con1 of
          LT -> deflt   -- Missed it already; the alts are in increasing order
          EQ -> Just alt
          GT -> assert (not (con1 == DEFAULT)) $ go alts deflt

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
gets in the way; cf #3118.)  Then the full-laziness pass might produce
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
filters down the matching alternatives in GHC.Core.Opt.Simplify.rebuildCase.
-}

---------------------------------
mergeCaseAlts :: Id -> [CoreAlt] -> Maybe ([CoreBind], [CoreAlt])
-- See Note [Merge Nested Cases]
mergeCaseAlts outer_bndr (Alt DEFAULT _ deflt_rhs : outer_alts)
  | Just (joins, inner_alts) <- go deflt_rhs
  = Just (joins, mergeAlts outer_alts inner_alts)
                -- NB: mergeAlts gives priority to the left
                --      case x of
                --        A -> e1
                --        DEFAULT -> case x of
                --                      A -> e2
                --                      B -> e3
                -- When we merge, we must ensure that e1 takes
                -- precedence over e2 as the value for A!
  where
    go :: CoreExpr -> Maybe ([CoreBind], [CoreAlt])

    -- Whizzo: we can merge!
    go (Case (Var inner_scrut_var) inner_bndr _ inner_alts)
       | inner_scrut_var == outer_bndr
       , not (inner_bndr == outer_bndr)   -- Avoid shadowing
       , let wrap_let rhs' = Let (NonRec inner_bndr (Var outer_bndr)) rhs'
                -- inner_bndr is never dead!  It's the scrutinee!
                -- The let is OK even for unboxed binders
                -- See Note [Merge Nested Cases] wrinkle (MC2)
             do_one (Alt con bndrs rhs)
               | any (== outer_bndr) bndrs = Nothing
               | otherwise                 = Just (Alt con bndrs (wrap_let rhs))
       = do { alts' <- mapM do_one inner_alts
            ; return ([], alts') }

    -- Deal with tagToEnum# See Note [Merge Nested Cases] wrinkle (MC3)
    go (App (App (Var f) (Type type_arg)) (Var v))
      | v == outer_bndr
      , Just TagToEnumOp <- isPrimOpId_maybe f
      , Just tc  <- tyConAppTyCon_maybe type_arg
      , Just (dc1:dcs) <- tyConDataCons_maybe tc   -- At least one data constructor
      , dcs `lengthAtMost` 3  -- Arbitrary
      = return ( [], mk_alts dc1 dcs)
      where
        mk_lit dc = mkLitIntUnchecked $ toInteger $ dataConTagZ dc
        mk_rhs dc = Var (dataConWorkId dc)
        mk_alts dc1 dcs =  Alt DEFAULT              [] (mk_rhs dc1)
                        : [Alt (LitAlt (mk_lit dc)) [] (mk_rhs dc) | dc <- dcs]

    -- Float out let/join bindings
    -- See Note [Merge Nested Cases] wrinkle (MC4)
    go (Let bind body)
      | null outer_alts || isJoinBind bind
      = do { (joins, alts) <- go body

             -- Check for capture; but only if we could otherwise do a merge
           ; let capture = outer_bndr `elem` bindersOf bind
                           || outer_bndr `elemVarSet` bindFreeVars bind
           ; guard (not capture)

           ; return (bind:joins, alts ) }
      | otherwise
      = Nothing

    -- We don't want ticks to get in the way; just push them inwards.
    -- (This happens when you add SourceTicks e.g. GHC.Num.Integer.integerLt#)
    go (Tick t body)
      = do { (joins, alts) <- go body
           ; return (joins, [Alt con bs (Tick t rhs) | Alt con bs rhs <- alts]) }

    go _ = Nothing

mergeCaseAlts _ _ = Nothing

---------------------------------
mergeAlts :: [Alt a] -> [Alt a] -> [Alt a]
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

trimConArgs DEFAULT      args = assert (null args) []
trimConArgs (LitAlt _)   args = assert (null args) []
trimConArgs (DataAlt dc) args = dropList (dataConUnivTyVars dc) args

filterAlts :: TyCon                -- ^ Type constructor of scrutinee's type (used to prune possibilities)
           -> [Type]               -- ^ And its type arguments
           -> [AltCon]             -- ^ 'imposs_cons': constructors known to be impossible due to the form of the scrutinee
           -> [Alt b] -- ^ Alternatives
           -> ([AltCon], [Alt b])
             -- Returns:
             --  1. Constructors that will never be encountered by the
             --     *default* case (if any).  A superset of imposs_cons
             --  2. The new alternatives, trimmed by
             --        a) remove imposs_cons
             --        b) remove constructors which can't match because of GADTs
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
  = imposs_deflt_cons `seqList`
      (imposs_deflt_cons, addDefault trimmed_alts maybe_deflt)
  -- Very important to force `imposs_deflt_cons` as that forces `alt_cons`, which
  -- is essentially as retaining `alts_wo_default` or any `Alt b` for that matter
  -- leads to a huge space leak (see #22102 and !8896)
  where
    (alts_wo_default, maybe_deflt) = findDefault alts
    alt_cons = [con | Alt con _ _ <- alts_wo_default]

    trimmed_alts = filterOut (impossible_alt inst_tys) alts_wo_default

    imposs_cons_set = Set.fromList imposs_cons
    imposs_deflt_cons =
      imposs_cons ++ filterOut (`Set.member` imposs_cons_set) alt_cons
         -- "imposs_deflt_cons" are handled
         --   EITHER by the context,
         --   OR by a non-DEFAULT branch in this case expression.

    impossible_alt :: [Type] -> Alt b -> Bool
    impossible_alt _ (Alt con _ _) | con `Set.member` imposs_cons_set = True
    impossible_alt inst_tys (Alt (DataAlt con) _ _) = dataConCannotMatch inst_tys con
    impossible_alt _  _                             = False

-- | Refine the default alternative to a 'DataAlt', if there is a unique way to do so.
-- See Note [Refine DEFAULT case alternatives]
refineDefaultAlt :: [Unique]          -- ^ Uniques for constructing new binders
                 -> Mult              -- ^ Multiplicity annotation of the case expression
                 -> TyCon             -- ^ Type constructor of scrutinee's type
                 -> [Type]            -- ^ Type arguments of scrutinee's type
                 -> [AltCon]          -- ^ Constructors that cannot match the DEFAULT (if any)
                 -> [CoreAlt]
                 -> (Bool, [CoreAlt]) -- ^ 'True', if a default alt was replaced with a 'DataAlt'
refineDefaultAlt us mult tycon tys imposs_deflt_cons all_alts
  | Alt DEFAULT _ rhs : rest_alts <- all_alts
  , isAlgTyCon tycon            -- It's a data type, tuple, or unboxed tuples.
  , not (isNewTyCon tycon)      -- Exception 1 in Note [Refine DEFAULT case alternatives]
  , not (isTypeDataTyCon tycon) -- Exception 2 in Note [Refine DEFAULT case alternatives]
  , Just all_cons <- tyConDataCons_maybe tycon
  , let imposs_data_cons = mkUniqSet [con | DataAlt con <- imposs_deflt_cons]
                             -- We now know it's a data type, so we can use
                             -- UniqSet rather than Set (more efficient)
        impossible con   = con `elementOfUniqSet` imposs_data_cons
                             || dataConCannotMatch tys con
  = case filterOut impossible all_cons of
       -- Eliminate the default alternative
       -- altogether if it can't match:
       []    -> (False, rest_alts)

       -- It matches exactly one constructor, so fill it in:
       [con] -> (True, mergeAlts rest_alts [Alt (DataAlt con) (ex_tvs ++ arg_ids) rhs])
                       -- We need the mergeAlts to keep the alternatives in the right order
             where
                (ex_tvs, arg_ids) = dataConRepInstPat us mult con tys

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

{- Note [Merge Nested Cases]
~~~~~~~~~~~~~~~~~~~~~~~~~
       case e of b {             ==>   case e of b {
         p1 -> rhs1                      p1 -> rhs1
         ...                             ...
         pm -> rhsm                      pm -> rhsm
         _  -> case b of b' {            pn -> let b'=b in rhsn
                     pn -> rhsn          ...
                     ...                 po -> let b'=b in rhso
                     po -> rhso          _  -> let b'=b in rhsd
                     _  -> rhsd
       }

which merges two cases in one case when -- the default alternative of
the outer case scrutinises the same variable as the outer case. This
transformation is called Case Merging.  It avoids that the same
variable is scrutinised multiple times.

Wrinkles

(MC1) Historical note. I tried making `mergeCaseAlts` "looks though" an inner
     single-alternative case-on-variable. For example
       case x of {
          ...outer-alts...
          DEFAULT -> case y of (a,b) ->
                     case x of { A -> rhs1; B -> rhs2 }
    ===>
       case x of
         ...outer-alts...
         a -> case y of (a,b) -> rhs1
         B -> case y of (a,b) -> rhs2

    This duplicates the `case y` but it removes the case x; so it is a win
    in terms of execution time (combining the cases on x) at the cost of
    perhaps duplicating the `case y`.  A case in point is integerEq, which
    is defined thus
        integerEq :: Integer -> Integer -> Bool
        integerEq !x !y = isTrue# (integerEq# x y)
    which becomes
        integerEq
          = \ (x :: Integer) (y_aAL :: Integer) ->
              case x of x1 { __DEFAULT ->
              case y of y1 { __DEFAULT ->
              case x1 of {
                IS x2 -> case y1 of {
                           __DEFAULT -> GHC.Types.False;
                           IS y2     -> tagToEnum# @Bool (==# x2 y2) };
                IP x2 -> ...
                IN x2 -> ...
    We want to merge the outer `case x` with the inner `case x1`.

    But (a) this is all a bit dubious: see #24251, and
        (b) it is hard to combine with (MC4)
    So I'm not doing this any more.  If we want to do it, we'll handle it
    separately: #24251.

    End of historical note

(MC2) The auxiliary bindings b'=b are annoying, because they force another
      simplifier pass, but there seems no easy way to avoid them.  See
      Note [Which transformations are innocuous] in GHC.Core.Opt.Stats.

(MC3) Consider
         case f x of (r::Int#) -> tagToEnum# r :: Bool
      `mergeCaseAlts` as a special case to treat this as if it was
         case f x of r ->
           case r of { 0# -> False; 1# -> True }
      which can be merged to
         case f x of { 0# -> False; 1# -> True }

      To see why this is important, return to
         case f x of (r::Int#) -> tagToEnum# r :: Bool
      and supppose `f` inlines to a case expression.  Then then we get
         let $j r = tagToEnum# r
         case .. of { .. jump $j 0#; ...jump $j 1# ... }
      Now if the entire expression is consumed by another case-expression,
      that outer case will only see (tagToEnum# r) which it can't do much
      with.  Whereas the result of the above case-merge generates much better
      code: no branching on Int#

(MC4) Consider
          case f x of r ->
            join $j y = <rhs> in
            case r of { ...alts ... }
      This is pretty common, and it a pity for it to defeat the case-merge
      transformation; and it makes the optimiser fragile to inlining decisions
      for join points.

      So `mergeCaseAlts` floats out any join points. It doesn't float out
      non-join-points unless the /outer/ case has just one alternative; doing
      so would risk more allocation

(MC5) See Note [Cascading case merge]

See also Note [Example of case-merging and caseRules] in GHC.Core.Opt.Simplify.Utils


Note [Cascading case merge]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Case merging should cascade in one sweep, because the Simplifier tries it /after/
simplifying (and hence case-merging) the inner case.  For example

      case e of a {
        DEFAULT -> case a of b
                      DEFAULT -> case b of c {
                                     DEFAULT -> e
                                     A -> ea
                      B -> eb
        C -> ec
==> {simplify inner case}
      case e of a {
        DEFAULT -> case a of b
                      DEFAULT -> let c = b in e
                      A -> let c = b in ea
                      B -> eb
        C -> ec
==> {case-merge on outer case}
      case e of a {
        DEFAULT -> let b = a in let c = b in e
        A -> let b = a in let c = b in ea
        B -> let b = a in eb
        C -> ec


However here's a tricky case that we still don't catch, and I don't
see how to catch it in one pass:

  case x of c1 { I# a1 ->
  case a1 of c2 ->
    0 -> ...
    DEFAULT -> case x of c3 { I# a2 ->
               case a2 of ...

After occurrence analysis (and its binder-swap) we get this

  case x of c1 { I# a1 ->
  let x = c1 in         -- Binder-swap addition
  case a1 of c2 ->
    0 -> ...
    DEFAULT -> case x of c3 { I# a2 ->
               case a2 of ...

When we simplify the inner case x, we'll see that
x=c1=I# a1.  So we'll bind a2 to a1, and get

  case x of c1 { I# a1 ->
  case a1 of c2 ->
    0 -> ...
    DEFAULT -> case a1 of ...

This is correct, but we can't do a case merge in this sweep
because c2 /= a1.  Reason: the binding c1=I# a1 went inwards
without getting changed to c1=I# c2.

I don't think this is worth fixing, even if I knew how. It'll
all come out in the next pass anyway.


Note [Refine DEFAULT case alternatives]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
refineDefaultAlt replaces the DEFAULT alt with a constructor if there
is one possible value it could be.

The simplest example being
    foo :: () -> ()
    foo x = case x of !_ -> ()
which rewrites to
    foo :: () -> ()
    foo x = case x of () -> ()

There are two reasons in general why replacing a DEFAULT alternative
with a specific constructor is desirable.

1. We can simplify inner expressions.  For example

       data Foo = Foo1 ()

       test :: Foo -> ()
       test x = case x of
                  DEFAULT -> mid (case x of
                                    Foo1 x1 -> x1)

   refineDefaultAlt fills in the DEFAULT here with `Foo ip1` and then
   x becomes bound to `Foo ip1` so is inlined into the other case
   which causes the KnownBranch optimisation to kick in. If we don't
   refine DEFAULT to `Foo ip1`, we are left with both case expressions.

2. combineIdenticalAlts does a better job. For example (Simon Jacobi)
       data D = C0 | C1 | C2

       case e of
         DEFAULT -> e0
         C0      -> e1
         C1      -> e1

   When we apply combineIdenticalAlts to this expression, it can't
   combine the alts for C0 and C1, as we already have a default case.
   But if we apply refineDefaultAlt first, we get
       case e of
         C0 -> e1
         C1 -> e1
         C2 -> e0
   and combineIdenticalAlts can turn that into
       case e of
         DEFAULT -> e1
         C2 -> e0

   It isn't obvious that refineDefaultAlt does this but if you look
   at its one call site in GHC.Core.Opt.Simplify.Utils then the
   `imposs_deflt_cons` argument is populated with constructors which
   are matched elsewhere.

There are two exceptions where we avoid refining a DEFAULT case:

* Exception 1: Newtypes

  We can have a newtype, if we are just doing an eval:

    case x of { DEFAULT -> e }

  And we don't want to fill in a default for them!

* Exception 2: `type data` declarations

  The data constructors for a `type data` declaration (see
  Note [Type data declarations] in GHC.Rename.Module) do not exist at the
  value level. Nevertheless, it is possible to strictly evaluate a value
  whose type is a `type data` declaration. Test case
  type-data/should_compile/T2294b.hs contains an example:

    type data T a where
      A :: T Int

    f :: T a -> ()
    f !x = ()

  We want to generate the following Core for f:

    f = \(@a) (x :: T a) ->
         case x of
           __DEFAULT -> ()

  Namely, we do _not_ want to match on `A`, as it doesn't exist at the value
  level! See wrinkle (W2b) in Note [Type data declarations] in GHC.Rename.Module


Note [Combine identical alternatives]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

Note [Combine identical alternatives: wrinkles]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* It's important that we try to combine alternatives *before*
  simplifying them, rather than after. Reason: because
  Simplify.simplAlt may zap the occurrence info on the binders in the
  alternatives, which in turn defeats combineIdenticalAlts use of
  isDeadBinder (see #7360).

  You can see this in the call to combineIdenticalAlts in
  GHC.Core.Opt.Simplify.Utils.prepareAlts.  Here the alternatives have type InAlt
  (the "In" meaning input) rather than OutAlt.

* combineIdenticalAlts does not work well for nullary constructors
      case x of y
         []    -> f []
         (_:_) -> f y
  Here we won't see that [] and y are the same.  Sigh! This problem
  is solved in CSE, in GHC.Core.Opt.CSE.combineAlts, which does a better version
  of combineIdenticalAlts. But sadly it doesn't have the occurrence info we have
  here.
  See Note [Combine case alts: awkward corner] in GHC.Core.Opt.CSE).

Note [Care with impossible-constructors when combining alternatives]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have (#10538)
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
known-con alternatives that we have eliminated. (In #11172 we
missed the first one.)

-}

combineIdenticalAlts :: [AltCon]    -- Constructors that cannot match DEFAULT
                     -> [CoreAlt]
                     -> (Bool,      -- True <=> something happened
                         [AltCon],  -- New constructors that cannot match DEFAULT
                         [CoreAlt]) -- New alternatives
-- See Note [Combine identical alternatives]
-- True <=> we did some combining, result is a single DEFAULT alternative
combineIdenticalAlts imposs_deflt_cons (Alt con1 bndrs1 rhs1 : rest_alts)
  | all isDeadBinder bndrs1    -- Remember the default
  , not (null elim_rest) -- alternative comes first
  = (True, imposs_deflt_cons', deflt_alt : filtered_rest)
  where
    (elim_rest, filtered_rest) = partition identical_to_alt1 rest_alts
    deflt_alt = Alt DEFAULT [] (mkTicks (concat tickss) rhs1)

     -- See Note [Care with impossible-constructors when combining alternatives]
    imposs_deflt_cons' = imposs_deflt_cons `minusList` elim_cons
    elim_cons = elim_con1 ++ map (\(Alt con _ _) -> con) elim_rest
    elim_con1 = case con1 of     -- Don't forget con1!
                  DEFAULT -> []
                  _       -> [con1]

    cheapEqTicked e1 e2 = cheapEqExpr' tickishFloatable e1 e2
    identical_to_alt1 (Alt _con bndrs rhs)
      = all isDeadBinder bndrs && rhs `cheapEqTicked` rhs1
    tickss = map (\(Alt _ _ rhs) -> stripTicksT tickishFloatable rhs) elim_rest

combineIdenticalAlts imposs_cons alts
  = (False, imposs_cons, alts)

-- Scales the multiplicity of the binders of a list of case alternatives. That
-- is, in [C x1…xn -> u], the multiplicity of x1…xn is scaled.
scaleAltsBy :: Mult -> [CoreAlt] -> [CoreAlt]
scaleAltsBy w alts = map scaleAlt alts
  where
    scaleAlt :: CoreAlt -> CoreAlt
    scaleAlt (Alt con bndrs rhs) = Alt con (map scaleBndr bndrs) rhs

    scaleBndr :: CoreBndr -> CoreBndr
    scaleBndr b = scaleVarBy w b


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
turn into just "x" in mkTick. At least if `x` is not a function.

Note [Empty case is trivial]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The expression (case (x::Int) Bool of {}) is just a type-changing
case used when we are sure that 'x' will not return.  See
Note [Empty case alternatives] in GHC.Core.

If the scrutinee is trivial, then so is the whole expression; and the
CoreToSTG pass in fact drops the case expression leaving only the
scrutinee.

Having more trivial expressions is good.  Moreover, if we don't treat
it as trivial we may land up with let-bindings like
   let v = case x of {} in ...
and after CoreToSTG that gives
   let v = x in ...
and that confuses the code generator (#11155). So best to kill
it off at source.
-}

{-# INLINE trivial_expr_fold #-}
trivial_expr_fold :: (Id -> r) -> (Literal -> r) -> r -> r -> CoreExpr -> r
-- ^ The worker function for Note [exprIsTrivial] and Note [getIdFromTrivialExpr]
-- This is meant to have the code of both functions in one place and make it
-- easy to derive custom predicates.
--
-- (trivial_expr_fold k_id k_triv k_not_triv e)
-- * returns (k_id x) if `e` is a variable `x` (with trivial wrapping)
-- * returns (k_lit x) if `e` is a trivial literal `l` (with trivial wrapping)
-- * returns k_triv if `e` is a literal, type, or coercion (with trivial wrapping)
-- * returns k_not_triv otherwise
--
-- where "trivial wrapping" is
-- * Type application or abstraction
-- * Ticks other than `tickishIsCode`
-- * `case e of {}` an empty case
trivial_expr_fold k_id k_lit k_triv k_not_triv = go
  where
    -- If you change this function, be sure to change SetLevels.notWorthFloating
    -- as well!
    -- (Or yet better: Come up with a way to share code with this function.)
    go (Var v)                            = k_id v  -- See Note [Variables are trivial]
    go (Lit l)    | litIsTrivial l        = k_lit l
    go (Type _)                           = k_triv
    go (Coercion _)                       = k_triv
    go (App f t)  | not (isRuntimeArg t)  = go f
    go (Lam b e)  | not (isRuntimeVar b)  = go e
    go (Tick t e) | not (tickishIsCode t) = go e              -- See Note [Tick trivial]
    go (Cast e _)                         = go e
    go (Case e b _ as)
      | null as
      = go e     -- See Note [Empty case is trivial]
      | Just rhs <- isUnsafeEqualityCase e b as
      = go rhs   -- See (U2) of Note [Implementing unsafeCoerce] in base:Unsafe.Coerce
    go _                                  = k_not_triv

exprIsTrivial :: CoreExpr -> Bool
exprIsTrivial e = trivial_expr_fold (const True) (const True) True False e

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

getIdFromTrivialExpr :: HasDebugCallStack => CoreExpr -> Id
-- See Note [getIdFromTrivialExpr]
getIdFromTrivialExpr e = trivial_expr_fold id (const panic) panic panic e
  where
    panic = pprPanic "getIdFromTrivialExpr" (ppr e)

getIdFromTrivialExpr_maybe :: CoreExpr -> Maybe Id
getIdFromTrivialExpr_maybe e = trivial_expr_fold Just (const Nothing) Nothing Nothing e

{- *********************************************************************
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

exprIsDupable :: Platform -> CoreExpr -> Bool
exprIsDupable platform e
  = isJust (go dupAppSize e)
  where
    go :: Int -> CoreExpr -> Maybe Int
    go n (Type {})     = Just n
    go n (Coercion {}) = Just n
    go n (Var {})      = decrement n
    go n (Tick _ e)    = go n e
    go n (Cast e _)    = go n e
    go n (App f a) | Just n' <- go n a = go n' f
    go n (Lit lit) | litIsDupable platform lit = decrement n
    go _ _ = Nothing

    decrement :: Int -> Maybe Int
    decrement 0 = Nothing
    decrement n = Just (n-1)

dupAppSize :: Int
dupAppSize = 8   -- Size of term we are prepared to duplicate
                 -- This is *just* big enough to make test MethSharing
                 -- inline enough join points.  Really it should be
                 -- smaller, and could be if we fixed #4960.

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
duplicate a primop (#5623):
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

Note [exprIsCheap]
~~~~~~~~~~~~~~~~~~
See also Note [Interaction of exprIsWorkFree and lone variables] in GHC.Core.Unfold

@exprIsCheap@ looks at a Core expression and returns \tr{True} if
it is obviously in weak head normal form, or is cheap to get to WHNF.
Note that that's not the same as exprIsDupable; an expression might be
big, and hence not dupable, but still cheap.

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

-------------------------------------
type CheapAppFun = Id -> Arity -> Bool
  -- Is an application of this function to n *value* args
  -- always cheap, assuming the arguments are cheap?
  -- True mainly of data constructors, partial applications;
  -- but with minor variations:
  --    isWorkFreeApp
  --    isCheapApp
  --    isExpandableApp

exprIsCheapX :: CheapAppFun -> Bool -> CoreExpr -> Bool
{-# INLINE exprIsCheapX #-}
-- allow specialization of exprIsCheap, exprIsWorkFree and exprIsExpandable
-- instead of having an unknown call to ok_app
-- expandable=True <=> Treat Case and Let as cheap, if their sub-expressions are.
--                     This flag is set for exprIsExpandable
exprIsCheapX ok_app expandable e
  = ok e
  where
    ok e = go 0 e

    -- n is the number of value arguments
    go n (Var v)                      = ok_app v n
    go _ (Lit {})                     = True
    go _ (Type {})                    = True
    go _ (Coercion {})                = True
    go n (Cast e _)                   = go n e
    go n (Case scrut _ _ alts)        = not expandable && ok scrut &&
                                        and [ go n rhs | Alt _ _ rhs <- alts ]
    go n (Tick t e) | tickishCounts t = False
                    | otherwise       = go n e
    go n (Lam x e)  | isRuntimeVar x  = n==0 || go (n-1) e
                    | otherwise       = go n e
    go n (App f e)  | isRuntimeArg e  = go (n+1) f && ok e
                    | otherwise       = go n f
    go n (Let (NonRec _ r) e)         = not expandable && go n e && ok r
    go n (Let (Rec prs) e)            = not expandable && go n e && all (ok . snd) prs

      -- Case: see Note [Case expressions are work-free]
      -- App, Let: see Note [Arguments and let-bindings exprIsCheapX]

--------------------
exprIsWorkFree :: CoreExpr -> Bool
-- See Note [exprIsWorkFree]
exprIsWorkFree e = exprIsCheapX isWorkFreeApp False e

--------------------
exprIsCheap :: CoreExpr -> Bool
-- See Note [exprIsCheap]
exprIsCheap e = exprIsCheapX isCheapApp False e

--------------------
exprIsExpandable :: CoreExpr -> Bool
-- See Note [exprIsExpandable]
exprIsExpandable e = exprIsCheapX isExpandableApp True e

isWorkFreeApp :: CheapAppFun
isWorkFreeApp fn n_val_args
  | n_val_args == 0           -- No value args
  = True
  | n_val_args < idArity fn   -- Partial application
  = True
  | otherwise
  = case idDetails fn of
      DataConWorkId {} -> True
      PrimOpId op _    -> primOpIsWorkFree op
      _                -> False

isCheapApp :: CheapAppFun
isCheapApp fn n_val_args
  | isWorkFreeApp fn n_val_args = True
  | isDeadEndId fn              = True  -- See Note [isCheapApp: bottoming functions]
  | otherwise
  = case idDetails fn of
      -- DataConWorkId {} -> _  -- Handled by isWorkFreeApp
      RecSelId {}      -> n_val_args == 1  -- See Note [Record selection]
      ClassOpId {}     -> n_val_args == 1
      PrimOpId op _    -> primOpIsCheap op
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
      -- DataConWorkId {} -> _  -- Handled by isWorkFreeApp
      RecSelId {}  -> n_val_args == 1  -- See Note [Record selection]
      ClassOpId {} -> n_val_args == 1
      PrimOpId {}  -> False
      _ | isDeadEndId fn     -> False
          -- See Note [isExpandableApp: bottoming functions]
        | isConLikeId fn     -> True
        | all_args_are_preds -> True
        | otherwise          -> False

  where
     -- See if all the arguments are PredTys (implicit params or classes)
     -- If so we'll regard it as expandable; see Note [Expandable overloadings]
     all_args_are_preds = all_pred_args n_val_args (idType fn)

     all_pred_args n_val_args ty
       | n_val_args == 0
       = True

       | Just (bndr, ty) <- splitPiTy_maybe ty
       = case bndr of
           Named {}  -> all_pred_args n_val_args ty
           Anon _ af -> isInvisibleFunArg af && all_pred_args (n_val_args-1) ty

       | otherwise
       = False

{- Note [isCheapApp: bottoming functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
I'm not sure why we have a special case for bottoming
functions in isCheapApp.  Maybe we don't need it.

Note [exprIsExpandable]
~~~~~~~~~~~~~~~~~~~~~~~
An expression is "expandable" if we are willing to duplicate it, if doing
so might make a RULE or case-of-constructor fire.  Consider
   let x = (a,b)
       y = build g
   in ....(case x of (p,q) -> rhs)....(foldr k z y)....

We don't inline 'x' or 'y' (see Note [Lone variables] in GHC.Core.Unfold),
but we do want

 * the case-expression to simplify
   (via exprIsConApp_maybe, exprIsLiteral_maybe)

 * the foldr/build RULE to fire
   (by expanding the unfolding during rule matching)

So we classify the unfolding of a let-binding as "expandable" (via the
uf_expandable field) if we want to do this kind of on-the-fly
expansion.  Specifically:

* True of constructor applications (K a b)

* True of applications of a "CONLIKE" Id; see Note [CONLIKE pragma] in GHC.Types.Basic.
  (NB: exprIsCheap might not be true of this)

* False of case-expressions.  If we have
    let x = case ... in ...(case x of ...)...
  we won't simplify.  We have to inline x.  See #14688.

* False of let-expressions (same reason); and in any case we
  float lets out of an RHS if doing so will reveal an expandable
  application (see SimplEnv.doFloatFromRhs).

* Take care: exprIsExpandable should /not/ be true of primops.  I
  found this in test T5623a:
    let q = /\a. Ptr a (a +# b)
    in case q @ Float of Ptr v -> ...q...

  q's inlining should not be expandable, else exprIsConApp_maybe will
  say that (q @ Float) expands to (Ptr a (a +# b)), and that will
  duplicate the (a +# b) primop, which we should not do lightly.
  (It's quite hard to trigger this bug, but T13155 does so for GHC 8.0.)

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
They'd expect the rule to fire. But since negate is overloaded, we might
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
-- | To a first approximation, 'exprOkForSpeculation' returns True of
-- an expression that is:
--
--  * Safe to evaluate even if normal order eval might not
--    evaluate the expression at all, and
--
--  * Safe /not/ to evaluate even if normal order would do so
--
-- More specifically, this means that:
--  * A: Evaluation of the expression reaches weak-head-normal-form,
--  * B: soon,
--  * C: without causing a write side effect (e.g. writing a mutable variable).
--
-- In particular, an expression that may
--  * throw a synchronous Haskell exception, or
--  * risk an unchecked runtime exception (e.g. array
--    out of bounds, divide by zero)
-- is /not/ considered OK-for-speculation, as these violate condition A.
--
-- For 'exprOkToDiscard', condition A is weakened to allow expressions
-- that might risk an unchecked runtime exception but must otherwise
-- reach weak-head-normal-form.
-- (Note that 'exprOkForSpeculation' implies 'exprOkToDiscard')
--
-- But in fact both functions are a bit more conservative than the above,
-- in at least the following ways:
--
--  * W1: We do not take advantage of already-evaluated lifted variables.
--        As a result, 'exprIsHNF' DOES NOT imply 'exprOkForSpeculation';
--        if @y@ is a case-binder of lifted type, then @exprIsHNF y@ is
--        'True', while @exprOkForSpeculation y@ is 'False'.
--        See Note [exprOkForSpeculation and evaluated variables] for why.
--  * W2: Read-effects on mutable variables are currently also included.
--        See Note [Classifying primop effects] "GHC.Builtin.PrimOps".
--  * W3: Currently, 'exprOkForSpeculation' always returns 'False' for
--        let-expressions.  Lets can be stacked deeply, so we just give up.
--        In any case, the argument of 'exprOkForSpeculation' is usually in
--        a strict context, so any lets will have been floated away.
--
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
-- We can only do this if the @y# +# 1#@ is ok for speculation: it has no
-- side effects, and can't diverge or raise an exception.
--
--
-- See also Note [Classifying primop effects] in "GHC.Builtin.PrimOps"
-- and Note [Transformations affected by primop effects].
--
-- 'exprOkForSpeculation' is used to define Core's let-can-float
-- invariant.  (See Note [Core let-can-float invariant] in
-- "GHC.Core".)  It is therefore frequently called on arguments of
-- unlifted type, especially via 'needsCaseBinding'.  But it is
-- sometimes called on expressions of lifted type as well.  For
-- example, see Note [Speculative evaluation] in "GHC.CoreToStg.Prep".


exprOkForSpeculation, exprOkToDiscard :: CoreExpr -> Bool
exprOkForSpeculation = expr_ok fun_always_ok primOpOkForSpeculation
exprOkToDiscard      = expr_ok fun_always_ok primOpOkToDiscard

fun_always_ok :: Id -> Bool
fun_always_ok _ = True

-- | A special version of 'exprOkForSpeculation' used during
-- Note [Speculative evaluation]. When the predicate arg `fun_ok` returns False
-- for `b`, then `b` is never considered ok-for-spec.
exprOkForSpecEval :: (Id -> Bool) -> CoreExpr -> Bool
exprOkForSpecEval fun_ok = expr_ok fun_ok primOpOkForSpeculation

expr_ok :: (Id -> Bool) -> (PrimOp -> Bool) -> CoreExpr -> Bool
expr_ok _ _ (Lit _)      = True
expr_ok _ _ (Type _)     = True
expr_ok _ _ (Coercion _) = True

expr_ok fun_ok primop_ok (Var v)    = app_ok fun_ok primop_ok v []
expr_ok fun_ok primop_ok (Cast e _) = expr_ok fun_ok primop_ok e
expr_ok fun_ok primop_ok (Lam b e)
                 | isTyVar b = expr_ok fun_ok primop_ok  e
                 | otherwise = True

-- Tick annotations that *tick* cannot be speculated, because these
-- are meant to identify whether or not (and how often) the particular
-- source expression was evaluated at runtime.
expr_ok fun_ok primop_ok (Tick tickish e)
   | tickishCounts tickish = False
   | otherwise             = expr_ok fun_ok primop_ok e

expr_ok _ _ (Let {}) = False
-- See W3 in the Haddock comment for exprOkForSpeculation

expr_ok fun_ok primop_ok (Case scrut bndr _ alts)
  =  -- See Note [exprOkForSpeculation: case expressions]
     expr_ok fun_ok primop_ok scrut
  && isUnliftedType (idType bndr)
      -- OK to call isUnliftedType: binders always have a fixed RuntimeRep
  && all (\(Alt _ _ rhs) -> expr_ok fun_ok primop_ok rhs) alts
  && altsAreExhaustive alts

expr_ok fun_ok primop_ok other_expr
  | (expr, args) <- collectArgs other_expr
  = case stripTicksTopE (not . tickishCounts) expr of
        Var f ->
           app_ok fun_ok primop_ok f args

        -- 'LitRubbish' is the only literal that can occur in the head of an
        -- application and will not be matched by the above case (Var /= Lit).
        -- See Note [How a rubbish literal can be the head of an application]
        -- in GHC.Types.Literal
        Lit lit | debugIsOn, not (isLitRubbish lit)
                 -> pprPanic "Non-rubbish lit in app head" (ppr lit)
                 | otherwise
                 -> True

        _ -> False

-----------------------------
app_ok :: (Id -> Bool) -> (PrimOp -> Bool) -> Id -> [CoreArg] -> Bool
app_ok fun_ok primop_ok fun args
  | not (fun_ok fun)
  = False -- This code path is only taken for Note [Speculative evaluation]

  | idArity fun > n_val_args
  -- Partial application: just check passing the arguments is OK
  = args_ok

  | otherwise
  = case idDetails fun of
      DFunId new_type -> not new_type
         -- DFuns terminate, unless the dict is implemented
         -- with a newtype in which case they may not

      DataConWorkId dc
        | isLazyDataConRep dc
        -> args_ok
        | otherwise
        -> fields_ok (dataConRepStrictness dc)

      ClassOpId _ is_terminating_result
        | is_terminating_result -- See Note [exprOkForSpeculation and type classes]
        -> assertPpr (n_val_args == 1) (ppr fun $$ ppr args) $
           True
           -- assert: terminating result type => can't be applied;
           -- c.f the _other case below

      PrimOpId op _
        | primOpIsDiv op
        , Lit divisor <- Partial.last args
            -- there can be 2 args (most div primops) or 3 args
            -- (WordQuotRem2Op), hence the use of last/init
        -> not (isZeroLit divisor) && all (expr_ok fun_ok primop_ok) (Partial.init args)
              -- Special case for dividing operations that fail
              -- In general they are NOT ok-for-speculation
              -- (which primop_ok will catch), but they ARE OK
              -- if the divisor is definitely non-zero.
              -- Often there is a literal divisor, and this
              -- can get rid of a thunk in an inner loop

        | otherwise -> primop_ok op && args_ok

      _other  -- Unlifted and terminating types;
              -- Also c.f. the Var case of exprIsHNF
         |  isTerminatingType fun_ty  -- See Note [exprOkForSpeculation and type classes]
         || definitelyUnliftedType fun_ty
         -> assertPpr (n_val_args == 0) (ppr fun $$ ppr args)
            True  -- Both terminating types (e.g. Eq a), and unlifted types (e.g. Int#)
                  -- are non-functions and so will have no value args.  The assert is
                  -- just to check this.
                  -- (If we added unlifted function types this would change,
                  -- and we'd need to actually test n_val_args == 0.)

         -- Functions that terminate fast without raising exceptions etc
         -- See (U12) of Note [Implementing unsafeCoerce]
         | fun `hasKey` unsafeEqualityProofIdKey -> True

         | otherwise -> False
             -- NB: even in the nullary case, do /not/ check
             --     for evaluated-ness of the fun;
             --     see Note [exprOkForSpeculation and evaluated variables]
  where
    fun_ty       = idType fun
    n_val_args   = valArgCount args
    (arg_tys, _) = splitPiTys fun_ty

    -- Even if a function call itself is OK, any unlifted
    -- args are still evaluated eagerly and must be checked
    args_ok = all2Prefix arg_ok arg_tys args
    arg_ok :: PiTyVarBinder -> CoreExpr -> Bool
    arg_ok (Named _) _ = True   -- A type argument
    arg_ok (Anon ty _) arg      -- A term argument
       | definitelyLiftedType (scaledThing ty)
       = True -- lifted args are not evaluated eagerly
       | otherwise
       = expr_ok fun_ok primop_ok arg

    -- Used for strict DataCon worker arguments
    -- See (SFC1) of Note [Strict fields in Core]
    fields_ok str_marks = all3Prefix field_ok arg_tys str_marks args
    field_ok :: PiTyVarBinder -> StrictnessMark -> CoreExpr -> Bool
    field_ok (Named _)   _   _ = True
    field_ok (Anon ty _) str arg
       | NotMarkedStrict <- str                 -- iff it's a lazy field
       , definitelyLiftedType (scaledThing ty)  -- and its type is lifted
       = True                                   -- then the worker app does not eval
       | otherwise
       = expr_ok fun_ok primop_ok arg

-----------------------------
altsAreExhaustive :: [Alt b] -> Bool
-- True  <=> the case alternatives are definitely exhaustive
-- False <=> they may or may not be
altsAreExhaustive []
  = True    -- The scrutinee never returns; see Note [Empty case alternatives] in GHC.Core
altsAreExhaustive (Alt con1 _ _ : alts)
  = case con1 of
      DEFAULT   -> True
      LitAlt {} -> False
      DataAlt c -> alts `lengthIs` (tyConFamilySize (dataConTyCon c) - 1)
      -- It is possible to have an exhaustive case that does not
      -- enumerate all constructors, notably in a GADT match, but
      -- we behave conservatively here -- I don't think it's important
      -- enough to deserve special treatment

-- | Should we look past this tick when eta-expanding the given function?
--
-- See Note [Ticks and mandatory eta expansion]
-- Takes the function we are applying as argument.
etaExpansionTick :: Id -> GenTickish pass -> Bool
etaExpansionTick id t
  = hasNoBinding id &&
    ( tickishFloatable t || isProfTick t )

{- Note [exprOkForSpeculation and type classes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (#22745, #15205)

  \(d :: C a b). case eq_sel (sc_sel d) of
                   (co :: t1 ~# t2) [Dead] ->  blah

We know that
* eq_sel's argument (sc_sel d) has dictionary type, so it definitely terminates
  (again Note [NON-BOTTOM-DICTS invariant] in GHC.Core)
* eq_sel is simply a superclass selector, and hence is fast
* The field that eq_sel picks is of unlifted type, and hence can't be bottom
  (remember the dictionary argument itself is non-bottom)

So we can treat (eq_sel (sc_sel d)) as ok-for-speculation.  We must check

a) That the function is a class-op, with IdDetails of ClassOpId

b) That the result type of the class-op is terminating or unlifted.  E.g. for
     class C a => D a where ...
     class C a where { op :: a -> a }
   Since C is represented by a newtype, (sc_sel (d :: D a)) might
   not be terminating.

Rather than repeatedly test if the result of the class-op is a
terminating/unlifted type, we cache it as a field of ClassOpId. See
GHC.Types.Id.Make.mkDictSelId for where this field is initialised.

Note [exprOkForSpeculation: case expressions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
exprOkForSpeculation accepts very special case expressions.
Reason: (a ==# b) is ok-for-speculation, but the litEq rules
in GHC.Core.Opt.ConstantFold convert it (a ==# 3#) to
   case a of { DEFAULT -> 0#; 3# -> 1# }
for excellent reasons described in
  GHC.Core.Opt.ConstantFold Note [The litEq rule: converting equality to case].
So, annoyingly, we want that case expression to be
ok-for-speculation too. Bother.

But we restrict it sharply:

* We restrict it to unlifted scrutinees. Consider this:
     case x of y {
       DEFAULT -> ... (let v::Int# = case y of { True  -> e1
                                               ; False -> e2 }
                       in ...) ...

  Does the RHS of v satisfy the let-can-float invariant?  Previously we said
  yes, on the grounds that y is evaluated.  But the binder-swap done
  by GHC.Core.Opt.SetLevels would transform the inner alternative to
     DEFAULT -> ... (let v::Int# = case x of { ... }
                     in ...) ....
  which does /not/ satisfy the let-can-float invariant, because x is
  not evaluated. See Note [Binder-swap during float-out]
  in GHC.Core.Opt.SetLevels.  To avoid this awkwardness it seems simpler
  to stick to unlifted scrutinees where the issue does not
  arise.

* We restrict it to exhaustive alternatives. A non-exhaustive
  case manifestly isn't ok-for-speculation. for example,
  this is a valid program (albeit a slightly dodgy one)
    let v = case x of { B -> ...; C -> ... }
    in case x of
         A -> ...
         _ ->  ...v...v....
  Should v be considered ok-for-speculation?  Its scrutinee may be
  evaluated, but the alternatives are incomplete so we should not
  evaluate it strictly.

  Now, all this is for lifted types, but it'd be the same for any
  finite unlifted type. We don't have many of them, but we might
  add unlifted algebraic types in due course.


----- Historical note: #15696: --------
  Previously GHC.Core.Opt.SetLevels used exprOkForSpeculation to guide
  floating of single-alternative cases; it now uses exprIsHNF
  Note [Floating single-alternative cases].

  But in those days, consider
    case e of x { DEAFULT ->
      ...(case x of y
            A -> ...
            _ -> ...(case (case x of { B -> p; C -> p }) of
                       I# r -> blah)...
  If GHC.Core.Opt.SetLevels considers the inner nested case as
  ok-for-speculation it can do case-floating (in GHC.Core.Opt.SetLevels).
  So we'd float to:
    case e of x { DEAFULT ->
    case (case x of { B -> p; C -> p }) of I# r ->
    ...(case x of y
            A -> ...
            _ -> ...blah...)...
  which is utterly bogus (seg fault); see #5453.

----- Historical note: #3717: --------
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
redundant) by recognising that the (case <# ds 5 of { ... }) is
ok-for-speculation, even though it has /lifted/ type.  But now join
points do the job nicely.
------- End of historical note ------------


Note [exprOkForSpeculation and evaluated variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider these examples:
 * case x of y { DEFAULT -> ....y.... }
   Should 'y' (alone) be considered ok-for-speculation?

 * case x of y { DEFAULT -> ....let z = dataToTagLarge# y... }
   Should (dataToTagLarge# y) be considered ok-for-spec? Recall that
     dataToTagLarge# :: forall a. a -> Int#
   must always evaluate its argument. (See also Note [DataToTag overview].)

You could argue 'yes', because in the case alternative we know that
'y' is evaluated.  But the binder-swap transformation, which is
extremely useful for float-out, changes these expressions to
   case x of y { DEFAULT -> ....x.... }
   case x of y { DEFAULT -> ....let z = dataToTagLarge# x... }

And now the expression does not obey the let-can-float invariant!  Yikes!
Moreover we really might float (dataToTagLarge# x) outside the case,
and then it really, really doesn't obey the let-can-float invariant.

The solution is simple: exprOkForSpeculation does not try to take
advantage of the evaluated-ness of (lifted) variables.  And it returns
False (always) for primops that perform evaluation.  We achieve the latter
by marking the relevant primops as "ThrowsException" or
"ReadWriteEffect"; see also Note [Classifying primop effects] in
GHC.Builtin.PrimOps.

Note that exprIsHNF /can/ and does take advantage of evaluated-ness;
it doesn't have the trickiness of the let-can-float invariant to worry about.

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
-- to perform case-to-let for lifted expressions, which changes:
--
-- > case x of x' { _ -> e }
--
--    into:
--
-- > let x' = x in e
--
-- and in so doing makes the binding lazy.
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
-- Suppose @f x@ diverges; then @C (f x)@ is not a value.
-- We check for this using needsCaseBinding below
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
exprIsHNFlike :: HasDebugCallStack => (Var -> Bool) -> (Unfolding -> Bool) -> CoreExpr -> Bool
exprIsHNFlike is_con is_con_unf e
  = -- pprTraceWith "hnf" (\r -> ppr r <+> ppr e) $
    is_hnf_like e
  where
    is_hnf_like (Var v) -- NB: There are no value args at this point
      =  id_app_is_value v [] -- Catches nullary constructors,
                              --      so that [] and () are values, for example
                              -- and (e.g.) primops that don't have unfoldings
      || is_con_unf (idUnfolding v)
        -- Check the thing's unfolding; it might be bound to a value
        --   or to a guaranteed-evaluated variable (isEvaldUnfolding)
        --   Contrast with Note [exprOkForSpeculation and evaluated variables]
        -- We don't look through loop breakers here, which is a bit conservative
        -- but otherwise I worry that if an Id's unfolding is just itself,
        -- we could get an infinite loop

      || definitelyUnliftedType (idType v)
        -- Unlifted binders are always evaluated (#20140)

    is_hnf_like (Lit l)          = not (isLitRubbish l)
        -- Regarding a LitRubbish as ConLike leads to unproductive inlining in
        -- WWRec, see #20035
    is_hnf_like (Type _)         = True       -- Types are honorary Values;
                                              -- we don't mind copying them
    is_hnf_like (Coercion _)     = True       -- Same for coercions
    is_hnf_like (Lam b e)        = isRuntimeVar b || is_hnf_like e
    is_hnf_like (Tick tickish e) = not (tickishCounts tickish)
                                   && is_hnf_like e
                                      -- See Note [exprIsHNF Tick]
    is_hnf_like (Cast e _)       = is_hnf_like e
    is_hnf_like (App e a)
      | isValArg a               = app_is_value e [a]
      | otherwise                = is_hnf_like e
    is_hnf_like (Let _ e)        = is_hnf_like e  -- Lazy let(rec)s don't affect us
    is_hnf_like (Case e b _ as)
      | Just rhs <- isUnsafeEqualityCase e b as
      = is_hnf_like rhs
    is_hnf_like _                = False

    -- Collect arguments through Casts and Ticks and call id_app_is_value
    app_is_value :: CoreExpr -> [CoreArg] -> Bool
    app_is_value (Var f)    as = id_app_is_value f as
    app_is_value (Tick _ f) as = app_is_value f as
    app_is_value (Cast f _) as = app_is_value f as
    app_is_value (App f a)  as | isValArg a = app_is_value f (a:as)
                               | otherwise  = app_is_value f as
    app_is_value _          _  = False

    id_app_is_value id val_args =
      -- See Note [exprIsHNF for function applications]
      --   for the specification and examples
      case compare (idArity id) (length val_args) of
        EQ | is_con id ->      -- Saturated app of a DataCon/CONLIKE Id
          case mb_str_marks id of
            Just str_marks ->  -- with strict fields; see (SFC1) of Note [Strict fields in Core]
              assert (val_args `equalLength` str_marks) $
              fields_hnf str_marks
            Nothing ->         -- without strict fields: like PAP
              args_hnf         -- NB: CONLIKEs are lazy!

        GT ->                  -- PAP: Check unlifted val_args
          args_hnf

        _  -> False

      where
        -- Saturated, Strict DataCon: Check unlifted val_args and strict fields
        fields_hnf str_marks = all3Prefix check_field val_arg_tys str_marks val_args

        -- PAP: Check unlifted val_args
        args_hnf             = all2Prefix check_arg   val_arg_tys           val_args

        fun_ty = idType id
        val_arg_tys = mapMaybe anonPiTyBinderType_maybe (collectPiTyBinders fun_ty)
          -- val_arg_tys = map exprType val_args, but much less costly.
          -- The obvious definition regresses T16577 by 30% so we don't do it.

        check_arg a_ty a
          | mightBeUnliftedType a_ty = is_hnf_like a
          | otherwise                = True
         -- Check unliftedness; for example f (x /# 12#) where f has arity two,
         -- and the first argument is unboxed. This is not a value!
         -- But  f 34#  is a value, so check args for HNFs.
         -- NB: We check arity (and CONLIKEness) first because it's cheaper
         --     and we reject quickly on saturated apps.
        check_field a_ty str a
          | mightBeUnliftedType a_ty = is_hnf_like a
          | isMarkedStrict str       = is_hnf_like a
          | otherwise                = True
          -- isMarkedStrict: Respect Note [Strict fields in Core]

        mb_str_marks id
          | Just dc <- isDataConWorkId_maybe id
          , not (isLazyDataConRep dc)
          = Just (dataConRepStrictness dc)
          | otherwise
          = Nothing

{-# INLINE exprIsHNFlike #-}

{-
Note [exprIsHNF Tick]
~~~~~~~~~~~~~~~~~~~~~
We can discard source annotations on HNFs as long as they aren't
tick-like:

  scc c (\x . e)    =>  \x . e
  scc c (C x1..xn)  =>  C x1..xn

So we regard these as HNFs.  Tick annotations that tick are not
regarded as HNF if the expression they surround is HNF, because the
tick is there to tell us that the expression was evaluated, so we
don't want to discard a seq on it.

Note [exprIsHNF for function applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider an application with an Id head where the argument is a redex:

  f <redex>

Is this expression a value?

The answer depends on the type of `f`, its arity and whether or not it is a
strict data constructor. The decision diagram is as follows:

* If <redex> is unlifted, it is *not* a value (regardless of arity!)
* Otherwise, <redex> is lifted.
  Does its `idArity` (a lower bound on the actual arity)
  exceed the number of actual arguments (= 1)?
  * If so, it is a PAP and thus a value
  * If not, it is a saturated call.
    Is it a lazy data constructor?   Then it is a value.
    Is it a strict data constructor? Then it is *not* a value. (See also Note [Strict fields in Core].)
    Otherwise, it is a regular, possibly saturated function call, and hence *not* a value.

The code in exprIsHNF is tweaked for efficiency, hence it delays the
unliftedness check after the arity check.

Here are a few examples (enshrined in testcase AppIsHNF) to bring home this
point. Let us say that

  f :: Int# -> Int -> Int -> Int, with idArity 3
  expensive# :: Int -> Int#  -- unlifted result
  expensive  :: Int -> Int   -- lifted result
  data T where
    K1 :: !Int -> Int -> T -- strict field
    K2 :: Int# -> Int -> T -- unlifted field

Now consider

  f (expensive# 1) 2    -- Not HNF
  f 1# (expensive 2)    -- HNF

  K1 1 (expensive 2)   -- HNF
  K1 (expensive 1) 2   -- Not HNF
  K1 (expensive 1)     -- HNF      (!)

  K2 1# (expensive 1)   -- HNF
  K2 (expensive# 1) 2   -- Not HNF
  K2 (expensive# 1)     -- Not HNF (!)

Note that the cases marked (!) exemplify that strict fields are different to
unlifted fields when considering partial applications: Unlifted fields are
evaluated eagerly whereas evaluation of strict fields is delayed until the call
is saturated.
-}

-- | Can we bind this 'CoreExpr' at the top level?
exprIsTopLevelBindable :: CoreExpr -> Type -> Bool
-- See Note [Core top-level string literals]
-- Precondition: exprType expr = ty
-- Top-level literal strings can't even be wrapped in ticks
--   see Note [Core top-level string literals] in "GHC.Core"
exprIsTopLevelBindable expr ty
  = not (mightBeUnliftedType ty)
    -- Note that 'expr' may not have a fixed runtime representation here,
    -- consequently we must use 'mightBeUnliftedType' rather than 'isUnliftedType',
    -- as the latter would panic.
  || exprIsTickedString expr
  || isBoxedType ty && exprIsNestedTrivialConApp expr

-- | Check if the expression is zero or more Ticks wrapped around a literal
-- string.
exprIsTickedString :: CoreExpr -> Bool
exprIsTickedString = isJust . exprIsTickedString_maybe

-- | Check if the expression is a constructor worker application to arguments
-- which are either trivial or themselves constructor worker applications, etc.
exprIsNestedTrivialConApp :: CoreExpr -> Bool
exprIsNestedTrivialConApp x
  | (Var v, xs) <- collectArgs x
  , Just dc <- isDataConWorkId_maybe v
  = and (zipWith f (map isBanged (dataConImplBangs dc)) xs)
  where
    f bang x
      | not bang
      , exprIsTrivial x
      = True
      | (Var v, xs) <- collectArgs x
      , Just dc <- isDataConWorkId_maybe v
      = and (zipWith f (map isBanged (dataConImplBangs dc)) xs)
      | otherwise
      = False
exprIsNestedTrivialConApp _ = False

-- | Extract a literal string from an expression that is zero or more Ticks
-- wrapped around a literal string. Returns Nothing if the expression has a
-- different shape.
-- Used to "look through" Ticks in places that need to handle literal strings.
exprIsTickedString_maybe :: CoreExpr -> Maybe ByteString
exprIsTickedString_maybe (Lit (LitString bs)) = Just bs
exprIsTickedString_maybe (Tick t e)
  -- we don't tick literals with CostCentre ticks, compare to mkTick
  | tickishPlace t == PlaceCostCentre = Nothing
  | otherwise = exprIsTickedString_maybe e
exprIsTickedString_maybe _ = Nothing

{-
************************************************************************
*                                                                      *
             Instantiating data constructors
*                                                                      *
************************************************************************

These InstPat functions go here to avoid circularity between DataCon and Id
-}

dataConRepInstPat   ::                 [Unique] -> Mult -> DataCon -> [Type] -> ([TyCoVar], [Id])
dataConRepFSInstPat :: [FastString] -> [Unique] -> Mult -> DataCon -> [Type] -> ([TyCoVar], [Id])

dataConRepInstPat   = dataConInstPat (repeat ((fsLit "ipv")))
dataConRepFSInstPat = dataConInstPat

dataConInstPat :: [FastString]          -- A long enough list of FSs to use for names
               -> [Unique]              -- An equally long list of uniques, at least one for each binder
               -> Mult                  -- The multiplicity annotation of the case expression: scales the multiplicity of variables
               -> DataCon
               -> [Type]                -- Types to instantiate the universally quantified tyvars
               -> ([TyCoVar], [Id])     -- Return instantiated variables
-- dataConInstPat arg_fun fss us mult con inst_tys returns a tuple
-- (ex_tvs, arg_ids),
--
--   ex_tvs are intended to be used as binders for existential type args
--
--   arg_ids are intended to be used as binders for value arguments,
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
dataConInstPat fss uniqs mult con inst_tys
  = assert (univ_tvs `equalLength` inst_tys) $
    (ex_bndrs, arg_ids)
  where
    univ_tvs = dataConUnivTyVars con
    ex_tvs   = dataConExTyCoVars con
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

    mk_ex_var :: Subst -> (TyCoVar, FastString, Unique) -> (Subst, TyCoVar)
    mk_ex_var subst (tv, fs, uniq) = (Type.extendTCvSubstWithClone subst tv
                                       new_tv
                                     , new_tv)
      where
        new_tv | isTyVar tv
               = mkTyVar (mkSysTvName uniq fs) kind
               | otherwise
               = mkCoVar (mkSystemVarName uniq fs) kind
        kind   = Type.substTyUnchecked subst (varType tv)

      -- Make value vars, instantiating types
    arg_ids = zipWith4 mk_id_var id_uniqs id_fss arg_tys arg_strs
    mk_id_var uniq fs (Scaled m ty) str
      = setCaseBndrEvald str $  -- See Note [Mark evaluated arguments]
        mkUserLocalOrCoVar (mkVarOccFS fs) uniq
                           (mult `mkMultMul` m) (Type.substTy full_subst ty) noSrcSpan

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

c.f. add_evals in GHC.Core.Opt.Simplify.simplAlt

************************************************************************
*                                                                      *
         Equality
*                                                                      *
************************************************************************
-}

-- | A cheap equality test which bales out fast!
--      If it returns @True@ the arguments are definitely equal,
--      otherwise, they may or may not be equal.
cheapEqExpr :: Expr b -> Expr b -> Bool
cheapEqExpr = cheapEqExpr' (const False)

-- | Cheap expression equality test, can ignore ticks by type.
cheapEqExpr' :: (CoreTickish -> Bool) -> Expr b -> Expr b -> Bool
{-# INLINE cheapEqExpr' #-}
cheapEqExpr' ignoreTick e1 e2
  = go e1 e2
  where
    go (Var v1)   (Var v2)         = v1 == v2
    go (Lit lit1) (Lit lit2)       = lit1 == lit2
    go (Type t1)  (Type t2)        = t1 `eqType` t2
    go (Coercion c1) (Coercion c2) = c1 `eqCoercion` c2
    go (App f1 a1) (App f2 a2)     = f1 `go` f2 && a1 `go` a2
    go (Cast e1 t1) (Cast e2 t2)   = e1 `go` e2 && t1 `eqCoercion` t2

    go (Tick t1 e1) e2 | ignoreTick t1 = go e1 e2
    go e1 (Tick t2 e2) | ignoreTick t2 = go e1 e2
    go (Tick t1 e1) (Tick t2 e2) = t1 == t2 && e1 `go` e2

    go _ _ = False



-- Used by diffBinds, which is itself only used in GHC.Core.Lint.lintAnnots
eqTickish :: RnEnv2 -> CoreTickish -> CoreTickish -> Bool
eqTickish env (Breakpoint lext lid lids lmod) (Breakpoint rext rid rids rmod)
      = lid == rid &&
        map (rnOccL env) lids == map (rnOccR env) rids &&
        lext == rext &&
        lmod == rmod
eqTickish _ l r = l == r

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
--
-- Only used in GHC.Core.Lint.lintAnnots
diffBinds :: Bool -> RnEnv2 -> [(Var, CoreExpr)] -> [(Var, CoreExpr)]
          -> ([SDoc], RnEnv2)
diffBinds top env binds1 = go (length binds1) env binds1
 where go _    env []     []
          = ([], env)
       go _fuel env [] binds2
          -- No binds remaining to compare on the left? Bail out early.
          = (warn env [] binds2, env)
       go _fuel env binds1 []
          -- No binds remaining to compare on the right? Bail out early.
          = (warn env binds1 [], env)
       go fuel env binds1@(bind1:_) binds2@(_:_)
          -- Iterated over all binds without finding a match? Then
          -- try speculatively matching binders by order.
          | fuel == 0
          = if not $ env `inRnEnvL` fst bind1
            then let env' = uncurry (rnBndrs2 env) $ unzip $
                            zip (sort $ map fst binds1) (sort $ map fst binds2)
                 in go (length binds1) env' binds1 binds2
            -- If we have already tried that, give up
            else (warn env binds1 binds2, env)
       go fuel env ((bndr1,expr1):binds1) binds2
          | let matchExpr (bndr,expr) =
                  (isTyVar bndr || not top || null (diffIdInfo env bndr bndr1)) &&
                  null (diffExpr top (rnBndr2 env bndr1 bndr) expr1 expr)

          , (binds2l, (bndr2,_):binds2r) <- break matchExpr binds2
          = go (length binds1) (rnBndr2 env bndr1 bndr2)
                binds1 (binds2l ++ binds2r)
          | otherwise -- No match, so push back (FIXME O(n^2))
          = go (fuel-1) env (binds1++[(bndr1,expr1)]) binds2

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
         -- Special case for TyVar, which we checked were bound to the same types in
         -- diffExpr, but don't have any IdInfo we would panic if called diffIdInfo.
         -- These let-bound types are created temporarily by the simplifier but inlined
         -- immediately.
         | isTyVar bndr1 && isTyVar bndr2
         = []
         | otherwise
         = diffIdInfo env bndr1 bndr2

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
  | isDeadEndId absent && isDeadEndId absent2 = []
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
    -- See Note [Empty case alternatives] in GHC.Data.TrieMap
  = diffExpr top env e1 e2 ++ concat (zipWith diffAlt a1 a2)
  where env' = rnBndr2 env b1 b2
        diffAlt (Alt c1 bs1 e1) (Alt c2 bs2 e2)
          | c1 /= c2  = [text "alt-cons " <> ppr c1 <> text " /= " <> ppr c2]
          | otherwise = diffExpr top (rnBndrs2 env' bs1 bs2) e1 e2
diffExpr _  _ e1 e2
  = [fsep [ppr e1, text "/=", ppr e2]]

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
  = locBind "in unfolding of" bndr1 bndr2 $
    diffUnfold env (realUnfoldingInfo info1) (realUnfoldingInfo info2)
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
diffUnfold env (CoreUnfolding t1 _ _ c1 g1)
               (CoreUnfolding t2 _ _ c2 g2)
  | c1 == c2 && g1 == g2
  = diffExpr False env t1 t2
diffUnfold _   uf1 uf2
  = [fsep [ppr uf1, text "/=", ppr uf2]]

-- | Add location information to diff messages
locBind :: String -> Var -> Var -> [SDoc] -> [SDoc]
locBind loc b1 b2 diffs = map addLoc diffs
  where addLoc d            = d $$ nest 2 (parens (text loc <+> bindLoc))
        bindLoc | b1 == b2  = ppr b1
                | otherwise = ppr b1 <> char '/' <> ppr b2


{- *********************************************************************
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

-- | If @normSplitTyConApp_maybe _ ty = Just (tc, tys, co)@
-- then @ty |> co = tc tys@. It's 'splitTyConApp_maybe', but looks through
-- coercions via 'topNormaliseType_maybe'. Hence the \"norm\" prefix.
normSplitTyConApp_maybe :: FamInstEnvs -> Type -> Maybe (TyCon, [Type], Coercion)
normSplitTyConApp_maybe fam_envs ty
  | let Reduction co ty1 = topNormaliseType_maybe fam_envs ty
                           `orElse` (mkReflRedn Representational ty)
  , Just (tc, tc_args) <- splitTyConApp_maybe ty1
  = Just (tc, tc_args, co)
normSplitTyConApp_maybe _ _ = Nothing

{-
*****************************************************
*
* InScopeSet things
*
*****************************************************
-}


extendInScopeSetBind :: InScopeSet -> CoreBind -> InScopeSet
extendInScopeSetBind (InScope in_scope) binds
   = InScope $ foldBindersOfBindStrict extendVarSet in_scope binds

extendInScopeSetBndrs :: InScopeSet -> [CoreBind] -> InScopeSet
extendInScopeSetBndrs (InScope in_scope) binds
   = InScope $ foldBindersOfBindsStrict extendVarSet in_scope binds

mkInScopeSetBndrs :: [CoreBind] -> InScopeSet
mkInScopeSetBndrs binds = foldBindersOfBindsStrict extendInScopeSet emptyInScopeSet binds

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

dumpIdInfoOfProgram :: Bool -> (IdInfo -> SDoc) -> CoreProgram -> SDoc
dumpIdInfoOfProgram dump_locals ppr_id_info binds = vcat (map printId ids)
  where
  ids = sortBy (stableNameCmp `on` getName) (concatMap getIds binds)
  getIds (NonRec i _) = [ i ]
  getIds (Rec bs)     = map fst bs
  -- By default only include full info for exported ids, unless we run in the verbose
  -- pprDebug mode.
  printId id | isExportedId id || dump_locals = ppr id <> colon <+> (ppr_id_info (idInfo id))
             | otherwise       = empty

{-
************************************************************************
*                                                                      *
\subsection{Tag inference things}
*                                                                      *
************************************************************************
-}

{- Note [Call-by-value for worker args]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we unbox a constructor with strict fields we want to
preserve the information that some of the arguments came
out of strict fields and therefore should be already properly
tagged, however we can't express this directly in core.

Instead what we do is generate a worker like this:

  data T = MkT A !B

  foo = case T of MkT a b -> $wfoo a b

  $wfoo a b = case b of b' -> rhs[b/b']

This makes the worker strict in b causing us to use a more efficient
calling convention for `b` where the caller needs to ensure `b` is
properly tagged and evaluated before it's passed to $wfoo. See Note [CBV Function Ids].

Usually the argument will be known to be properly tagged at the call site so there is
no additional work for the caller and the worker can be more efficient since it can
assume the presence of a tag.

This is especially true for recursive functions like this:
    -- myPred expect it's argument properly tagged
    myPred !x = ...

    loop :: MyPair -> Int
    loop (MyPair !x !y) =
        case x of
            A -> 1
            B -> 2
            _ -> loop (MyPair (myPred x) (myPred y))

Here we would ordinarily not be strict in y after unboxing.
However if we pass it as a regular argument then this means on
every iteration of loop we will incur an extra seq on y before
we can pass it to `myPred` which isn't great! That is in STG after
tag inference we get:

    Rec {
    Find.$wloop [InlPrag=[2], Occ=LoopBreaker]
      :: Find.MyEnum -> Find.MyEnum -> GHC.Prim.Int#
    [GblId[StrictWorker([!, ~])],
    Arity=2,
    Str=<1L><ML>,
    Unf=OtherCon []] =
        {} \r [x y]
            case x<TagProper> of x' [Occ=Once1] {
              __DEFAULT ->
                  case y of y' [Occ=Once1] {
                  __DEFAULT ->
                  case Find.$wmyPred y' of pred_y [Occ=Once1] {
                  __DEFAULT ->
                  case Find.$wmyPred x' of pred_x [Occ=Once1] {
                  __DEFAULT -> Find.$wloop pred_x pred_y;
                  };
                  };
              Find.A -> 1#;
              Find.B -> 2#;
            };
    end Rec }

Here comes the tricky part: If we make $wloop strict in both x/y and we get:

    Rec {
    Find.$wloop [InlPrag=[2], Occ=LoopBreaker]
      :: Find.MyEnum -> Find.MyEnum -> GHC.Prim.Int#
    [GblId[StrictWorker([!, !])],
    Arity=2,
    Str=<1L><!L>,
    Unf=OtherCon []] =
        {} \r [x y]
            case y<TagProper> of y' [Occ=Once1] { __DEFAULT ->
            case x<TagProper> of x' [Occ=Once1] {
              __DEFAULT ->
                  case Find.$wmyPred y' of pred_y [Occ=Once1] {
                  __DEFAULT ->
                  case Find.$wmyPred x' of pred_x [Occ=Once1] {
                  __DEFAULT -> Find.$wloop pred_x pred_y;
                  };
                  };
              Find.A -> 1#;
              Find.B -> 2#;
            };
    end Rec }

Here both x and y are known to be tagged in the function body since we pass strict worker args using unlifted cbv.
This means the seqs on x and y both become no-ops and compared to the first version the seq on `y` disappears at runtime.

The downside is that the caller of $wfoo potentially has to evaluate `y` once if we can't prove it isn't already evaluated.
But y coming out of a strict field is in WHNF so safe to evaluated. And most of the time it will be properly tagged+evaluated
already at the call site because of the EPT Invariant! See Note [EPT enforcement] for more in this.
This makes GHC itself around 1% faster despite doing slightly more work! So this is generally quite good.

We only apply this when we think there is a benefit in doing so however. There are a number of cases in which
it would be useless to insert an extra seq. ShouldStrictifyIdForCbv tries to identify these to avoid churn in the
simplifier. See Note [Which Ids should be strictified] for details on this.
-}
mkStrictFieldSeqs :: [(Id,StrictnessMark)] -> CoreExpr -> (CoreExpr)
mkStrictFieldSeqs args rhs =
  foldr addEval rhs args
    where
      case_ty = exprType rhs
      addEval :: (Id,StrictnessMark) -> (CoreExpr) -> (CoreExpr)
      addEval (arg_id,arg_cbv) (rhs)
        -- Argument representing strict field.
        | isMarkedStrict arg_cbv
        , shouldStrictifyIdForCbv arg_id
        -- Make sure to remove unfoldings here to avoid the simplifier dropping those for OtherCon[] unfoldings.
        = Case (Var $! zapIdUnfolding arg_id) arg_id case_ty ([Alt DEFAULT [] rhs])
        -- Normal argument
        | otherwise = do
          rhs

{- Note [Which Ids should be strictified]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For some arguments we would like to convince GHC to pass them call by value.
One way to achieve this is described in see Note [Call-by-value for worker args].

We separate the concerns of "should we pass this argument using cbv" and
"should we do so by making the rhs strict in this argument".
This note deals with the second part.

There are multiple reasons why we might not want to insert a seq in the rhs to
strictify a functions argument:

1) The argument doesn't exist at runtime.

For zero width types (like Types) there is no benefit as we don't operate on them
at runtime at all. This includes things like void#, coercions and state tokens.

2) The argument is a unlifted type.

If the argument is a unlifted type the calling convention already is explicitly
cbv. This means inserting a seq on this argument wouldn't do anything as the seq
would be a no-op *and* it wouldn't affect the calling convention.

3) The argument is absent.

If the argument is absent in the body there is no advantage to it being passed as
cbv to the function. The function won't ever look at it so we don't safe any work.

This mostly happens for join point. For example we might have:

    data T = MkT ![Int] [Char]
    f t = case t of MkT xs{strict} ys-> snd (xs,ys)

and abstract the case alternative to:

    f t = join j1 = \xs ys -> snd (xs,ys)
          in case t of MkT xs{strict} ys-> j1 xs xy

While we "use" xs inside `j1` it's not used inside the function `snd` we pass it to.
In short a absent demand means neither our RHS, nor any function we pass the argument
to will inspect it. So there is no work to be saved by forcing `xs` early.

NB: There is an edge case where if we rebox we *can* end up seqing an absent value.
Note [Absent fillers] has an example of this. However this is so rare it's not worth
caring about here.

4) The argument is already strict.

Consider this code:

    data T = MkT ![Int]
    f t = case t of MkT xs{strict} -> reverse xs

The `xs{strict}` indicates that `xs` is used strictly by the `reverse xs`.
If we do a w/w split, and add the extra eval on `xs`, we'll get

    $wf xs =
        case xs of xs1 ->
            let t = MkT xs1 in
            case t of MkT xs2 -> reverse xs2

That's not wrong; but the w/w body will simplify to

    $wf xs = case xs of xs1 -> reverse xs1

and now we'll drop the `case xs` because `xs1` is used strictly in its scope.
Adding that eval was a waste of time.  So don't add it for strictly-demanded Ids.

5) Functions

Functions are tricky (see Note [TagInfo of functions] in EnforceEpt).
But the gist of it even if we make a higher order function argument strict
we can't avoid the tag check when it's used later in the body.
So there is no benefit.

-}
-- | Do we expect there to be any benefit if we make this var strict
-- in order for it to get treated as as cbv argument?
-- See Note [Which Ids should be strictified]
-- See Note [CBV Function Ids] for more background.
shouldStrictifyIdForCbv :: Var -> Bool
shouldStrictifyIdForCbv = wantCbvForId False

-- Like shouldStrictifyIdForCbv but also wants to use cbv for strict args.
shouldUseCbvForId :: Var -> Bool
shouldUseCbvForId = wantCbvForId True

-- When we strictify we want to skip strict args otherwise the logic is the same
-- as for shouldUseCbvForId so we common up the logic here.
-- Basically returns true if it would be beneficial for runtime to pass this argument
-- as CBV independent of weither or not it's correct. E.g. it might return true for lazy args
-- we are not allowed to force.
wantCbvForId :: Bool -> Var -> Bool
wantCbvForId cbv_for_strict v
  -- Must be a runtime var.
  -- See Note [Which Ids should be strictified] point 1)
  | isId v
  , not $ isZeroBitTy ty
  -- Unlifted things don't need special measures to be treated as cbv
  -- See Note [Which Ids should be strictified] point 2)
  , mightBeLiftedType ty
  -- Functions sometimes get a zero tag so we can't eliminate the tag check.
  -- See Note [TagInfo of functions] in EnforceEpt.
  -- See Note [Which Ids should be strictified] point 5)
  , not $ isFunTy ty
  -- If the var is strict already a seq is redundant.
  -- See Note [Which Ids should be strictified] point 4)
  , not (isStrictDmd dmd) || cbv_for_strict
  -- If the var is absent a seq is almost always useless.
  -- See Note [Which Ids should be strictified] point 3)
  , not (isAbsDmd dmd)
  = True
  | otherwise
  = False
  where
    ty = idType v
    dmd = idDemandInfo v

{- *********************************************************************
*                                                                      *
             unsafeEqualityProof
*                                                                      *
********************************************************************* -}

isUnsafeEqualityCase :: CoreExpr -> Id -> [CoreAlt] -> Maybe CoreExpr
-- See (U3) and (U4) in
-- Note [Implementing unsafeCoerce] in base:Unsafe.Coerce
isUnsafeEqualityCase scrut bndr alts
  | [Alt ac _ rhs] <- alts
  , DataAlt dc <- ac
  , dc `hasKey` unsafeReflDataConKey
  , isDeadBinder bndr
      -- We can only discard the case if the case-binder is dead
      -- It usually is, but see #18227
  , Var v `App` _ `App` _ `App` _ <- scrut
  , v `hasKey` unsafeEqualityProofIdKey
      -- Check that the scrutinee really is unsafeEqualityProof
      -- and not, say, error
  = Just rhs
  | otherwise
  = Nothing
