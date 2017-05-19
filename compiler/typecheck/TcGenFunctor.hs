{-
(c) The University of Glasgow 2011


The deriving code for the Functor, Foldable, and Traversable classes
(equivalent to the code in TcGenDeriv, for other classes)
-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module TcGenFunctor (
        FFoldType(..), functorLikeTraverse,
        deepSubtypesContaining, foldDataConArgs,

        gen_Functor_binds, gen_Foldable_binds, gen_Traversable_binds
    ) where

import Bag
import DataCon
import FastString
import HsSyn
import Panic
import PrelNames
import RdrName
import SrcLoc
import State
import TcGenDeriv
import TcType
import TyCon
import TyCoRep
import Type
import Util
import Var
import VarSet
import MkId (coerceId)
import TysWiredIn (true_RDR, false_RDR)

import Data.Maybe (catMaybes, isJust)

{-
************************************************************************
*                                                                      *
                        Functor instances

 see http://www.mail-archive.com/haskell-prime@haskell.org/msg02116.html

*                                                                      *
************************************************************************

For the data type:

  data T a = T1 Int a | T2 (T a)

We generate the instance:

  instance Functor T where
      fmap f (T1 b1 a) = T1 b1 (f a)
      fmap f (T2 ta)   = T2 (fmap f ta)

Notice that we don't simply apply 'fmap' to the constructor arguments.
Rather
  - Do nothing to an argument whose type doesn't mention 'a'
  - Apply 'f' to an argument of type 'a'
  - Apply 'fmap f' to other arguments
That's why we have to recurse deeply into the constructor argument types,
rather than just one level, as we typically do.

What about types with more than one type parameter?  In general, we only
derive Functor for the last position:

  data S a b = S1 [b] | S2 (a, T a b)
  instance Functor (S a) where
    fmap f (S1 bs)    = S1 (fmap f bs)
    fmap f (S2 (p,q)) = S2 (a, fmap f q)

However, we have special cases for
         - tuples
         - functions

More formally, we write the derivation of fmap code over type variable
'a for type 'b as ($fmap 'a 'b).  In this general notation the derived
instance for T is:

  instance Functor T where
      fmap f (T1 x1 x2) = T1 ($(fmap 'a 'b1) x1) ($(fmap 'a 'a) x2)
      fmap f (T2 x1)    = T2 ($(fmap 'a '(T a)) x1)

  $(fmap 'a 'b)          =  \x -> x     -- when b does not contain a
  $(fmap 'a 'a)          =  f
  $(fmap 'a '(b1,b2))    =  \x -> case x of (x1,x2) -> ($(fmap 'a 'b1) x1, $(fmap 'a 'b2) x2)
  $(fmap 'a '(T b1 b2))  =  fmap $(fmap 'a 'b2)   -- when a only occurs in the last parameter, b2
  $(fmap 'a '(b -> c))   =  \x b -> $(fmap 'a' 'c) (x ($(cofmap 'a 'b) b))

For functions, the type parameter 'a can occur in a contravariant position,
which means we need to derive a function like:

  cofmap :: (a -> b) -> (f b -> f a)

This is pretty much the same as $fmap, only without the $(cofmap 'a 'a) case:

  $(cofmap 'a 'b)          =  \x -> x     -- when b does not contain a
  $(cofmap 'a 'a)          =  error "type variable in contravariant position"
  $(cofmap 'a '(b1,b2))    =  \x -> case x of (x1,x2) -> ($(cofmap 'a 'b1) x1, $(cofmap 'a 'b2) x2)
  $(cofmap 'a '[b])        =  map $(cofmap 'a 'b)
  $(cofmap 'a '(T b1 b2))  =  fmap $(cofmap 'a 'b2)   -- when a only occurs in the last parameter, b2
  $(cofmap 'a '(b -> c))   =  \x b -> $(cofmap 'a' 'c) (x ($(fmap 'a 'c) b))

Note that the code produced by $(fmap _ _) is always a higher order function,
with type `(a -> b) -> (g a -> g b)` for some g. When we need to do pattern
matching on the type, this means create a lambda function (see the (,) case above).
The resulting code for fmap can look a bit weird, for example:

  data X a = X (a,Int)
  -- generated instance
  instance Functor X where
      fmap f (X x) = (\y -> case y of (x1,x2) -> X (f x1, (\z -> z) x2)) x

The optimizer should be able to simplify this code by simple inlining.

An older version of the deriving code tried to avoid these applied
lambda functions by producing a meta level function. But the function to
be mapped, `f`, is a function on the code level, not on the meta level,
so it was eta expanded to `\x -> [| f $x |]`. This resulted in too much eta expansion.
It is better to produce too many lambdas than to eta expand, see ticket #7436.
-}

gen_Functor_binds :: SrcSpan -> TyCon -> (LHsBinds GhcPs, BagDerivStuff)
-- When the argument is phantom, we can use  fmap _ = coerce
-- See Note [Phantom types with Functor, Foldable, and Traversable]
gen_Functor_binds loc tycon
  | Phantom <- last (tyConRoles tycon)
  = (unitBag fmap_bind, emptyBag)
  where
    fmap_name = L loc fmap_RDR
    fmap_bind = mkRdrFunBind fmap_name fmap_eqns
    fmap_eqns = [mkSimpleMatch fmap_match_ctxt
                               [nlWildPat]
                               coerce_Expr]
    fmap_match_ctxt = mkPrefixFunRhs fmap_name

gen_Functor_binds loc tycon
  = (listToBag [fmap_bind, replace_bind], emptyBag)
  where
    data_cons = tyConDataCons tycon
    fmap_name = L loc fmap_RDR

    -- See Note [EmptyDataDecls with Functor, Foldable, and Traversable]
    fmap_bind = mkRdrFunBindEC 2 id fmap_name fmap_eqns
    fmap_match_ctxt = mkPrefixFunRhs fmap_name

    fmap_eqn con = flip evalState bs_RDRs $
                     match_for_con fmap_match_ctxt [f_Pat] con =<< parts
      where
        parts = sequence $ foldDataConArgs ft_fmap con

    fmap_eqns = map fmap_eqn data_cons

    ft_fmap :: FFoldType (State [RdrName] (LHsExpr GhcPs))
    ft_fmap = FT { ft_triv = mkSimpleLam $ \x -> return x
                   -- fmap f = \x -> x
                 , ft_var  = return f_Expr
                   -- fmap f = f
                 , ft_fun  = \g h -> do
                     gg <- g
                     hh <- h
                     mkSimpleLam2 $ \x b -> return $
                       nlHsApp hh (nlHsApp x (nlHsApp gg b))
                   -- fmap f = \x b -> h (x (g b))
                 , ft_tup = \t gs -> do
                     gg <- sequence gs
                     mkSimpleLam $ mkSimpleTupleCase (match_for_con CaseAlt) t gg
                   -- fmap f = \x -> case x of (a1,a2,..) -> (g1 a1,g2 a2,..)
                 , ft_ty_app = \_ g -> nlHsApp fmap_Expr <$> g
                   -- fmap f = fmap g
                 , ft_forall = \_ g -> g
                 , ft_bad_app = panic "in other argument in ft_fmap"
                 , ft_co_var = panic "contravariant in ft_fmap" }

    -- See Note [Deriving <$]
    replace_name = L loc replace_RDR

    -- See Note [EmptyDataDecls with Functor, Foldable, and Traversable]
    replace_bind = mkRdrFunBindEC 2 id replace_name replace_eqns
    replace_match_ctxt = mkPrefixFunRhs replace_name

    replace_eqn con = flip evalState bs_RDRs $
        match_for_con replace_match_ctxt [z_Pat] con =<< parts
      where
        parts = traverse (fmap replace) $ foldDataConArgs ft_replace con

    replace_eqns = map replace_eqn data_cons

    ft_replace :: FFoldType (State [RdrName] Replacer)
    ft_replace = FT { ft_triv = fmap Nested $ mkSimpleLam $ \x -> return x
                   -- (p <$) = \x -> x
                 , ft_var  = fmap Immediate $ mkSimpleLam $ \_ -> return z_Expr
                   -- (p <$) = const p
                 , ft_fun  = \g h -> do
                     gg <- replace <$> g
                     hh <- replace <$> h
                     fmap Nested $ mkSimpleLam2 $ \x b -> return $
                       nlHsApp hh (nlHsApp x (nlHsApp gg b))
                   -- (<$) p = \x b -> h (x (g b))
                 , ft_tup = \t gs -> do
                     gg <- traverse (fmap replace) gs
                     fmap Nested . mkSimpleLam $
                          mkSimpleTupleCase (match_for_con CaseAlt) t gg
                   -- (p <$) = \x -> case x of (a1,a2,..) -> (g1 a1,g2 a2,..)
                 , ft_ty_app = \_ gm -> do
                       g <- gm
                       case g of
                         Nested g' -> pure . Nested $
                                          nlHsApp fmap_Expr $ g'
                         Immediate _ -> pure . Nested $
                                          nlHsApp replace_Expr z_Expr
                   -- (p <$) = fmap (p <$)
                 , ft_forall = \_ g -> g
                 , ft_bad_app = panic "in other argument in ft_replace"
                 , ft_co_var = panic "contravariant in ft_replace" }

    -- Con a1 a2 ... -> Con (f1 a1) (f2 a2) ...
    match_for_con :: HsMatchContext RdrName
                  -> [LPat GhcPs] -> DataCon -> [LHsExpr GhcPs]
                  -> State [RdrName] (LMatch GhcPs (LHsExpr GhcPs))
    match_for_con ctxt = mkSimpleConMatch ctxt $
        \con_name xs -> return $ nlHsApps con_name xs  -- Con x1 x2 ..

-- See Note [Deriving <$]
data Replacer = Immediate {replace :: LHsExpr GhcPs}
              | Nested {replace :: LHsExpr GhcPs}

{- Note [Deriving <$]
   ~~~~~~~~~~~~~~~~~~

We derive the definition of <$. Allowing this to take the default definition
can lead to memory leaks: mapping over a structure with a constant function can
fill the result structure with trivial thunks that retain the values from the
original structure. The simplifier seems to handle this all right for simple
types, but not for recursive ones. Consider

data Tree a = Bin !(Tree a) a !(Tree a) | Tip deriving Functor

-- fmap _ Tip = Tip
-- fmap f (Bin l v r) = Bin (fmap f l) (f v) (fmap f r)

Using the default definition of <$, we get (<$) x = fmap (\_ -> x) and that
simplifies no further. Why is that? `fmap` is defined recursively, so GHC
cannot inline it. The static argument transformation would turn the definition
into a non-recursive one

-- fmap f = go where
--   go Tip = Tip
--   go (Bin l v r) = Bin (go l) (f v) (go r)

which GHC could inline, producing an efficient definion of `<$`. But there are
several problems. First, GHC does not perform the static argument transformation
by default, even with -O2. Second, even when it does perform the static argument
transformation, it does so only when there are at least two static arguments,
which is not the case for fmap. Finally, when the type in question is
non-regular, such as

data Nesty a = Z a | S (Nesty a) (Nest (a, a))

the function argument is no longer (entirely) static, so the static argument
transformation will do nothing for us.

Applying the default definition of `<$` will produce a tree full of thunks that
look like ((\_ -> x) x0), which represents unnecessary thunk allocation and
also retention of the previous value, potentially leaking memory. Instead, we
derive <$ separately. Two aspects are different from fmap: the case of the
sought type variable (ft_var) and the case of a type application (ft_ty_app).
The interesting one is ft_ty_app. We have to distinguish two cases: the
"immediate" case where the type argument *is* the sought type variable, and
the "nested" case where the type argument *contains* the sought type variable.

The immediate case:

Suppose we have

data Imm a = Imm (F ... a)

Then we want to define

x <$ Imm q = Imm (x <$ q)

The nested case:

Suppose we have

data Nes a = Nes (F ... (G a))

Then we want to define

x <$ Nes q = Nes (fmap (x <$) q)

We use the Replacer type to tag whether the expression derived for applying
<$ to the last type variable was the ft_var case (immediate) or one of the
others (letting ft_forall pass through as usual).

We could, but do not, give tuples special treatment to improve efficiency
in some cases. Suppose we have

data Nest a = Z a | S (Nest (a,a))

The optimal definition would be

x <$ Z _ = Z x
x <$ S t = S ((x, x) <$ t)

which produces a result with maximal internal sharing. The reason we do not
attempt to treat this case specially is that we have no way to give
user-provided tuple-like types similar treatment. If the user changed the
definition to

data Pair a = Pair a a
data Nest a = Z a | S (Nest (Pair a))

they would experience a surprising degradation in performance. -}


{-
Utility functions related to Functor deriving.

Since several things use the same pattern of traversal, this is abstracted into functorLikeTraverse.
This function works like a fold: it makes a value of type 'a' in a bottom up way.
-}

-- Generic traversal for Functor deriving
-- See Note [FFoldType and functorLikeTraverse]
data FFoldType a      -- Describes how to fold over a Type in a functor like way
   = FT { ft_triv    :: a
          -- ^ Does not contain variable
        , ft_var     :: a
          -- ^ The variable itself
        , ft_co_var  :: a
          -- ^ The variable itself, contravariantly
        , ft_fun     :: a -> a -> a
          -- ^ Function type
        , ft_tup     :: TyCon -> [a] -> a
          -- ^ Tuple type
        , ft_ty_app  :: Type -> a -> a
          -- ^ Type app, variable only in last argument
        , ft_bad_app :: a
          -- ^ Type app, variable other than in last argument
        , ft_forall  :: TcTyVar -> a -> a
          -- ^ Forall type
     }

functorLikeTraverse :: forall a.
                       TyVar         -- ^ Variable to look for
                    -> FFoldType a   -- ^ How to fold
                    -> Type          -- ^ Type to process
                    -> a
functorLikeTraverse var (FT { ft_triv = caseTrivial,     ft_var = caseVar
                            , ft_co_var = caseCoVar,     ft_fun = caseFun
                            , ft_tup = caseTuple,        ft_ty_app = caseTyApp
                            , ft_bad_app = caseWrongArg, ft_forall = caseForAll })
                    ty
  = fst (go False ty)
  where
    go :: Bool        -- Covariant or contravariant context
       -> Type
       -> (a, Bool)   -- (result of type a, does type contain var)

    go co ty | Just ty' <- tcView ty = go co ty'
    go co (TyVarTy    v) | v == var = (if co then caseCoVar else caseVar,True)
    go co (FunTy x y)  | isPredTy x = go co y
                       | xc || yc   = (caseFun xr yr,True)
        where (xr,xc) = go (not co) x
              (yr,yc) = go co       y
    go co (AppTy    x y) | xc = (caseWrongArg,   True)
                         | yc = (caseTyApp x yr, True)
        where (_, xc) = go co x
              (yr,yc) = go co y
    go co ty@(TyConApp con args)
       | not (or xcs)     = (caseTrivial, False)   -- Variable does not occur
       -- At this point we know that xrs, xcs is not empty,
       -- and at least one xr is True
       | isTupleTyCon con = (caseTuple con xrs, True)
       | or (init xcs)    = (caseWrongArg, True)         -- T (..var..)    ty
       | Just (fun_ty, _) <- splitAppTy_maybe ty         -- T (..no var..) ty
                          = (caseTyApp fun_ty (last xrs), True)
       | otherwise        = (caseWrongArg, True)   -- Non-decomposable (eg type function)
       where
         -- When folding over an unboxed tuple, we must explicitly drop the
         -- runtime rep arguments, or else GHC will generate twice as many
         -- variables in a unboxed tuple pattern match and expression as it
         -- actually needs. See Trac #12399
         (xrs,xcs) = unzip (map (go co) (dropRuntimeRepArgs args))
    go co (ForAllTy (TvBndr v vis) x)
       | isVisibleArgFlag vis = panic "unexpected visible binder"
       | v /= var && xc       = (caseForAll v xr,True)
       where (xr,xc) = go co x

    go _ _ = (caseTrivial,False)

-- Return all syntactic subterms of ty that contain var somewhere
-- These are the things that should appear in instance constraints
deepSubtypesContaining :: TyVar -> Type -> [TcType]
deepSubtypesContaining tv
  = functorLikeTraverse tv
        (FT { ft_triv = []
            , ft_var = []
            , ft_fun = (++)
            , ft_tup = \_ xs -> concat xs
            , ft_ty_app = (:)
            , ft_bad_app = panic "in other argument in deepSubtypesContaining"
            , ft_co_var = panic "contravariant in deepSubtypesContaining"
            , ft_forall = \v xs -> filterOut ((v `elemVarSet`) . tyCoVarsOfType) xs })


foldDataConArgs :: FFoldType a -> DataCon -> [a]
-- Fold over the arguments of the datacon
foldDataConArgs ft con
  = map foldArg (dataConOrigArgTys con)
  where
    foldArg
      = case getTyVar_maybe (last (tyConAppArgs (dataConOrigResTy con))) of
             Just tv -> functorLikeTraverse tv ft
             Nothing -> const (ft_triv ft)
    -- If we are deriving Foldable for a GADT, there is a chance that the last
    -- type variable in the data type isn't actually a type variable at all.
    -- (for example, this can happen if the last type variable is refined to
    -- be a concrete type such as Int). If the last type variable is refined
    -- to be a specific type, then getTyVar_maybe will return Nothing.
    -- See Note [DeriveFoldable with ExistentialQuantification]
    --
    -- The kind checks have ensured the last type parameter is of kind *.

-- Make a HsLam using a fresh variable from a State monad
mkSimpleLam :: (LHsExpr GhcPs -> State [RdrName] (LHsExpr GhcPs))
            -> State [RdrName] (LHsExpr GhcPs)
-- (mkSimpleLam fn) returns (\x. fn(x))
mkSimpleLam lam = do
    (n:names) <- get
    put names
    body <- lam (nlHsVar n)
    return (mkHsLam [nlVarPat n] body)

mkSimpleLam2 :: (LHsExpr GhcPs -> LHsExpr GhcPs
             -> State [RdrName] (LHsExpr GhcPs))
             -> State [RdrName] (LHsExpr GhcPs)
mkSimpleLam2 lam = do
    (n1:n2:names) <- get
    put names
    body <- lam (nlHsVar n1) (nlHsVar n2)
    return (mkHsLam [nlVarPat n1,nlVarPat n2] body)

-- "Con a1 a2 a3 -> fold [x1 a1, x2 a2, x3 a3]"
--
-- @mkSimpleConMatch fold extra_pats con insides@ produces a match clause in
-- which the LHS pattern-matches on @extra_pats@, followed by a match on the
-- constructor @con@ and its arguments. The RHS folds (with @fold@) over @con@
-- and its arguments, applying an expression (from @insides@) to each of the
-- respective arguments of @con@.
mkSimpleConMatch :: Monad m => HsMatchContext RdrName
                 -> (RdrName -> [LHsExpr GhcPs] -> m (LHsExpr GhcPs))
                 -> [LPat GhcPs]
                 -> DataCon
                 -> [LHsExpr GhcPs]
                 -> m (LMatch GhcPs (LHsExpr GhcPs))
mkSimpleConMatch ctxt fold extra_pats con insides = do
    let con_name = getRdrName con
    let vars_needed = takeList insides as_RDRs
    let bare_pat = nlConVarPat con_name vars_needed
    let pat = if null vars_needed
          then bare_pat
          else nlParPat bare_pat
    rhs <- fold con_name
                (zipWith (\i v -> i `nlHsApp` nlHsVar v) insides vars_needed)
    return $ mkMatch ctxt (extra_pats ++ [pat]) rhs
                     (noLoc emptyLocalBinds)

-- "Con a1 a2 a3 -> fmap (\b2 -> Con a1 b2 a3) (traverse f a2)"
--
-- @mkSimpleConMatch2 fold extra_pats con insides@ behaves very similarly to
-- 'mkSimpleConMatch', with two key differences:
--
-- 1. @insides@ is a @[Maybe (LHsExpr RdrName)]@ instead of a
--    @[LHsExpr RdrName]@. This is because it filters out the expressions
--    corresponding to arguments whose types do not mention the last type
--    variable in a derived 'Foldable' or 'Traversable' instance (i.e., the
--    'Nothing' elements of @insides@).
--
-- 2. @fold@ takes an expression as its first argument instead of a
--    constructor name. This is because it uses a specialized
--    constructor function expression that only takes as many parameters as
--    there are argument types that mention the last type variable.
--
-- See Note [Generated code for DeriveFoldable and DeriveTraversable]
mkSimpleConMatch2 :: Monad m
                  => HsMatchContext RdrName
                  -> (LHsExpr GhcPs -> [LHsExpr GhcPs]
                                      -> m (LHsExpr GhcPs))
                  -> [LPat GhcPs]
                  -> DataCon
                  -> [Maybe (LHsExpr GhcPs)]
                  -> m (LMatch GhcPs (LHsExpr GhcPs))
mkSimpleConMatch2 ctxt fold extra_pats con insides = do
    let con_name = getRdrName con
        vars_needed = takeList insides as_RDRs
        pat = nlConVarPat con_name vars_needed
        -- Make sure to zip BEFORE invoking catMaybes. We want the variable
        -- indicies in each expression to match up with the argument indices
        -- in con_expr (defined below).
        exps = catMaybes $ zipWith (\i v -> (`nlHsApp` nlHsVar v) <$> i)
                                   insides vars_needed
        -- An element of argTysTyVarInfo is True if the constructor argument
        -- with the same index has a type which mentions the last type
        -- variable.
        argTysTyVarInfo = map isJust insides
        (asWithTyVar, asWithoutTyVar) = partitionByList argTysTyVarInfo as_Vars

        con_expr
          | null asWithTyVar = nlHsApps con_name asWithoutTyVar
          | otherwise =
              let bs   = filterByList  argTysTyVarInfo bs_RDRs
                  vars = filterByLists argTysTyVarInfo bs_Vars as_Vars
              in mkHsLam (map nlVarPat bs) (nlHsApps con_name vars)

    rhs <- fold con_expr exps
    return $ mkMatch ctxt (extra_pats ++ [pat]) rhs
                     (noLoc emptyLocalBinds)

-- "case x of (a1,a2,a3) -> fold [x1 a1, x2 a2, x3 a3]"
mkSimpleTupleCase :: Monad m => ([LPat GhcPs] -> DataCon -> [a]
                                 -> m (LMatch GhcPs (LHsExpr GhcPs)))
                  -> TyCon -> [a] -> LHsExpr GhcPs -> m (LHsExpr GhcPs)
mkSimpleTupleCase match_for_con tc insides x
  = do { let data_con = tyConSingleDataCon tc
       ; match <- match_for_con [] data_con insides
       ; return $ nlHsCase x [match] }

{-
************************************************************************
*                                                                      *
                        Foldable instances

 see http://www.mail-archive.com/haskell-prime@haskell.org/msg02116.html

*                                                                      *
************************************************************************

Deriving Foldable instances works the same way as Functor instances,
only Foldable instances are not possible for function types at all.
Given (data T a = T a a (T a) deriving Foldable), we get:

  instance Foldable T where
      foldr f z (T x1 x2 x3) =
        $(foldr 'a 'a) x1 ( $(foldr 'a 'a) x2 ( $(foldr 'a '(T a)) x3 z ) )

-XDeriveFoldable is different from -XDeriveFunctor in that it filters out
arguments to the constructor that would produce useless code in a Foldable
instance. For example, the following datatype:

  data Foo a = Foo Int a Int deriving Foldable

would have the following generated Foldable instance:

  instance Foldable Foo where
    foldr f z (Foo x1 x2 x3) = $(foldr 'a 'a) x2

since neither of the two Int arguments are folded over.

The cases are:

  $(foldr 'a 'a)         =  f
  $(foldr 'a '(b1,b2))   =  \x z -> case x of (x1,x2) -> $(foldr 'a 'b1) x1 ( $(foldr 'a 'b2) x2 z )
  $(foldr 'a '(T b1 b2)) =  \x z -> foldr $(foldr 'a 'b2) z x  -- when a only occurs in the last parameter, b2

Note that the arguments to the real foldr function are the wrong way around,
since (f :: a -> b -> b), while (foldr f :: b -> t a -> b).

One can envision a case for types that don't contain the last type variable:

  $(foldr 'a 'b)         =  \x z -> z     -- when b does not contain a

But this case will never materialize, since the aforementioned filtering
removes all such types from consideration.
See Note [Generated code for DeriveFoldable and DeriveTraversable].

Foldable instances differ from Functor and Traversable instances in that
Foldable instances can be derived for data types in which the last type
variable is existentially quantified. In particular, if the last type variable
is refined to a more specific type in a GADT:

  data GADT a where
      G :: a ~ Int => a -> G Int

then the deriving machinery does not attempt to check that the type a contains
Int, since it is not syntactically equal to a type variable. That is, the
derived Foldable instance for GADT is:

  instance Foldable GADT where
      foldr _ z (GADT _) = z

See Note [DeriveFoldable with ExistentialQuantification].

Note [Deriving null]
~~~~~~~~~~~~~~~~~~~~

In some cases, deriving the definition of 'null' can produce much better
results than the default definition. For example, with

  data SnocList a = Nil | Snoc (SnocList a) a

the default definition of 'null' would walk the entire spine of a
nonempty snoc-list before concluding that it is not null. But looking at
the Snoc constructor, we can immediately see that it contains an 'a', and
so 'null' can return False immediately if it matches on Snoc. When we
derive 'null', we keep track of things that cannot be null. The interesting
case is type application. Given

  data Wrap a = Wrap (Foo (Bar a))

we use

  null (Wrap fba) = all null fba

but if we see

  data Wrap a = Wrap (Foo a)

we can just use

  null (Wrap fa) = null fa

Indeed, we allow this to happen even for tuples:

  data Wrap a = Wrap (Foo (a, Int))

produces

  null (Wrap fa) = null fa

As explained in Note [Deriving <$], giving tuples special performance treatment
could surprise users if they switch to other types, but Ryan Scott seems to
think it's okay to do it for now.
-}

gen_Foldable_binds :: SrcSpan -> TyCon -> (LHsBinds GhcPs, BagDerivStuff)
-- When the parameter is phantom, we can use foldMap _ _ = mempty
-- See Note [Phantom types with Functor, Foldable, and Traversable]
gen_Foldable_binds loc tycon
  | Phantom <- last (tyConRoles tycon)
  = (unitBag foldMap_bind, emptyBag)
  where
    foldMap_name = L loc foldMap_RDR
    foldMap_bind = mkRdrFunBind foldMap_name foldMap_eqns
    foldMap_eqns = [mkSimpleMatch foldMap_match_ctxt
                                  [nlWildPat, nlWildPat]
                                  mempty_Expr]
    foldMap_match_ctxt = mkPrefixFunRhs foldMap_name

gen_Foldable_binds loc tycon
  | null data_cons  -- There's no real point producing anything but
                    -- foldMap for a type with no constructors.
  = (unitBag foldMap_bind, emptyBag)

  | otherwise
  = (listToBag [foldr_bind, foldMap_bind, null_bind], emptyBag)
  where
    data_cons = tyConDataCons tycon

    foldr_bind = mkRdrFunBind (L loc foldable_foldr_RDR) eqns
    eqns = map foldr_eqn data_cons
    foldr_eqn con
      = evalState (match_foldr z_Expr [f_Pat,z_Pat] con =<< parts) bs_RDRs
      where
        parts = sequence $ foldDataConArgs ft_foldr con

    foldMap_name = L loc foldMap_RDR

    -- See Note [EmptyDataDecls with Functor, Foldable, and Traversable]
    foldMap_bind = mkRdrFunBindEC 2 (const mempty_Expr)
                      foldMap_name foldMap_eqns

    foldMap_eqns = map foldMap_eqn data_cons

    foldMap_eqn con
      = evalState (match_foldMap [f_Pat] con =<< parts) bs_RDRs
      where
        parts = sequence $ foldDataConArgs ft_foldMap con

    -- Given a list of NullM results, produce Nothing if any of
    -- them is NotNull, and otherwise produce a list of Maybes
    -- with Justs representing unknowns and Nothings representing
    -- things that are definitely null.
    convert :: [NullM a] -> Maybe [Maybe a]
    convert = traverse go where
      go IsNull = Just Nothing
      go NotNull = Nothing
      go (NullM a) = Just (Just a)

    null_name = L loc null_RDR
    null_match_ctxt = mkPrefixFunRhs null_name
    null_bind = mkRdrFunBind null_name null_eqns
    null_eqns = map null_eqn data_cons
    null_eqn con
      = flip evalState bs_RDRs $ do
          parts <- sequence $ foldDataConArgs ft_null con
          case convert parts of
            Nothing -> return $
              mkMatch null_match_ctxt [nlParPat (nlWildConPat con)]
                false_Expr (noLoc emptyLocalBinds)
            Just cp -> match_null [] con cp

    -- Yields 'Just' an expression if we're folding over a type that mentions
    -- the last type parameter of the datatype. Otherwise, yields 'Nothing'.
    -- See Note [FFoldType and functorLikeTraverse]
    ft_foldr :: FFoldType (State [RdrName] (Maybe (LHsExpr GhcPs)))
    ft_foldr
      = FT { ft_triv    = return Nothing
             -- foldr f = \x z -> z
           , ft_var     = return $ Just f_Expr
             -- foldr f = f
           , ft_tup     = \t g -> do
               gg  <- sequence g
               lam <- mkSimpleLam2 $ \x z ->
                 mkSimpleTupleCase (match_foldr z) t gg x
               return (Just lam)
             -- foldr f = (\x z -> case x of ...)
           , ft_ty_app  = \_ g -> do
               gg <- g
               mapM (\gg' -> mkSimpleLam2 $ \x z -> return $
                 nlHsApps foldable_foldr_RDR [gg',z,x]) gg
             -- foldr f = (\x z -> foldr g z x)
           , ft_forall  = \_ g -> g
           , ft_co_var  = panic "contravariant in ft_foldr"
           , ft_fun     = panic "function in ft_foldr"
           , ft_bad_app = panic "in other argument in ft_foldr" }

    match_foldr :: LHsExpr GhcPs
                -> [LPat GhcPs]
                -> DataCon
                -> [Maybe (LHsExpr GhcPs)]
                -> State [RdrName] (LMatch GhcPs (LHsExpr GhcPs))
    match_foldr z = mkSimpleConMatch2 LambdaExpr $ \_ xs -> return (mkFoldr xs)
      where
        -- g1 v1 (g2 v2 (.. z))
        mkFoldr :: [LHsExpr GhcPs] -> LHsExpr GhcPs
        mkFoldr = foldr nlHsApp z

    -- See Note [FFoldType and functorLikeTraverse]
    ft_foldMap :: FFoldType (State [RdrName] (Maybe (LHsExpr GhcPs)))
    ft_foldMap
      = FT { ft_triv = return Nothing
             -- foldMap f = \x -> mempty
           , ft_var  = return (Just f_Expr)
             -- foldMap f = f
           , ft_tup  = \t g -> do
               gg  <- sequence g
               lam <- mkSimpleLam $ mkSimpleTupleCase match_foldMap t gg
               return (Just lam)
             -- foldMap f = \x -> case x of (..,)
           , ft_ty_app = \_ g -> fmap (nlHsApp foldMap_Expr) <$> g
             -- foldMap f = foldMap g
           , ft_forall = \_ g -> g
           , ft_co_var = panic "contravariant in ft_foldMap"
           , ft_fun = panic "function in ft_foldMap"
           , ft_bad_app = panic "in other argument in ft_foldMap" }

    match_foldMap :: [LPat GhcPs]
                  -> DataCon
                  -> [Maybe (LHsExpr GhcPs)]
                  -> State [RdrName] (LMatch GhcPs (LHsExpr GhcPs))
    match_foldMap = mkSimpleConMatch2 CaseAlt $ \_ xs -> return (mkFoldMap xs)
      where
        -- mappend v1 (mappend v2 ..)
        mkFoldMap :: [LHsExpr GhcPs] -> LHsExpr GhcPs
        mkFoldMap [] = mempty_Expr
        mkFoldMap xs = foldr1 (\x y -> nlHsApps mappend_RDR [x,y]) xs

    -- See Note [FFoldType and functorLikeTraverse]
    -- Yields NullM an expression if we're folding over an expression
    -- that may or may not be null. Yields IsNull if it's certainly
    -- null, and yields NotNull if it's certainly not null.
    -- See Note [Deriving null]
    ft_null :: FFoldType (State [RdrName] (NullM (LHsExpr GhcPs)))
    ft_null
      = FT { ft_triv = return IsNull
             -- null = \_ -> True
           , ft_var  = return NotNull
             -- null = \_ -> False
           , ft_tup  = \t g -> do
               gg  <- sequence g
               case convert gg of
                 Nothing -> pure NotNull
                 Just ggg ->
                   NullM <$> (mkSimpleLam $ mkSimpleTupleCase match_null t ggg)
             -- null = \x -> case x of (..,)
           , ft_ty_app = \_ g -> flip fmap g $ \nestedResult ->
                              case nestedResult of
                                -- If e definitely contains the parameter,
                                -- then we can test if (G e) contains it by
                                -- simply checking if (G e) is null
                                NotNull -> NullM null_Expr
                                -- This case is unreachable--it will actually be
                                -- caught by ft_triv
                                IsNull -> IsNull
                                -- The general case uses (all null),
                                -- (all (all null)), etc.
                                NullM nestedTest -> NullM $
                                                    nlHsApp all_Expr nestedTest
             -- null fa = null fa, or null fa = all null fa, or null fa = True
           , ft_forall = \_ g -> g
           , ft_co_var = panic "contravariant in ft_null"
           , ft_fun = panic "function in ft_null"
           , ft_bad_app = panic "in other argument in ft_null" }

    match_null :: [LPat GhcPs]
                  -> DataCon
                  -> [Maybe (LHsExpr GhcPs)]
                  -> State [RdrName] (LMatch GhcPs (LHsExpr GhcPs))
    match_null = mkSimpleConMatch2 CaseAlt $ \_ xs -> return (mkNull xs)
      where
        -- v1 && v2 && ..
        mkNull :: [LHsExpr GhcPs] -> LHsExpr GhcPs
        mkNull [] = true_Expr
        mkNull xs = foldr1 (\x y -> nlHsApps and_RDR [x,y]) xs

data NullM a =
    IsNull   -- Definitely null
  | NotNull  -- Definitely not null
  | NullM a  -- Unknown

{-
************************************************************************
*                                                                      *
                        Traversable instances

 see http://www.mail-archive.com/haskell-prime@haskell.org/msg02116.html
*                                                                      *
************************************************************************

Again, Traversable is much like Functor and Foldable.

The cases are:

  $(traverse 'a 'a)          =  f
  $(traverse 'a '(b1,b2))    =  \x -> case x of (x1,x2) ->
     liftA2 (,) ($(traverse 'a 'b1) x1) ($(traverse 'a 'b2) x2)
  $(traverse 'a '(T b1 b2))  =  traverse $(traverse 'a 'b2)  -- when a only occurs in the last parameter, b2

Like -XDeriveFoldable, -XDeriveTraversable filters out arguments whose types
do not mention the last type parameter. Therefore, the following datatype:

  data Foo a = Foo Int a Int

would have the following derived Traversable instance:

  instance Traversable Foo where
    traverse f (Foo x1 x2 x3) =
      fmap (\b2 -> Foo x1 b2 x3) ( $(traverse 'a 'a) x2 )

since the two Int arguments do not produce any effects in a traversal.

One can envision a case for types that do not mention the last type parameter:

  $(traverse 'a 'b)          =  pure     -- when b does not contain a

But this case will never materialize, since the aforementioned filtering
removes all such types from consideration.
See Note [Generated code for DeriveFoldable and DeriveTraversable].
-}

gen_Traversable_binds :: SrcSpan -> TyCon -> (LHsBinds GhcPs, BagDerivStuff)
-- When the argument is phantom, we can use traverse = pure . coerce
-- See Note [Phantom types with Functor, Foldable, and Traversable]
gen_Traversable_binds loc tycon
  | Phantom <- last (tyConRoles tycon)
  = (unitBag traverse_bind, emptyBag)
  where
    traverse_name = L loc traverse_RDR
    traverse_bind = mkRdrFunBind traverse_name traverse_eqns
    traverse_eqns =
        [mkSimpleMatch traverse_match_ctxt
                       [nlWildPat, z_Pat]
                       (nlHsApps pure_RDR [nlHsApp coerce_Expr z_Expr])]
    traverse_match_ctxt = mkPrefixFunRhs traverse_name

gen_Traversable_binds loc tycon
  = (unitBag traverse_bind, emptyBag)
  where
    data_cons = tyConDataCons tycon

    traverse_name = L loc traverse_RDR

    -- See Note [EmptyDataDecls with Functor, Foldable, and Traversable]
    traverse_bind = mkRdrFunBindEC 2 (nlHsApp pure_Expr)
                                   traverse_name traverse_eqns
    traverse_eqns = map traverse_eqn data_cons
    traverse_eqn con
      = evalState (match_for_con [f_Pat] con =<< parts) bs_RDRs
      where
        parts = sequence $ foldDataConArgs ft_trav con

    -- Yields 'Just' an expression if we're folding over a type that mentions
    -- the last type parameter of the datatype. Otherwise, yields 'Nothing'.
    -- See Note [FFoldType and functorLikeTraverse]
    ft_trav :: FFoldType (State [RdrName] (Maybe (LHsExpr GhcPs)))
    ft_trav
      = FT { ft_triv    = return Nothing
             -- traverse f = pure x
           , ft_var     = return (Just f_Expr)
             -- traverse f = f x
           , ft_tup     = \t gs -> do
               gg  <- sequence gs
               lam <- mkSimpleLam $ mkSimpleTupleCase match_for_con t gg
               return (Just lam)
             -- traverse f = \x -> case x of (a1,a2,..) ->
             --                           liftA2 (,,) (g1 a1) (g2 a2) <*> ..
           , ft_ty_app  = \_ g -> fmap (nlHsApp traverse_Expr) <$> g
             -- traverse f = traverse g
           , ft_forall  = \_ g -> g
           , ft_co_var  = panic "contravariant in ft_trav"
           , ft_fun     = panic "function in ft_trav"
           , ft_bad_app = panic "in other argument in ft_trav" }

    -- Con a1 a2 ... -> liftA2 (\b1 b2 ... -> Con b1 b2 ...) (g1 a1)
    --                    (g2 a2) <*> ...
    match_for_con :: [LPat GhcPs]
                  -> DataCon
                  -> [Maybe (LHsExpr GhcPs)]
                  -> State [RdrName] (LMatch GhcPs (LHsExpr GhcPs))
    match_for_con = mkSimpleConMatch2 CaseAlt $
                                             \con xs -> return (mkApCon con xs)
      where
        -- liftA2 (\b1 b2 ... -> Con b1 b2 ...) x1 x2 <*> ..
        mkApCon :: LHsExpr GhcPs -> [LHsExpr GhcPs] -> LHsExpr GhcPs
        mkApCon con [] = nlHsApps pure_RDR [con]
        mkApCon con [x] = nlHsApps fmap_RDR [con,x]
        mkApCon con (x1:x2:xs) =
            foldl appAp (nlHsApps liftA2_RDR [con,x1,x2]) xs
          where appAp x y = nlHsApps ap_RDR [x,y]

-----------------------------------------------------------------------

f_Expr, z_Expr, fmap_Expr, replace_Expr, mempty_Expr, foldMap_Expr,
    traverse_Expr, coerce_Expr, pure_Expr, true_Expr, false_Expr,
    all_Expr, null_Expr :: LHsExpr GhcPs
f_Expr        = nlHsVar f_RDR
z_Expr        = nlHsVar z_RDR
fmap_Expr     = nlHsVar fmap_RDR
replace_Expr  = nlHsVar replace_RDR
mempty_Expr   = nlHsVar mempty_RDR
foldMap_Expr  = nlHsVar foldMap_RDR
traverse_Expr = nlHsVar traverse_RDR
coerce_Expr   = nlHsVar (getRdrName coerceId)
pure_Expr     = nlHsVar pure_RDR
true_Expr     = nlHsVar true_RDR
false_Expr    = nlHsVar false_RDR
all_Expr      = nlHsVar all_RDR
null_Expr     = nlHsVar null_RDR

f_RDR, z_RDR :: RdrName
f_RDR = mkVarUnqual (fsLit "f")
z_RDR = mkVarUnqual (fsLit "z")

as_RDRs, bs_RDRs :: [RdrName]
as_RDRs = [ mkVarUnqual (mkFastString ("a"++show i)) | i <- [(1::Int) .. ] ]
bs_RDRs = [ mkVarUnqual (mkFastString ("b"++show i)) | i <- [(1::Int) .. ] ]

as_Vars, bs_Vars :: [LHsExpr GhcPs]
as_Vars = map nlHsVar as_RDRs
bs_Vars = map nlHsVar bs_RDRs

f_Pat, z_Pat :: LPat GhcPs
f_Pat = nlVarPat f_RDR
z_Pat = nlVarPat z_RDR

{-
Note [DeriveFoldable with ExistentialQuantification]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Functor and Traversable instances can only be derived for data types whose
last type parameter is truly universally polymorphic. For example:

  data T a b where
    T1 ::                 b   -> T a b   -- YES, b is unconstrained
    T2 :: Ord b   =>      b   -> T a b   -- NO, b is constrained by (Ord b)
    T3 :: b ~ Int =>      b   -> T a b   -- NO, b is constrained by (b ~ Int)
    T4 ::                 Int -> T a Int -- NO, this is just like T3
    T5 :: Ord a   => a -> b   -> T a b   -- YES, b is unconstrained, even
                                         -- though a is existential
    T6 ::                 Int -> T Int b -- YES, b is unconstrained

For Foldable instances, however, we can completely lift the constraint that
the last type parameter be truly universally polymorphic. This means that T
(as defined above) can have a derived Foldable instance:

  instance Foldable (T a) where
    foldr f z (T1 b)   = f b z
    foldr f z (T2 b)   = f b z
    foldr f z (T3 b)   = f b z
    foldr f z (T4 b)   = z
    foldr f z (T5 a b) = f b z
    foldr f z (T6 a)   = z

    foldMap f (T1 b)   = f b
    foldMap f (T2 b)   = f b
    foldMap f (T3 b)   = f b
    foldMap f (T4 b)   = mempty
    foldMap f (T5 a b) = f b
    foldMap f (T6 a)   = mempty

In a Foldable instance, it is safe to fold over an occurrence of the last type
parameter that is not truly universally polymorphic. However, there is a bit
of subtlety in determining what is actually an occurrence of a type parameter.
T3 and T4, as defined above, provide one example:

  data T a b where
    ...
    T3 :: b ~ Int => b   -> T a b
    T4 ::            Int -> T a Int
    ...

  instance Foldable (T a) where
    ...
    foldr f z (T3 b) = f b z
    foldr f z (T4 b) = z
    ...
    foldMap f (T3 b) = f b
    foldMap f (T4 b) = mempty
    ...

Notice that the argument of T3 is folded over, whereas the argument of T4 is
not. This is because we only fold over constructor arguments that
syntactically mention the universally quantified type parameter of that
particular data constructor. See foldDataConArgs for how this is implemented.

As another example, consider the following data type. The argument of each
constructor has the same type as the last type parameter:

  data E a where
    E1 :: (a ~ Int) => a   -> E a
    E2 ::              Int -> E Int
    E3 :: (a ~ Int) => a   -> E Int
    E4 :: (a ~ Int) => Int -> E a

Only E1's argument is an occurrence of a universally quantified type variable
that is syntactically equivalent to the last type parameter, so only E1's
argument will be folded over in a derived Foldable instance.

See Trac #10447 for the original discussion on this feature. Also see
https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/DeriveFunctor
for a more in-depth explanation.

Note [FFoldType and functorLikeTraverse]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Deriving Functor, Foldable, and Traversable all require generating expressions
which perform an operation on each argument of a data constructor depending
on the argument's type. In particular, a generated operation can be different
depending on whether the type mentions the last type variable of the datatype
(e.g., if you have data T a = MkT a Int, then a generated foldr expression would
fold over the first argument of MkT, but not the second).

This pattern is abstracted with the FFoldType datatype, which provides hooks
for the user to specify how a constructor argument should be folded when it
has a type with a particular "shape". The shapes are as follows (assume that
a is the last type variable in a given datatype):

* ft_triv:    The type does not mention the last type variable at all.
              Examples: Int, b

* ft_var:     The type is syntactically equal to the last type variable.
              Moreover, the type appears in a covariant position (see
              the Deriving Functor instances section of the user's guide
              for an in-depth explanation of covariance vs. contravariance).
              Example: a (covariantly)

* ft_co_var:  The type is syntactically equal to the last type variable.
              Moreover, the type appears in a contravariant position.
              Example: a (contravariantly)

* ft_fun:     A function type which mentions the last type variable in
              the argument position, result position or both.
              Examples: a -> Int, Int -> a, Maybe a -> [a]

* ft_tup:     A tuple type which mentions the last type variable in at least
              one of its fields. The TyCon argument of ft_tup represents the
              particular tuple's type constructor.
              Examples: (a, Int), (Maybe a, [a], Either a Int), (# Int, a #)

* ft_ty_app:  A type is being applied to the last type parameter, where the
              applied type does not mention the last type parameter (if it
              did, it would fall under ft_bad_app). The Type argument to
              ft_ty_app represents the applied type.

              Note that functions, tuples, and foralls are distinct cases
              and take precedence of ft_ty_app. (For example, (Int -> a) would
              fall under (ft_fun Int a), not (ft_ty_app ((->) Int) a).
              Examples: Maybe a, Either b a

* ft_bad_app: A type application uses the last type parameter in a position
              other than the last argument. This case is singled out because
              Functor, Foldable, and Traversable instances cannot be derived
              for datatypes containing arguments with such types.
              Examples: Either a Int, Const a b

* ft_forall:  A forall'd type mentions the last type parameter on its right-
              hand side (and is not quantified on the left-hand side). This
              case is present mostly for plumbing purposes.
              Example: forall b. Either b a

If FFoldType describes a strategy for folding subcomponents of a Type, then
functorLikeTraverse is the function that applies that strategy to the entirety
of a Type, returning the final folded-up result.

foldDataConArgs applies functorLikeTraverse to every argument type of a
constructor, returning a list of the fold results. This makes foldDataConArgs
a natural way to generate the subexpressions in a generated fmap, foldr,
foldMap, or traverse definition (the subexpressions must then be combined in
a method-specific fashion to form the final generated expression).

Deriving Generic1 also does validity checking by looking for the last type
variable in certain positions of a constructor's argument types, so it also
uses foldDataConArgs. See Note [degenerate use of FFoldType] in TcGenGenerics.

Note [Generated code for DeriveFoldable and DeriveTraversable]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We adapt the algorithms for -XDeriveFoldable and -XDeriveTraversable based on
that of -XDeriveFunctor. However, there an important difference between deriving
the former two typeclasses and the latter one, which is best illustrated by the
following scenario:

  data WithInt a = WithInt a Int# deriving (Functor, Foldable, Traversable)

The generated code for the Functor instance is straightforward:

  instance Functor WithInt where
    fmap f (WithInt a i) = WithInt (f a) i

But if we use too similar of a strategy for deriving the Foldable and
Traversable instances, we end up with this code:

  instance Foldable WithInt where
    foldMap f (WithInt a i) = f a <> mempty

  instance Traversable WithInt where
    traverse f (WithInt a i) = fmap WithInt (f a) <*> pure i

This is unsatisfying for two reasons:

1. The Traversable instance doesn't typecheck! Int# is of kind #, but pure
   expects an argument whose type is of kind *. This effectively prevents
   Traversable from being derived for any datatype with an unlifted argument
   type (Trac #11174).

2. The generated code contains superfluous expressions. By the Monoid laws,
   we can reduce (f a <> mempty) to (f a), and by the Applicative laws, we can
   reduce (fmap WithInt (f a) <*> pure i) to (fmap (\b -> WithInt b i) (f a)).

We can fix both of these issues by incorporating a slight twist to the usual
algorithm that we use for -XDeriveFunctor. The differences can be summarized
as follows:

1. In the generated expression, we only fold over arguments whose types
   mention the last type parameter. Any other argument types will simply
   produce useless 'mempty's or 'pure's, so they can be safely ignored.

2. In the case of -XDeriveTraversable, instead of applying ConName,
   we apply (\b_i ... b_k -> ConName a_1 ... a_n), where

   * ConName has n arguments
   * {b_i, ..., b_k} is a subset of {a_1, ..., a_n} whose indices correspond
     to the arguments whose types mention the last type parameter. As a
     consequence, taking the difference of {a_1, ..., a_n} and
     {b_i, ..., b_k} yields the all the argument values of ConName whose types
     do not mention the last type parameter. Note that [i, ..., k] is a
     strictly increasing—but not necessarily consecutive—integer sequence.

     For example, the datatype

       data Foo a = Foo Int a Int a

     would generate the following Traversable instance:

       instance Traversable Foo where
         traverse f (Foo a1 a2 a3 a4) =
           fmap (\b2 b4 -> Foo a1 b2 a3 b4) (f a2) <*> f a4

Technically, this approach would also work for -XDeriveFunctor as well, but we
decide not to do so because:

1. There's not much benefit to generating, e.g., ((\b -> WithInt b i) (f a))
   instead of (WithInt (f a) i).

2. There would be certain datatypes for which the above strategy would
   generate Functor code that would fail to typecheck. For example:

     data Bar f a = Bar (forall f. Functor f => f a) deriving Functor

   With the conventional algorithm, it would generate something like:

     fmap f (Bar a) = Bar (fmap f a)

   which typechecks. But with the strategy mentioned above, it would generate:

     fmap f (Bar a) = (\b -> Bar b) (fmap f a)

   which does not typecheck, since GHC cannot unify the rank-2 type variables
   in the types of b and (fmap f a).

Note [Phantom types with Functor, Foldable, and Traversable]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Given a type F :: * -> * whose type argument has a phantom role, we can always
produce lawful Functor and Traversable instances using

    fmap _ = coerce
    traverse _ = pure . coerce

Indeed, these are equivalent to any *strictly lawful* instances one could
write, except that this definition of 'traverse' may be lazier.  That is, if
instances obey the laws under true equality (rather than up to some equivalence
relation), then they will be essentially equivalent to these. These definitions
are incredibly cheap, so we want to use them even if it means ignoring some
non-strictly-lawful instance in an embedded type.

Foldable has far fewer laws to work with, which leaves us unwelcome
freedom in implementing it. At a minimum, we would like to ensure that
a derived foldMap is always at least as good as foldMapDefault with a
derived traverse. To accomplish that, we must define

   foldMap _ _ = mempty

in these cases.

This may have different strictness properties from a standard derivation.
Consider

   data NotAList a = Nil | Cons (NotAList a) deriving Foldable

The usual deriving mechanism would produce

   foldMap _ Nil = mempty
   foldMap f (Cons x) = foldMap f x

which is strict in the entire spine of the NotAList.

Final point: why do we even care about such types? Users will rarely if ever
map, fold, or traverse over such things themselves, but other derived
instances may:

   data Hasn'tAList a = NotHere a (NotAList a) deriving Foldable

Note [EmptyDataDecls with Functor, Foldable, and Traversable]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are some slightly tricky decisions to make about how to handle
Functor, Foldable, and Traversable instances for types with no constructors.
For fmap, the two basic options are

   fmap _ _ = error "Sorry, no constructors"

or

   fmap _ z = case z of

In most cases, the latter is more helpful: if the thunk passed to fmap
throws an exception, we're generally going to be much more interested in
that exception than in the fact that there aren't any constructors.

In order to match the semantics for phantoms (see note above), we need to
be a bit careful about 'traverse'. The obvious definition would be

   traverse _ z = case z of

but this is stricter than the one for phantoms. We instead use

   traverse _ z = pure $ case z of

For foldMap, the obvious choices are

   foldMap _ _ = mempty

or

   foldMap _ z = case z of

We choose the first one to be consistent with what foldMapDefault does for
a derived Traversable instance.
-}
