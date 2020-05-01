{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module GHC.HsToCore.Arrows.Expr
  ( DsArrExpr(..)
  , DsTArrExpr(..)
  , mkAppArr
  , mkExprArr
  , mkArrArr
  , mkFirstArr
  , mkLoopArr
  , mkComposeArr
  , mkChoiceArr
  , mkWrapArr
  , mkPremapArr

  , dsArrExpr
  ) where

#include "HsVersions.h"

import GHC.Prelude

import {-# SOURCE #-} GHC.HsToCore.Expr (dsExpr)

import GHC.Builtin.Names
import GHC.Builtin.Types
import GHC.Core
import GHC.Core.Make
import GHC.Core.Ppr (pprParendExpr)
import GHC.Core.TyCon (isTupleTyCon)
import GHC.Core.Type (splitPiTy)
import GHC.Core.Utils
import GHC.Data.List.SetOps (assocMaybe)
import GHC.Hs (CmdSyntaxTable, GhcTc)
import GHC.HsToCore.Monad
import GHC.Tc.Utils.TcType
import GHC.Utils.Misc
import GHC.Utils.Outputable

{- Note [Tidying arrow expressions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Unlike most of the desugarer, arrow expressions aren’t desugared
directly to CoreExprs but to the intermediate DsArrExpr type. This
type represents a “mostly desugared” arrow command, but with the
various arrow class methods yet to be filled in. This allows us to
perform a very (very) simple optimisation pass to clean up the tree
before going the rest of the way to a CoreExpr, which we do in dsArrExpr.

Doing any optimisation in the desugarer is somewhat unorthodox, since
even basic tidying up of desugared expressions is normally the simple
optimiser’s job. However, there’s one simplification we really want to
perform here, namely fusion of adjacent uses of `arr`:

         (e1 >>> e2) >>> e3  ==>  e1 >>> (e2 >>> e3)     [reassociate]
            arr f >>> arr g  ==>  arr (f >>> g)          [arr/arr]
    arr f >>> (arr g >>> e)  ==>  arr (f >>> g) >>> e    [arr/arr/>>>]

Morally, these should be RULES, which would let the optimiser take
care of them. Indeed, if you look in Control.Arrow, you’ll find RULES
very similar to the above rewritings! Alas, they are extremely
fragile, since they’re defined on class ops, which are aggressively
specialised away.

Until we have a way to avoid preemption from the builtin class op
rules, these simplifications are just too useful to pass up. Most
importantly, they give us freedom to generate stupid
environment-passing code (see Note [The command environment] in
GHC.HsToCore.Arrows.Expr) where we have confidence it will be
optimised away. For example, we might have

    (foo >>> arr (\(a, b) -> (b, a))) >>> arr (\(b, _) -> b) >>> bar

which is quite stupid, but the optimiser can’t do anything with it on
its own. But after our tidying, we’ll get

    foo >>> arr ((\(a, b) -> (b, a)) >>> (\(b, _) -> b)) >>> bar

at which point the inner lambda can be simplified further:

    foo >>> arr (\(_, b) -> b) >>> bar

This is all somewhat unsatisfying, since these rewritings might very
well do good things on other code, too. But doing nothing is much
worse: a previous incarnation of the arrow desugarer didn’t do this
tidying but instead went to some length to avoid ever generating
“stupid” expressions like the one above. That made the desugaring code
significantly more difficult to read (and it still generated bad code
sometimes). So for now we do this here, but maybe we can get rid of it
in the future if the optimiser becomes cleverer. -}

-- | A “mostly desugared” arrow command, with only the skeleton of the
-- tree left to be replaced by arrow class ops. For an explanation of
-- why this exists, see Note [Tidying arrow expressions].
data DsArrExpr
  -- | @app@
  = AppArr
  -- | @e@
  | ExprArr CoreExpr
  -- | @arr e@
  | ArrArr CoreExpr
  -- | @first e@
  | FirstArr DsTArrExpr
  -- | @loop e@
  | LoopArr DsTArrExpr
  -- | @e1 >>> e2@
  | ComposeArr DsTArrExpr DsTArrExpr
  -- | @e1 ||| e2@
  | ChoiceArr DsTArrExpr DsTArrExpr
  -- | @wrap e@ (Holds a desugared 'HsWrap'.)
  | WrapArr (CoreExpr -> CoreExpr) DsTArrExpr

-- | A 'DsArrExpr' annotated with input and output types.
data DsTArrExpr = TypedArr Type Type DsArrExpr

instance Outputable DsArrExpr where
  ppr AppArr       = text "app"
  ppr (ExprArr e)  = pprParendExpr e
  ppr (ArrArr e)   = parens (text "arr" <+> parens (ppr e))
  ppr (FirstArr e) = parens (text "first" <+> parens (ppr e))
  ppr (LoopArr e)  = parens (text "loop" <+> parens (ppr e))
  ppr (ComposeArr e1 e2)
    = parens $ hang (parens (ppr e1)) 2 (text ">>>" <+> parens (ppr e2))
  ppr (ChoiceArr e1 e2)
    = parens $ hang (parens (ppr e1)) 2 (text "|||" <+> parens (ppr e2))
  ppr (WrapArr _ e)
    -- This is a hack, but getting enough information here to print
    -- something useful is annoying, and this instance is only used
    -- for debugging, anyway.
    = parens (text "<wrap>" <+> parens (ppr e))

instance Outputable DsTArrExpr where
  ppr (TypedArr t1 t2 e)
    = hang (ppr e)
         2 (dcolon <+> text "a" <+> sep [pprParendType t1, pprParendType t2])

-- -------------------------------------------------------------------
-- DsTArrExpr constructors

-- | mkAppArr a in out = app :: a (a in out, in) out
mkAppArr :: Type -> Type -> Type -> DsTArrExpr
mkAppArr arrow_ty in_ty out_ty
  = TypedArr (mkBoxedTupleTy [mkAppTys arrow_ty [in_ty, out_ty], in_ty])
             out_ty AppArr

-- | mkExprArr in out e = e :: a in out
mkExprArr :: Type -> Type -> CoreExpr -> DsTArrExpr
mkExprArr in_ty out_ty expr = TypedArr in_ty out_ty (ExprArr expr)

-- | mkArrArr in out e = arr (e :: in -> out) :: a in out
mkArrArr :: Type -> Type -> CoreExpr -> DsTArrExpr
mkArrArr in_ty out_ty expr = TypedArr in_ty out_ty (ArrArr expr)

-- | mkFirstArr t e = first (e :: a in out) :: a (in, t) (out, t)
mkFirstArr :: HasDebugCallStack => Type -> DsTArrExpr -> DsTArrExpr
mkFirstArr snd_ty expr@(TypedArr in_ty out_ty _)
  = TypedArr (mkBoxedTupleTy [in_ty, snd_ty])
             (mkBoxedTupleTy [out_ty, snd_ty])
             (FirstArr expr)

-- | mkLoopArr e = loop (e :: a (in, t) (out, t)) :: a in out
mkLoopArr :: HasDebugCallStack => DsTArrExpr -> DsTArrExpr
mkLoopArr expr@(TypedArr in_ty out_ty _)
  | Just (pair_tc1, [in_ty', snd_ty1]) <- tcSplitTyConApp_maybe in_ty
  , Just (pair_tc2, [out_ty', snd_ty2]) <- tcSplitTyConApp_maybe out_ty
  = ASSERT2( isTupleTyCon pair_tc1 && isTupleTyCon pair_tc2, ppr expr )
    ASSERT2( snd_ty1 `eqType` snd_ty2, ppr expr )
    TypedArr in_ty' out_ty' (LoopArr expr)
  | otherwise
  = pprPanic "mkLoopArr" $ ppr expr

-- | mkComposeArr e1 e2 = ((e1 :: a in t) >>> (e2 :: a t out)) :: a in out
mkComposeArr :: HasDebugCallStack => DsTArrExpr -> DsTArrExpr -> DsTArrExpr
mkComposeArr expr1@(TypedArr in_ty mid_ty1 _) expr2@(TypedArr mid_ty2 out_ty _)
  = ASSERT2( mid_ty1 `eqType` mid_ty2, ppr expr1 $$ ppr expr2 )
    TypedArr in_ty out_ty (ComposeArr expr1 expr2)

-- | mkChoiceArr e1 e2
--     = ((e1 :: a in1 out) ||| (e2 :: a in2 out)) :: a (Either in1 in2) out
mkChoiceArr :: HasDebugCallStack => DsTArrExpr -> DsTArrExpr -> DsTArrExpr
mkChoiceArr expr1@(TypedArr in_ty1 out_ty1 _) expr2@(TypedArr in_ty2 out_ty2 _)
  = ASSERT2( out_ty1 `eqType` out_ty2, ppr expr1 $$ ppr expr2 )
    TypedArr (mkEitherTy in_ty1 in_ty2) out_ty1 (ChoiceArr expr1 expr2)

mkWrapArr :: (CoreExpr -> CoreExpr) -> DsTArrExpr -> DsTArrExpr
mkWrapArr wrap expr@(TypedArr in_ty out_ty _)
  = TypedArr in_ty out_ty $ WrapArr wrap expr

-- | mkPremapArr b f g = (arr (f :: b -> c) >>> (g :: a c d)) :: a b d
mkPremapArr :: HasDebugCallStack => Type -> CoreExpr -> DsTArrExpr -> DsTArrExpr
mkPremapArr in_ty f_expr g_expr@(TypedArr out_ty _ _)
  = mkComposeArr (mkArrArr in_ty out_ty f_expr) g_expr

-- -------------------------------------------------------------------
-- dsArrExpr

dsArrExpr :: CmdSyntaxTable GhcTc -> DsTArrExpr -> DsM CoreExpr
dsArrExpr stx_table expr = do
  meths <- mkArrMethods stx_table
  ds_arr_expr meths expr

ds_arr_expr :: DsArrMethods -> DsTArrExpr -> DsM CoreExpr
ds_arr_expr DsArrMethods{..} expr = ds_expr <$> tidy_up_typed expr
  where
    -- see Note [Tidying arrow expressions]
    tidy_up_typed :: DsTArrExpr -> DsM DsTArrExpr
    tidy_up_typed (TypedArr in_ty out_ty expr)
      = TypedArr in_ty out_ty <$> tidy_up expr

    -- see Note [Tidying arrow expressions]
    tidy_up :: DsArrExpr -> DsM DsArrExpr
    tidy_up AppArr        = pure AppArr
    tidy_up (ExprArr e)   = pure $ ExprArr e
    tidy_up (ArrArr e)    = pure $ ArrArr e
    tidy_up (FirstArr e)  = FirstArr <$> tidy_up_typed e
    tidy_up (LoopArr e)   = LoopArr <$> tidy_up_typed e
    tidy_up (WrapArr w e) = WrapArr w <$> tidy_up_typed e
    tidy_up (ChoiceArr e1 e2)
      = ChoiceArr <$> tidy_up_typed e1 <*> tidy_up_typed e2

    -- Reassociate (>>>) to the right:
    --   (e1 >>> e2) >>> e3  ==>  e1 >>> (e2 >>> e3)
    -- We want to do this on the way down, since it avoids doing
    -- needless work on sequences of left-associated (>>>)s.
    tidy_up (ComposeArr (TypedArr _ _ (ComposeArr e1 e2)) e3)
      = tidy_up $ ComposeArr e1 (mkComposeArr e2 e3)
    tidy_up (ComposeArr e1 e2)
      = do e1' <- tidy_up_typed e1
           e2' <- tidy_up_typed e2
           tidy_up_compose e1' e2'

    -- Fuses `arr` as described in Note [Tidying arrow expressions].
    -- We do this on the way back up, *after* reassociating (>>>), since
    -- the reassociation may uncover more opportunities for fusion.
    -- (Indeed, that is the entire reason we reassociate at all!)
    tidy_up_compose :: DsTArrExpr -> DsTArrExpr -> DsM DsArrExpr
    -- arr f >>> arr g  ==>  arr (f >>> g)
    tidy_up_compose (TypedArr t _ (ArrArr f)) (TypedArr _ _ (ArrArr g))
      = ArrArr <$> fns_compose t f g

    -- arr f >>> (arr g >>> e)  ==>  arr (f >>> g) >>> e
    tidy_up_compose (TypedArr a _ (ArrArr f))
                    (TypedArr _ _ (ComposeArr (TypedArr _ b (ArrArr g)) e))
      = do h <- fns_compose a f g
           pure $ ComposeArr (mkArrArr a b h) e

    -- We could easily do more rewritings here, such as
    --   first (arr f >>> e)  ==>  arr (first f) >>> first e
    -- but it’s not clear that they’d actually do much, and that would
    -- be encroaching even more on the optimiser’s responsibilities.
    tidy_up_compose e1 e2 = pure $ ComposeArr e1 e2

    -- fns_compose t f g = \(x :: t) -> g (f x)
    fns_compose arg_ty f g = do
      arg_id <- newSysLocalDs arg_ty
      pure $ Lam arg_id $ mkCoreApps g [mkCoreApps f [Var arg_id]]

    -- We’re done tidying, so it’s time to do the actual codegen.
    ds_expr :: DsTArrExpr -> CoreExpr
    ds_expr (TypedArr _ _ (ExprArr expr)) = expr
    ds_expr (TypedArr _ _ (WrapArr wrap expr)) = wrap $ ds_expr expr

    -- app :: forall b c. a (a b c, b) c
    ds_expr expr@(TypedArr in_ty out_ty AppArr)
      | Just (_, [_, in_ty']) <- tcSplitTyConApp_maybe in_ty
      = mkApps am_app [Type in_ty', Type out_ty]
      | otherwise = pprPanic "ds_expr: app" $ ppr expr

    -- arr :: forall b c. (b -> c) -> a b c
    ds_expr (TypedArr in_ty out_ty (ArrArr expr))
      = mkApps am_arr [Type in_ty, Type out_ty, expr]

    -- first :: forall b c d. a b c -> a (b, d) (c, d)
    ds_expr expr@(TypedArr in_ty out_ty (FirstArr expr'))
      | Just (_, [in_ty', snd_ty]) <- tcSplitTyConApp_maybe in_ty
      , Just (_, [out_ty', _]) <- tcSplitTyConApp_maybe out_ty
      = mkApps am_first [Type in_ty', Type out_ty', Type snd_ty, ds_expr expr']
      | otherwise = pprPanic "ds_expr: first" $ ppr expr

    -- loop :: forall b d c. a (b, d) (c, d) -> a b c
    ds_expr (TypedArr in_ty out_ty (LoopArr expr))
      | TypedArr in_ty' _ _ <- expr
      , (_, snd_ty) <- tcSplitAppTy in_ty'
      = mkApps am_loop [Type in_ty, Type snd_ty, Type out_ty, ds_expr expr]

    -- (>>>) :: forall b c d. a b c -> a c d -> a b d
    ds_expr (TypedArr in_ty out_ty (ComposeArr expr1 expr2))
      | TypedArr _ mid_ty _ <- expr1
      = mkApps am_compose [ Type in_ty, Type mid_ty, Type out_ty
                          , ds_expr expr1, ds_expr expr2 ]

    -- (|||) :: forall b d c. a b d -> a c d -> a (Either b c) d
    ds_expr (TypedArr _ out_ty (ChoiceArr expr1 expr2))
      | TypedArr left_ty _ _ <- expr1
      , TypedArr right_ty _ _ <- expr2
      = mkApps am_choice [ Type left_ty, Type out_ty, Type right_ty
                         , ds_expr expr1, ds_expr expr2 ]

-- -------------------------------------------------------------------
-- arrow methods

data DsArrMethods = DsArrMethods {
  am_arr, am_compose, am_first, am_app, am_choice, am_loop :: CoreExpr }

mkArrMethods :: CmdSyntaxTable GhcTc -> DsM DsArrMethods
-- See Note [CmdSyntaxTable] in GHC.Hs.Expr
mkArrMethods tc_meths = do
  prs <- traverse (traverse dsExpr) tc_meths

  -- NB: Some of these lookups might fail, but that's OK if the
  -- symbol is never used. That's why we use Maybe first and then
  -- panic. An eager panic caused trouble in typecheck/should_compile/tc192
  let mb_arr     = assocMaybe prs arrAName
      mb_compose = assocMaybe prs composeAName
      mb_first   = assocMaybe prs firstAName
      mb_app     = assocMaybe prs appAName
      mb_choice  = assocMaybe prs choiceAName
      mb_loop    = assocMaybe prs loopAName

  check_lev_poly 3 mb_arr
  check_lev_poly 5 mb_compose
  check_lev_poly 4 mb_first
  check_lev_poly 2 mb_app
  check_lev_poly 5 mb_choice
  check_lev_poly 4 mb_loop

  pure DsArrMethods
    { am_arr     = unmaybe mb_arr arrAName
    , am_compose = unmaybe mb_compose composeAName
    , am_first   = unmaybe mb_first firstAName
    , am_app     = unmaybe mb_app appAName
    , am_choice  = unmaybe mb_choice choiceAName
    , am_loop    = unmaybe mb_loop loopAName
    }
  where
    unmaybe Nothing name = pprPanic "mkArrMethods" (text "Not found:" <+> ppr name)
    unmaybe (Just exp) _ = exp

    -- returns the result type of a pi-type (that is, a forall or a function)
    -- Note that this result type may be ill-scoped.
    res_type :: Type -> Type
    res_type = snd . splitPiTy

    check_lev_poly :: Int -- arity
                   -> Maybe CoreExpr -> DsM ()
    check_lev_poly _     Nothing = return ()
    check_lev_poly arity (Just exp)
      = dsNoLevPoly (nTimes arity res_type (exprType exp))
          (text "In the result of the function" <+> quotes (ppr exp))
