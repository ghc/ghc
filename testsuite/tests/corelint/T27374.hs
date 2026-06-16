-- Regression test for #27374: optCoercion used to drop an ambient Sym on a
-- certain shape of (lint-valid) input coercion, namely a TyConAppCo
-- transitively composed with  Sym (SelCo:Fun(arg) (InstCo-chain over a Refl of
-- a forall-type)), where the instantiating arguments are Sym of an axiom-like
-- coercion ax :: [n] ~ n.  See Note [Ambient sym and InstCo] in
-- GHC.Core.Coercion.Opt for the fix.
--
-- We build the coercion by hand, feed it to optCoercion, then (a) check that
-- the optimiser preserved the coercion's kind and (b) Core-Lint the result.
-- We run this for two choices of ax, which used to expose the bug differently:
--
--   * ax = a free coercion variable.  Like a real type-family axiom, a CoVarCo
--     resists Sym-distribution (the optimiser keeps Sym on the *outside*), so
--     the dropped Sym used to stay visible as an ill-formed  Sym ax ; Sym ax
--     trans: the output was ill-kinded *and* Core Lint rejected it.
--
--   * ax = a UnivCo.  opt_univ freely flips its types, so the dropped Sym
--     collapsed into the coercion: the output was ill-kinded but still linted
--     clean.  This shows -dcore-lint alone does not catch the bug, which is why
--     a self-standing kind-preservation check is valuable.
--
-- The input is reflexive-but-not-Refl, so a correct optimiser reduces both to
-- Refl; we assert kind preservation and that both coercions lint.

module Main where

import System.Environment ( getArgs )
import System.Exit ( exitFailure )
import Control.Monad ( unless )
import Control.Monad.IO.Class ( liftIO )
import Data.Maybe ( isNothing )

import GHC
  ( runGhc, getSessionDynFlags, setSessionDynFlags, getLogger )
import GHC.Driver.Session
  ( GeneralFlag(Opt_SuppressUniques), gopt_set )

import GHC.Core ( Expr(Coercion) )
import GHC.Core.Coercion
  ( mkReflCo, mkTyConAppCo, mkSymCo, mkTransCo, mkSelCo, mkInstCo, mkCoVarCo
  , mkUnivCo, mkCoercionType, coercionKind )
import GHC.Core.Coercion.Opt
  ( optCoercion, OptCoercionOpts(..) )
import GHC.Core.Lint ( lintExpr )
import GHC.Driver.Config.Core.Lint ( initLintConfig )

import GHC.Core.TyCo.Rep ( UnivCoProvenance(PluginProv), CoSel(SelFun)
                         , FunSel(SelArg), mkVisFunTyMany )
import GHC.Core.Type ( Type, mkTyConApp, mkTyVarTy, mkSpecForAllTys )
import GHC.Core.TyCo.Compare ( eqType )
import GHC.Core.TyCo.Subst ( mkEmptySubst )

import GHC.Builtin.Types ( maybeTyCon, listTyCon, tupleTyCon, mkBoxedTupleTy )
import GHC.Builtin.Types.Prim ( alphaTyVars )

import GHC.Types.Var ( TyVar, CoVar, mkCoVar )
import GHC.Types.Var.Set ( mkVarSet )
import GHC.Types.Var.Env ( mkInScopeSet )
import GHC.Types.Name ( mkSystemName )
import GHC.Types.Name.Occurrence ( mkVarOcc )
import GHC.Builtin.Uniques ( mkBuiltinUnique )
import GHC.Types.Basic ( Boxity(Boxed) )
import GHC.Core.TyCon ( TyCon )
import GHC.Core.Coercion ( Role(Nominal) )
import GHC.Core.TyCo.Rep ( Coercion )

import GHC.Data.Pair ( Pair(..) )
import GHC.Utils.Outputable
  ( ($$), (<+>), text, ppr, nest, blankLine, SDoc )
import GHC.Utils.Error ( pprMessageBag, putMsg )

--------------------------------------------------------------------------------

-- A fresh free type variable n, and three forall-bound vars a, b, c, all :: Type
nv, av, bv, cv :: TyVar
(nv : av : bv : cv : _) = alphaTyVars

nTy :: Type
nTy = mkTyVarTy nv

tup3 :: TyCon
tup3 = tupleTyCon Boxed 3

-- The two axiom-like coercions  ax :: [n] ~ n   (the [n] stands in for a
-- type-family redex such as  Dim (Vec n)).  See the header for why these two
-- choices expose the bug differently.
axv :: CoVar
axv = mkCoVar (mkSystemName (mkBuiltinUnique 712) (mkVarOcc "ax"))
              (mkCoercionType Nominal (mkTyConApp listTyCon [nTy]) nTy)

axCoVar, axUniv :: Coercion
axCoVar = mkCoVarCo axv
axUniv  = mkUnivCo (PluginProv "T27374") [] Nominal
                   (mkTyConApp listTyCon [nTy]) nTy

-- Three differently-shaped wrappers, mirroring the (ArityPeano (Peano _)),
-- (KnownNat _), ((~) ... _ ... _ ...) shapes in the original report.
wrapA, wrapB, wrapC :: Coercion -> Coercion
wrapA co = mkTyConAppCo Nominal maybeTyCon [mkTyConAppCo Nominal maybeTyCon [co]]
wrapB co = mkTyConAppCo Nominal maybeTyCon [co]
wrapC co = mkTyConAppCo Nominal (tupleTyCon Boxed 2) [co, co]

-- forall (a b c :: Type). (a, b, c) -> a
forallTy :: Type
forallTy = mkSpecForAllTys [av, bv, cv] $
           mkVisFunTyMany (mkBoxedTupleTy [mkTyVarTy av, mkTyVarTy bv, mkTyVarTy cv])
                          (mkTyVarTy av)

-- The triggering input coercion, parameterised over the axiom-like ax.
mkInCo :: Coercion -> Coercion
mkInCo ax = mkTransCo co1 co2
  where
    argA = mkSymCo (wrapA ax)  -- arg_x :: wrap_x n ~ wrap_x [n]
    argB = mkSymCo (wrapB ax)
    argC = mkSymCo (wrapC ax)
    forallRefl = mkReflCo Nominal forallTy
    inner = mkInstCo (mkInstCo (mkInstCo forallRefl argA) argB) argC
    co2   = mkSymCo (mkSelCo (SelFun SelArg) inner)
    co1   = mkTyConAppCo Nominal tup3 [argA, argB, argC]

--------------------------------------------------------------------------------

main :: IO ()
main = do
  [libdir] <- getArgs
  runGhc (Just libdir) $ do
    getSessionDynFlags >>= setSessionDynFlags . flip gopt_set Opt_SuppressUniques
    dflags <- getSessionDynFlags
    logger <- getLogger
    liftIO $ do
      let in_scope  = mkInScopeSet (mkVarSet [nv, axv])
          subst     = mkEmptySubst in_scope
          opts      = OptCoercionOpts { optCoercionEnabled = True }
          lintCfg   = initLintConfig dflags [nv, axv]

          report mb = case mb of
                        Nothing   -> text "lint: OK"
                        Just errs -> pprMessageBag errs

          -- (ok, doc) for one choice of axiom-like coercion.  `ok` holds iff
          -- optCoercion preserved the kind and both coercions Core-Lint.
          check :: String -> Coercion -> (Bool, SDoc)
          check label inCo = (kindOK && lintOK, doc)
            where
              out_co      = optCoercion opts subst inCo
              Pair il ir  = coercionKind inCo
              Pair ol orr = coercionKind out_co
              kindOK      = il `eqType` ol && ir `eqType` orr
              lintIn      = lintExpr lintCfg (Coercion inCo)
              lintOut     = lintExpr lintCfg (Coercion out_co)
              lintOK      = isNothing lintIn && isNothing lintOut
              doc = text label
                 $$ nest 2 (text "in_co:"  <+> ppr inCo
                         $$ text "out_co:" <+> ppr out_co
                         $$ text "in_ty1:" <+> ppr il
                         $$ text "in_ty2:" <+> ppr ir
                         $$ text "out_ty1:" <+> ppr ol
                         $$ text "out_ty2:" <+> ppr orr
                         $$ text "kind preserved:" <+> ppr kindOK
                         $$ text "lint in_co:"  <+> report lintIn
                         $$ text "lint out_co:" <+> report lintOut)

          (okCoVar, docCoVar) = check "covar ax (was: ill-kinded, lint-rejected):"
                                      (mkInCo axCoVar)
          (okUniv,  docUniv)  = check "univ ax (was: ill-kinded, lint-clean):"
                                      (mkInCo axUniv)
      putMsg logger (docCoVar $$ blankLine $$ docUniv)
      -- Fail (non-zero exit) if optCoercion did not preserve the kind, or if
      -- either coercion failed to lint, in either case.  See #27374.
      unless (okCoVar && okUniv) exitFailure
