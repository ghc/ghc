{-# LANGUAGE BlockArguments #-}

-- | Regression test for #27374:
--
--  - manually construct some coercions that are not InstCo but become InstCo
--    after optimisation
--  - check that the coercion optimiser properly optimises these by running
--    Core Lint on the output

module Main (main) where

-- base 
import Control.Monad.IO.Class
  ( liftIO )
import Data.Foldable
  ( for_ )
import System.Environment
  ( getArgs )

-- ghc
import GHC
  ( runGhc, getSessionDynFlags, getLogger )
import GHC.Builtin.Types
  ( maybeTyCon, tupleTyCon, mkBoxedTupleTy )
import GHC.Core
  ( Expr(Coercion) )
import GHC.Core.Coercion
  ( Coercion, CoVar, MCoercion(..)
  , Role(..), CoSel(..), FunSel(..)
  , mkCoercionType
  , mkCoVar, mkCoVarCo, mkForAllCo, mkInstCo, mkReflCo, mkNomReflCo
  , mkSelCo, mkSymCo, mkTransCo, mkTyConAppCo
  , tyCoVarsOfCo
  )
import GHC.Core.Coercion.Opt
  ( OptCoercionOpts(..), optCoercion )
import GHC.Core.Lint        
  ( lintExpr )
import GHC.Core.Predicate   
  ( mkNomEqPred )
import GHC.Core.TyCo.FVs    
  ( tyCoVarsOfCoList )
import GHC.Core.TyCo.Rep    
  ( Type(CoercionTy) )
import GHC.Core.Type

import GHC.Data.FastString
  ( fsLit )

import GHC.Driver.Config.Core.Lint
  ( initLintConfig )

import GHC.Types.Basic
  ( Boxity(Boxed) )
import GHC.Types.Var
  ( mkTyVar, coreTyLamForAllTyFlag )
import GHC.Types.Var.Env        
  ( mkInScopeSet )
import GHC.Types.Name           
  ( mkInternalName )
import GHC.Types.Name.Occurrence
  ( mkTyVarOccFS, mkVarOccFS )
import GHC.Types.SrcLoc         
  ( noSrcSpan )
import GHC.Types.Unique         
  ( mkUniqueGrimily )

import GHC.Utils.Error     
  ( pprMessageBag, putMsg )
import GHC.Utils.Outputable
  ( (<+>), text, vcat )

--------------------------------------------------------------------------------
-- Helpers for building fresh variables
--------------------------------------------------------------------------------

mkTv :: Int -> String -> Kind -> TyVar
mkTv i s =
  mkTyVar $
    mkInternalName
      ( mkUniqueGrimily (fromIntegral i) )
      ( mkTyVarOccFS (fsLit s) )
      noSrcSpan

mkCv :: Int -> String -> Type -> CoVar
mkCv i s =
  mkCoVar $
    mkInternalName
      ( mkUniqueGrimily (fromIntegral i) )
      ( mkVarOccFS (fsLit s) )
      noSrcSpan

--------------------------------------------------------------------------------
-- ForAllCo-over-type
--------------------------------------------------------------------------------

co1 :: Coercion
co1 =
  -- Use mkTransCo to get a coercion that is not InstCo but optimises to InstCo
  lhs `mkTransCo` rhs 
  where
    p = mkTv 21 "p" liftedTypeKind
    q = mkTv 22 "q" liftedTypeKind
    a = mkTv 23 "a" liftedTypeKind
    b = mkTv 24 "b" liftedTypeKind

    tup2 = tupleTyCon Boxed 2

    -- cv :: p ~# q
    cv = mkCoVarCo $
           mkCv 25 "cv" $
             mkCoercionType Nominal (mkTyVarTy p) (mkTyVarTy q)

    -- arg :: Maybe q ~ Maybe p
    arg = mkSymCo (mkTyConAppCo Nominal maybeTyCon [cv])

    -- forallCo = <forall a b. (a, b) -> a>_N
    forallCo =
      mkReflCo Nominal $
        mkSpecForAllTys [a, b] $
          mkVisFunTyMany
            (mkBoxedTupleTy [mkTyVarTy a, mkTyVarTy b])
            (mkTyVarTy a)

    -- forallCo @arg @arg
    inst_co = mkInstCo (mkInstCo forallCo arg) arg

    rhs   = mkSymCo (mkSelCo (SelFun SelArg) inst_co)
    lhs   = mkTyConAppCo Nominal tup2 [arg, arg]

--------------------------------------------------------------------------------
-- ForAllCo-over-coercion
--------------------------------------------------------------------------------

co2 :: Coercion
co2 =
  -- This outer 'sym' was necessary to trigger the specific bug
  mkSymCo $
    mkInstCo ( mkSymCo g ) arg
  where
    k1  = mkTyVarTy $ mkTv 11 "k1" liftedTypeKind
    k2  = mkTyVarTy $ mkTv 12 "k2" liftedTypeKind

    x   = mkTv 13 "x" k1

    -- cv :: k1 ~# Type
    cv_ty = mkNomEqPred k1 liftedTypeKind
    cv    = mkCv 14 "cv" cv_ty

    -- hk :: (k1 ~# Type) ~# (k2 ~# Type)
    hk  = mkCoVarCo (mkCv 15 "hk" (mkNomEqPred cv_ty (mkNomEqPred k2 liftedTypeKind)))

    -- body :: (x |> cv) ~# (x |> cv)
    body = mkNomReflCo (mkCastTy (mkTyVarTy x) (mkCoVarCo cv))

    -- g :: (forall (cv  :: k1 ~# Type). x |> cv) 
    --   ~# (forall (cv' :: k2 ~# Type). x |> (sel 2 hk ; cv' ; sym (sel 3 hk)))
    g   = mkForAllCo cv vis vis (MCo hk) body
    vis = coreTyLamForAllTyFlag

    -- two distinct coercions to instantiate at
    coL = mkCoVarCo (mkCv 16 "coL" (mkNomEqPred k1 liftedTypeKind))
    coR = mkCoVarCo (mkCv 17 "coR" (mkNomEqPred k2 liftedTypeKind))
    arg = mkCoVarCo (mkCv 18 "arg" (mkNomEqPred (CoercionTy coL) (CoercionTy coR)))

--------------------------------------------------------------------------------
-- Test runner
--------------------------------------------------------------------------------

test_cos :: [ ( String, Coercion ) ]
test_cos =
  [ ( "InstCo (TyVar)", co1 )
  , ( "InstCo (CoVar)", co2 )
  ]

opt_co :: Coercion -> Coercion
opt_co co =
  let in_scope = mkInScopeSet $ tyCoVarsOfCo co
  in
    optCoercion
      ( OptCoercionOpts { optCoercionEnabled = True } )
      ( mkEmptySubst in_scope )
      co

main :: IO ()
main = do
  [libdir] <- getArgs
  runGhc ( Just libdir ) do
    dflags <- getSessionDynFlags
    logger <- getLogger
    liftIO $ for_ test_cos \ ( name, co ) -> do
      -- Optimise each test coercion and run the result through Core Lint.
      let
        co'      = opt_co co
        in_scope = tyCoVarsOfCoList co'
        lint_cfg = initLintConfig dflags in_scope
      for_ ( lintExpr lint_cfg ( Coercion co' ) ) \ errs ->
        putMsg logger $
          vcat [ text "Core Lint error for" <+> text name
               , pprMessageBag errs
               ]
