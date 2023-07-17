{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- base
import Control.Monad
  ( forM_ )
import Control.Monad.IO.Class
  ( liftIO )
import System.Environment
  ( getArgs )

-- ghc
import GHC
  ( runGhc
  , getSessionDynFlags, setSessionDynFlags
  , getLogger
  )

import GHC.Builtin.Types
  ( intTy
  , liftedDataConTy, liftedRepTy
  )
import GHC.Builtin.PrimOps
  ( PrimOp(CatchOp, RaiseOp) )
import GHC.Builtin.PrimOps.Ids
  ( primOpId )
import GHC.Builtin.Types.Prim
  ( runtimeRep1Ty, runtimeRep1TyVar
  , openAlphaTy, openAlphaTyVar
  )

import GHC.Core
  ( CoreExpr, Expr(Var, Type)
  , mkApps
  )
import GHC.Core.Lint
  ( lintExpr )
import GHC.Core.Type
  ( mkVisFunTyMany )

import GHC.Driver.Config.Core.Lint
import GHC.Driver.Session
  ( GeneralFlag(Opt_SuppressUniques), gopt_set )

import GHC.Types.Id.Make
  ( coerceId )
import GHC.Types.Var ( Id )

import GHC.Utils.Error
  ( pprMessageBag, putMsg )
import GHC.Utils.Outputable
  ( (<+>), ($$), text )

--------------------------------------------------------------------------------

test_exprs :: [ ( String, CoreExpr ) ]
test_exprs  =
  -- coerce :: forall {r1} {r2} (a :: TYPE r1) (b :: TYPE r2)
  --        .  Coercible a b => a -> b
  [ ("coerce OK", ) $
      -- coerce @LiftedRep
      mkApps (Var coerceId)
        [ Type liftedRepTy ]
  , ("coerce BAD 1", ) $
      -- coerce
      Var coerceId
  , ("coerce BAD 2", ) $
      -- coerce @r
      mkApps (Var coerceId)
        [ Type runtimeRep1Ty ]

  -- raise#
  --   :: forall {l} {r} (a :: TYPE (BoxedRep l)) (b :: TYPE r). a -> b
  , ("raise# OK 1", ) $
      -- raise# @Lifted @LiftedRep @Int @(z -> z), where z :: TYPE r
      mkApps (Var $ primOpId RaiseOp)
        [ Type liftedDataConTy
        , Type liftedRepTy
        , Type intTy
        , Type $ mkVisFunTyMany openAlphaTy openAlphaTy
        ]
  , ("raise# OK 2", ) $
      -- raise# @Lifted @r
      mkApps (Var $ primOpId RaiseOp)
        [ Type liftedDataConTy
        , Type runtimeRep1Ty
        ]

  -- catch# :: forall {q} {k} (a :: TYPE q) (b :: TYPE (BoxedRep k)). IO# a -> (b -> IO# a) -> IO# a,
  -- where IO# a is shorthand for  State# RealWorld -> (# State# RealWorld, a #)
  , ("catch# BAD 1", ) $
      -- catch#
      (Var $ primOpId CatchOp)
  , ("catch# BAD 2", ) $
      -- catch# @r @Lifted
      mkApps (Var $ primOpId CatchOp)
        [ Type runtimeRep1Ty
        , Type liftedDataConTy ]
  , ("catch# OK", ) $
      -- catch# @LiftedRep @Lifted
      mkApps (Var $ primOpId CatchOp)
        [ Type liftedRepTy
        , Type liftedDataConTy ]
  ]

-- These will be considered in-scope by the Core Lint checks.
in_scope :: [ Id ]
in_scope = [ runtimeRep1TyVar, openAlphaTyVar ]

main :: IO ()
main = do
  [libdir] <- getArgs
  runGhc (Just libdir) do
    getSessionDynFlags >>= setSessionDynFlags . flip gopt_set Opt_SuppressUniques
    dflags <- getSessionDynFlags
    logger <- getLogger
    liftIO do
      forM_ test_exprs \ ( test_name, expr ) ->
        forM_ ( lintExpr (initLintConfig dflags in_scope) expr ) \ errs ->
          putMsg logger
            ( pprMessageBag errs $$ text "in" <+> text test_name )
