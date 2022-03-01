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
  ( PrimOp(RaiseOp) )
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
  [ ("coerce OK", ) $
      -- coerce @LiftedRep
      mkApps (Var coerceId)
        [ Type liftedRepTy ]
  , ("coerce BAD 1", ) $
      -- coerce
      mkApps (Var coerceId) []
  , ("coerce BAD 2", ) $
      -- coerce @r
      mkApps (Var coerceId)
        [ Type runtimeRep1Ty ]
  , ("raise# OK", ) $
      -- raise# @Lifted @LiftedRep @Int @(z -> z), where z :: TYPE r
      mkApps (Var $ primOpId RaiseOp)
        [ Type liftedDataConTy
        , Type liftedRepTy
        , Type intTy
        , Type $ mkVisFunTyMany openAlphaTy openAlphaTy
        ]
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
        forM_ ( lintExpr dflags in_scope expr ) \ errs ->
          putMsg logger
            ( pprMessageBag errs $$ text "in" <+> text test_name )
