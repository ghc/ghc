{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TupleSections #-}

-- | Adds cost-centers to call sites selected with the @-fprof-caller=...@
-- flag.
module GHC.Core.Opt.CallerCC
    ( addCallerCostCentres
    , CallerCcFilter(..)
    , NamePattern(..)
    , parseCallerCcFilter
    ) where

import Data.Maybe

import Control.Applicative
import GHC.Utils.Monad.State.Strict
import Control.Monad

import GHC.Prelude
import GHC.Utils.Outputable as Outputable
import GHC.Driver.DynFlags
import GHC.Types.CostCentre
import GHC.Types.CostCentre.State
import GHC.Types.Name hiding (varName)
import GHC.Types.Tickish
import GHC.Unit.Module.ModGuts
import GHC.Types.SrcLoc
import GHC.Types.Var
import GHC.Unit.Types
import GHC.Data.FastString
import GHC.Core
import GHC.Core.Opt.Monad
import GHC.Core.Opt.CallerCC.Types


addCallerCostCentres :: ModGuts -> CoreM ModGuts
addCallerCostCentres guts = do
  dflags <- getDynFlags
  let filters = callerCcFilters dflags
  let env :: Env
      env = Env
        { thisModule = mg_module guts
        , ccState = newCostCentreState
        , countEntries = gopt Opt_ProfCountEntries dflags
        , revParents = []
        , filters = filters
        }
  let guts' = guts { mg_binds = doCoreProgram env (mg_binds guts)
                   }
  return guts'

doCoreProgram :: Env -> CoreProgram -> CoreProgram
doCoreProgram env binds = flip evalState newCostCentreState $ do
    mapM (doBind env) binds

doBind :: Env -> CoreBind -> M CoreBind
doBind env (NonRec b rhs) = NonRec b <$> doExpr (addParent b env) rhs
doBind env (Rec bs) = Rec <$> mapM doPair bs
  where
    doPair (b,rhs) = (b,) <$> doExpr (addParent b env) rhs

doExpr :: Env -> CoreExpr -> M CoreExpr
doExpr env e@(Var v)
  | needsCallSiteCostCentre env v = do
    let nameDoc :: SDoc
        nameDoc = withUserStyle alwaysQualify DefaultDepth $
          hcat (punctuate dot (map ppr (parents env))) <> parens (text "calling:" <> ppr v)

        ccName :: CcName
        ccName = mkFastString $ renderWithContext defaultSDocContext nameDoc
    ccIdx <- getCCIndex' ccName
    let count = countEntries env
        span = case revParents env of
          top:_ -> nameSrcSpan $ varName top
          _     -> noSrcSpan
        cc = NormalCC (mkExprCCFlavour ccIdx) ccName (thisModule env) span
        tick :: CoreTickish
        tick = ProfNote cc count True
    pure $ Tick tick e
  | otherwise = pure e
doExpr _env e@(Lit _)       = pure e
doExpr env (f `App` x)      = App <$> doExpr env f <*> doExpr env x
doExpr env (Lam b x)        = Lam b <$> doExpr env x
doExpr env (Let b rhs)      = Let <$> doBind env b <*> doExpr env rhs
doExpr env (Case scrut b ty alts) =
    Case <$> doExpr env scrut <*> pure b <*> pure ty <*> mapM doAlt alts
  where
    doAlt (Alt con bs rhs)  = Alt con bs <$> doExpr env rhs
doExpr env (Cast expr co)   = Cast <$> doExpr env expr <*> pure co
doExpr env (Tick t e)       = Tick t <$> doExpr env e
doExpr _env e@(Type _)      = pure e
doExpr _env e@(Coercion _)  = pure e

type M = State CostCentreState

getCCIndex' :: FastString -> M CostCentreIndex
getCCIndex' name = state (getCCIndex name)

data Env = Env
  { thisModule  :: Module
  , countEntries :: !Bool
  , ccState     :: CostCentreState
  , revParents  :: [Id]
  , filters     :: [CallerCcFilter]
  }

addParent :: Id -> Env -> Env
addParent i env = env { revParents = i : revParents env }

parents :: Env -> [Id]
parents env = reverse (revParents env)

needsCallSiteCostCentre :: Env -> Id -> Bool
needsCallSiteCostCentre env i =
    any matches (filters env)
  where
    matches :: CallerCcFilter -> Bool
    matches ccf =
        checkModule && checkFunc
      where
        checkModule =
          case ccfModuleName ccf of
            Just modFilt
              | Just iMod <- nameModule_maybe (varName i)
              -> moduleName iMod == modFilt
              | otherwise -> False
            Nothing -> True
        checkFunc =
            occNameMatches (ccfFuncName ccf) (getOccName i)

