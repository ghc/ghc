{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TupleSections #-}

-- | Adds cost-centers after the core piple has run.
module GHC.Core.LateCC
    ( addLateCostCentres
    ) where

import Control.Applicative
import GHC.Utils.Monad.State.Strict
import Control.Monad

import GHC.Prelude
import GHC.Driver.Session
import GHC.Types.CostCentre
import GHC.Types.CostCentre.State
import GHC.Types.Name hiding (varName)
import GHC.Types.Tickish
import GHC.Unit.Module.ModGuts
import GHC.Types.Var
import GHC.Unit.Types
import GHC.Data.FastString
import GHC.Core
import GHC.Core.Opt.Monad
import GHC.Types.Id
import GHC.Core.Utils (mkTick)

addLateCostCentres :: ModGuts -> CoreM ModGuts
addLateCostCentres guts = do
  dflags <- getDynFlags
  let env :: Env
      env = Env
        { thisModule = mg_module guts
        , ccState = newCostCentreState
        , dflags = dflags
        }
  let guts' = guts { mg_binds = doCoreProgram env (mg_binds guts)
                   }
  return guts'

doCoreProgram :: Env -> CoreProgram -> CoreProgram
doCoreProgram env binds = flip evalState newCostCentreState $ do
    mapM (doBind env) binds

doBind :: Env -> CoreBind -> M CoreBind
doBind env (NonRec b rhs) = NonRec b <$> doBndr env b rhs
doBind env (Rec bs) = Rec <$> mapM doPair bs
  where
    doPair :: ((Id, CoreExpr) -> M (Id, CoreExpr))
    doPair (b,rhs) = (b,) <$> doBndr env b rhs

doBndr :: Env -> Id -> CoreExpr -> M CoreExpr
doBndr env bndr rhs = do
    let name = idName bndr
        name_loc = nameSrcSpan name
        cc_name = getOccFS name
        count = gopt Opt_ProfCountEntries (dflags env)
    cc_flavour <- getCCExprFlavour cc_name
    let cc_mod = thisModule env
        bndrCC = NormalCC cc_flavour cc_name cc_mod name_loc
        note = ProfNote bndrCC count True
    return $ mkTick note rhs

type M = State CostCentreState

getCCExprFlavour :: FastString -> M CCFlavour
getCCExprFlavour name = ExprCC <$> getCCIndex' name

getCCIndex' :: FastString -> M CostCentreIndex
getCCIndex' name = state (getCCIndex name)

data Env = Env
  { thisModule  :: Module
  , dflags      :: DynFlags
  , ccState     :: CostCentreState
  }

