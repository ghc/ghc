{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}

module Oracles.Flavour
  ( oracles
  , askDynGhcPrograms
  , askGhcProfiled
  ) where

import Base
import Flavour
import Settings (flavour)
import Oracles.Setting

newtype DynGhcPrograms =
  DynGhcPrograms Stage deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
type instance RuleResult DynGhcPrograms = Bool

newtype GhcProfiled =
  GhcProfiled Stage deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
type instance RuleResult GhcProfiled = Bool

oracles :: Rules ()
oracles = do
  void $ addOracle $ \(DynGhcPrograms stage) -> do
    cross <- crossStage stage
    from_flavour <- flip dynamicGhcPrograms stage =<< flavour
    return (from_flavour && not cross)
  void $ addOracle $ \(GhcProfiled stage) ->
    ghcProfiled <$> flavour <*> pure (succStage stage)

askDynGhcPrograms :: Stage -> Action Bool
askDynGhcPrograms s = askOracle $ DynGhcPrograms s

askGhcProfiled :: Stage -> Action Bool
askGhcProfiled s = askOracle $ GhcProfiled s
