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

newtype DynGhcPrograms =
  DynGhcPrograms () deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
type instance RuleResult DynGhcPrograms = Bool

newtype GhcProfiled =
  GhcProfiled Stage deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
type instance RuleResult GhcProfiled = Bool

oracles :: Rules ()
oracles = do
  void $ addOracle $ \(DynGhcPrograms _) -> dynamicGhcPrograms =<< flavour
  void $ addOracle $ \(GhcProfiled stage) -> ghcProfiled <$> flavour <*> pure stage

askDynGhcPrograms :: Action Bool
askDynGhcPrograms = askOracle $ DynGhcPrograms ()

askGhcProfiled :: Stage -> Action Bool
askGhcProfiled s = askOracle $ GhcProfiled s
