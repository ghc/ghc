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
  GhcProfiled () deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
type instance RuleResult GhcProfiled = Bool

oracles :: Rules ()
oracles = do
  void $ addOracle $ \(DynGhcPrograms _) -> dynamicGhcPrograms =<< flavour
  void $ addOracle $ \(GhcProfiled _) -> ghcProfiled <$> flavour

askDynGhcPrograms :: Action Bool
askDynGhcPrograms = askOracle $ DynGhcPrograms ()

askGhcProfiled :: Action Bool
askGhcProfiled = askOracle $ GhcProfiled ()
