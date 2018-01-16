{-# LANGUAGE CPP, DeriveDataTypeable #-}

module HsDoc (
  HsDocString(..),
  LHsDocString,
  ppr_mbDoc
  ) where

#include "HsVersions.h"

import GhcPrelude

import Outputable
import SrcLoc
import FastString

import Data.Data

-- | Haskell Documentation String
newtype HsDocString = HsDocString FastString
  deriving (Eq, Show, Data)

-- | Located Haskell Documentation String
type LHsDocString = Located HsDocString

instance Outputable HsDocString where
  ppr (HsDocString fs) = ftext fs

ppr_mbDoc :: Maybe LHsDocString -> SDoc
ppr_mbDoc (Just doc) = ppr doc
ppr_mbDoc Nothing    = empty

