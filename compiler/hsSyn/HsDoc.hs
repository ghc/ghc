{-# LANGUAGE DeriveDataTypeable #-}

module HsDoc (
  HsDocString(..),
  LHsDocString,
  ppr_mbDoc
  ) where

#include "HsVersions.h"

import Outputable
import SrcLoc
import FastString

import Data.Data

newtype HsDocString = HsDocString FastString
  deriving (Eq, Show, Data, Typeable)

type LHsDocString = Located HsDocString

instance Outputable HsDocString where
  ppr _ = text "<document comment>"

ppr_mbDoc :: Maybe LHsDocString -> SDoc
ppr_mbDoc (Just doc) = ppr doc
ppr_mbDoc Nothing    = empty

