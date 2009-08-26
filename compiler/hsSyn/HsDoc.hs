module HsDoc (
  HsDocString(..),
  LHsDocString,
  ppr_mbDoc
  ) where

#include "HsVersions.h"

import Outputable
import SrcLoc
import FastString

newtype HsDocString = HsDocString FastString
  deriving (Eq, Show)

type LHsDocString = Located HsDocString

instance Outputable HsDocString where
  ppr _ = text "<document comment>"

ppr_mbDoc :: Maybe LHsDocString -> SDoc
ppr_mbDoc (Just doc) = ppr doc
ppr_mbDoc Nothing    = empty

