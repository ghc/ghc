{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE CPP, DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

module HsDoc
  ( HsDocString
      , pattern HsDocString
  , LHsDocString
  , ppr_mbDoc
  ) where

#include "HsVersions.h"

import Outputable
import SrcLoc ()
import FastString
import Data.Data

import AST

-- -----------------------------------------------------------------------------
-- * Data Declarations
-- -----------------------------------------------------------------------------

-- | Haskell Documentation String
type HsDocString  = DocString
pattern
   HsDocString :: FastString -> HsDocString
pattern
   HsDocString a = DocString a

{-# COMPLETE
      HsDocString
  #-}

-- | Located Haskell Documentation String
type LHsDocString = LDocString

-- -----------------------------------------------------------------------------
-- * Utilities
-- -----------------------------------------------------------------------------

deriving instance
  Data HsDocString

-- ------------------------------------

deriving instance
  Eq   HsDocString
deriving instance
  Show HsDocString

-- -----------------------------------------------------------------------------
-- * Pretty Printing
-- -----------------------------------------------------------------------------

instance Outputable HsDocString where
  ppr (HsDocString fs) = ftext fs

ppr_mbDoc :: Maybe LHsDocString -> SDoc
ppr_mbDoc (Just doc) = ppr doc
ppr_mbDoc Nothing    = empty
