{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module ForeignCallDecl
  ( ForeignCallDecl(..)
  , ForeignCallOptions(..)
  , isSafeForeignCallDecl
  ) where

import GhcPrelude

import Binary
import Demand
import Outputable
import ForeignCall

import Data.Data

data ForeignCallDecl = CCall
  { fcd_spec :: CCallSpec
  , fcd_options :: ForeignCallOptions
  }
  deriving (Eq, Data)

data ForeignCallOptions = ForeignCallOptions
  { fco_hasSideEffects :: Bool
  , fco_canFail :: Bool
  , fco_commutable :: Bool
  , fco_strictness :: StrictSig
  }
  deriving (Eq, Data)

isSafeForeignCallDecl :: ForeignCallDecl -> Bool
isSafeForeignCallDecl (CCall (CCallSpec _ _ safe) _) = playSafe safe

-- We may need more clues to distinguish foreign calls
-- but this simple printer will do for now
instance Outputable ForeignCallDecl where
  ppr (CCall cc spec)  = ppr cc <+> "where" $+$ ppr spec

instance Outputable ForeignCallOptions where
  ppr (ForeignCallOptions se cf c s) = vcat
    [ "has_side_effects" <+> "=" <+> ppr se
    , "can_fail" <+> "=" <+> ppr cf
    , "commutable" <+> "=" <+> ppr c
    , "strictness" <+> "=" <+> ppr s
    ]

instance Binary ForeignCallDecl where
  put_ bh (CCall spec strict_sig) = do
    put_ bh spec
    put_ bh strict_sig
  get bh = CCall <$> get bh <*> get bh

instance Binary ForeignCallOptions where
  put_ bh (ForeignCallOptions se cf c s) = do
    put_ bh se
    put_ bh cf
    put_ bh c
    put_ bh s
  get bh = ForeignCallOptions
    <$> get bh <*> get bh <*> get bh <*> get bh
