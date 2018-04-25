
{-# LANGUAGE DeriveDataTypeable #-}

module T4003B where

import {-# SOURCE #-} T4003A (HsExpr)

import Data.Data

data HsLit = HsChar
  deriving (Data, Typeable)

data HsOverLit id
  = OverLit (HsExpr id)
  deriving (Data, Typeable)

data OverLitVal = HsIntegral
  deriving (Data, Typeable)

