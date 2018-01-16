{-# LANGUAGE DeriveDataTypeable #-}

module T4003A where

import T4003B

import Data.Data

data MyId = MyId
    deriving (Data, Typeable)

data HsExpr id
  = HsOverLit (HsOverLit id)
  | HsBracketOut (HsExpr MyId)
  deriving (Data, Typeable)
