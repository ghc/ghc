{-# LANGUAGE OverloadedStrings #-}

module GHC.StgToJS.UnitUtils
  ( unitModuleString
  , moduleGlobalSymbol
  , moduleExportsSymbol
  ) where

import GHC.Prelude

import GHC.Data.ShortText as ST
import GHC.Unit.Module
import GHC.Utils.Encoding

unitModuleString :: Module -> String
unitModuleString mod = mconcat
  [ unitIdString (moduleUnitId mod)
  , ":"
  , moduleNameString (moduleName mod)
  ]

-- | the global linkable unit of a module exports this symbol, depend on it to
--   include that unit (used for cost centres)
moduleGlobalSymbol :: Module -> ShortText
moduleGlobalSymbol m = mconcat
  [ "h$"
  , ST.pack (zEncodeString $ unitModuleString m)
  , "_<global>"
  ]

moduleExportsSymbol :: Module -> ShortText
moduleExportsSymbol m = mconcat
  [ "h$"
  , ST.pack (zEncodeString $ unitModuleString m)
  , "_<exports>"
  ]
