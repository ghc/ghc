{-# LANGUAGE OverloadedStrings #-}

module GHC.StgToJS.UnitUtils
  ( unitModuleString
  , moduleGlobalSymbol
  , moduleExportsSymbol
  ) where

import GHC.Prelude

import GHC.Data.FastString
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
moduleGlobalSymbol :: Module -> FastString
moduleGlobalSymbol m = mconcat -- TODO: Is there a better way to concatenate FastStrings?
  [ "h$"
  , mkFastString (zEncodeString $ unitModuleString m)
  , "_<global>"
  ]

moduleExportsSymbol :: Module -> FastString
moduleExportsSymbol m = mconcat -- TODO: Is there a better way to concatenate FastStrings?
  [ "h$"
  , mkFastString (zEncodeString $ unitModuleString m)
  , "_<exports>"
  ]
