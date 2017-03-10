{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module T13394 where

import Data.ByteString

newtype ProperName =
  ProperName { runProperName :: ByteString
               -- purescript actually uses the Text type, but this works
               -- just as well for the purposes of illustrating the bug
             }
newtype ModuleName = ModuleName [ProperName]

pattern TypeDataSymbol :: ModuleName
pattern TypeDataSymbol = ModuleName [ProperName "Type"]
