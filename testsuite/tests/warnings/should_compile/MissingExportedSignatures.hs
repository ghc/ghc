{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wmissing-exported-signatures #-}
{-# OPTIONS_GHC -Wmissing-pattern-synonym-signatures #-}
module MissingExportedSignatures (pattern Yes, foo) where

pattern Yes = True

pattern No = False

foo = True

bar = False
