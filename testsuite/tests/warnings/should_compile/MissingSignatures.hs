{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wmissing-signatures #-}
{-# OPTIONS_GHC -Wmissing-pattern-synonym-signatures #-}
module MissingSignatures where

pattern Yes = True

foo = True
