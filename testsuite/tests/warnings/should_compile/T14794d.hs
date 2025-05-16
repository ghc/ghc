{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# OPTIONS_GHC -Wmissing-pattern-synonym-signatures #-}

module T14794d (testExported, data TestExported) where

-- These should generate warnings:

pattern TestExported <- True

pattern TestUnexported <- True

-- These should not generate warnings:

testExported = True

testUnexported = True
