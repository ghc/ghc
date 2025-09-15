{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# OPTIONS_GHC -Wmissing-exported-pattern-synonym-signatures #-}

module T14794e (testExported, data TestExported) where

-- These should generate warnings:

pattern TestExported <- True

-- These should not generate warnings:

testExported = True

testUnexported = True

pattern TestUnexported <- True
