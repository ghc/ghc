{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wmissing-signatures #-}

module T14794a (testExported, pattern TestExported) where

-- These should generate warnings:

testExported = True

testUnexported = True

-- These should not generate warnings:

pattern TestExported <- True

pattern TestUnexported <- True
