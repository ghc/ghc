{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wmissing-signatures -Wmissing-exported-signatures #-}

module T14794c (testExported, pattern TestExported) where

-- These should generate warnings:

testExported = True

testUnexported = True

-- These should not generate warnings:

pattern TestExported <- True

pattern TestUnexported <- True
