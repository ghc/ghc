{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wmissing-exported-signatures #-}

module T14794b (testExported, pattern TestExported) where

-- These should generate warnings:

testExported = True

-- These should not generate warnings:

testUnexported = True

pattern TestExported <- True

pattern TestUnexported <- True
