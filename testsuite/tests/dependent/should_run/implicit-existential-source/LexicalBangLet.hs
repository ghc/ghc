{-# LANGUAGE BangPatterns, NoExperimentalDependentTypes #-}
module LexicalBangLet (ignoreTilde, ignoreBang, readBangField) where

import LexicalLetProvider

ignoreTilde :: Bool -> Bool
ignoreTilde flag = let ~(value, reader) = bottomPayload flag in True

ignoreBang :: Bool -> Bool
ignoreBang flag = let !(value, reader) = bottomPayload flag in True

readBangField :: Int
readBangField = let !(value, reader) = lazyPayload in reader value
