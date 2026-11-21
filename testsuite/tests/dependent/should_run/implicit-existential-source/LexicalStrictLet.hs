{-# LANGUAGE LinearTypes, MonoLocalBinds, NoExperimentalDependentTypes, Strict #-}
module LexicalStrictLet (ignoreStrict, ignoreStrictTilde, readStrictField, repackStrict, readLinearStrict) where

import LexicalLetProvider

ignoreStrict :: Bool -> Bool
ignoreStrict flag = let (value, reader) = bottomPayload flag in True

ignoreStrictTilde :: Bool -> Bool
ignoreStrictTilde flag = let ~(value, reader) = bottomPayload flag in True

readStrictField :: Int
readStrictField = let (value, reader) = lazyPayload in reader value

repackStrict :: HiddenPayload -> HiddenPayload
repackStrict package = let (value, reader) = package in (value, reader)

readLinearStrict :: LinearPayload %1 -> Int
readLinearStrict package = let (value, reader) = package in reader value
