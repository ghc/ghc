{-# LANGUAGE BangPatterns, LinearTypes, MonoLocalBinds, NoExperimentalDependentTypes #-}
module LexicalLinearLet (readLinear, readPlain) where

import LexicalLetProvider

readLinear :: LinearPayload %1 -> Int
readLinear package = let !(value, reader) = package in reader value

readPlain :: (Bool, Bool %1 -> Int) %1 -> Int
readPlain pair = let !(value, reader) = pair in reader value
