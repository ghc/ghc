{-# LANGUAGE BangPatterns, LinearTypes, MonoLocalBinds, NoExperimentalDependentTypes #-}
module LexicalLinearLetReject where

import LexicalLetProvider

readLinear :: LinearPayload %1 -> Int
readLinear package = let !(value, reader) = package in reader value
