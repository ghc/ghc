{-# LANGUAGE BangPatterns, LinearTypes, MonoLocalBinds, NoExperimentalDependentTypes #-}
module LexicalLinearRule (readFromRule) where

import LexicalLetProvider

{-# NOINLINE readFromRule #-}
readFromRule :: Bool -> LinearPayload %1 -> Int
readFromRule _ package = case package of (value, reader) -> reader value

{-# RULES "read/linear-let" forall (flag :: Bool).
    readFromRule flag = (\package -> let !(value, reader) = package in reader value) #-}
