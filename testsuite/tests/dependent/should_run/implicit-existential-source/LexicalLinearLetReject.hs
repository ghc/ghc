{-# LANGUAGE BangPatterns, LinearTypes, MonoLocalBinds, NoExperimentalDependentTypes #-}
module LexicalLinearLetReject where

import LexicalLetProvider

readLazy :: LinearPayload %1 -> Int
readLazy package = let ~(value, reader) = package in reader value

readTwice :: LinearPayload %1 -> (Int, Int)
readTwice package = let !(value, reader) = package in (reader value, reader value)
