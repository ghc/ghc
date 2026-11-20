{-# LANGUAGE DataKinds, ExperimentalDependentTypes, ImpredicativeTypes, RankNTypes #-}
module QuickLookRuntime where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import RetainedDot (Flag)

type Branch = foreach (b :: Bool) -> Flag b -> Bool
type Mixed = exists (w :: Type) | (Proxy w, Branch)

function :: [Mixed] -> [Mixed]
function xs = reverse (reverse xs)

index :: foreach (b :: Bool) -> [exists (w :: Type) | (Proxy w, Flag b)] -> [exists (w :: Type) | (Proxy w, Flag b)]
index _ xs = reverse (reverse xs)
