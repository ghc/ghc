{-# LANGUAGE QuantifiedConstraints#-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Main where

import TestUtils
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Either
import Data.Maybe
import Data.Bifunctor (first)
import GHC.Plugins (moduleNameString, nameStableString, nameOccName, occNameString, isDerivedOccName)
import GHC.Iface.Ext.Types


import Data.Typeable

data Some c where
    Some :: c a => a -> Some c

extractSome :: (Typeable a, forall x. c x => Typeable x) => Some c -> Maybe a
extractSome (Some a) = cast a

f :: (forall x. Ord x => Eq [x]) => ()
f = ()
{-# NOINLINE f #-}

g :: ()
g = f

useQC :: forall c a. (c a, forall x. c x => Show x) => a -> String
useQC x = show x

points :: [(Int,Int)]
points = [(22,26),(29, 5), (32, 13)]

main = do
  (df, hf) <- readTestHie "T25709.hie"
  let refmap = generateReferencesMap $ getAsts $ hie_asts hf
  traverse (explainEv df hf refmap) points
