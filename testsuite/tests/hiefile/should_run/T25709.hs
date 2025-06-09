{-# LANGUAGE QuantifiedConstraints#-}
{-# LANGUAGE UndecidableInstances #-}
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

points :: [(Int,Int)]
points = [(21,11)]

main = do
  (df, hf) <- readTestHie "T25709.hie"
  undefined
