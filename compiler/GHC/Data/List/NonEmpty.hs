module GHC.Data.List.NonEmpty (module Data.List.NonEmpty, module GHC.Data.List.NonEmpty) where

import Prelude (Applicative (..), Bool, (.))
import qualified Control.Monad as List (zipWithM)
import Data.List.NonEmpty hiding (unzip)
import qualified Data.List as List

zipWithM :: Applicative f => (a -> b -> f c) -> NonEmpty a -> NonEmpty b -> f (NonEmpty c)
zipWithM f (a:|as) (b:|bs) = liftA2 (:|) (f a b) (List.zipWithM f as bs)
-- Inline to enable fusion of `List.zipWithM`
-- See Note [Fusion for zipN/zipWithN] in List.hs
{-# INLINE zipWithM #-}

unzip :: NonEmpty (a, b) -> (NonEmpty a, NonEmpty b)
unzip ((a,b):|xs) = (a:|as, b:|bs)
  where
    (as, bs) = List.unzip xs

isSingleton :: NonEmpty a -> Bool
isSingleton = List.null . tail
