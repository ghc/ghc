{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

module RuleEqs where

import qualified Data.List

type family Element mono
type instance Element [a] = a

class MonoFoldable mono where
  otoList :: mono -> [Element mono]

instance MonoFoldable [a] where
  otoList = id

ointercalate :: (MonoFoldable mono, Monoid (Element mono))
             => Element mono
             -> mono
             -> Element mono
ointercalate x = mconcat . Data.List.intersperse x . otoList
{-# INLINE [0] ointercalate #-}
{-# RULES "ointercalate list" forall x. ointercalate x = Data.List.intercalate x . otoList #-}
