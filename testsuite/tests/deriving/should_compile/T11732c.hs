{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
module T11732c where

import Data.Kind

class Cat k (cat :: k -> k -> Type) where
  catId   :: cat a a
  catComp :: cat b c -> cat a b -> cat a c

instance Cat Type (->) where
  catId   = id
  catComp = (.)

newtype Fun2 a b = Fun2 (a -> b) deriving (Cat Type)

-- The ticket says this should work:
--   newtype Fun1 a b = Fun1 (a -> b) deriving (Cat k)
-- But that requires that the kind of (Cat k) to depend on k, where k is local
-- This is all due for an update, anyway, when #14331 is done, and it's unclear
-- what the correct behavior here is, anyway. (Richard thinks: reject!)
