{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeInType #-}
module T11732c where

import Data.Kind

class Cat k (cat :: k -> k -> *) where
  catId   :: cat a a
  catComp :: cat b c -> cat a b -> cat a c

instance Cat * (->) where
  catId   = id
  catComp = (.)

newtype Fun1 a b = Fun1 (a -> b) deriving (Cat k)
newtype Fun2 a b = Fun2 (a -> b) deriving (Cat *)
