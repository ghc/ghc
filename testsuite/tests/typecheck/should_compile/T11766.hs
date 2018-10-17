{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module T11766 where

import Data.Maybe (isJust)

data Wrapper a = Wrapper a deriving (Show)

class Resolution a
instance Resolution (Wrapper a)

class (Resolution b, Resolution d) => C a b c d | a -> b, c -> d, a d -> c, b c -> a where
  cfun :: (b -> d) -> a -> c

instance {-# OVERLAPPABLE #-} (Resolution b, Resolution d, a ~ b, c ~ d) => C a b c d where
  cfun = ($)

instance {-# OVERLAPPING #-} (C b c d e) => C (Maybe a -> b) c (Maybe a -> d) e where
  cfun f b = \x -> cfun f (b x)

foo :: Maybe a -> Wrapper Bool
foo = Wrapper . isJust

t1 = cfun id foo $! Nothing
t2 = let f = cfun id foo in f Nothing
t3 = cfun id foo Nothing
t4 = cfun id foo $ Nothing
