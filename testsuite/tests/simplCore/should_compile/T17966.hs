{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- The issue here is whether $cm gets a specialiation
-- See #17966

module T17966 where

class C a b where
  m :: Show c => a -> b -> c -> String

instance Show b => C Bool b where
  m a b c = show a ++ show b ++ show c
  {-# INLINABLE [0] m #-}

f :: (C a b, Show c) => a -> b -> c -> String
f a b c = m a b c ++ "!"
{-# INLINABLE [0] f #-}

x :: String
x = f True () (Just 42)
