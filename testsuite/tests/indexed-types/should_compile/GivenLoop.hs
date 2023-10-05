{-# LANGUAGE TypeFamilies #-}

module GivenLoop where

type family UnMaybe a where
  UnMaybe (Maybe b) = b

class C c where
  meth :: c

instance C (Maybe d) where
  meth = Nothing

f :: (e ~ Maybe (UnMaybe e)) => e
f = meth
