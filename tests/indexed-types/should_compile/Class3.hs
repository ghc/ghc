{-# LANGUAGE TypeFamilies #-}

module Class3 where

class C a where
  foo :: a -> a
instance C ()

bar :: (a ~ ()) => a -> a
bar = foo

