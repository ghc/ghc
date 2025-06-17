{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, FlexibleInstances, GHC2021 #-}

module T13526 where

class C a where
  op :: a -> a

instance {-# OVERLAPPING #-} C [Char] where
  op x = x

instance C a => C [a] where
  op (x:xs) = [op x]

instance C a => C (Maybe a) where
  op x = error "urk"

-- We should get no complaint
foo :: C [a] => a -> [a]
foo x = op [x]

bar :: C (Maybe a) => a -> Maybe a
bar x = op (Just x)
