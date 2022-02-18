{-# LANGUAGE TypeFamilies, FunctionalDependencies, RankNTypes, MultiParamTypeClasses #-}
module T4254b where

class FD a b | a -> b where
  op :: a -> b;
  op = undefined

instance FD Int Bool

fails :: forall a b. (a~Int,FD a b) => a -> Bool
fails  = op
-- Could fail: no proof that b~Bool
-- But can also succeed; it's not a *wanted* constraint
