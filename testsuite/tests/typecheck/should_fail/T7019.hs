{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE UndecidableInstances #-}

module T7019a where

newtype Free c a = Free { runFree :: forall r. c r => (a -> r) -> r }

type C c = forall a. c (Free c a)

-- Notice that C is a synonym, illegal!
instance C c => Monad (Free c) where
  return a = Free ($ a)
  Free f >>= g = f g


