{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module WorkingGenerics where
import GHC.Generics

-- type family DiffT (p :: * -> *) :: * -> *

data Void  deriving(Generic)

class Diff a  where
  type family Patch a :: *
  type Patch a = GPatch (Rep a) a

  diff :: a -> a -> Patch a
  default diff :: (Generic a, GDiff (Rep a), Patch a ~ (GPatch (Rep a)) a) => a -> a -> Patch a
  diff a a' = gdiff (from a) (from a')

class GDiff (gen :: * -> *)  where
  type family GPatch gen :: * -> *
  gdiff :: gen a -> gen a -> (GPatch gen) a

instance GDiff V1 where
  type GPatch V1 = V1
  gdiff v1 _ = undefined

-- meta info, we simply tunnel through
instance (GDiff f) => GDiff (M1 i t f)  where
  type GPatch (M1 i t f) =  M1 i t (GPatch f)
  gdiff (M1 x) (M1 x') = M1 $ gdiff x x'


instance Diff Void

