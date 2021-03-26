{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module T18388 where

class C x y where
  m :: x -> y -> y

newtype Tagged x a = MkTagged a
instance C x (Tagged x a) where
  m _ = id

$([d| newtype Id1 a = MkId1 a
        deriving (C x) via forall x. Tagged x a

      newtype Id2 a = MkId2 a
        deriving (C x) via           Tagged x a
    |])

newtype List1 a = MkList1 [a]
newtype List2 a = MkList2 [a]
$([d| deriving via forall a. [a] instance Eq a => Eq (List1 a) |])
$([d| deriving via           [a] instance Eq a => Eq (List2 a) |])

$([d| f = id @a :: forall a. a -> a |])
