{-# LANGUAGE DataKinds, KindSignatures, GADTs, TypeFamilies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-overlapping-patterns #-}

module T8970 where

import Data.Kind (Type)

data K = Foo
       | Bar

data D1 :: K -> Type where
    F1 :: D1 Foo
    B1 :: D1 Bar

class C (a :: K -> Type) where
    data D2 a :: K -> Type
    foo :: a k -> D2 a k -> Bool

instance C D1 where
    data D2 D1 k where
              F2 :: D2 D1 Foo
              B2 :: D2 D1 Bar
    foo F1 F2 = True
    foo B1 B2 = True
