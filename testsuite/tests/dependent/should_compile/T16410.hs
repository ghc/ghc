{-# Language DataKinds    #-}
{-# Language GADTs        #-}
{-# Language InstanceSigs #-}
{-# Language PolyKinds    #-}
{-# Language TypeFamilies #-}

module T16410 where

import Data.Kind

class Category (tag::Type) where
 type Strip tag :: Type

class Category tag => Stripped tag where
 type Hom tag::Strip tag -> Strip tag -> Type

instance Category () where
 type Strip () = ()
instance Stripped () where
 type Hom () = Unit1

data Unit1 :: () -> () -> Type where U1 :: Unit1 '() '()
data Tag
data Unit2 :: () -> () -> Type where U2 :: Unit2 '() '()

instance Category Tag where
 type Strip Tag = ()

instance Stripped Tag where
 type Hom Tag = Unit1

