{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module T10361b where

import GHC.Generics

---------------------------------------------------------------------
class Convert a where
    type Result a
    type instance Result a = GResult (Rep a)

    convert :: a -> Result a
    default convert
            :: (Generic a, GConvert (Rep a), Result a ~ GResult (Rep a))
            => a -> Result a
    convert x = gconvert (from x)

instance Convert Float where
    type Result Float = Float
    convert = id

instance Convert Int where
    type Result Int = Int
    convert = id

---------------------------------------------------------------------
class GConvert f where
    type GResult f
    gconvert :: f p -> GResult f

instance (Convert c) => GConvert (K1 i c) where
    type GResult (K1 i c) = Result c
    gconvert (K1 x) = convert x

instance (GConvert f) => GConvert (M1 i t f) where
    type GResult (M1 i t f) = GResult f
    gconvert (M1 x) = gconvert x

instance (GConvert f, GConvert g) => GConvert (f :*: g) where
    type GResult (f :*: g) = (GResult f, GResult g)
    gconvert (x :*: y) = (gconvert x, gconvert y)

---------------------------------------------------------------------

data Data1 = Data1 Int Float
    deriving (Generic)

instance Convert Data1

val :: (Int, Float)
val = convert $ Data1 0 0.0

data Data2 = Data2 Int Float
    deriving (Generic, Convert)
