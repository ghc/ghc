{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DefaultSignatures, FlexibleContexts, TypeOperators #-}
module SpeedTest (Bar (..), Foo0 (..), Foo1 (..), Foo2 (..), Foo3 (..)) where

import GHC.Generics
import Data.Typeable (Typeable)

-------------------------------------------------------------------------------
-- Generic class
-------------------------------------------------------------------------------

class Bar a where
    bar :: a -> [String]
    bar x = bar' x []

    bar' :: a -> [String] -> [String]
    default bar' :: (Generic a, GBar (Rep a)) => a -> [String] -> [String]
    bar' x = gbar (from x)

class GBar f where
    gbar :: f a -> [String] -> [String]

instance (GBar a, GBar b) => GBar (a :*: b) where
    gbar (a :*: b) = gbar a . gbar b

instance GBar a => GBar (M1 i c a) where
    gbar (M1 x) = gbar x

instance Bar a => GBar (K1 i a) where
    gbar (K1 x) = bar' x

instance Bar a => Bar [a] where
    bar' = foldr (.) id . map bar'

instance Bar a => Bar (Maybe a) where
    bar' = maybe id bar'

instance Bar Bool where
    bar' = (:) . show

instance Bar Char where
    bar' = (:) . show

instance Bar Int where
    bar' = (:) . show

-------------------------------------------------------------------------------
-- Another generic class
-------------------------------------------------------------------------------

class Quu a where
    quu :: a -> [String]
    quu x = quu' x []

    quu' :: a -> [String] -> [String]
    default quu' :: (Generic a, GQuu (Rep a)) => a -> [String] -> [String]
    quu' x = gquu (from x)

class GQuu f where
    gquu :: f a -> [String] -> [String]

instance (GQuu a, GQuu b) => GQuu (a :*: b) where
    gquu (a :*: b) = gquu a . gquu b

instance GQuu a => GQuu (M1 i c a) where
    gquu (M1 x) = gquu x

instance Quu a => GQuu (K1 i a) where
    gquu (K1 x) = quu' x

instance Quu a => Quu [a] where
    quu' = foldr (.) id . map quu'

instance Quu a => Quu (Maybe a) where
    quu' = maybe id quu'

instance Quu Bool where
    quu' = (:) . show

instance Quu Char where
    quu' = (:) . show

instance Quu Int where
    quu' = (:) . show

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data Foo0 = Foo0
    { foo0Field00 :: !String -- Should really have Text
    , foo0Field01 :: !Int
    , foo0Field02 :: ![Int]
    , foo0Field03 :: !(Maybe Bool)
    , foo0Field04 :: !Bool
    , foo0Field05 :: !String
    , foo0Field06 :: !Int
    , foo0Field07 :: ![Int]
    , foo0Field08 :: !(Maybe Bool)
    , foo0Field09 :: !Bool
    , foo0Field10 :: !String
    , foo0Field11 :: !Int
    , foo0Field12 :: ![Int]
    , foo0Field13 :: !(Maybe Bool)
    , foo0Field14 :: !Bool
    , foo0Field15 :: !String
    , foo0Field16 :: !Int
    , foo0Field17 :: ![Int]
    , foo0Field18 :: !(Maybe Bool)
    , foo0Field19 :: !Bool
    }
  deriving (Eq, Ord, Show, Typeable, Generic)

instance Bar Foo0
instance Quu Foo0

data Foo1 = Foo1
    { foo1Field00 :: !String -- Should really have Text
    , foo1Field01 :: !Int
    , foo1Field02 :: ![Int]
    , foo1Field03 :: !(Maybe Bool)
    , foo1Field04 :: !Bool
    , foo1Field05 :: !String
    , foo1Field06 :: !Int
    , foo1Field07 :: ![Int]
    , foo1Field08 :: !(Maybe Bool)
    , foo1Field09 :: !Bool
    , foo1Field10 :: !String
    , foo1Field11 :: !Int
    , foo1Field12 :: ![Int]
    , foo1Field13 :: !(Maybe Bool)
    , foo1Field14 :: !Bool
    , foo1Field15 :: !String
    , foo1Field16 :: !Int
    , foo1Field17 :: ![Int]
    , foo1Field18 :: !(Maybe Bool)
    , foo1Field19 :: !Bool
    }
  deriving (Eq, Ord, Show, Typeable, Generic)

instance Bar Foo1
instance Quu Foo1

data Foo2 = Foo2
    { foo2Field00 :: !String -- Should really have Text
    , foo2Field01 :: !Int
    , foo2Field02 :: ![Int]
    , foo2Field03 :: !(Maybe Bool)
    , foo2Field04 :: !Bool
    , foo2Field05 :: !String
    , foo2Field06 :: !Int
    , foo2Field07 :: ![Int]
    , foo2Field08 :: !(Maybe Bool)
    , foo2Field09 :: !Bool
    , foo2Field10 :: !String
    , foo2Field11 :: !Int
    , foo2Field12 :: ![Int]
    , foo2Field13 :: !(Maybe Bool)
    , foo2Field14 :: !Bool
    , foo2Field15 :: !String
    , foo2Field16 :: !Int
    , foo2Field17 :: ![Int]
    , foo2Field18 :: !(Maybe Bool)
    , foo2Field19 :: !Bool
    }
  deriving (Eq, Ord, Show, Typeable, Generic)

instance Bar Foo2
instance Quu Foo2

data Foo3 = Foo3
    { foo3Field00 :: !String -- Should really have Text
    , foo3Field01 :: !Int
    , foo3Field02 :: ![Int]
    , foo3Field03 :: !(Maybe Bool)
    , foo3Field04 :: !Bool
    , foo3Field05 :: !String
    , foo3Field06 :: !Int
    , foo3Field07 :: ![Int]
    , foo3Field08 :: !(Maybe Bool)
    , foo3Field09 :: !Bool
    , foo3Field10 :: !String
    , foo3Field11 :: !Int
    , foo3Field12 :: ![Int]
    , foo3Field13 :: !(Maybe Bool)
    , foo3Field14 :: !Bool
    , foo3Field15 :: !String
    , foo3Field16 :: !Int
    , foo3Field17 :: ![Int]
    , foo3Field18 :: !(Maybe Bool)
    , foo3Field19 :: !Bool
    }
  deriving (Eq, Ord, Show, Typeable, Generic)

instance Bar Foo3
instance Quu Foo3
