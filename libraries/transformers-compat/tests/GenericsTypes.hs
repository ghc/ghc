{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
#endif
module GenericsTypes where

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative
#endif

import Data.Functor.Classes
import Data.Functor.Classes.Generic

#if __GLASGOW_HASKELL__ >= 706
import GHC.Generics (Generic1)
#endif
import GHC.Exts

import Test.QuickCheck (Arbitrary(..), oneof)

#if __GLASGOW_HASKELL__ < 804
import Data.Eq.Deriving (deriveEq)
import Data.Ord.Deriving (deriveOrd)
import Generics.Deriving.TH (deriveAll1)
import Text.Show.Deriving (deriveShow)
#endif

#if __GLASGOW_HASKELL__ < 806
import Text.Read.Deriving (deriveRead)
#endif


data TestParam a = TestParam a (Maybe a) (Maybe (Maybe a))
  deriving (Eq, Ord, Read, Show)

instance Arbitrary a => Arbitrary (TestParam a) where
  arbitrary = TestParam <$> arbitrary <*> arbitrary <*> arbitrary

data Prim a = Prim a Char# Double# Int# Float# Word#
  deriving (Eq, Ord, Show)

instance Arbitrary a => Arbitrary (Prim a) where
  arbitrary = do
    a    <- arbitrary
    C# c <- arbitrary
    D# d <- arbitrary
    I# i <- arbitrary
    F# f <- arbitrary
    W# w <- arbitrary
    return $ Prim a c d i f w

data T# a = MkT1# a
          | MkT2# { getT2# :: a, (##) :: a }
          | a `MkT3#` a
  deriving (Eq, Ord, Show)

instance Arbitrary a => Arbitrary (T# a) where
  arbitrary = oneof [ MkT1# <$> arbitrary
                    , MkT2# <$> arbitrary <*> arbitrary
                    , MkT3# <$> arbitrary <*> arbitrary
                    ]

infixl 3 :!:
infix  4 :@:
infixr 5 `Backticks`
infixr 6 `FakeInfix`
data Infix a = (:!:) a Double
             | a :@: ()
             | a `Backticks` Bool
             | FakeInfix a Int
  deriving (Eq, Ord, Read, Show)

instance Arbitrary a => Arbitrary (Infix a) where
  arbitrary = oneof [ (:!:)     <$> arbitrary <*> arbitrary
                    , (:@:)     <$> arbitrary <*> arbitrary
                    , Backticks <$> arbitrary <*> arbitrary
                    , FakeInfix <$> arbitrary <*> arbitrary
                    ]

infixr 1 :., :..., :....
data GADT a where
    (:.)    ::            b ->        ()            -> GADT b
    (:..)   ::            c ->        Bool          -> GADT c
    (:...)  ::            d ->        Double -> Int -> GADT d
    (:....) :: { gadt1 :: e, gadt2 :: Char }        -> GADT e
  deriving (Eq, Ord, Read, Show)

instance Arbitrary a => Arbitrary (GADT a) where
  arbitrary = oneof [ (:.)    <$> arbitrary <*> arbitrary
                    , (:..)   <$> arbitrary <*> arbitrary
                    , (:...)  <$> arbitrary <*> arbitrary <*> arbitrary
                    , (:....) <$> arbitrary <*> arbitrary
                    ]

infixl 4 :%:
data Record a = Prefix { rec1 :: Int, rec2 :: a }
              | Int :%: a
  deriving (Eq, Ord, Read, Show)

instance Arbitrary a => Arbitrary (Record a) where
  arbitrary = oneof [ Prefix <$> arbitrary <*> arbitrary
                    , (:%:)  <$> arbitrary <*> arbitrary
                    ]

data Empty a

instance Arbitrary (Empty a) where
  arbitrary = return $ error "Arbitrary Empty"

#if __GLASGOW_HASKELL__ == 700
-- Workaround for GHC Trac #5041
$(deriveRead ''T#)
#elif __GLASGOW_HASKELL__ == 804
-- Workaround for GHC Trac #14918
$(deriveRead ''T#)
#else
deriving instance Read a => Read (T# a)
#endif

#if __GLASGOW_HASKELL__ >= 804
deriving instance Eq   (Empty a)
deriving instance Ord  (Empty a)
deriving instance Read (Empty a)
deriving instance Show (Empty a)
deriving instance Generic1 Empty
#else
$(deriveEq   ''Empty)
$(deriveOrd  ''Empty)
$(deriveRead ''Empty)
$(deriveShow ''Empty)
$(deriveAll1 ''Empty)
#endif

#if __GLASGOW_HASKELL__ >= 706
deriving instance Generic1 TestParam
deriving instance Generic1 T#
deriving instance Generic1 Infix
deriving instance Generic1 GADT
deriving instance Generic1 Record
#else
$(deriveAll1 ''TestParam)
$(deriveAll1 ''T#)
$(deriveAll1 ''Infix)
$(deriveAll1 ''GADT)
$(deriveAll1 ''Record)
#endif

#if __GLASGOW_HASKELL__ >= 800
deriving instance Generic1 Prim
#else
$(deriveAll1 ''Prim)
#endif

#define CLASS1_INSTANCE(class,type,method,impl) \
instance class type where { method = impl };    \

#if MIN_VERSION_transformers(0,4,0) && !(MIN_VERSION_transformers(0,5,0))
# define TRANSFORMERS_FOUR 1
#endif

#if defined(TRANSFORMERS_FOUR)
# define EQ1_INSTANCE(type)   CLASS1_INSTANCE(Eq1,type,eq1,eq1Default)
# define ORD1_INSTANCE(type)  CLASS1_INSTANCE(Ord1,type,compare1,compare1Default)
# define READ1_INSTANCE(type) CLASS1_INSTANCE(Read1,type,readsPrec1,readsPrec1Default)
# define SHOW1_INSTANCE(type) CLASS1_INSTANCE(Show1,type,showsPrec1,showsPrec1Default)
#else
# define EQ1_INSTANCE(type)   CLASS1_INSTANCE(Eq1,type,liftEq,liftEqDefault)
# define ORD1_INSTANCE(type)  CLASS1_INSTANCE(Ord1,type,liftCompare,liftCompareDefault)
# define READ1_INSTANCE(type) CLASS1_INSTANCE(Read1,type,liftReadsPrec,liftReadsPrecDefault)
# define SHOW1_INSTANCE(type) CLASS1_INSTANCE(Show1,type,liftShowsPrec,liftShowsPrecDefault)
#endif

#define CLASS1_INSTANCES(type) \
EQ1_INSTANCE(type)   \
ORD1_INSTANCE(type)  \
READ1_INSTANCE(type) \
SHOW1_INSTANCE(type) \

CLASS1_INSTANCES(TestParam)
CLASS1_INSTANCES(T#)
CLASS1_INSTANCES(Infix)
CLASS1_INSTANCES(GADT)
CLASS1_INSTANCES(Record)
CLASS1_INSTANCES(Empty)

EQ1_INSTANCE(Prim)
ORD1_INSTANCE(Prim)
SHOW1_INSTANCE(Prim)
