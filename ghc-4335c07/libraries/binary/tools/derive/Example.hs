{-# LANGUAGE DeriveDataTypeable #-}

import Data.Generics

import Data.Binary

import BinaryDerive

data Foo = Bar
    deriving (Typeable, Data, Show, Eq)

instance Binary Main.Foo where
  put Bar = return ()
  get = return Bar

data Color = RGB Int Int Int
           | CMYK Int Int Int Int
    deriving (Typeable, Data, Show, Eq)

instance Binary Main.Color where
  put (RGB a b c) = putWord8 0 >> put a >> put b >> put c
  put (CMYK a b c d) = putWord8 1 >> put a >> put b >> put c >> put d
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> get >>= \b -> get >>= \c -> return (RGB a b c)
      1 -> get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> return (CMYK a b c d)

data Computer = Laptop { weight :: Int }
              | Desktop { speed :: Int, memory :: Int }
    deriving (Typeable, Data, Show, Eq)

instance Binary Main.Computer where
  put (Laptop a) = putWord8 0 >> put a
  put (Desktop a b) = putWord8 1 >> put a >> put b
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> return (Laptop a)
      1 -> get >>= \a -> get >>= \b -> return (Desktop a b)

data Exp = ExpOr Exp Exp
         | ExpAnd Exp Exp
         | ExpEq Exp Exp
         | ExpNEq Exp Exp
         | ExpAdd Exp Exp
         | ExpSub Exp Exp
         | ExpVar String
         | ExpInt Int
    deriving (Typeable, Data, Show, Eq)

instance Binary Main.Exp where
  put (ExpOr a b) = putWord8 0 >> put a >> put b
  put (ExpAnd a b) = putWord8 1 >> put a >> put b
  put (ExpEq a b) = putWord8 2 >> put a >> put b
  put (ExpNEq a b) = putWord8 3 >> put a >> put b
  put (ExpAdd a b) = putWord8 4 >> put a >> put b
  put (ExpSub a b) = putWord8 5 >> put a >> put b
  put (ExpVar a) = putWord8 6 >> put a
  put (ExpInt a) = putWord8 7 >> put a
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> get >>= \b -> return (ExpOr a b)
      1 -> get >>= \a -> get >>= \b -> return (ExpAnd a b)
      2 -> get >>= \a -> get >>= \b -> return (ExpEq a b)
      3 -> get >>= \a -> get >>= \b -> return (ExpNEq a b)
      4 -> get >>= \a -> get >>= \b -> return (ExpAdd a b)
      5 -> get >>= \a -> get >>= \b -> return (ExpSub a b)
      6 -> get >>= \a -> return (ExpVar a)
      7 -> get >>= \a -> return (ExpInt a)
      _ -> fail "no decoding"
