{-# LANGUAGE RankNTypes, DeriveDataTypeable #-}

module Main where

import Data.Generics

data MyMaybe a = MyNothing | MyJust a deriving (Data, Typeable)

test1 :: ()
test1 = undefined `ext1Q` (\ (Just _) -> ()) $ Just ()

test1' :: ()
test1' = undefined `ext1Q` (\ (MyJust _) -> ()) $ MyJust ()

newtype Q r a = Q { unQ :: a -> r }

ext2Q :: (Data d, Typeable2 t)
      => (d -> q) -> (forall d1 d2. (Data d1, Data d2) => t d1 d2 -> q)
      -> d -> q
ext2Q def ext arg =
  case dataCast2 (Q ext) of
    Just (Q ext') -> ext' arg
    Nothing       -> def arg

data MyPair a b = MyPair a b deriving (Data, Typeable)

test2 :: ()
test2 = undefined `ext2Q` (\(_,_) -> ()) $ ((),())

test2' :: ()
test2' = undefined `ext2Q` (\(MyPair _ _) -> ()) $ MyPair () ()

main = do { print test1; print test1'; print test2; print test2' }
