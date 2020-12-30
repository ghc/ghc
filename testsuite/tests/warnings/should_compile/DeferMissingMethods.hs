{-# LANGUAGE NoIncomplete #-}
{-# OPTIONS_GHC -fdefer-missing-methods #-}
-- we'll switch this warning off because it's on in Wdefault
{-# OPTIONS_GHC -Wno-missing-methods #-}

module A where

class C a where
  m1 :: a
  m2 :: a

instance C Bool where
  m1 = False
