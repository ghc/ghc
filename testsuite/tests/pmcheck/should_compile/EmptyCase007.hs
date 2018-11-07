{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE TypeFamilies, EmptyCase, LambdaCase #-}

-- Check interaction between Newtypes and Type Families
module EmptyCase007 where

import Data.Kind (Type)

type family FA a :: Type -- just an open type family
type instance FA Int     = (Char, Bool)
type instance FA Char    = Char
type instance FA [a]     = [FA a]
type instance FA (a,b,b) = Void1

newtype Foo2 a = Foo2 (FA a)

data Void1

-- Non-exhaustive. Missing: (_ :: Foo2 a) (no info about a)
f05 :: Foo2 a -> ()
f05 = \case

-- Non-exhaustive. Missing: (_ :: Foo2 (a, a)) (does not reduce)
f06 :: Foo2 (a, a) -> ()
f06 = \case

-- Exhaustive (reduces to Void)
f07 :: Foo2 (Int, Char, Char) -> ()
f07 = \case

-- Non-exhaustive. Missing: Foo2 (_, _)
f08 :: Foo2 Int -> ()
f08 = \case

-- Non-exhaustive. Missing: Foo2 _
f09 :: Foo2 Char -> ()
f09 = \case

-- Non-exhaustive. Missing: (_ :: Char)
-- This is a more general trick: If the warning gives you a constructor form
-- and you don't know what the type of the underscore is, just match against
-- the constructor form, and the warning you'll get will fill the type in.
f09' :: Foo2 Char -> ()
f09' (Foo2 x) = case x of {}

-- Non-exhaustive. Missing: Foo2 [], Foo2 (_:_)
f10 :: Foo2 [Int] -> ()
f10 = \case
