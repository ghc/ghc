{-# OPTIONS_GHC -fwarn-incomplete-patterns      #-}
{-# LANGUAGE EmptyCase, LambdaCase              #-}
{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

-- Check some type families and type synonyms
module EmptyCase003 where

import Data.Kind (Type)

type family A (a :: Type) :: Type

-- Conservatively considered non-exhaustive (A a missing),
-- since A a does not reduce to anything.
f1 :: A a -> a -> b
f1 = \case

data Void

type family B (a :: Type) :: Type
type instance B a = Void

-- Exhaustive
f2 :: B a -> b
f2 = \case

type family C (a :: Type) :: Type
type instance C Int  = Char
type instance C Bool = Void

-- Non-exhaustive (C a missing, no info about `a`)
f3 :: C a -> a -> b
f3 = \case

-- Non-exhaustive (_ :: Char missing): C Int rewrites
-- to Char (which is trivially inhabited)
f4 :: C Int -> a
f4 = \case

-- Exhaustive: C Bool rewrites to Void
f5 :: C Bool -> a
f5 = \case

-- type family D (a :: Type) :: Type
-- type instance D x = D x -- non-terminating
--
-- -- Exhaustive but *impossible* to detect that, since rewriting
-- -- D Int does not terminate (the checker should loop).
-- f6 :: D Int -> a
-- f6 = \case

data Zero
data Succ (n :: Type)

type TenC n = Succ (Succ (Succ (Succ (Succ
             (Succ (Succ (Succ (Succ (Succ n)))))))))

type Ten = TenC Zero

type Hundred = TenC (TenC (TenC (TenC (TenC
              (TenC (TenC (TenC (TenC (TenC Zero)))))))))

type family E (n :: Type) (a :: Type) :: Type
type instance E Zero     b = b
type instance E (Succ n) b = E n b

-- Exhaustive (10 rewrites)
f7 :: E Ten Void -> b
f7 = \case

-- Exhaustive (100 rewrites)
f8 :: E Hundred Void -> b
f8 = \case

type family Add (a :: Type) (b :: Type) :: Type
type instance Add Zero     m = m
type instance Add (Succ n) m = Succ (Add n m)

type family Mult (a :: Type) (b :: Type) :: Type
type instance Mult Zero     m = Zero
type instance Mult (Succ n) m = Add m (Mult n m)

type Five = Succ (Succ (Succ (Succ (Succ Zero))))
type Four = Succ (Succ (Succ (Succ Zero)))

-- Exhaustive (80 rewrites)
f9 :: E (Mult Four (Mult Four Five)) Void -> a
f9 = \case

-- This gets killed on my dell
--
-- -- Exhaustive (390625 rewrites)
-- f10 :: E (Mult (Mult (Mult Five Five)
--                      (Mult Five Five))
--                (Mult (Mult Five Five)
--                      (Mult Five Five)))
--          Void -> a
-- f10 = \case
