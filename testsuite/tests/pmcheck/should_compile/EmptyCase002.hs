{-# OPTIONS_GHC -fwarn-incomplete-patterns  #-}
{-# LANGUAGE EmptyCase, LambdaCase          #-}
{-# LANGUAGE GADTs, TypeFamilies            #-}

-- Check some newtypes, in combination with GADTs and TypeFamilies
module EmptyCase002 where

import Data.Kind (Type)

newtype T = MkT H
newtype G = MkG T
newtype H = MkH G

-- Exhaustive but it cannot be detected.
f1 :: T -> a
f1 = \case

data A

data B = B1 | B2

data C :: Type -> Type where
  C1 :: C Int
  C2 :: C Bool

data D :: Type -> Type -> Type where
  D1 :: D Int  Bool
  D2 :: D Bool Char

type family E (a :: Type) :: Type where
  E Int  = Bool
  E Bool = Char

newtype T1 a = MkT1 a
newtype T2 b = MkT2 b

-- Exhaustive
f2 :: T1 A -> z
f2 = \case

-- Non-exhaustive. Missing cases: MkT1 B1, MkT1 B2
f3 :: T1 B -> z
f3 = \case

-- Non-exhaustive. Missing cases: MkT1 False, MkT1 True
f4 :: T1 (E Int) -> z
f4 = \case

-- Non-exhaustive. Missing cases: MkT1 (MkT2 (MkT1 D2))
f5 :: T1 (T2 (T1 (D (E Int) (E (E Int))))) -> z
f5 = \case

-- Exhaustive. Not an EmptyCase but good to have for
-- comparison with the example above
f6 :: T1 (T2 (T1 (D (E Int) (E (E Int))))) -> ()
f6 = \case MkT1 (MkT2 (MkT1 D2)) -> ()
