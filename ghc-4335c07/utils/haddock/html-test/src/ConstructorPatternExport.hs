{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module ConstructorPatternExport (
    pattern FooCons
  , pattern MyRecCons
  , pattern (:+)
  , pattern BlubCons
  , pattern MyGADTCons
  ) where

data Foo a = FooCons String a

data MyRec = MyRecCons { one :: Bool, two :: Int }

data MyInfix a = String :+ a

data Blub = forall b. Show b => BlubCons b

data MyGADT :: * -> * where
  MyGADTCons :: forall a. Eq a => a -> Int -> MyGADT (Maybe String)

pattern MyGADTCons' :: () => forall a. Eq a => a -> Int -> MyGADT (Maybe String)
pattern MyGADTCons' x y = MyGADTCons x y