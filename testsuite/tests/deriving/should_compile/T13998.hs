{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}

module T13998 where

import Data.Type.Equality

class EqForall f where
  eqForall :: f a -> f a -> Bool

class EqForall f => EqForallPoly f where
  eqForallPoly :: f a -> f b -> Bool
  default eqForallPoly :: TestEquality f => f a -> f b -> Bool
  eqForallPoly = defaultEqForallPoly

defaultEqForallPoly :: (TestEquality f, EqForall f) => f a -> f b -> Bool
defaultEqForallPoly x y = case testEquality x y of
  Nothing -> False
  Just Refl -> eqForall x y


data Atom = AtomInt | AtomString | AtomBool

data Value (a :: Atom) where
  ValueInt :: Int -> Value 'AtomInt
  ValueString :: String -> Value 'AtomString
  ValueBool :: Bool -> Value 'AtomBool

instance TestEquality Value where
  testEquality (ValueInt _) (ValueInt _) = Just Refl
  testEquality (ValueString _) (ValueString _) = Just Refl
  testEquality (ValueBool _) (ValueBool _) = Just Refl
  testEquality _ _ = Nothing

instance EqForall Value where
  eqForall (ValueInt a) (ValueInt b) = a == b
  eqForall (ValueString a) (ValueString b) = a == b
  eqForall (ValueBool a) (ValueBool b) = a == b

instance EqForallPoly Value
