{-# LANGUAGE TypeFamilies #-}
module Main where

main :: IO ()
main = do
  let f :: (Bool, Bool) -> (Bool, Bool) -> (Bool, Bool)
      f (True, False) (False, False) = (False, True)
      f _ _ = (True, False)
      ((i, b), v) = ((False,True),[(False,True),(False,False),(True,True),(True,False),(False,False),(False,True),(True,True),(True,True),(False,True),(True,False),(False,False),(True,True),(True,True),(False,False),(False,False),(False,True),(True,False),(True,False),(True,True),(True,True),(False,True),(True,False),(True,False),(True,True),(False,False),(True,True),(False,False),(True,False),(False,True),(True,True)])
  print $ foldlTest f (i, b) v

type FoldlTest a = (a -> a -> a) -> a -> [a] -> Bool

foldlTest :: FoldlTest (Bool, Bool)
foldlTest f (i, b) v =
  foldl f (i, b) v == foldl (\x -> f (unmodel x)) (i, b) v

class TestData a where
  type Model a
  unmodel :: Model a -> a

instance TestData Bool where
  type Model Bool = Bool
  unmodel = id

instance (Eq a, Eq b, TestData a, TestData b) => TestData (a,b) where
  type Model (a,b) = (Model a, Model b)
  unmodel (a,b) = (unmodel a, unmodel b)
