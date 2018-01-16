{-# LANGUAGE DeriveTraversable #-}

import Data.Monoid (Endo (..))
import Control.Exception (evaluate)

data Tree a = Bin !(Tree a) a !(Tree a) | Tip
  deriving (Functor, Foldable)

t1, t2, t3, t4, t5 :: Tree ()
t1 = Bin Tip () Tip
t2 = Bin t1 () t1
t3 = Bin t2 () t2
t4 = Bin t3 () t3
t5 = Bin t4 () t4
t6 = Bin t5 () t5
t7 = Bin t6 () t6

replaceManyTimes :: Functor f => f a -> f Int
replaceManyTimes xs = appEndo
  (foldMap (\x -> Endo (x <$)) [1..20000])
  (0 <$ xs)

main :: IO ()
main = do
  evaluate $ sum $ replaceManyTimes t7
  pure ()
