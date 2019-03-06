{-# LANGUAGE GADTs, TypeInType, ExistentialQuantification #-}

module T16221 where

-- Failed Lint
data T3 a = forall k (b :: k). MkT3 (T3 b) !Int

-- Works with GADT
data T4 a where
   MkT4 :: T4 b -> !Int -> T4 a

-- Works with CUSK
data T5 (a :: j) = forall k (b :: k). MkT5 (T5 b) !Int
