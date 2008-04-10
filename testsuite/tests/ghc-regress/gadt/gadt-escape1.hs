{-# OPTIONS -XGADTs -XExistentialQuantification #-}

module Escape where

data ExpGADT t where
  ExpInt :: Int -> ExpGADT Int

data Hidden = forall t . Hidden (ExpGADT t) (ExpGADT t)

hval = Hidden (ExpInt 0) (ExpInt 1)

-- With the type sig this is ok, but without it should
-- be rejected becuase the result type is wobbly
--    weird1 :: ExpGADT Int

weird1 = case (hval :: Hidden) of Hidden (ExpInt _) a -> a

weird2 :: ExpGADT Int
weird2 = case (hval :: Hidden) of Hidden (ExpInt _) a -> a
