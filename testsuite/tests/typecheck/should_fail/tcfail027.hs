-- !!! tests for CycleErr in classes
module TcFail where

class (B a) => A a where
 op1 :: a -> a

class (A a) => B a where
 op2 :: a -> a -> a
