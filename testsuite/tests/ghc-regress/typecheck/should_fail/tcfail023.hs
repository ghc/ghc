module ShouldFail where

-- !!! Duplicate instances

data B = C

class A a where
 op :: a -> a

instance A B where
 op C = True

instance A B where
 op C = True


