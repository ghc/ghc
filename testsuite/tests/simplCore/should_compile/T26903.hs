{-# LANGUAGE DefaultSignatures #-}
module T26903 where

newtype T a = MkT [a]

class C a where
  op :: [a] -> [a] -> T a

  -- This default method
  --  * Has an INLINE pragma
  --  * Is too big to inline without a pragma
  --  * Has arity zero
  {-# INLINE[1] op #-}
  default op :: Ord a => [a] -> [a] -> T a
  op = \xs ys -> MkT $ if xs>ys then reverse (reverse (reverse (reverse xs)))
                                else reverse (reverse (reverse (reverse (xs ++ ys))))

instance C Int where {}

test :: [Int] -> T Int
test xs = op [] xs
  -- We expect to see `op` inlined into the RHS of `test`

