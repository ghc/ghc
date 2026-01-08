module T26746 where

import Data.Coerce

newtype Foo a = Foo (Foo a)
newtype Age = MkAge Int

ex1 :: (Foo Age) -> (Foo Int)
ex1 = coerce

newtype Womble a = MkWomble (Foo a)

ex2 :: Womble (Foo Age) -> (Foo Int)
ex2 = coerce

ex3 :: (Foo Age) -> Womble (Foo Int)
ex3 = coerce


-- Surprisingly this one works:
newtype Z1 = MkZ1 Z2
newtype Z2 = MKZ2 Z1

ex4 :: Z1 -> Z2
ex4 = coerce

-- But this one does not (commented out)
-- newtype Y1 = MkY1 Y2
-- newtype Y2 = MKY2 Y3
-- newtype Y3 = MKY3 Y1
--
-- ex5 :: Y1 -> Y3
-- ex5 = coerce
