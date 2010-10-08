{-# LANGUAGE FlexibleContexts #-}

module TcRun038_B where

class Foo a where
  op :: a -> Int

-- Note the (Foo Int) constraint here; and the fact
-- that there is no (Foo Int) instance in this module
-- It's in the importing module!

bar :: Foo Int => Int -> Int
bar x = op x + 7
