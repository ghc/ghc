{-# LANGUAGE MonadComprehensions #-}

module T21360 where

data Foo = A {a :: Int} | B deriving Show

sworble :: Foo -> Maybe Foo
sworble foo = do
  bar@A{} <- Just foo
  return $ bar { a = 9 }
    -- we should not get a warning, because long-distance information
    -- from the previous line should allow us to see that the record update
    -- is not partial

swooble :: Foo -> Maybe Foo
swooble foo = do
  bar@A{} <- Just foo
  return $ case bar of { A _ -> A 9 }
  -- same here

-- same as swooble but using a monad comprehension
blorble :: Foo -> Maybe Foo
blorble foo = [ case bar of { A _ -> A 9 } | bar@A{} <- Just foo ]
