module T22688a where

data Foo m a

instance Foo m where
  fmap _ x = case x of
