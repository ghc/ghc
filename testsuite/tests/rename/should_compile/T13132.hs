module Bug where

newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }

foo bar baz = (`runContT` bar.baz)
