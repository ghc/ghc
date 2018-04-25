{-# LANGUAGE DuplicateRecordFields #-}
module Bug where

newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }
newtype ContT2 r m a = ContT2 { runContT :: (a -> m r) -> m r }

foo bar baz = (`runContT` bar.baz)

woo x y = (`runContT` x `y` x)
