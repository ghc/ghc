{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
module T13242a where

data T where A :: forall a . Eq a => a -> T

test :: IO Bool
test = do
  A x <- undefined
  _ <- return 'a'
  _ <- return 'b'
  return (x == x)
