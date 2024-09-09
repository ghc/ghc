
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module T25996 where

main :: IO ()
main = do
  pure ()
  where
    biz :: IO ()
    biz = do
      pure (10 :: Integer) -- This warning should be reported only once
      pure ()

biz' :: IO ()
biz' = do
  pure (10 :: Integer)
  pure ()
