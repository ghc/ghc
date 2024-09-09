{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

main :: IO ()
main = do
  pure ()
  where
    biz :: IO ()
    biz = do
      pure (10 :: Integer)
      pure ()

biz' :: IO ()
biz' = do
  pure (10 :: Integer)
  pure ()
