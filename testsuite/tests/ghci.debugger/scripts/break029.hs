{-# LANGUAGE ApplicativeDo #-}

f :: Int -> IO Int
f x = do
  y <- return (x + 1)
  return (y * 2)
