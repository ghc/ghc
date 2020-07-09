-- According to the Report this should reduce to (). However, in #18151 it was
-- reported that GHC bottoms.
x :: ()
x = seq (True `undefined`) ()
{-# NOINLINE x #-}

main :: IO ()
main = do
  print x

