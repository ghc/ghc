{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}

import GHC.Records

data Pair t = P { a, b :: t }

instance Num t => HasField "sum" (Pair t) t where
  getField p = p.a + p.b

newtype Wrap = W { p :: Pair Double }

main = do
  print $ P{ a = 40, b = 2 }.sum
  print $ W{ p = P 0 0 }.p{ a = 1 }{ b = 10 }.sum
