{-# LANGUAGE RecordWildCards, RecursiveDo #-}

module TT where

data T = T {t1, t2 :: Int}

f :: T -> Int
f d = x
    where T {t1 = x, ..} = d

g :: T -> Int
g (T {t1 = x, ..}) = x

-- The fix to this test also affected the dorec checking code, hence this:
h :: Maybe Int
h = do
    rec
      T {t1 = x, ..} <- Just $ T 1 1
    return x
