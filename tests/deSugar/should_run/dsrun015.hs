{-# OPTIONS_GHC -XRecordWildCards -XNamedFieldPuns #-}

-- This is a very partial test of the record-wildcard extension
-- but better than nothing

module Main where

data T = C { x :: Int, y :: Int }
       | D { x :: Int, b :: Bool }

select :: T -> Int
select = x

f :: (T,T) -> Int
f v = let (C {..}, d) = v in Main.x d

mkC a =
    let x = a + 1
        y = a * 2
    in  C{..}

sumC C{..} = x + y

foo x b =
    let y = x+1
    in  (C{..}, let x = 100 in D{..})

bar a =
    let (C{..}, d) = a
    in  (x + y + Main.x d, let D{..} = d in b)

main = do
    print $ sumC $ mkC 10
    print $ bar $ foo 5 True
