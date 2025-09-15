main :: IO ()
main = print (f (3 + 4 :: Int))

f :: Int -> Int
f x = x
{-# NOINLINE [1] f #-}

{-# RULES "rule" forall x. f x = 8 #-}
