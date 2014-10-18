{-# LANGUAGE OverloadedRecordFields #-}

data T = MkT { foo :: Int, bar :: Int }

-- Test multiple fields
f :: (r { foo :: a, bar :: a }, Num a) => r -> a
f x = foo x + bar x

main = print $ f MkT { foo = 2, bar = 3 }
