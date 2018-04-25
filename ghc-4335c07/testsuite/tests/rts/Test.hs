module Test where

f :: Int -> Int
f x = x + head caf

-- make sure that even when we have a CAF, we can unload the code
caf :: [Int]
caf = [1..10]

foreign export ccall f :: Int -> Int

