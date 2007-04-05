
module Main (main) where

import Language.Haskell.TH

u1 :: a
u1 = undefined

u2 :: a
u2 = undefined

f :: a
f = undefined

(.+.) :: a
(.+.) = undefined

main :: IO ()
main = do runQ [| f u1 u2 |] >>= p
          runQ [| u1 `f` u2 |] >>= p
          runQ [| (.+.) u1 u2 |] >>= p
          runQ [| u1 .+. u2 |] >>= p
          runQ [| (:) u1 u2 |] >>= p
          runQ [| u1 : u2 |] >>= p
          runQ [| \((:) x xs) -> x |] >>= p
          runQ [| \(x : xs) -> x |] >>= p
          runQ [d| class Foo a b where
                       foo :: a -> b   |] >>= p

p :: Ppr a => a -> IO ()
p = putStrLn . pprint

