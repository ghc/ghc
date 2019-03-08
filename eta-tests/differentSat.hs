import System.Environment (getArgs)

{-# NOINLINE foo #-}
foo :: Int -> Int -> Int -> Int
foo = \x y ->
       if x == 0
       then \z -> z - y
       else \d -> d + x + y

{-# NOINLINE bar #-}
bar :: Int -> Int -> Int -> Int
bar = \a b c -> foo a 2 3 + sum (map (foo 1 a) [a,b,c])

main :: IO ()
main =
  do { a:b:c:_ <- getArgs
     ; let a' = read a
           b' = read b
           c' = read c
     ; print $ bar a' b' c' }
