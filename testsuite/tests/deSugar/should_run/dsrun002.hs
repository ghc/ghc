{- Tests let-expressions in do-statements -}

module Main( main ) where

foo = do
        putStr "a"
        let x = "b" in putStr x
        putStr "c"

main = do
         putStr "a"
         foo
         let x = "b" in putStrLn x

