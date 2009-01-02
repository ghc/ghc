module Main where

data X = X ()

{-# NOINLINE newX #-}
newX :: () -> IO X
newX n = do
    let {-# NOINLINE value #-}
        value = n
    return (X value)

main = do
    x <- newX (error "Why?")
    case x of
        X _ -> return ()
