{-# LANGUAGE ForeignFunctionInterface #-}

main :: IO ()
main = putStrLn "hello world"

foreign export ccall "hello" hello :: IO ()

hello :: IO ()
hello = putStrLn "hello world again"
