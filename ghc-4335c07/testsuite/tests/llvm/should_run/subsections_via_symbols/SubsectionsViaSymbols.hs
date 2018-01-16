{-# LANGUAGE ForeignFunctionInterface #-}
module SymbolsViaSections where
foreign export ccall test :: Int -> IO ()
test :: Int -> IO ()
test i = putStrLn (replicate i '.')
