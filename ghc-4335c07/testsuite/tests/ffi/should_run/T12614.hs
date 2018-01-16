{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

{-# NOINLINE test_reg #-}
test_reg :: Int -> IO ()
test_reg x = c_foo 0 0 x (x + x `quot` 10) 0 0 0 0

{-# NOINLINE test_stack #-}
test_stack :: Int -> IO ()
test_stack x = c_foo 0 0 x 0 0 0 (x + x `quot` 10) 0

foreign import ccall unsafe "foo"
   c_foo :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()

main :: IO ()
main = do
    test_reg 202
    test_reg 203
    test_reg 204
    test_stack 202
    test_stack 203
    test_stack 204
