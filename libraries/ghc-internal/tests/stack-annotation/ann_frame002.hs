
{-# LANGUAGE ScopedTypeVariables #-}

import TestUtils

main :: IO ()
main = do
  foo baz
  bar

foo :: IO () -> IO ()
foo act = annotateCallStackIO $ do
  putStrLn "Start some work"
  act
  putStrLn "Finish some work"

baz :: IO ()
baz = annotateCallStackIO $ do
  print (fib 20)
  decodeAndPrintAnnotationFrames

bar :: IO ()
bar = annotateCallStackIO $ annotateStackStringIO "bar" $ do
  putStrLn "Some more work in bar"
  print (fib 21)
  decodeAndPrintAnnotationFrames

fib :: Int -> Int
fib n
  | n <= 1 = 1
  | otherwise = fib (n - 1) + fib (n - 2)
