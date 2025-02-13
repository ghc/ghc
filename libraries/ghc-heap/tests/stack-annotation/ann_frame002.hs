
{-# LANGUAGE ScopedTypeVariables #-}

import TestUtils

main :: IO ()
main = do
  foo baz
  bar

foo :: HasCallStack => IO () -> IO ()
foo act = annotateCallStackM $ do
  putStrLn "Start some work"
  act
  putStrLn "Finish some work"

baz :: HasCallStack => IO ()
baz = annotateCallStackM $ do
  print (fib 20)
  decodeAndPrintAnnotationFrames

bar :: IO ()
bar = annotateCallStackM $ annotateStringM "bar" $ do
  putStrLn "Some more work in bar"
  print (fib 21)
  decodeAndPrintAnnotationFrames

fib :: Int -> Int
fib n
  | n <= 1 = 1
  | otherwise = fib (n - 1) + fib (n - 2)
