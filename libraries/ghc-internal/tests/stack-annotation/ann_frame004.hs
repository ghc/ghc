{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Control.Exception
import TestUtils

main :: IO ()
main = do
  bar `catch` \ (_ :: ErrorCall) -> pure ()

bar :: IO ()
bar = annotateCallStackIO $ annotateStackStringIO "bar" $ do
  print (annotateCallStack $ fib 21)

fib :: Int -> Int
fib n
  | n <= 1 = 1
  | n == 8 = unsafePerformIO $ do
      decodeAndPrintAnnotationFrames
      throw $ ErrorCall "Random cut-off to force an error"
  | otherwise = annotateCallStack (fib (n - 1)) + fib (n - 2)
