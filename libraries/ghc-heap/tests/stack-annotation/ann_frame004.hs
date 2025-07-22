{-# LANGUAGE ScopedTypeVariables #-}
import Control.Monad
import GHC.Stack.Types
import Control.Exception
import Control.Exception.Backtrace
import GHC.Stack.Annotation.Experimental

main :: IO ()
main = do
  bar

bar :: IO ()
bar = annotateCallStackM $ annotateStringM "bar" $ do
  putStrLn "Some more work in bar"
  print (annotateCallStack $ fib 21)

fib :: Int -> Int
fib n
  | n <= 1 = 1
  | n >= 21 = throw $ ErrorCall "This fib implementation supports only up to the 21st fibonacci number"
  | otherwise = fib (n - 1) + fib (n - 2)
