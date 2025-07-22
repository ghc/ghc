
import GHC.Stack.Annotation.Experimental
import Control.Exception.Backtrace

hello :: Int -> Int -> Int
hello x y = annotateShow (x,y) $
    x + y + 42
{-# OPAQUE hello #-}

main :: IO ()
main = do
  setBacktraceMechanismState IPEBacktrace True
  print $ hello 2 3
  print $ tailCallEx 4 5

{-# INLINE tailCallEx #-}
tailCallEx :: Int -> Int -> Int
tailCallEx a b = annotateShow "tailCallEx" $
  foo a b

{-# INLINE foo #-}
foo :: Int -> Int -> Int
foo a b = annotateShow "foo" $
  bar $ a * b

bar c = annotateShow "bar" $
  error $ show $ c + c

