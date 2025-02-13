
{-# LANGUAGE ScopedTypeVariables #-}

import GHC.Exts

import GHC.Stack.Annotation.Experimental
import qualified GHC.Stack.CloneStack as CloneStack
import GHC.Exts.Heap.Closures
import qualified GHC.Exts.Stack.Decode as Decode
import GHC.Internal.Stack.Types
import qualified GHC.Internal.Stack.CloneStack as CloneStack

import System.IO.Unsafe
import Unsafe.Coerce

{-# NOINLINE decodeAnnotationFrames #-}
decodeAnnotationFrames :: IO [String]
decodeAnnotationFrames = do
  stack <- CloneStack.cloneMyStack
  decoded <- Decode.decodeStack stack
  pure
      [ show a
      | AnnFrame _ (Box ann) <- ssc_stack decoded
      , StackAnnotation a <- [unsafeCoerce ann]
      ]

{-# NOINLINE printAnnotationStack #-}
printAnnotationStack :: [String] -> IO ()
printAnnotationStack xs = do
  putStrLn "Annotation stack: "
  mapM_ putStrLn xs

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
  decodeAnnotationFrames >>= printAnnotationStack

bar :: IO ()
bar = annotateCallStackM $ annotateStackM "bar" $ do
  putStrLn "Some more ork in bar"
  print (fib 21)
  decodeAnnotationFrames >>= printAnnotationStack

fib :: Int -> Int
fib n
  | n <= 1 = 1
  | otherwise = fib (n - 1) + fib (n - 2)
