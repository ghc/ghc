
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
import GHC.Exts.Heap.Closures (GenStgStackClosure)

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
bar = annotateCallStackM $ annotateStringM "bar" $ do
  putStrLn "Some more work in bar"
  print (fib 21)
  decodeAnnotationFrames >>= printAnnotationStack

fib :: Int -> Int
fib n
  | n <= 1 = 1
  | otherwise = fib (n - 1) + fib (n - 2)

{-# NOINLINE decodeAnnotationFrames #-}
decodeAnnotationFrames :: IO [String]
decodeAnnotationFrames = do
  stack <- CloneStack.cloneMyStack
  decoded <- Decode.decodeStack stack
  pure $ unwindStack decoded

unwindStack :: GenStgStackClosure Box -> [String]
unwindStack stack_closure =
  [ ann
  | a <- ssc_stack stack_closure
  , ann <- case a of
          AnnFrame _ (Box ann) ->
            [ displayStackAnnotation a
            | SomeStackAnnotation a <- [unsafeCoerce ann]
            ]
          UnderflowFrame _ underflow_stack_closure -> unwindStack underflow_stack_closure
          _ -> []
  ]
