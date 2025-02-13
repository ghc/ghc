import GHC.Exts
import GHC.Exts.Heap.Closures as Closures
import GHC.Exts.Stack.Decode
import GHC.Stack.CloneStack
import GHC.Stack.Annotation.Experimental
import System.IO.Unsafe
import Unsafe.Coerce

hello :: Int -> Int -> Int
hello x y = annotateStack (x,y) $
  decodeAndPrintAnnotationFrames $!
    x + y + 42
{-# OPAQUE hello #-}

{-# NOINLINE decodeAndPrintAnnotationFrames #-}
decodeAndPrintAnnotationFrames :: a -> a
decodeAndPrintAnnotationFrames a = unsafePerformIO $ do
  stack <- GHC.Stack.CloneStack.cloneMyStack
  decoded <- GHC.Exts.Stack.Decode.decodeStack stack
  print [ show a
        | Closures.AnnFrame _ (Box ann) <- Closures.ssc_stack decoded
        , StackAnnotation a <- pure $ unsafeCoerce ann
        ]
  pure a

main :: IO ()
main = do
  print $ hello 2 3
  print $ tailCallEx 4 5

{-# INLINE tailCallEx #-}
tailCallEx :: Int -> Int -> Int
tailCallEx a b = annotateStack "tailCallEx" $ foo a b

{-# INLINE foo #-}
foo :: Int -> Int -> Int
foo a b = annotateStack "foo" $ bar $ a * b

bar c = annotateStack "bar" $
  decodeAndPrintAnnotationFrames $
    c + c

