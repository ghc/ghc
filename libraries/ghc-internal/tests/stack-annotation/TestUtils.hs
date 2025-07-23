{-# LANGUAGE LambdaCase #-}
module TestUtils (
  decodeAndPrintAnnotationFrames,
  unsafeDecodeAndPrintAnnotationFrames,
  decodeAnnotationFrames,
  -- * Re-exports to make tests shorter
  HasCallStack,
  unsafePerformIO,
  module GHC.Stack.Annotation.Experimental,
)
where

import Control.Exception
import GHC.Exts
import GHC.Exts.Heap.Closures as Closures
import GHC.Exts.Stack.Decode
import GHC.Stack.Annotation.Experimental
import GHC.Stack.CloneStack
import GHC.Stack.Types
import System.IO.Unsafe
import Unsafe.Coerce

{-# NOINLINE decodeAndPrintAnnotationFrames #-}
decodeAndPrintAnnotationFrames :: IO ()
decodeAndPrintAnnotationFrames =
  decodeAnnotationFrames >>= \ case
    [] -> putStrLn "No stack annotations found"
    annos -> do
      putStrLn "Stack annotations:"
      mapM_ (putStrLn . ("- " ++)) annos

unsafeDecodeAndPrintAnnotationFrames :: a -> a
unsafeDecodeAndPrintAnnotationFrames a =
  unsafePerformIO $ do
    decodeAndPrintAnnotationFrames
    evaluate a

decodeAnnotationFrames :: IO [String]
decodeAnnotationFrames = do
  stack <- GHC.Stack.CloneStack.cloneMyStack
  decoded <- GHC.Exts.Stack.Decode.decodeStack stack
  pure [ displayStackAnnotation a
       | Closures.AnnFrame _ (Box ann) <- Closures.ssc_stack decoded
       , SomeStackAnnotation a <- pure $ unsafeCoerce ann
       ]
