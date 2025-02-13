{-# LANGUAGE MagicHash #-}
{-# LANGUAGE GADTs #-}

import Data.Typeable
import GHC.Exts
import GHC.Exts.Heap.Closures as Closures
import GHC.Exts.Stack.Decode
import GHC.Stack.CloneStack
import System.IO.Unsafe
import Unsafe.Coerce

data StackAnnotation where
  StackAnnotation :: forall a. (Typeable a, Show a) => a -> StackAnnotation

annotateStack
  :: forall a r b.
     (Typeable a, Show a)
  => a -> b -> b
annotateStack ann =
  annotateStack# (StackAnnotation ann)

hello :: Int -> Int -> Int
hello x y = annotateStack (x,y) $ unsafePerformIO $ do
  stack <- GHC.Stack.CloneStack.cloneMyStack
  decoded <- GHC.Exts.Stack.Decode.decodeStack stack
  print [ show x
        | Closures.AnnFrame _ (Box ann) <- Closures.ssc_stack decoded
        , StackAnnotation x <- pure $ unsafeCoerce ann
        ]
  return $ x + y + 42
{-# OPAQUE hello #-}

main :: IO ()
main =
  print $ hello 2 3

