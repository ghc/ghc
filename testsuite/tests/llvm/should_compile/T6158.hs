module M where

import Control.Monad.Primitive
import qualified Data.Vector.Unboxed.Mutable as V
import System.IO.Unsafe (unsafePerformIO)

foo :: ()
foo = unsafePerformIO $ do
    _ <- V.replicate 1 0.0 :: IO (V.MVector RealWorld Double)
    return ()

