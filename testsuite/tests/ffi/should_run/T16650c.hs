{-# language GADTSyntax #-}
{-# language KindSignatures #-}
{-# language UnliftedFFITypes #-}
{-# language ForeignFunctionInterface #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}
{-# language ExplicitForAll #-}

-- Test for shims when passing an array of lifted values
-- to a foreign function.
-- See test T16650a for more commentary.

import GHC.Exts
import GHC.Word
import GHC.IO
import Data.Kind (Type)

main :: IO ()
main = do
  mbs <- newArray 2 ((+55) :: Int -> Int)
  case box mbs of
    Box x -> print =<< c_is_doubleton_homogeneous (unsafeCoerce# x)

foreign import ccall unsafe "is_doubleton_homogenous"
  c_is_doubleton_homogeneous :: forall (a :: Type).
    MutableArray# RealWorld a -> IO Word8

data Box :: Type where
  Box :: (Any :: TYPE 'UnliftedRep) -> Box

-- An array of unary integer functions
data MutableArray :: Type where
  MutableArray :: MutableArray# RealWorld (Int -> Int) -> MutableArray

box :: MutableArray -> Box
{-# noinline box #-}
box (MutableArray x) = Box (unsafeCoerce# x)

-- Allocate a new array of unary integer functions.
newArray :: Int -> (Int -> Int) -> IO MutableArray
newArray (I# len#) x = IO $ \s0 -> case newArray# len# x s0 of
  (# s1, a# #) -> (# s1, MutableArray a# #)

