{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

-- | Test sized primops (Int8#, Word16#, etc.)
module Main where

import GHC.Exts
import GHC.Word
import GHC.Int
import GHC.Num.Integer
import Control.Monad

data N a = N
  { name                :: String
  , min_bound           :: a
  , max_bound           :: a
  , signed              :: Bool
  , nbits               :: Word
  , shift_right_logical :: a -> Word -> a
  , shift_right_arith   :: Maybe (a -> Word -> a)
  , shift_left          :: a -> Word -> a
  , eq                  :: a -> a -> Bool
  , gt                  :: a -> a -> Bool
  , ge                  :: a -> a -> Bool
  , lt                  :: a -> a -> Bool
  , le                  :: a -> a -> Bool
  , from_integer        :: Integer -> a
  , mbit_and            :: Maybe (a -> a -> a)
  , mbit_or             :: Maybe (a -> a -> a)
  , mbit_xor            :: Maybe (a -> a -> a)
  }

w8 :: N Word8
w8 = N
  { name                = "W8"
  , min_bound           = minBound
  , max_bound           = maxBound
  , signed              = False
  , nbits               = 8
  , shift_right_logical = \(W8# a) (W# n) -> W8# (uncheckedShiftRLWord8# a (word2Int# n))
  , shift_right_arith   = Nothing
  , shift_left          = \(W8# a) (W# n) -> W8# (uncheckedShiftLWord8# a (word2Int# n))
  , eq                  = \(W8# a) (W8# b) -> isTrue# (eqWord8# a b)
  , gt                  = \(W8# a) (W8# b) -> isTrue# (gtWord8# a b)
  , ge                  = \(W8# a) (W8# b) -> isTrue# (geWord8# a b)
  , lt                  = \(W8# a) (W8# b) -> isTrue# (ltWord8# a b)
  , le                  = \(W8# a) (W8# b) -> isTrue# (leWord8# a b)
  , from_integer        = fromIntegral
  , mbit_and            = Just \(W8# a) (W8# b) -> W8# (andWord8# a b)
  , mbit_or             = Just \(W8# a) (W8# b) -> W8# (orWord8# a b)
  , mbit_xor            = Just \(W8# a) (W8# b) -> W8# (xorWord8# a b)
  }

w16 :: N Word16
w16 = N
  { name                = "W16"
  , min_bound           = minBound
  , max_bound           = maxBound
  , signed              = False
  , nbits               = 16
  , shift_right_logical = \(W16# a) (W# n) -> W16# (uncheckedShiftRLWord16# a (word2Int# n))
  , shift_right_arith   = Nothing
  , shift_left          = \(W16# a) (W# n) -> W16# (uncheckedShiftLWord16# a (word2Int# n))
  , eq                  = \(W16# a) (W16# b) -> isTrue# (eqWord16# a b)
  , gt                  = \(W16# a) (W16# b) -> isTrue# (gtWord16# a b)
  , ge                  = \(W16# a) (W16# b) -> isTrue# (geWord16# a b)
  , lt                  = \(W16# a) (W16# b) -> isTrue# (ltWord16# a b)
  , le                  = \(W16# a) (W16# b) -> isTrue# (leWord16# a b)
  , from_integer        = fromIntegral
  , mbit_and            = Just \(W16# a) (W16# b) -> W16# (andWord16# a b)
  , mbit_or             = Just \(W16# a) (W16# b) -> W16# (orWord16# a b)
  , mbit_xor            = Just \(W16# a) (W16# b) -> W16# (xorWord16# a b)
  }

w32 :: N Word32
w32 = N
  { name                = "W32"
  , min_bound           = minBound
  , max_bound           = maxBound
  , signed              = False
  , nbits               = 32
  , shift_right_logical = \(W32# a) (W# n) -> W32# (uncheckedShiftRLWord32# a (word2Int# n))
  , shift_right_arith   = Nothing
  , shift_left          = \(W32# a) (W# n) -> W32# (uncheckedShiftLWord32# a (word2Int# n))
  , eq                  = \(W32# a) (W32# b) -> isTrue# (eqWord32# a b)
  , gt                  = \(W32# a) (W32# b) -> isTrue# (gtWord32# a b)
  , ge                  = \(W32# a) (W32# b) -> isTrue# (geWord32# a b)
  , lt                  = \(W32# a) (W32# b) -> isTrue# (ltWord32# a b)
  , le                  = \(W32# a) (W32# b) -> isTrue# (leWord32# a b)
  , from_integer        = fromIntegral
  , mbit_and            = Just \(W32# a) (W32# b) -> W32# (andWord32# a b)
  , mbit_or             = Just \(W32# a) (W32# b) -> W32# (orWord32# a b)
  , mbit_xor            = Just \(W32# a) (W32# b) -> W32# (xorWord32# a b)
  }


i8 :: N Int8
i8 = N
  { name                = "I8"
  , min_bound           = minBound
  , max_bound           = maxBound
  , signed              = True
  , nbits               = 8
  , shift_right_logical = \(I8# a) (W# n) -> I8# (uncheckedShiftRLInt8# a (word2Int# n))
  , shift_right_arith   = Just \(I8# a) (W# n) -> I8# (uncheckedShiftRAInt8# a (word2Int# n))
  , shift_left          = \(I8# a) (W# n) -> I8# (uncheckedShiftLInt8# a (word2Int# n))
  , eq                  = \(I8# a) (I8# b) -> isTrue# (eqInt8# a b)
  , gt                  = \(I8# a) (I8# b) -> isTrue# (gtInt8# a b)
  , ge                  = \(I8# a) (I8# b) -> isTrue# (geInt8# a b)
  , lt                  = \(I8# a) (I8# b) -> isTrue# (ltInt8# a b)
  , le                  = \(I8# a) (I8# b) -> isTrue# (leInt8# a b)
  , from_integer        = fromIntegral
  , mbit_and            = Nothing
  , mbit_or             = Nothing
  , mbit_xor            = Nothing
  }

i16 :: N Int16
i16 = N
  { name                = "I16"
  , min_bound           = minBound
  , max_bound           = maxBound
  , signed              = True
  , nbits               = 16
  , shift_right_logical = \(I16# a) (W# n) -> I16# (uncheckedShiftRLInt16# a (word2Int# n))
  , shift_right_arith   = Just \(I16# a) (W# n) -> I16# (uncheckedShiftRAInt16# a (word2Int# n))
  , shift_left          = \(I16# a) (W# n) -> I16# (uncheckedShiftLInt16# a (word2Int# n))
  , eq                  = \(I16# a) (I16# b) -> isTrue# (eqInt16# a b)
  , gt                  = \(I16# a) (I16# b) -> isTrue# (gtInt16# a b)
  , ge                  = \(I16# a) (I16# b) -> isTrue# (geInt16# a b)
  , lt                  = \(I16# a) (I16# b) -> isTrue# (ltInt16# a b)
  , le                  = \(I16# a) (I16# b) -> isTrue# (leInt16# a b)
  , from_integer        = fromIntegral
  , mbit_and            = Nothing
  , mbit_or             = Nothing
  , mbit_xor            = Nothing
  }

i32 :: N Int32
i32 = N
  { name                = "I32"
  , min_bound           = minBound
  , max_bound           = maxBound
  , signed              = True
  , nbits               = 32
  , shift_right_logical = \(I32# a) (W# n) -> I32# (uncheckedShiftRLInt32# a (word2Int# n))
  , shift_right_arith   = Just \(I32# a) (W# n) -> I32# (uncheckedShiftRAInt32# a (word2Int# n))
  , shift_left          = \(I32# a) (W# n) -> I32# (uncheckedShiftLInt32# a (word2Int# n))
  , eq                  = \(I32# a) (I32# b) -> isTrue# (eqInt32# a b)
  , gt                  = \(I32# a) (I32# b) -> isTrue# (gtInt32# a b)
  , ge                  = \(I32# a) (I32# b) -> isTrue# (geInt32# a b)
  , lt                  = \(I32# a) (I32# b) -> isTrue# (ltInt32# a b)
  , le                  = \(I32# a) (I32# b) -> isTrue# (leInt32# a b)
  , from_integer        = fromIntegral
  , mbit_and            = Nothing
  , mbit_or             = Nothing
  , mbit_xor            = Nothing
  }


main :: IO ()
main = do
  test w8
  test w16
  test w32

  test i8
  test i16
  test i32

{-# NOINLINE test #-}
test :: (Integral a, Show a) => N a -> IO ()
test sys@(N {..}) = do

  putStrLn $ "--------------------------"
  putStrLn $ "Testing " ++ name
  putStrLn $ "--------------------------"

  let assert s False = putStrLn ("FAILED: " ++ s)
      --assert s True = return ()
      assert s True = putStrLn ("PASSED: " ++ s)

  let zero = from_integer 0
  let one  = from_integer 1

  -- right-shift zero must be zero
  forM_ [0..nbits-1] \n ->
    assert ("0 >> " ++ show n ++ " == 0")
      (zero `eq` shift_right_logical zero n)

  -- left-shift zero must be zero
  forM_ [0..nbits-1] \n ->
    assert ("0 << " ++ show n ++ " == 0")
      (zero `eq` shift_left zero n)

  -- left-shift 1
  forM_ [0..nbits-1] \n -> do
    let expected = from_integer (1 `integerShiftL` n)
    assert ("1 << " ++ show n ++ " == " ++ show expected)
      (expected `eq` shift_left one n)

  -- logical right-shift minBound
  forM_ [0..nbits-1] \n -> do
    let expected
         | n == 0    = min_bound
         | otherwise = from_integer (integerAbs (fromIntegral min_bound) `integerShiftR` n)
    assert (show min_bound ++ " >> " ++ show n ++ " == " ++ show expected ++ " (logical)")
      (expected `eq` shift_right_logical min_bound n)

  -- arithmetic right-shift minBound
  forM_ shift_right_arith \shift_right_arithmetic -> do
    forM_ [0..nbits-1] \n -> do
      let minb = fromIntegral min_bound :: Integer
      let expected = from_integer (minb `integerShiftR` n)
      assert (show min_bound ++ " >> " ++ show n ++ " == " ++ show expected ++ " (arithmetic)")
        (expected `eq` shift_right_arithmetic min_bound n)

  -- and with 0 must be 0
  forM_ mbit_and \bit_and -> do
    forM_ [0..nbits-1] \n -> do
      let v = one `shift_left` n
      assert ("0 .&. " ++ show v ++ " == 0")
        (zero `eq` bit_and v zero)

    -- test with an overflowed value
    let v = max_bound + max_bound + max_bound
    assert ("0 .&. " ++ show v ++ " == 0")
      (zero `eq` bit_and v zero)

  -- or with 0 must be constant
  forM_ mbit_or \bit_or -> do
    forM_ [0..nbits-1] \n -> do
      let v = one `shift_left` n
      assert ("0 .|. " ++ show v ++ " == " ++ show v)
        (v `eq` bit_or v zero)

    -- test with an overflowed value
    let v = max_bound + max_bound + max_bound
    assert ("0 .|. " ++ show v ++ " == " ++ show v)
      (v `eq` bit_or v zero)

  -- xor . xor = id
  forM_ mbit_xor \bit_xor -> do
    forM_ [0..nbits-2] \n -> do
      -- v0 == v0' but hopefully GHC doesn't see it statically
      let v0  = one `shift_left` n
      let v1  = one `shift_left` (n+1)
      let v0' = v1 `shift_right_logical` 1
      let v = max_bound + max_bound + max_bound
      assert (show v ++ " `xor` " ++ show v0 ++ " == " ++ show v0' ++ " `xor` " ++ show v)
        (bit_xor v v0 `eq` bit_xor v0' v)

  -- test comparison operators
  forM_ [0..nbits-2-(if signed then 1 else 0)] \n -> do
    -- v0 == v0' but hopefully GHC doesn't see it statically
    let v0 = one `shift_left` n
    let v1 = one `shift_left` (n+1)
    let v0' = v1 `shift_right_logical` 1
    assert (show v0 ++ " < " ++ show v1)
      (v0 `lt` v1)
    assert (show v0 ++ " <= " ++ show v1)
      (v0 `le` v1)
    assert (show v0 ++ " > " ++ show v1)
      (v1 `gt` v0)

    assert (show v0 ++ " <= " ++ show v0')
      (v0 `le` v0')
    assert (show v0 ++ " >= " ++ show v0')
      (v0 `ge` v0')
    assert (show v0 ++ " == " ++ show v0')
      (v0 `eq` v0')
