{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Data.Char (toUpper)
import GHC.Exts
import GHC.Word
import Numeric (showHex)

pdep8 :: Word8 -> Word8 -> Word8
pdep8 (W8# a) (W8# b) = W8# (wordToWord8# (pdep8# (word8ToWord# a) (word8ToWord# b)))
{-# NOINLINE pdep8 #-}

pdep16 :: Word16 -> Word16 -> Word16
pdep16 (W16# a) (W16# b) = W16# (wordToWord16# (pdep16# (word16ToWord# a) (word16ToWord# b)))
{-# NOINLINE pdep16 #-}

pdep32 :: Word32 -> Word32 -> Word32
pdep32 (W32# a) (W32# b) = W32# (wordToWord32# (pdep32# (word32ToWord# a) (word32ToWord# b)))
{-# NOINLINE pdep32 #-}

pdep64 :: Word64 -> Word64 -> Word64
pdep64 (W64# a) (W64# b) = W64# (pdep64# a b)
{-# NOINLINE pdep64 #-}

pext8 :: Word8 -> Word8 -> Word8
pext8 (W8# a) (W8# b) = W8# (wordToWord8# (pext8# (word8ToWord# a) (word8ToWord# b)))
{-# NOINLINE pext8 #-}

pext16 :: Word16 -> Word16 -> Word16
pext16 (W16# a) (W16# b) = W16# (wordToWord16# (pext16# (word16ToWord# a) (word16ToWord# b)))
{-# NOINLINE pext16 #-}

pext32 :: Word32 -> Word32 -> Word32
pext32 (W32# a) (W32# b) = W32# (wordToWord32# (pext32# (word32ToWord# a) (word32ToWord# b)))
{-# NOINLINE pext32 #-}

pext64 :: Word64 -> Word64 -> Word64
pext64 (W64# a) (W64# b) = W64# (pext64# a b)
{-# NOINLINE pext64 #-}

valueSource :: Integral i => i
valueSource = fromInteger 0xA7F7A7F7A7F7A7F7

valueMask   :: Integral i => i
valueMask   = fromInteger 0x5555555555555555

printIntrinsicCall :: forall i. Integral i => String -> (i -> i -> i) -> IO ()
printIntrinsicCall label f =
  let op1 = valueSource
      op2 = valueMask
      pad s =
          let hex :: Integral a => a -> String
              hex = flip showHex ""
              str = toUpper <$> hex s
              len = length $ hex (maxBound :: Word64)
              n   = length str
          in  "0x" <> replicate (len - n) '0' <> str
  in  putStrLn $ unwords [ label, pad op1, pad op2, "=", pad (f op1 op2) ]

main :: IO ()
main = do
  printIntrinsicCall "pdep8 " pdep8
  printIntrinsicCall "pdep16" pdep16
  printIntrinsicCall "pdep32" pdep32
  printIntrinsicCall "pdep64" pdep64
  printIntrinsicCall "pext8 " pext8
  printIntrinsicCall "pext16" pext16
  printIntrinsicCall "pext32" pext32
  printIntrinsicCall "pext64" pext64
