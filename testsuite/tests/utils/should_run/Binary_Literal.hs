{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- LLM generated test. So if it's weird it's for no good reason.
--
-- Property tests for the 'Binary' instance of 'Literal' in GHC.Types.Literal.
--
-- We check that
--  * arbitrary literals round trip through 'put_' and 'get',
--  * a whole batch of literals written into one buffer reads back in order,
--    that is the reader consumes exactly the bytes the writer produced,
--  * and, since numeric literals carry an 'Integer', that the SLEB128 based
--    'Binary Integer' instance agrees with a reference implementation.
module Main (main) where

import GHC.Data.FastString
import GHC.Platform ( genericPlatform )
import GHC.Types.Basic ( FunctionOrData(..) )
import GHC.Types.Literal
import GHC.Types.Literal.Floating
import GHC.Utils.Binary

import Control.Monad ( replicateM )
import Data.Bits
import qualified Data.ByteString as BS
import Data.Ratio ( (%) )
import Data.Word
import GHC.Float ( castFloatToWord32, castWord32ToFloat
                 , castDoubleToWord64, castWord64ToDouble )
import Numeric ( showHex )
import System.IO.Unsafe ( unsafePerformIO )

import MiniQuickCheck

--------------------------------------------------------------------------------
-- Encoding and decoding

-- | 'LitLabel' contains a 'FastString', which the 'Binary' instance writes
-- through a table in the handle's user data. Interface files fill this in with
-- a deduplication table. We only need something that round trips, so we write
-- the bytes of the string inline.
withFastStringWriter :: WriteBinHandle -> WriteBinHandle
withFastStringWriter = addWriterToUserData (BinaryWriter (\bh fs -> put_ bh (bytesFS fs)))

withFastStringReader :: ReadBinHandle -> ReadBinHandle
withFastStringReader = addReaderToUserData (BinaryReader (\bh -> mkFastStringByteString <$> get bh))

-- | Serialise the values and also return the position after the last of them.
encodeAll :: Binary a => [a] -> (BS.ByteString, Bin ())
encodeAll xs = unsafePerformIO $ do
  bh <- withFastStringWriter <$> openBinMem 1024
  mapM_ (put_ bh) xs
  end <- tellBinWriter bh
  bs <- withBinBuffer bh (return . BS.copy)
  return (bs, end)

encode :: Binary a => a -> BS.ByteString
encode x = fst (encodeAll [x])

-- | Read back @n@ values and check that doing so consumed exactly the bytes
-- the writer produced, no more and no less.
decodeAll :: Binary a => Int -> (BS.ByteString, Bin ()) -> [a]
decodeAll n (bs, end) = unsafePerformIO $ do
  bh <- withFastStringReader <$> unsafeUnpackBinBuffer bs
  xs <- replicateM n (get bh)
  end' <- tellBinReader bh
  if end' == end
    then return xs
    else fail $ "reader stopped at " ++ show end' ++ ", writer at " ++ show end

roundTrip :: Binary a => [a] -> [a]
roundTrip xs = decodeAll (length xs) (encodeAll xs)

roundTrip1 :: Binary a => a -> a
roundTrip1 x = case roundTrip [x] of
  [x'] -> x'
  _    -> error "roundTrip1"

--------------------------------------------------------------------------------
-- Literals with structural equality and a Show instance

-- | 'Literal' has neither a 'Show' instance nor an 'Eq' instance which compares
-- all fields: 'LitLabel' ignores the 'FunctionOrData' and 'LitFloating'
-- identifies the different representations of the same value. For a
-- serialisation test we want the stricter notion.
newtype Lit = Lit Literal

instance Show Lit where
  show (Lit l) = showLit l

instance Eq Lit where
  Lit a == Lit b = eqLit a b

eqLit :: Literal -> Literal -> Bool
eqLit (LitLabel fs1 fod1) (LitLabel fs2 fod2)
  = fs1 == fs2 && fod1 == fod2
eqLit (LitFloating ty1 v1) (LitFloating ty2 v2)
  -- 'Eq LitFloating' compares NaNs bitwise but identifies different
  -- representations of the same value; the derived 'Show' distinguishes the
  -- representations but not NaN payloads. Together they compare structurally.
  = ty1 == ty2 && v1 == v2 && show v1 == show v2
eqLit a b = a == b

showLit :: Literal -> String
showLit lit = case lit of
  LitChar c        -> "LitChar " ++ show c
  LitNumber nt i   -> "LitNumber " ++ showLitNumType nt ++ " " ++ show i
  LitString bs     -> "LitString " ++ show bs
  LitNullAddr      -> "LitNullAddr"
  LitRubbish {}    -> "LitRubbish"
  LitFloating ty v -> "LitFloating " ++ show ty ++ " (" ++ show v ++ ") " ++ bits ty v
  LitLabel fs fod  -> "LitLabel " ++ show (bytesFS fs) ++ " " ++ showFod fod
  where
    -- The bit pattern is needed to tell apart NaNs.
    bits LitFloat  v = "0x" ++ showHex (castFloatToWord32  (litFloatingToHostFloat  v)) ""
    bits LitDouble v = "0x" ++ showHex (castDoubleToWord64 (litFloatingToHostDouble v)) ""

    showFod IsFunction = "IsFunction"
    showFod IsData     = "IsData"

showLitNumType :: LitNumType -> String
showLitNumType nt = case nt of
  LitNumBigNat -> "LitNumBigNat"
  LitNumInt    -> "LitNumInt"
  LitNumInt8   -> "LitNumInt8"
  LitNumInt16  -> "LitNumInt16"
  LitNumInt32  -> "LitNumInt32"
  LitNumInt64  -> "LitNumInt64"
  LitNumWord   -> "LitNumWord"
  LitNumWord8  -> "LitNumWord8"
  LitNumWord16 -> "LitNumWord16"
  LitNumWord32 -> "LitNumWord32"
  LitNumWord64 -> "LitNumWord64"

--------------------------------------------------------------------------------
-- Generators

-- | A number in @[0, n)@. Uses the high bits of the LCG state, which are the
-- more random ones.
choose :: Int -> Gen Int
choose n = (`mod` n) . fromIntegral . (`shiftR` 32) <$> arbitraryWord64

oneOf :: [Gen a] -> Gen a
oneOf gens = do
  i <- choose (length gens)
  gens !! i

listOf :: Int -> Gen a -> Gen [a]
listOf maxLen gen = do
  n <- choose (maxLen + 1)
  replicateM n gen

-- | 'MiniQuickCheck's 'Integer' instance generates values of up to 192 bits,
-- which rarely hit the boundaries of the SLEB128 encoding. So we mix in small
-- values and values around powers of two.
genInteger :: Gen Integer
genInteger = oneOf
  [ arbitrary
  , fromIntegral . subtract 300 <$> choose 601
  , do k <- choose 200
       d <- subtract 2 <$> choose 5
       neg <- arbitrary
       let v = 2 ^ k + toInteger d
       return (if neg then negate v else v)
  ]

genLitNumType :: Gen LitNumType
genLitNumType = oneOf (map pure [LitNumBigNat ..])

-- | Numeric literals are always in range for their type, see
-- Note [Word/Int underflow/overflow] in GHC.Types.Literal. The encoding is
-- free to rely on that, so we generate only such literals.
genLitNumber :: Gen Literal
genLitNumber = do
  nt <- genLitNumType
  i  <- genInteger
  -- 'mkLitNumberWrap' wraps into the range of the fixed width types but
  -- refuses negative 'BigNat's.
  let i' | LitNumBigNat <- nt = abs i
         | otherwise          = i
  return (mkLitNumberWrap genericPlatform nt i')

-- | Random bit patterns, so that we also get infinities, negative zero,
-- subnormals and NaNs with various payloads.
genFloat :: Gen Float
genFloat = castWord32ToFloat <$> arbitrary

genDouble :: Gen Double
genDouble = castWord64ToDouble <$> arbitrary

genRational :: Gen Rational
genRational = do
  n <- genInteger
  NonZero d <- arbitrary @(NonZero Integer)
  return (n % d)

genLitFloating :: Gen LitFloating
genLitFloating = oneOf
  [ floatToLitFloating <$> genFloat
  , doubleToLitFloating <$> genDouble
  , rationalToLitFloating <$> genRational
  ]

genLitFloatingType :: Gen LitFloatingType
genLitFloatingType = oneOf [ pure LitFloat, pure LitDouble ]

genByteString :: Gen BS.ByteString
genByteString = BS.pack <$> listOf 64 arbitrary

genFunctionOrData :: Gen FunctionOrData
genFunctionOrData = oneOf [ pure IsFunction, pure IsData ]

-- | Any literal except 'LitRubbish', which has no 'Binary' encoding, see
-- Note [Rubbish literals] in GHC.Types.Literal.
genLiteral :: Gen Literal
genLiteral = oneOf
  [ LitChar <$> arbitrary
  , genLitNumber
  , LitString <$> genByteString
  , pure LitNullAddr
  , LitFloating <$> genLitFloatingType <*> genLitFloating
  , LitLabel <$> (mkFastStringByteString <$> genByteString) <*> genFunctionOrData
  ]

instance Arbitrary Lit where
  arbitrary = Lit <$> genLiteral

newtype Lits = Lits [Lit]
  deriving (Eq, Show)

instance Arbitrary Lits where
  arbitrary = Lits <$> listOf 32 arbitrary

newtype I = I Integer
  deriving (Eq, Show)

instance Arbitrary I where
  arbitrary = I <$> genInteger

--------------------------------------------------------------------------------
-- Properties

-- | Reference implementation of the SLEB128 encoding.
slebRef :: Integer -> [Word8]
slebRef = go
  where
    go val =
      let byte = fromIntegral (val .&. 0x7f) :: Word8
          val' = val `shiftR` 7
          signBit = testBit byte 6
          done = (val' == 0 && not signBit) || (val' == -1 && signBit)
      in if done
          then [byte]
          else setBit byte 7 : go val'

prop_literalRoundTrip :: Lit -> PropertyCheck
prop_literalRoundTrip (Lit l) = Lit (roundTrip1 l) === Lit l

prop_literalBatchRoundTrip :: Lits -> PropertyCheck
prop_literalBatchRoundTrip (Lits ls) = Lits (map Lit (roundTrip [ l | Lit l <- ls ])) === Lits ls

prop_integerRoundTrip :: I -> PropertyCheck
prop_integerRoundTrip (I i) = roundTrip1 i === i

prop_integerEncoding :: I -> PropertyCheck
prop_integerEncoding (I i) = BS.unpack (encode i) === slebRef i

tests :: Test
tests = Group "Binary"
  [ Group "Literal"
      [ Property "round trip" prop_literalRoundTrip
      , Property "batch round trip" prop_literalBatchRoundTrip
      ]
  , Group "Integer"
      [ Property "round trip" prop_integerRoundTrip
      , Property "SLEB128 encoding" prop_integerEncoding
      ]
  ]

main :: IO ()
main = runTestsMain (Iterations 1000) tests
