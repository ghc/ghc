module Generator.WordEncoding where

import Data.ByteString.Builder qualified as BB
import Data.Word

toWord8 :: (Show a, Enum a) => a -> Word8
toWord8 a =
  let w = fromEnum a
  in if 0 <= w && w <= 0xff
      then fromIntegral w
      else error $ "Cannot convert to Word8: " <> show a

-- | Encode a list of values as a byte map, using their 'Enum' instance.
--
-- __Note:__ 'Enum' instance must respect the following:
--
-- * @fromEnum minBound >= 0x00@
-- * @fromEnum maxBound <= 0xff@
enumMapToAddrLiteral ::
  forall a.
  (Bounded a, Enum a, Show a) =>
  -- | Values to encode
  [a] ->
  -- | String to append
  BB.Builder ->
  BB.Builder
enumMapToAddrLiteral xs cs = foldr go cs xs
  where
    go :: a -> BB.Builder -> BB.Builder
    go x acc = BB.char7 '\\' <> BB.word8Dec (toWord8 x) <> acc
