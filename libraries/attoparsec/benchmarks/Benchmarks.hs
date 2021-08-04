{-# LANGUAGE BangPatterns, CPP #-}

import Common ()
import Control.Applicative (many)
import Criterion.Main (bench, bgroup, defaultMain, nf)
import Data.Bits
import Data.Char (isAlpha)
import Data.Word (Word32)
import Data.Word (Word8)
import Numbers (numbers)
import Common (chunksOf)
import Text.Parsec.Text ()
import Text.Parsec.Text.Lazy ()
import qualified Warp
import qualified Aeson
import qualified Genome
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.Attoparsec.ByteString.Lazy as ABL
import qualified Data.Attoparsec.Text as AT
import qualified Data.Attoparsec.Text.Lazy as ATL
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified HeadersByteString
import qualified HeadersText
import qualified Links
import qualified Text.Parsec as P
import qualified Sets

main :: IO ()
main = do
  let s  = take 1024 . cycle $ ['a'..'z'] ++ ['A'..'Z']
      !b = BC.pack s
      !bl = BL.fromChunks . map BC.pack . chunksOf 4 $ s
      !t = T.pack s
      !tl = TL.fromChunks . map T.pack . chunksOf 4 $ s
  aeson <- Aeson.aeson
  headersBS <- HeadersByteString.headers
  headersT <- HeadersText.headers
  defaultMain [
     bgroup "many" [
       bgroup "attoparsec" [
         bench "B" $ nf (AB.parse (many (AC.satisfy AC.isAlpha_ascii))) b
       , bench "BL" $ nf (ABL.parse (many (AC.satisfy AC.isAlpha_ascii))) bl
       , bench "T" $ nf (AT.parse (many (AT.satisfy AC.isAlpha_ascii))) t
       , bench "TL" $ nf (ATL.parse (many (AT.satisfy AC.isAlpha_ascii))) tl
       ]
     , bgroup "parsec" [
         bench "S" $ nf (P.parse (many (P.satisfy AC.isAlpha_ascii)) "") s
       , bench "B" $ nf (P.parse (many (P.satisfy AC.isAlpha_ascii)) "") b
       , bench "BL" $ nf (P.parse (many (P.satisfy AC.isAlpha_ascii)) "") bl
       , bench "T" $ nf (P.parse (many (P.satisfy AC.isAlpha_ascii)) "") t
       , bench "TL" $ nf (P.parse (many (P.satisfy AC.isAlpha_ascii)) "") tl
       ]
     ]
   , bgroup "comparison" [
       bgroup "many-vs-takeWhile" [
         bench "many" $ nf (AB.parse (many (AC.satisfy AC.isAlpha_ascii))) b
       , bench "takeWhile" $ nf (AB.parse (AC.takeWhile AC.isAlpha_ascii)) b
       ]
     , bgroup "letter-vs-isAlpha" [
         bench "letter" $ nf (AB.parse (many AC.letter_ascii)) b
       , bench "isAlpha" $ nf (AB.parse (many (AC.satisfy AC.isAlpha_ascii))) b
       ]
     ]
   , bgroup "takeWhile" [
       bench "isAlpha" $ nf (ABL.parse (AC.takeWhile isAlpha)) bl
     , bench "isAlpha_ascii" $ nf (ABL.parse (AC.takeWhile AC.isAlpha_ascii)) bl
     , bench "isAlpha_iso8859_15" $
       nf (ABL.parse (AC.takeWhile AC.isAlpha_iso8859_15)) bl
     , bench "T isAlpha" $ nf (AT.parse (AT.takeWhile isAlpha)) t
     , bench "TL isAlpha" $ nf (ATL.parse (AT.takeWhile isAlpha)) tl
     ]
   , bgroup "takeWhile1" [
       bench "isAlpha" $ nf (ABL.parse (AC.takeWhile1 isAlpha)) bl
     , bench "isAlpha_ascii" $ nf (ABL.parse (AC.takeWhile1 AC.isAlpha_ascii)) bl
     , bench "T isAlpha" $ nf (AT.parse (AT.takeWhile1 isAlpha)) t
     , bench "TL isAlpha" $ nf (ATL.parse (AT.takeWhile1 isAlpha)) tl
     ]
   , bench "word32LE" $ nf (AB.parse word32LE) b
   , bgroup "scan" [
       bench "short" $ nf (AB.parse quotedString) (BC.pack "abcdefghijk\"")
     , bench "long" $ nf (AB.parse quotedString) b
     ]
   , aeson
   , Genome.genome
   , headersBS
   , headersT
   , Links.links
   , numbers
   , Sets.benchmarks
   , Warp.benchmarks
   ]

-- Benchmarks bind and (potential) bounds-check merging.
word32LE :: AB.Parser Word32
word32LE = do
    w1 <- AB.anyWord8
    w2 <- AB.anyWord8
    w3 <- AB.anyWord8
    w4 <- AB.anyWord8
    return $! (fromIntegral w1 :: Word32) +
        fromIntegral w2 `unsafeShiftL` 8 +
        fromIntegral w3 `unsafeShiftL` 16 +
        fromIntegral w4 `unsafeShiftL` 32

doubleQuote, backslash :: Word8
doubleQuote = 34
backslash = 92
{-# INLINE backslash #-}
{-# INLINE doubleQuote #-}

-- | Parse a string without a leading quote.
quotedString :: AB.Parser B.ByteString
quotedString = AB.scan False $ \s c -> if s then Just False
                                       else if c == doubleQuote
                                            then Nothing
                                            else Just (c == backslash)

#if !MIN_VERSION_base(4,5,0)
unsafeShiftL :: Bits a => a -> Int -> a
unsafeShiftL = shiftL
#endif
