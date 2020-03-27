{-# language BangPatterns #-}

module BaLitCmplTH
  ( ascii
  ) where

import Data.Char (ord)
import Data.Word
import Foreign.Ptr
import Foreign.ForeignPtr
import GHC.ForeignPtr
import Foreign.Storable
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax hiding (Bytes)
import System.IO.Unsafe

import qualified Data.List as List

ascii :: QuasiQuoter
ascii = QuasiQuoter
  { quoteExp = asciiQuote LitE
  , quotePat = asciiQuote LitP
  , quoteType = error "cannot use ascii quasiquoter for a type"
  , quoteDec = error "cannot use ascii quasiquoter for a top-level declaration"
  }

asciiQuote :: (Lit -> a) -> String -> Q a
asciiQuote g str
  | all (\c -> ord c >= 32 && ord c <= 126) str =
      pure (g (ByteArrayPrimL (uncurry3 mkBytes (packChars str))))
  | otherwise = fail "Found codepoints outside the range [32,126]"

type Bytes = (ForeignPtr Word8, Word, Word)

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c

------------------------------------------------
-- Everything below here copied from bytestring.
------------------------------------------------

packChars :: [Char] -> (ForeignPtr Word8, Word, Word)
packChars cs = unsafePackLenChars (List.length cs) cs

unsafePackLenChars :: Int -> [Char] -> Bytes
unsafePackLenChars len cs0 =
    unsafeCreate len $ \p -> go p cs0
  where
    go !_ []     = return ()
    go !p (c:cs) = poke p (c2w c) >> go (p `plusPtr` 1) cs

unsafeCreate :: Int -> (Ptr Word8 -> IO ()) -> Bytes
unsafeCreate l f = unsafeDupablePerformIO (create l f)

create :: Int -> (Ptr Word8 -> IO ()) -> IO Bytes
create l f = do
    fp <- mallocPlainForeignPtrBytes l
    withForeignPtr fp $ \p -> f p
    return (fp,0,fromIntegral l)

c2w :: Char -> Word8
c2w = fromIntegral . ord
