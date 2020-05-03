{-# language BangPatterns #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}

module BaLitCmplTH
  ( ascii
  , asciiToByteArray#
  ) where

import Data.Char (ord)
import Data.Word
import Foreign.Ptr
import Foreign.ForeignPtr
import GHC.Exts
import GHC.ForeignPtr
import GHC.ST (ST(ST),runST)
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

data ByteArray = ByteArray { unByteArray :: ByteArray# }

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c

asciiToByteArray# :: [Char] -> ByteArray#
{-# noinline asciiToByteArray# #-}
asciiToByteArray# str = unByteArray $ runST $ ST $ \s0 ->
  let !(I# len) = length str in
  case newByteArray# len s0 of
    (# s1, dst #) ->
      let go [] !ix !s = case unsafeFreezeByteArray# dst s of
            (# s', res #) -> (# s', ByteArray res #)
          go (C# c : cs) !ix !s = case writeCharArray# dst ix c s of
            s' -> go cs (ix +# 1#) s'
       in go str 0# s1

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
