{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE ForeignFunctionInterface #-}
----------------------------------------------------------------
--                                                    2010.10.09
-- |
-- Module      :  IsSpace
-- Copyright   :  Copyright (c) 2010 wren ng thornton
-- License     :  BSD
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  experimental
-- Portability :  portable (FFI)
--
-- A benchmark for comparing different definitions of predicates
-- for detecting whitespace. As of the last run the results are:
-- 
-- * Data.Char.isSpace             : 14.44786 us +/- 258.0377 ns
-- * isSpace_DataChar              : 43.25154 us +/- 655.7037 ns
-- * isSpace_Char                  : 29.26598 us +/- 454.1445 ns
-- * isPerlSpace                   :
-- * Data.Attoparsec.Char8.isSpace : 81.87335 us +/- 1.195903 us
-- * isSpace_Char8                 : 11.84677 us +/- 178.9795 ns
-- * isSpace_w8                    : 11.55470 us +/- 133.7644 ns
----------------------------------------------------------------
module IsSpace (main) where

import qualified Data.Char             as C
import           Data.Word             (Word8)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as B8
import           Foreign.C.Types       (CInt)

import           Criterion             (bench, nf)
import           Criterion.Main        (defaultMain)

----------------------------------------------------------------
----- Character predicates
-- N.B. \x9..\xD == "\t\n\v\f\r"

-- | Recognize the same characters as Perl's @/\s/@ in Unicode mode.
-- In particular, we recognize POSIX 1003.2 @[[:space:]]@ except
-- @\'\v\'@, and recognize the Unicode @\'\x85\'@, @\'\x2028\'@,
-- @\'\x2029\'@. Notably, @\'\x85\'@ belongs to Latin-1 (but not
-- ASCII) and therefore does not belong to POSIX 1003.2 @[[:space:]]@
-- (nor non-Unicode @/\s/@).
isPerlSpace :: Char -> Bool
isPerlSpace c
    =  (' '      == c)
    || ('\t' <= c && c <= '\r' && c /= '\v')
    || ('\x85'   == c)
    || ('\x2028' == c)
    || ('\x2029' == c)
{-# INLINE isPerlSpace #-}


-- | 'Data.Attoparsec.Char8.isSpace', duplicated here because it's
-- not exported. This is the definition as of attoparsec-0.8.1.0.
isSpace :: Char -> Bool
isSpace c = c `B8.elem` spaces
    where
    spaces = B8.pack " \n\r\t\v\f"
    {-# NOINLINE spaces #-}
{-# INLINE isSpace #-}


-- | An alternate version of 'Data.Attoparsec.Char8.isSpace'.
isSpace_Char8 :: Char -> Bool
isSpace_Char8 c =  (' ' == c) || ('\t' <= c && c <= '\r')
{-# INLINE isSpace_Char8 #-}


-- | An alternate version of 'Data.Char.isSpace'. This uses the
-- same trick as 'isSpace_Char8' but we include Unicode whitespaces
-- too, in order to have the same results as 'Data.Char.isSpace'
-- (whereas 'isSpace_Char8' doesn't recognize Unicode whitespace).
isSpace_Char :: Char -> Bool
isSpace_Char c
    =  (' '    == c)
    || ('\t' <= c && c <= '\r')
    || ('\xA0' == c)
    || (iswspace (fromIntegral (C.ord c)) /= 0)
{-# INLINE isSpace_Char #-}

foreign import ccall unsafe "u_iswspace"
    iswspace :: CInt -> CInt

-- | Verbatim version of 'Data.Char.isSpace' (i.e., 'GHC.Unicode.isSpace'
-- as of base-4.2.0.2) in order to try to figure out why 'isSpace_Char'
-- is slower than 'Data.Char.isSpace'. It appears to be something
-- special in how the base library was compiled.
isSpace_DataChar :: Char -> Bool
isSpace_DataChar c =
    c == ' '     ||
    c == '\t'    ||
    c == '\n'    ||
    c == '\r'    ||
    c == '\f'    ||
    c == '\v'    ||
    c == '\xa0'  ||
    iswspace (fromIntegral (C.ord c)) /= 0
{-# INLINE isSpace_DataChar #-}


-- | A 'Word8' version of 'Data.Attoparsec.Char8.isSpace'.
isSpace_w8 :: Word8 -> Bool
isSpace_w8 w = (w == 32) || (9 <= w && w <= 13)
{-# INLINE isSpace_w8 #-}

----------------------------------------------------------------

main :: IO ()
main = defaultMain
    [ bench "Data.Char.isSpace" $ nf (map C.isSpace)        ['\x0'..'\255']
    , bench "isSpace_DataChar"  $ nf (map isSpace_DataChar) ['\x0'..'\255']
    , bench "isSpace_Char"      $ nf (map isSpace_Char)     ['\x0'..'\255']
    , bench "isPerlSpace"       $ nf (map isPerlSpace)      ['\x0'..'\255']
    , bench "Data.Attoparsec.Char8.isSpace"
                                $ nf (map isSpace)          ['\x0'..'\255']
    , bench "isSpace_Char8"     $ nf (map isSpace_Char8)    ['\x0'..'\255']
    , bench "isSpace_w8"        $ nf (map isSpace_w8)       [0..255]
    ]

----------------------------------------------------------------
----------------------------------------------------------- fin.
