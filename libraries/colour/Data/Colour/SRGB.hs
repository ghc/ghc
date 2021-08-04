{-
Copyright (c) 2008
Russell O'Connor

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
-}
-- |Specifies 'Colour's in accordance with the sRGB standard.
module Data.Colour.SRGB
 (Colour, RGB(..)
 ,sRGB24, sRGBBounded, sRGB
 ,toSRGB24, toSRGBBounded, toSRGB

 ,sRGB24shows, sRGB24show
 ,sRGB24reads, sRGB24read

 ,sRGBSpace
 )
where

import Data.Word (Word8)
import Numeric (readHex, showHex)
import Data.Colour.Internal (quantize)
import Data.Colour.SRGB.Linear
import Data.Colour.RGBSpace hiding (transferFunction)

{- Non-linear colour space -}
{- the sRGB transfer function approximates a gamma of about 1/2.2 -}
transferFunction lin | lin == 1         = 1
                     | lin <= 0.0031308 = 12.92*lin
                     | otherwise        = (1 + a)*lin**(1/2.4) - a
 where
  a = 0.055

invTransferFunction nonLin | nonLin == 1       = 1
                           | nonLin <= 0.04045 = nonLin/12.92
                           | otherwise         =
  ((nonLin + a)/(1 + a))**2.4
 where
  a = 0.055

-- |Construct a colour from an sRGB specification.
-- Input components are expected to be in the range [0..1].
sRGB :: (Ord b, Floating b) =>  b -> b -> b -> Colour b
sRGB = curryRGB (uncurryRGB rgb . fmap invTransferFunction)

-- |Construct a colour from an sRGB specification.
-- Input components are expected to be in the range [0..'maxBound'].
sRGBBounded :: (Ord b, Floating b, Integral a, Bounded a) =>
               a -> a -> a -> Colour b
sRGBBounded r' g' b' = uncurryRGB sRGB (fmap f (RGB r' g' b'))
 where
  f x' = (fromIntegral x'/m)
  m = fromIntegral $ maxBound `asTypeOf` r'

-- |Construct a colour from a 24-bit (three 8-bit words) sRGB
-- specification.
sRGB24 :: (Ord b, Floating b) => Word8 -> Word8 -> Word8 -> Colour b
sRGB24 = sRGBBounded

-- |Return the sRGB colour components in the range [0..1].
toSRGB :: (Ord b, Floating b) => Colour b -> RGB b
toSRGB c = fmap transferFunction (toRGB c)

{- Results are clamped and quantized -}
-- |Return the approximate sRGB colour components in the range
-- [0..'maxBound'].
-- Out of range values are clamped.
toSRGBBounded :: (RealFrac b, Floating b, Integral a, Bounded a) =>
                 Colour b -> RGB a
toSRGBBounded c = fmap f (toSRGB c)
 where
  f x' = quantize (m*x')
  m = fromIntegral $ maxBound `asTypeOf` (f undefined)

-- |Return the approximate 24-bit sRGB colour components as three 8-bit
-- components.
-- Out of range values are clamped.
toSRGB24 :: (RealFrac b, Floating b) => Colour b -> RGB Word8
toSRGB24 = toSRGBBounded

-- |Show a colour in hexadecimal form, e.g. \"#00aaff\"
sRGB24shows :: (RealFrac b, Floating b) => Colour b -> ShowS
sRGB24shows c =
  ("#"++) . showHex2 r' . showHex2 g' . showHex2 b'
 where
  RGB r' g' b' = toSRGB24 c
  showHex2 x | x <= 0xf = ("0"++) . showHex x
             | otherwise = showHex x

-- |Show a colour in hexadecimal form, e.g. \"#00aaff\"
sRGB24show :: (RealFrac b, Floating b) => Colour b -> String
sRGB24show x = sRGB24shows x ""

-- |Read a colour in hexadecimal form, e.g. \"#00aaff\" or \"00aaff\"
sRGB24reads :: (Ord b, Floating b) => ReadS (Colour b)
sRGB24reads "" = []
sRGB24reads x =
  [(sRGB24 a b c, c0)
  |(a,a0) <- readPair x', (b,b0) <- readPair a0, (c,c0) <- readPair b0]
 where
  x' | head x == '#' = tail x
     | otherwise = x
  readPair [] = []
  readPair [_] = []
  readPair a = [(x,a1)|(x,"") <- readHex a0]
   where
    (a0,a1) = splitAt 2 a

-- |Read a colour in hexadecimal form, e.g. \"#00aaff\" or \"00aaff\"
sRGB24read :: (Ord b, Floating b) => String -> (Colour b)
sRGB24read x | length rx /= 1 || not (null (snd (head rx))) =
  error "Data.Colour.SRGB.sRGB24read: no parse"
             | otherwise = fst (head rx)
 where
  rx = sRGB24reads x

-- |The sRGB colour space
sRGBSpace :: (Ord a, Floating a) => RGBSpace a
sRGBSpace = mkRGBSpace sRGBGamut transfer
 where
  transfer = TransferFunction transferFunction invTransferFunction (recip 2.2)
