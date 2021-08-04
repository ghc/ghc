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
-- |An 'RGBSpace' is characterized by 'Chromaticity' for red, green, and
-- blue, the 'Chromaticity' of the white point, and it's
-- 'TransferFunction'.
module Data.Colour.RGBSpace
 (Colour
  -- *RGB Tuple
 ,RGB(..)
 ,uncurryRGB, curryRGB

 -- *RGB Gamut
 ,RGBGamut
 ,mkRGBGamut, primaries, whitePoint
 ,inGamut
 -- *RGB Space
 ,TransferFunction(..)
 ,linearTransferFunction, powerTransferFunction
 ,inverseTransferFunction

 ,RGBSpace()
 ,mkRGBSpace ,gamut, transferFunction
 ,linearRGBSpace
 ,rgbUsingSpace
 ,toRGBUsingSpace
 )
where

import Data.Colour.CIE.Chromaticity
import Data.Colour.Matrix
import Data.Colour.RGB
import Data.Colour.SRGB.Linear

-- |Returns 'True' if the given colour lies inside the given gamut.
inGamut :: (Ord a, Fractional a) => RGBGamut -> Colour a -> Bool
inGamut gamut c = r && g && b
 where
  test x = 0 <= x && x <= 1
  RGB r g b = fmap test (toRGBUsingGamut gamut c)

rtf :: (Fractional b, Real a) => [[a]] -> [[b]]
rtf = map (map realToFrac)

rgbUsingGamut :: (Fractional a) => RGBGamut -> a -> a -> a -> Colour a
rgbUsingGamut gamut r g b = rgb r0 g0 b0
 where
  matrix = rtf $ matrixMult (xyz2rgb sRGBGamut) (rgb2xyz gamut)
  [r0,g0,b0] = mult matrix [r,g,b]

toRGBUsingGamut :: (Fractional a) => RGBGamut -> Colour a -> RGB a
toRGBUsingGamut gamut c = RGB r g b
 where
  RGB r0 g0 b0 = toRGB c
  matrix = rtf $ matrixMult (xyz2rgb gamut) (rgb2xyz sRGBGamut)
  [r,g,b] = mult matrix [r0,g0,b0]

-- |A 'transfer' function is a function that typically translates linear
-- colour space coordinates into non-linear coordinates.
-- The 'transferInverse' function reverses this by translating non-linear
-- colour space coordinates into linear coordinates.
-- It is required that
--
-- > transfer . transferInverse === id === transferInverse . inverse
--
-- (or that this law holds up to floating point rounding errors).
--
-- We also require that 'transfer' is approximately @(**transferGamma)@
-- (and hence 'transferInverse' is approximately
-- @(**(recip transferGamma))@).
-- The value 'transferGamma' is for informational purposes only, so there
-- is no bound on how good this approximation needs to be.
data TransferFunction a = TransferFunction
                          { transfer :: a -> a
                          , transferInverse :: a -> a
                          , transferGamma :: a }

-- |This is the identity 'TransferFunction'.
linearTransferFunction :: (Num a) => TransferFunction a
linearTransferFunction = TransferFunction id id 1

-- |This is the @(**gamma)@ 'TransferFunction'.
powerTransferFunction :: (Floating a) => a -> TransferFunction a
powerTransferFunction gamma =
  TransferFunction (**gamma) (**(recip gamma)) gamma

-- |This reverses a 'TransferFunction'.
inverseTransferFunction :: (Fractional a) => TransferFunction a -> TransferFunction a
inverseTransferFunction (TransferFunction for rev g) =
  TransferFunction rev for (recip g)

instance (Num a) => Semigroup (TransferFunction a) where
 (TransferFunction f0 f1 f) <> (TransferFunction g0 g1 g) =
   (TransferFunction (f0 . g0) (g1 . f1) (f*g))

instance (Num a) => Monoid (TransferFunction a) where
 mempty = linearTransferFunction

-- |An 'RGBSpace' is a colour coordinate system for colours laying
-- 'inGamut' of 'gamut'.
-- Linear coordinates are passed through a 'transferFunction' to
-- produce non-linear 'RGB' values.
data RGBSpace a = RGBSpace { gamut :: RGBGamut,
                             transferFunction :: TransferFunction a }

-- |An RGBSpace is specified by an 'RGBGamut' and a 'TransferFunction'.
mkRGBSpace :: RGBGamut
           -> TransferFunction a
           -> RGBSpace a
mkRGBSpace = RGBSpace

-- |Produce a linear colour space from an 'RGBGamut'.
linearRGBSpace :: (Num a) => RGBGamut -> RGBSpace a
linearRGBSpace gamut = RGBSpace gamut mempty

-- |Create a 'Colour' from red, green, and blue coordinates given in a
-- general 'RGBSpace'.
rgbUsingSpace :: (Fractional a) => RGBSpace a -> a -> a -> a -> Colour a
rgbUsingSpace space =
  curryRGB (uncurryRGB (rgbUsingGamut (gamut space)) . fmap tinv)
 where
  tinv = transferInverse (transferFunction space)

-- |Return the coordinates of a given 'Colour' for a general 'RGBSpace'.
toRGBUsingSpace :: (Fractional a) => RGBSpace a -> Colour a -> RGB a
toRGBUsingSpace space c = fmap t (toRGBUsingGamut (gamut space) c)
 where
  t = transfer (transferFunction space)
