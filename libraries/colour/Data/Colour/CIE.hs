{-
Copyright (c) 2008, 2009
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
-- |Colour operations defined by the International Commission on
-- Illumination (CIE).
module Data.Colour.CIE
 (Colour
 ,cieXYZ, cieXYZView, luminance
 ,toCIEXYZ -- depricated

 ,Chromaticity
 ,mkChromaticity, chromaCoords
 ,chromaX, chromaY, chromaZ
 ,chromaConvert
 ,chromaColour

 ,lightness, cieLABView, cieLAB --cieLuv
 )
where

import Data.List (foldl1')
import Data.Colour
import Data.Colour.RGB
import Data.Colour.SRGB.Linear
import Data.Colour.CIE.Chromaticity
import Data.Colour.Matrix

-- |Construct a 'Colour' from XYZ coordinates for the 2&#176; standard
-- (colourimetric) observer.
cieXYZ :: (Fractional a) => a -> a -> a -> Colour a
cieXYZ x y z = rgb r g b
 where
  [r,g,b] = mult matrix [x,y,z]
  matrix = map (map fromRational) xyz2rgb709

-- |Returns the XYZ colour coordinates for the 2&#176; standard
-- (colourimetric) observer.
cieXYZView :: (Fractional a) => Colour a -> (a,a,a)
cieXYZView c = (x,y,z)
 where
  RGB r g b = toRGB c
  [x,y,z] = mult matrix [r,g,b]
  matrix = map (map fromRational) rgb7092xyz

{-# DEPRECATED toCIEXYZ "`toCIEXYZ' has been renamed `cieXYZView'" #-}
toCIEXYZ x = cieXYZView x

{- CIE luminance -}
-- |Returns the Y colour coordinate (luminance) for the 2&#176; standard
-- (colourimetric) observer.
luminance :: (Fractional a) => Colour a -> a
luminance c = y
 where
  (x,y,z) = toCIEXYZ c

instance AffineSpace Chromaticity where
 affineCombo l z =
   foldl1' chromaAdd [chromaScale w a | (w,a) <- (1-total,z):l]
  where
   total = sum $ map fst l
   (Chroma x0 y0) `chromaAdd` (Chroma x1 y1) = Chroma (x0+x1) (y0+y1)
   s `chromaScale` (Chroma x y) = Chroma (s*x) (s*y)

-- |Constructs a colour from the given 'Chromaticity' and 'luminance'.
chromaColour :: (Fractional a) =>
                Chromaticity a
             -> a              -- ^ 'luminance'
             -> Colour a
chromaColour ch y = cieXYZ (s*ch_x) y (s*ch_z)
 where
  (ch_x, ch_y, ch_z) = chromaCoords ch
  s = y/ch_y

-- |Returns the lightness of a colour with respect to a given white point.
-- Lightness is a perceptually uniform measure.
lightness :: (Ord a, Floating a) => Chromaticity a -- ^White point
                                 -> Colour a -> a
lightness white_ch c | (6/29)^3 < y' = 116*y'**(1/3) - 16
                     | otherwise = (29/3)^3*y'
 where
  white = chromaColour white_ch 1.0
  y' = (luminance c/luminance white)

-- |Returns the CIELAB coordinates of a colour, which is a
-- perceptually uniform colour space.
-- The first coordinate is 'lightness'.
-- If you don't know what white point to use, use
-- 'Data.Colour.CIE.Illuminant.d65'.
cieLABView :: (Ord a, Floating a) => Chromaticity a -- ^White point
                              -> Colour a -> (a,a,a)
cieLABView white_ch c = (lightness white_ch c, a, b)
 where
  white = chromaColour white_ch 1.0
  (x,y,z) = toCIEXYZ c
  (xn,yn,zn) = toCIEXYZ white
  (fx, fy, fz) = (f (x/xn), f (y/yn), f (z/zn))
  a = 500*(fx - fy)
  b = 200*(fy - fz)
  f x | (6/29)^3 < x = x**(1/3)
      | otherwise = 841/108*x + 4/29

-- |Returns the colour for given CIELAB coordinates, which is a
-- perceptually uniform colour space.
-- If you don't know what white point to use, use
-- 'Data.Colour.CIE.Illuminant.d65'.
cieLAB :: (Ord a, Floating a) => Chromaticity a -- ^White point
                              -> a              -- ^L* coordinate (lightness)
                              -> a              -- ^a* coordinate
                              -> a              -- ^b* coordinate
                              -> Colour a
cieLAB white_ch l a b = cieXYZ (xn*transform fx)
                               (yn*transform fy)
                               (zn*transform fz)
 where
  white = chromaColour white_ch 1.0
  (xn,yn,zn) = toCIEXYZ white
  fx = fy + a/500
  fy = (l + 16)/116
  fz = fy - b/200
  delta = 6/29
  transform fa | fa > delta = fa^3
               | otherwise = (fa - 16/116)*3*delta^2

-- |Returns the CIELUV coordinates of a colour, which is a
-- perceptually uniform colour space.
-- If you don't know what white point to use, use
-- 'Data.Colour.CIE.Illuminant.d65'.
cieLuv :: (Ord a, Floating a) => Chromaticity a -- ^White point
                              -> Colour a -> (a,a,a)
cieLuv white_ch c = (l, 13*l*(u'-un'), 13*l*(v'-vn'))
 where
  white = chromaColour white_ch 1.0
  (u', v') = u'v' c
  (un', vn') = u'v' white
  l = lightness white_ch c
--------------------------------------------------------------------------
{- not for export -}
u'v' :: (Ord a, Floating a) => Colour a -> (a,a)
u'v' c = (4*x/(x+15*y+3*z), 9*y/(x+15*y+3*z))
 where
  (x,y,z) = toCIEXYZ c

rgb7092xyz = (rgb2xyz sRGBGamut)

xyz2rgb709 = inverse rgb7092xyz
