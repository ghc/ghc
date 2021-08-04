{-
Copyright (c) 2008,2009
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
module Data.Colour.RGB where

import Data.List (elemIndex, transpose)
import Data.Colour.Matrix
import Data.Colour.CIE.Chromaticity

-- |An RGB triple for an unspecified colour space.
data RGB a = RGB {channelRed :: !a
                 ,channelGreen :: !a
                 ,channelBlue :: !a
                 } deriving (Eq, Show, Read)

instance Functor RGB where
 fmap f (RGB r g b) = RGB (f r) (f g) (f b)

instance Applicative RGB where
 pure c = RGB c c c
 (RGB fr fg fb) <*> (RGB r g b) = RGB (fr r) (fg g) (fb b)

-- |Uncurries a function expecting three r, g, b parameters.
uncurryRGB :: (a -> a -> a -> b) -> RGB a -> b
uncurryRGB f (RGB r g b) = f r g b

-- |Curries a function expecting one RGB parameter.
curryRGB :: (RGB a -> b) -> a -> a -> a -> b
curryRGB f r g b = f (RGB r g b)

-- |An 'RGBGamut' is a 3-D colour &#8220;cube&#8221; that contains all the
-- colours that can be displayed by a RGB device.
-- The &#8220;cube&#8221; is normalized so that white has
-- 'Data.Colour.CIE.luminance' 1.
data RGBGamut = RGBGamut {primaries :: !(RGB (Chromaticity Rational))
                         ,whitePoint :: !(Chromaticity Rational)
                         } deriving (Eq)

instance Show RGBGamut where
  showsPrec d gamut = showParen (d > app_prec) showStr
   where
    showStr = showString "mkRGBGamut"
            . showString " " . (showsPrec (app_prec+1) (primaries gamut))
            . showString " " . (showsPrec (app_prec+1) (whitePoint gamut))

instance Read RGBGamut where
  readsPrec d r = readParen (d > app_prec)
                  (\r -> [(mkRGBGamut p w,t)
                         |("mkRGBGamut",s) <- lex r
                         ,(p,s0) <- readsPrec (app_prec+1) s
                         ,(w,t)  <- readsPrec (app_prec+1) s0]) r

-- |An RGB gamut is specified by three primary colours (red, green, and
-- blue) and a white point (often 'Data.Colour.CIE.Illuminant.d65').
mkRGBGamut :: RGB (Chromaticity Rational) -- ^ The three primaries
           -> Chromaticity Rational       -- ^ The white point
           -> RGBGamut
mkRGBGamut = RGBGamut

{- not for export -}

primaryMatrix :: (Fractional a) => (RGB (Chromaticity a)) -> [[a]]
primaryMatrix p =
  [[xr, xg, xb]
  ,[yr, yg, yb]
  ,[zr, zg, zb]]
 where
  RGB (xr, yr, zr)
      (xg, yg, zg)
      (xb, yb, zb) = fmap chromaCoords p

rgb2xyz :: RGBGamut -> [[Rational]]
rgb2xyz space =
  transpose (zipWith (map . (*)) as (transpose matrix))
 where
  (xn, yn, zn) = chromaCoords (whitePoint space)
  matrix = primaryMatrix (primaries space)
  as = mult (inverse matrix) [xn/yn, 1, zn/yn]

xyz2rgb :: RGBGamut -> [[Rational]]
xyz2rgb = inverse . rgb2xyz

hslsv :: (Fractional a, Ord a) => RGB a -> (a,a,a,a,a)
hslsv (RGB r g b) | mx == mn  = (0,0,mx,0 ,mx)
                  | otherwise = (h,s,l ,s0,mx)
 where
  mx = maximum [r,g,b]
  mn = minimum [r,g,b]
  l = (mx+mn)/2
  s | l <= 0.5 = (mx-mn)/(mx+mn)
    | otherwise = (mx-mn)/(2-(mx+mn))
  s0 = (mx-mn)/mx
  -- hue calcuation
  [x,y,z] = take 3 $ dropWhile (/=mx) [r,g,b,r,g]
  Just o = elemIndex mx [r,g,b]
  h0 = 60*(y-z)/(mx-mn) + 120*(fromIntegral o)
  h | h0 < 0 = h0 + 360
    | otherwise = h0

-- |The 'hue' coordinate of an 'RGB' value is in degrees. Its value is
-- always in the range 0-360.
hue :: (Fractional a, Ord a) => RGB a -> a
hue rgb = h
 where
  (h,_,_,_,_) = hslsv rgb

mod1 x | pf < 0 = pf+1
       | otherwise = pf
 where
  (_,pf) = properFraction x
