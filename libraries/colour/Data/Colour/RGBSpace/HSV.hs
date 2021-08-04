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

module Data.Colour.RGBSpace.HSV
 (RGB
 ,hsvView
 ,hue, saturation, value
 ,hsv
 )
where

import Data.Colour.RGB

-- |Returns the HSV (hue-saturation-value) coordinates of an 'RGB' triple.
-- See 'hue', 'saturation', and 'value'.
hsvView :: (Fractional a, Ord a) => RGB a -> (a,a,a)
hsvView rgb = (h,s,v)
 where
  (h,_,_,s,v) = hslsv rgb

-- |Returns the saturation coordinate (range [0,1]) of an 'RGB' triple for the HSV
-- (hue-saturation-value) system.
-- Note: This is different from 'Data.Colour.RGBSpace.HSL.saturation' for
-- the "Data.Colour.RGBSpace.HSL"
saturation :: (Fractional a, Ord a) => RGB a -> a
saturation rgb = s
 where
  (_,_,_,s,_) = hslsv rgb

-- |Returns the value coordinate (raonge [0,1]) of an 'RGB' triple for the HSV
-- (hue-saturation-value) system.
value :: (Fractional a, Ord a) => RGB a -> a
value rgb = v
 where
  (_,_,_,_,v) = hslsv rgb

-- |Convert HSV (hue-saturation-value) coordinates to an 'RGB' value.
-- Hue is expected to be measured in degrees [0,360], while saturation and
-- value are expected to be in the closed range [0,1].
hsv :: (RealFrac a, Ord a) => a -> a -> a -> RGB a
hsv h s v = case hi of
    0 -> RGB v t p
    1 -> RGB q v p
    2 -> RGB p v t
    3 -> RGB p q v
    4 -> RGB t p v
    5 -> RGB v p q
 where
  hi = floor (h/60) `mod` 6
  f = mod1 (h/60)
  p = v*(1-s)
  q = v*(1-f*s)
  t = v*(1-(1-f)*s)
