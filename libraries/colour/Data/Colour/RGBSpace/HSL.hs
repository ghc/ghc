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

module Data.Colour.RGBSpace.HSL
 (RGB
 ,hslView
 ,hue, saturation, lightness
 ,hsl
 )
where

import Data.Colour.RGB

-- |Returns the HSL (hue-saturation-lightness) coordinates of an 'RGB' triple.
-- See 'hue', 'saturation', and 'lightness'.
hslView :: (Fractional a, Ord a) => RGB a -> (a,a,a)
hslView rgb = (h,s,l)
 where
  (h,s,l,_,_) = hslsv rgb

-- |Returns the saturation coordinate (range [0, 1]) of an 'RGB' triple for the HSL
-- (hue-saturation-lightness) system.
-- Note: This is different from 'Data.Colour.RGBSpace.HSV.saturation' for
-- the "Data.Colour.RGBSpace.HSV"
saturation :: (Fractional a, Ord a) => RGB a -> a
saturation rgb = s
 where
  (_,s,_,_,_) = hslsv rgb

-- |Returns the lightness coordinate (range [0, 1]) of an 'RGB' triple for the HSL
-- (hue-saturation-lightness) system.
lightness :: (Fractional a, Ord a) => RGB a -> a
lightness rgb = l
 where
  (_,_,l,_,_) = hslsv rgb

-- |Convert HSL (hue-saturation-lightness) coordinates to an 'RGB' value.
-- Hue is expected to be measured in degrees [0,360], while saturation and
-- lightness are expected to be in the closed range [0,1].
hsl :: (RealFrac a, Ord a) => a -> a -> a -> RGB a
hsl h s l = fmap component t
 where
  hk = h/360
  tr = mod1 (hk + 1/3)
  tg = mod1 hk
  tb = mod1 (hk - 1/3)
  t = RGB tr tg tb
  q | l < 0.5 = l*(1+s)
    | otherwise = l + s - l*s
  p = 2*l - q
  component t | t < 1/6 = p + ((q-p)*6*t)
              | t < 1/2 = q
              | t < 2/3 = p + ((q-p)*6*(2/3-t))
              | otherwise = p
