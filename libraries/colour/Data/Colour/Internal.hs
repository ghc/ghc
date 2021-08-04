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
module Data.Colour.Internal where

import Data.List (foldl1')
import qualified Data.Colour.Chan as Chan
import Data.Colour.Chan (Chan(Chan))

data Red = Red
data Green = Green
data Blue = Blue

-- |This type represents the human preception of colour.
-- The @a@ parameter is a numeric type used internally for the
-- representation.
--
-- The 'Monoid' instance allows one to add colours, but beware that adding
-- colours can take you out of gamut.  Consider using 'blend' whenever
-- possible.

-- Internally we store the colour in linear ITU-R BT.709 RGB colour space.
data Colour a = RGB !(Chan Red a) !(Chan Green a) !(Chan Blue a)
                deriving (Eq)

-- |Change the type used to represent the colour coordinates.
colourConvert :: (Fractional b, Real a) => Colour a -> Colour b
colourConvert (RGB r g b) =
  RGB (Chan.convert r) (Chan.convert g) (Chan.convert b)

-- 'black' is the colourless colour.  It is the identity colour in
-- additive colour spaces.
black :: (Num a) => Colour a
black = RGB Chan.empty Chan.empty Chan.empty

instance (Num a) => Semigroup (Colour a) where
  (RGB r1 g1 b1) <> (RGB r2 g2 b2) =
    RGB (r1 `Chan.add` r2) (g1 `Chan.add` g2) (b1 `Chan.add` b2)

instance (Num a) => Monoid (Colour a) where
  mempty = black
  mconcat l = RGB (Chan.sum lr) (Chan.sum lg) (Chan.sum lb)
   where
    (lr,lg,lb) = unzip3 (map toRGB l)
    toRGB (RGB r g b) = (r,g,b)

data Alpha = Alpha

-- |This type represents a 'Colour' that may be semi-transparent.
--
-- The 'Monoid' instance allows you to composite colours.
--
-- >x `mappend` y == x `over` y
--
-- To get the (pre-multiplied) colour channel of an 'AlphaColour' @c@,
-- simply composite @c@ over black.
--
-- >c `over` black

-- Internally we use a premultiplied-alpha representation.
data AlphaColour a = RGBA !(Colour a) !(Chan Alpha a) deriving (Eq)

-- |This 'AlphaColour' is entirely transparent and has no associated
-- colour channel.
transparent :: (Num a) => AlphaColour a
transparent = RGBA (RGB Chan.empty Chan.empty Chan.empty) Chan.empty

-- |Change the type used to represent the colour coordinates.
alphaColourConvert :: (Fractional b, Real a) =>
  AlphaColour a -> AlphaColour b
alphaColourConvert (RGBA c a) = RGBA (colourConvert c) (Chan.convert a)

-- |Creates an opaque 'AlphaColour' from a 'Colour'.
opaque :: (Num a) => Colour a -> AlphaColour a
opaque c = RGBA c Chan.full

-- |Returns an 'AlphaColour' more transparent by a factor of @o@.
dissolve :: (Num a) => a -> AlphaColour a -> AlphaColour a
dissolve o (RGBA c a) = RGBA (darken o c) (Chan.scale o a)

-- |Creates an 'AlphaColour' from a 'Colour' with a given opacity.
--
-- >c `withOpacity` o == dissolve o (opaque c)
withOpacity :: (Num a) => Colour a -> a -> AlphaColour a
c `withOpacity` o = RGBA (darken o c) (Chan o)

--------------------------------------------------------------------------
-- Blending
--------------------------------------------------------------------------

class AffineSpace f where
 -- |Compute a affine Combination (weighted-average) of points.
 -- The last parameter will get the remaining weight.
 -- e.g.
 --
 -- >affineCombo [(0.2,a), (0.3,b)] c == 0.2*a + 0.3*b + 0.5*c
 --
 -- Weights can be negative, or greater than 1.0; however, be aware
 -- that non-convex combinations may lead to out of gamut colours.
 affineCombo :: (Num a) => [(a,f a)] -> f a -> f a

-- |Compute the weighted average of two points.
-- e.g.
--
-- >blend 0.4 a b = 0.4*a + 0.6*b
--
-- The weight can be negative, or greater than 1.0; however, be aware
-- that non-convex combinations may lead to out of gamut colours.
blend :: (Num a, AffineSpace f) => a -> f a -> f a -> f a
blend weight c1 c2 = affineCombo [(weight,c1)] c2

instance AffineSpace Colour where
 affineCombo l z =
   foldl1' mappend [darken w a | (w,a) <- (1-total,z):l]
  where
   total = sum $ map fst l

instance AffineSpace AlphaColour where
 affineCombo l z =
   foldl1' rgbaAdd [dissolve w a | (w,a) <- (1-total,z):l]
  where
   total = sum $ map fst l

--------------------------------------------------------------------------
-- composite
--------------------------------------------------------------------------

class ColourOps f where
 -- |@c1 \`over\` c2@ returns the 'Colour' created by compositing the
 -- 'AlphaColour' @c1@ over @c2@, which may be either a 'Colour' or
 -- 'AlphaColour'.
 over :: (Num a) => AlphaColour a -> f a -> f a
 -- |@darken s c@ blends a colour with black without changing it's opacity.
 --
 -- For 'Colour', @darken s c = blend s c mempty@
 darken :: (Num a) => a -> f a -> f a

instance ColourOps Colour where
 (RGBA (RGB r0 g0 b0) (Chan a0)) `over` (RGB r1 g1 b1) =
   RGB (Chan.over r0 a0 r1)
       (Chan.over g0 a0 g1)
       (Chan.over b0 a0 b1)
 darken s (RGB r g b) = RGB (Chan.scale s r)
                            (Chan.scale s g)
                            (Chan.scale s b)

instance ColourOps AlphaColour where
 c0@(RGBA _ a0@(Chan a0')) `over` (RGBA c1 a1) =
   RGBA (c0 `over` c1) (Chan.over a0 a0' a1)
 darken s (RGBA c a) = RGBA (darken s c) a

-- | 'AlphaColour' forms a monoid with 'over' and 'transparent'.
instance (Num a) => Semigroup (AlphaColour a) where
  (<>) = over

instance (Num a) => Monoid (AlphaColour a) where
  mempty = transparent

-- | @c1 \`atop\` c2@ returns the 'AlphaColour' produced by covering
-- the portion of @c2@ visible by @c1@.
-- The resulting alpha channel is always the same as the alpha channel
-- of @c2@.
--
-- >c1 `atop` (opaque c2) == c1 `over` (opaque c2)
-- >AlphaChannel (c1 `atop` c2) == AlphaChannel c2
atop :: (Fractional a) => AlphaColour a -> AlphaColour a -> AlphaColour a
atop (RGBA c0 (Chan a0)) (RGBA c1 (Chan a1)) =
  RGBA (darken a1 c0 `mappend` darken (1-a0) c1) (Chan a1)

-- |'round's and then clamps @x@ between 0 and 'maxBound'.
quantize :: (RealFrac a1, Integral a, Bounded a) => a1 -> a
quantize x | x <= fromIntegral l = l
           | fromIntegral h <= x = h
           | otherwise           = round x
 where
  l = minBound
  h = maxBound

{- Avoid using -}
-- |Returns the opacity of an 'AlphaColour'.
alphaChannel :: AlphaColour a -> a
alphaChannel (RGBA _ (Chan a)) = a

-- |Returns the colour of an 'AlphaColour'.
-- @colourChannel transparent@ is undefined and may result in @nan@ or an
-- error.
-- Its use is discouraged.
-- If you are desperate, use
--
-- >darken (recip (alphaChannel c)) (c `over` black)
colourChannel :: (Fractional a) => AlphaColour a -> Colour a
colourChannel (RGBA c (Chan a)) = darken (recip a) c

--------------------------------------------------------------------------
-- not for export
--------------------------------------------------------------------------

rgbaAdd (RGBA c1 a1) (RGBA c2 a2) =
  RGBA (c1 `mappend` c2) (a1 `Chan.add` a2)
