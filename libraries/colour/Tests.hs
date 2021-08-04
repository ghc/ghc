{-# LANGUAGE TypeSynonymInstances #-}
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
module Main where

import Data.Word (Word8)
import Control.Monad (liftM, liftM2, liftM3)
import Test.QuickCheck ( Arbitrary, CoArbitrary, Gen, Property
                       , (==>), arbitrary, choose, coarbitrary, forAll
                       )
import Test.Framework (defaultMain, defaultMainWithOpts, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Colour.Matrix
import Data.Colour
import Data.Colour.SRGB
import Data.Colour.SRGB.Linear
import Data.Colour.CIE
import Data.Colour.CIE.Illuminant (d65)
import Data.Colour.Names
--import Data.Colour.HDTV as HDTV
--import qualified Data.Colour.SDTV as SDTV
import Data.Colour.RGB
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSL
import Data.Colour.RGBSpace.HSV

default (Rational, Double, Float)

type RColour = Colour Rational
type DColour = Colour Double
type FColour = Colour Float
type RAlphaColour = AlphaColour Rational
type DAlphaColour = AlphaColour Double

instance (Real a, Fractional a, Arbitrary a) => Arbitrary (Colour a) where
  arbitrary = liftM3 mkColour arbitrary arbitrary arbitrary
   where
    mkColour r' g' b' = colourConvert (sRGB24 r' g' b'::Colour Double)

instance (Real a, Fractional a, CoArbitrary a) => CoArbitrary (Colour a) where
  coarbitrary c = coarbitrary (r,g,b)
   where
    (RGB r g b) = toRGB c

instance (Real a, Fractional a, Arbitrary a) =>
         Arbitrary (AlphaColour a) where
  arbitrary = liftM2 mkAlphaColour arbitrary arbitrary
   where
    mkAlphaColour :: (Fractional a) => Colour a -> Word8 -> AlphaColour a
    mkAlphaColour c a =
      c `withOpacity` (fromIntegral a/fromIntegral (maxBound `asTypeOf` a))

instance (Real a, Fractional a, CoArbitrary a) =>
         CoArbitrary (AlphaColour a) where
  coarbitrary ac = coarbitrary a . coarbitrary c
   where
    a = alphaChannel ac
    c = ac `over` black

instance (Fractional a, Arbitrary a) =>
         Arbitrary (Chromaticity a) where
  arbitrary = liftM2 mkChromaticity arbitrary arbitrary

instance (Fractional a, CoArbitrary a) =>
         CoArbitrary (Chromaticity a) where
  coarbitrary c = coarbitrary x . coarbitrary y
   where
    (x,y,_) = chromaCoords c

instance (Arbitrary a) => Arbitrary (RGB a) where
  arbitrary = liftM3 RGB arbitrary arbitrary arbitrary

instance (CoArbitrary a) => CoArbitrary (RGB a) where
  coarbitrary (RGB r g b) = coarbitrary (r,g,b)

instance Arbitrary RGBGamut where
  arbitrary = liftM2 RGBGamut arbitrary arbitrary

instance CoArbitrary RGBGamut where
  coarbitrary (RGBGamut p w) = coarbitrary p . coarbitrary w

-- generate RGB values with channels between 0 and 1.
rgbGen :: Gen (RGB Rational)
rgbGen = fmap (\(r,g,b) -> fmap toRational (RGB r g b)) (three zeroOne)

zeroOne = choose (0,1::Double)

two :: Monad m => m a -> m (a, a)
two m = liftM2 (,) m m

three :: Monad m => m a -> m (a, a, a)
three m = liftM3 (,,) m m m

good (RGBGamut p w) = p1 && p2
 where
  p1 = 0 /= determinant (primaryMatrix p)
  p2 = 0 /= let (x,y,z) = chromaCoords w in y

prop_matrixMult (a1,b1,c1) (d1,e1,f1) (g1,h1,i1)
                (a2,b2,c2) (d2,e2,f2) (g2,h2,i2)
                (x,y,z) = mult m1 (mult m2 v) == mult (matrixMult m1 m2) v
 where
  m1 = [[a1,b1,c1],[d1,e1,f1],[g1,h1,i1]]
  m2 = [[a2,b2,c2],[d2,e2,f2],[g2,h2,i2]]
  v :: [Rational]
  v = [x,y,z]

newtype Depth = Depth Int deriving Show

instance Arbitrary Depth where
  arbitrary = liftM Depth $ choose (0,11)

instance CoArbitrary Depth where
  coarbitrary (Depth x) = coarbitrary x

prop_toFromRGB :: RColour -> Bool
prop_toFromRGB c = uncurryRGB rgb (toRGB c) == c

prop_fromToRGB :: Rational -> Rational -> Rational -> Bool
prop_fromToRGB r g b = toRGB (rgb r g b) == RGB r g b

prop_toFromXYZ :: RColour -> Bool
prop_toFromXYZ c = (cieXYZ x y z) == c
 where
  (x,y,z) = cieXYZView c

prop_fromToXYZ :: Rational -> Rational -> Rational -> Bool
prop_fromToXYZ x y z = cieXYZView (cieXYZ x y z) == (x,y,z)

-- Uses the fact that an Arbitrary colour is an sRGB24 colour.
prop_toFromSRGB :: DColour -> Bool
prop_toFromSRGB c = uncurryRGB sRGB24 (toSRGB24 c) == c

prop_fromToSRGB :: Word8 -> Word8 -> Word8 -> Bool
prop_fromToSRGB r' g' b' = toSRGB24 (sRGB24 r' g' b') == RGB r' g' b'

prop_toFromLAB :: Chromaticity Double -> DColour -> Bool
prop_toFromLAB wp c = cc (cieLAB wp l a b) == cc c
 where
  (l,a,b) = cieLABView wp c
  cc = toSRGB24

{-
prop_fromToY'CbCr709 :: Word8 -> Word8 -> Word8 -> Bool
prop_fromToY'CbCr709 y' cb cr =
  HDTV.toY'CbCr (HDTV.y'CbCr y' cb cr) == (y',cb,cr)

prop_fromToY'CbCr601 :: Word8 -> Word8 -> Word8 -> Bool
prop_fromToY'CbCr601 y' cb cr =
  SDTV.toY'CbCr (SDTV.y'CbCr y' cb cr) == (y',cb,cr)
-}

prop_disolveId :: RAlphaColour -> Bool
prop_disolveId c = dissolve 1 c == c

prop_disolveTransparent :: RAlphaColour -> Bool
prop_disolveTransparent c = dissolve 0 c == transparent

prop_transparentOver :: RColour -> Bool
prop_transparentOver c = transparent `over` c == c

prop_overTransparent :: RAlphaColour -> Bool
prop_overTransparent c = c `over` transparent == c

prop_opaqueOver :: RColour -> RColour -> Bool
prop_opaqueOver c1 c2 = opaque c1 `over` c2 == c1

prop_overOpaque :: RAlphaColour -> RColour -> Bool
prop_overOpaque c1 c2 = c1 `over` opaque c2 == opaque (c1 `over` c2)

prop_blendOver :: Rational -> RColour -> RColour -> Bool
prop_blendOver o c1 c2 =
  (c1 `withOpacity` o) `over` c2 == blend o c1 c2

prop_blendTransparent :: Rational -> Rational -> RColour -> Bool
prop_blendTransparent o a c =
  blend o (c `withOpacity` a) transparent == c `withOpacity ` (o*a)

prop_blendFlip :: Rational -> RColour -> RColour -> Bool
prop_blendFlip o c1 c2 =
  blend (1-o) c2 c1 == blend o c1 c2

prop_darkenBlend :: Rational -> RColour -> Bool
prop_darkenBlend w c =
  blend w c mempty == darken w c

prop_darkenBlack :: RAlphaColour -> Bool
prop_darkenBlack c = darken 0 c == mempty `withOpacity` (alphaChannel c)

prop_darkenId :: RAlphaColour -> Bool
prop_darkenId c = darken 1 c == c

prop_atopOpaque :: RAlphaColour -> RColour -> Bool
prop_atopOpaque c0 c1 = c0 `atop` (opaque c1) == opaque (c0 `over` c1)

prop_transparentAtop :: RAlphaColour -> Bool
prop_transparentAtop c = transparent `atop` c == c

prop_atopTransparent :: RAlphaColour -> Bool
prop_atopTransparent c = c `atop` transparent == transparent

prop_atopAlpha :: RAlphaColour -> RAlphaColour -> Bool
prop_atopAlpha c0 c1 = alphaChannel (c0 `atop` c1) == alphaChannel c1

prop_showReadC :: Depth -> RColour -> Bool
prop_showReadC (Depth d) c = readsPrec d (showsPrec d c "") == [(c,"")]

prop_showReadAC :: Depth -> RAlphaColour -> Bool
prop_showReadAC (Depth d) c = readsPrec d (showsPrec d c "") == [(c,"")]

prop_sRGB24showlength :: DColour -> Bool
prop_sRGB24showlength c = length (sRGB24show c) == 7

prop_readshowSRGB24 :: DColour -> Bool
prop_readshowSRGB24 c =
  sRGB24show (sRGB24read (sRGB24show c)) == sRGB24show c

prop_luminance_white :: RGBGamut -> Property
prop_luminance_white space =
  good space ==> luminance (rgbUsingSpace (linearRGBSpace space) 1 1 1) == 1

prop_rgb :: Rational -> Rational -> Rational -> Bool
prop_rgb r g b =
  rgbUsingSpace (linearRGBSpace sRGBGamut) r g b == rgb r g b

prop_toRGB :: RColour -> Bool
prop_toRGB c =
  toRGBUsingSpace (linearRGBSpace sRGBGamut) c == toRGB c

prop_sRGB :: Double -> Double -> Double -> Bool
prop_sRGB r g b = rgbUsingSpace sRGBSpace r g b == sRGB r g b

prop_toSRGB :: DColour -> Bool
prop_toSRGB c =
  toRGBUsingSpace sRGBSpace c == toSRGB c

prop_hueRange :: RGB Rational -> Bool
prop_hueRange rgb = 0 <= h && h < 360
 where
  h = hue rgb

prop_toFromHSL :: Property
prop_toFromHSL = forAll rgbGen (\rgb -> hsl' (hslView rgb) == rgb)
 where
  hsl' (h,s,l) = hsl h s l

prop_fromToHSL :: Rational -> Property
prop_fromToHSL h = forAll (two (fmap toRational zeroOne))
  (\(s,l) -> checkHSL (hslView (hsl h s l)) (h,s,l))
 where
  checkHSL (h0,s0,l0) (h1,s1,l1) =
    snd (properFraction ((h0-h1)/360)::(Integer,Rational)) == 0
    && s0 == s1 && l0 == l1

prop_toFromHSV :: Property
prop_toFromHSV = forAll rgbGen (\rgb -> hsv' (hsvView rgb) == rgb)
 where
  hsv' (h,s,v) = hsv h s v

prop_fromToHSV :: Rational -> Property
prop_fromToHSV h = forAll (two (fmap toRational zeroOne))
  (\(s,v) -> checkHSV (hsvView (hsv h s v)) (h,s,v))
 where
  checkHSV (h0,s0,v0) (h1,s1,v1) =
    snd (properFraction ((h0-h1)/360)::(Integer,Rational)) == 0
    && s0 == s1 && v0 == v1

tests = [ testProperty "matrix-mult" prop_matrixMult
        , testProperty "RGB-to-from" prop_toFromRGB
        , testProperty "RGB-from-to" prop_fromToRGB
        , testProperty "XYZ-to-from" prop_toFromXYZ
        , testProperty "XYZ-from-to" prop_fromToXYZ
        , testProperty "sRGB-to-from" prop_toFromSRGB
        , testProperty "sRGB-from-to" prop_fromToSRGB
        , testProperty "cieLAB-to-from" (prop_toFromLAB d65)
--        , testProperty "Y'CbCr-709-from-to" prop_fromToY'CbCr709
--        , testProperty "Y'CbCr-601-from-to" prop_fromToY'CbCr601
        , testProperty "dissolve-id" prop_disolveId
        , testProperty "dissolve-transparent" prop_disolveTransparent
        , testProperty "transparent-over" prop_transparentOver
        , testProperty "over-transparent" prop_overTransparent
        , testProperty "opaque-over" prop_opaqueOver
        , testProperty "over-opaque" prop_overOpaque
        , testProperty "blend-over" prop_blendOver
        , testProperty "blend-transparent" prop_blendTransparent
        , testProperty "blend-flip" prop_blendFlip
        , testProperty "darken-blend" prop_darkenBlend
        , testProperty "darken-black" prop_darkenBlack
        , testProperty "darken-id" prop_darkenId
        , testProperty "atop-opaque" prop_atopOpaque
        , testProperty "transparent-atop" prop_transparentAtop
        , testProperty "atop-transparent" prop_atopTransparent
        , testProperty "atop-alpha" prop_atopAlpha
        , testProperty "colour-show-read" prop_showReadC
        , testProperty "alphaColour-show-read" prop_showReadAC
        , testProperty "sRGB24-show-length" prop_sRGB24showlength
        , testProperty "sRGB24-read-show" prop_readshowSRGB24
        , testProperty "luminance-white" prop_luminance_white
        , testProperty "rgb" prop_rgb
        , testProperty "toRGB" prop_toRGB
        , testProperty "sRGB" prop_sRGB
        , testProperty "toSRGB" prop_toSRGB
        , testProperty "hueRange" prop_hueRange
        , testProperty "toFromHSL" prop_toFromHSL
        , testProperty "fromToHSL" prop_fromToHSL
        , testProperty "toFromHSV" prop_toFromHSV
        , testProperty "fromToHSV" prop_fromToHSV
        ]

main = defaultMain tests
