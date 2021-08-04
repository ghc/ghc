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
-- |Standard illuminants defined by the International Commission on
-- Illumination (CIE).
module Data.Colour.CIE.Illuminant where

import Data.Colour.CIE.Chromaticity

-- |Incandescent \/ Tungsten
a   :: (Fractional a) => Chromaticity a
a   = mkChromaticity 0.44757 0.40745

-- |{obsolete} Direct sunlight at noon
b   :: (Fractional a) => Chromaticity a
b   = mkChromaticity 0.34842 0.35161

-- |{obsolete} Average \/ North sky Daylight
c   :: (Fractional a) => Chromaticity a
c   = mkChromaticity 0.31006 0.31616

-- |Horizon Light. ICC profile PCS
d50 :: (Fractional a) => Chromaticity a
d50 = mkChromaticity 0.34567 0.35850

-- |Mid-morning \/ Mid-afternoon Daylight
d55 :: (Fractional a) => Chromaticity a
d55 = mkChromaticity 0.33242 0.34743

-- |Noon Daylight: Television, sRGB color space
d65 :: (Fractional a) => Chromaticity a
d65 = mkChromaticity 0.31271 0.32902

-- |North sky Daylight
d75 :: (Fractional a) => Chromaticity a
d75 = mkChromaticity 0.29902 0.31485

-- |Equal energy
e   :: (Fractional a) => Chromaticity a
e   = mkChromaticity (1/3)   (1/3)

-- |Daylight Fluorescent
f1  :: (Fractional a) => Chromaticity a
f1  = mkChromaticity 0.31310 0.33727

-- |Cool White Fluorescent
f2  :: (Fractional a) => Chromaticity a
f2  = mkChromaticity 0.37208 0.37529

-- |White Fluorescent
f3  :: (Fractional a) => Chromaticity a
f3  = mkChromaticity 0.40910 0.39430

-- |Warm White Fluorescent
f4  :: (Fractional a) => Chromaticity a
f4  = mkChromaticity 0.44018 0.40329

-- |Daylight Fluorescent
f5  :: (Fractional a) => Chromaticity a
f5  = mkChromaticity 0.31379 0.34531

-- |Lite White Fluorescent
f6  :: (Fractional a) => Chromaticity a
f6  = mkChromaticity 0.37790 0.38835

-- |D65 simulator, Daylight simulator
f7  :: (Fractional a) => Chromaticity a
f7  = mkChromaticity 0.31292 0.32933

-- |D50 simulator, Sylvania F40 Design 50
f8  :: (Fractional a) => Chromaticity a
f8  = mkChromaticity 0.34588 0.35875

-- |Cool White Deluxe Fluorescent
f9  :: (Fractional a) => Chromaticity a
f9  = mkChromaticity 0.37417 0.37281

-- |Philips TL85, Ultralume 50
f10 :: (Fractional a) => Chromaticity a
f10 = mkChromaticity 0.34609 0.35986

-- |Philips TL84, Ultralume 40
f11 :: (Fractional a) => Chromaticity a
f11 = mkChromaticity 0.38052 0.37713

-- |Philips TL83, Ultralume 30
f12 :: (Fractional a) => Chromaticity a
f12 = mkChromaticity 0.43695 0.40441
