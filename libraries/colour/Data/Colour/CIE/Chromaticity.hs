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
module Data.Colour.CIE.Chromaticity where

data Chromaticity a = Chroma !a !a deriving (Eq)

-- |Constructs 'Chromaticity' from the CIE little /x/, little /y/
-- coordinates for the 2&#176; standard (colourimetric) observer.
mkChromaticity :: (Fractional a) => a -> a -> Chromaticity a
mkChromaticity = Chroma

-- |Returns the CIE little /x/, little /y/, little /z/ coordinates
-- for the 2&#176; standard (colourimetric) observer.
chromaCoords :: (Fractional a) => Chromaticity a -> (a, a, a)
chromaCoords (Chroma x y) = (x, y, 1 - x - y)

-- |Returns the CIE little /x/ coordinate
-- for the 2&#176; standard (colourimetric) observer.
chromaX :: (Fractional a) => Chromaticity a -> a
chromaX (Chroma x _y) = x

-- |Returns the CIE little /y/ coordinate
-- for the 2&#176; standard (colourimetric) observer.
chromaY :: (Fractional a) => Chromaticity a -> a
chromaY (Chroma _x y) = y

-- |Returns the CIE little /z/ coordinate
-- for the 2&#176; standard (colourimetric) observer.
chromaZ :: (Fractional a) => Chromaticity a -> a
chromaZ (Chroma x y) = 1 - x - y

-- |Change the type used to represent the chromaticity coordinates.
chromaConvert :: (Fractional b, Real a) => Chromaticity a -> Chromaticity b
chromaConvert (Chroma x y) = Chroma (realToFrac x) (realToFrac y)

instance (Fractional a, Show a) => Show (Chromaticity a) where
  showsPrec d c = showParen (d > app_prec) showStr
   where
    showStr = showString "mkChromaticity " . (showsPrec (app_prec+1) x)
            . showString " "          . (showsPrec (app_prec+1) y)
    (x,y,z) = chromaCoords c

instance (Fractional a, Read a) => Read (Chromaticity a) where
  readsPrec d r = readParen (d > app_prec)
                  (\r -> [(mkChromaticity x y,t)
                         |("mkChromaticity",s) <- lex r
                         ,(x,s0) <- readsPrec (app_prec+1) s
                         ,(y,t) <- readsPrec (app_prec+1) s0]) r

--------------------------------------------------------------------------
-- not for export
--------------------------------------------------------------------------

app_prec = 10

infix_prec = 9 `asTypeOf` app_prec
