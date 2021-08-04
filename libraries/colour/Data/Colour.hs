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
-- |Datatypes for representing the human perception of colour.
-- Includes common operations for blending and compositing colours.
-- The most common way of creating colours is either by name
-- (see "Data.Colour.Names") or by giving an sRGB triple
-- (see "Data.Colour.SRGB").
--
-- Methods of specifying Colours can be found in
--
-- - "Data.Colour.SRGB"
--
-- - "Data.Colour.SRGB.Linear"
--
-- - "Data.Colour.CIE"
--
-- Colours can be specified in a generic 'Data.Colour.RGBSpace.RGBSpace'
-- by using
--
-- - "Data.Colour.RGBSpace"

--TODO
-- - "Data.Colour.HDTV"
--
-- - "Data.Colour.SDTV"

module Data.Colour
 (
-- *Interfacing with Other Libraries\' Colour Spaces
--
-- |Executive summary: Always use "Data.Colour.SRGB" when interfacing with
-- other libraries.
-- Use 'Data.Colour.SRGB.toSRGB24' \/ 'Data.Colour.SRGB.sRGB24' when
-- interfacing with libraries wanting 'Data.Word.Word8' per channel.
-- Use 'Data.Colour.SRGB.toSRGB' \/ 'Data.Colour.SRGB.sRGB' when
-- interfacing with libraries wanting 'Double' or 'Float' per channel.
--
-- Interfacing with the colour for other libraries, such as cairo
-- (<http://www.haskell.org/gtk2hs/archives/category/cairo/>) and OpenGL
-- (<http://hackage.haskell.org/cgi-bin/hackage-scripts/package/OpenGL>),
-- can be a challenge because these libraries often do not use colour spaces
-- in a consistent way.
-- The problem is that these libraries work in a device dependent colour
-- space and give no indication what the colour space is.
-- For most devices this colours space is implicitly the non-linear sRGB
-- space.
-- However, to make matters worse, these libraries also do their
-- compositing and blending in the device colour space.
-- Blending and compositing ought to be done in a linear colour space,
-- but since the device space is typically non-linear sRGB, these libraries
-- typically produce colour blends that are too dark.
--
-- (Note that "Data.Colour" is a device /independent/ colour space, and
-- produces correct blends.
-- e.g. compare @toSRGB (blend 0.5 lime red)@ with @RGB 0.5 0.5 0@)
--
-- Because these other colour libraries can only blend in device colour
-- spaces, they are fundamentally broken and there is no \"right\" way
-- to interface with them.
-- For most libraries, the best one can do is assume they are working
-- with an sRGB colour space and doing incorrect blends.
-- In these cases use "Data.Colour.SRGB" to convert to and from the
-- colour coordinates.  This is the best advice for interfacing with cairo.
--
-- When using OpenGL, the choice is less clear.
-- Again, OpenGL usually does blending in the device colour space.
-- However, because blending is an important part of proper shading, one
-- may want to consider that OpenGL is working in a linear colour space,
-- and the resulting rasters are improperly displayed.
-- This is born out by the fact that OpenGL extensions that support
-- sRGB do so by converting sRGB input\/output to linear colour coordinates
-- for processing by OpenGL.
--
-- The best way to use OpenGL, is to use proper sRGB surfaces for textures
-- and rendering.
-- These surfaces will automatically convert to and from OpenGL's linear
-- colour space.
-- In this case, use "Data.Colour.SRGB.Linear" to interface OpenGL's linear
-- colour space.
--
-- If not using proper surfaces with OpenGL, then you have a choice between
-- having OpenGL do improper blending or improper display
-- If you are using OpenGL for 3D shading, I recommend using
-- "Data.Colour.SRGB.Linear" (thus choosing improper OpenGL display).
-- If you are not using OpenGL for 3D shading, I recommend using
-- "Data.Colour.SRGB" (thus choosing improper OpenGL blending).

-- *Colour type
  Colour
 ,colourConvert
 ,black

 ,AlphaColour
 ,opaque, withOpacity
 ,transparent
 ,alphaColourConvert
 ,alphaChannel

 -- *Colour operations
 -- |These operations allow combine and modify existing colours
 ,AffineSpace(..), blend

 ,ColourOps(..)
 ,dissolve, atop
 )
where

import Data.Char (isAlphaNum, isSpace)
import Data.Colour.Internal
import qualified Data.Colour.SRGB.Linear
import Data.Colour.CIE.Chromaticity (app_prec, infix_prec)

instance (Fractional a, Show a) => Show (Colour a) where
  showsPrec d c = showParen (d > app_prec) showStr
   where
    showStr = showString linearConstructorQualifiedName
            . showString " " . (showsPrec (app_prec+1) r)
            . showString " " . (showsPrec (app_prec+1) g)
            . showString " " . (showsPrec (app_prec+1) b)
    Data.Colour.SRGB.Linear.RGB r g b = Data.Colour.SRGB.Linear.toRGB c

instance (Fractional a, Read a) => Read (Colour a) where
  readsPrec d r = readParen (d > app_prec)
                  (\r -> [(Data.Colour.SRGB.Linear.rgb r0 g0 b0,t)
                         |(name,s) <- mylex r
                         ,name `elem` [linearConstructorName
                                      ,linearConstructorQualifiedName]
                         ,(r0,s0) <- readsPrec (app_prec+1) s
                         ,(g0,s1) <- readsPrec (app_prec+1) s0
                         ,(b0,t)  <- readsPrec (app_prec+1) s1]) r
   where
    mylex = return
          . span (\c -> isAlphaNum c || c `elem` "._'")
          . dropWhile isSpace

linearConstructorQualifiedName = "Data.Colour.SRGB.Linear.rgb"
linearConstructorName = "rgb"

instance (Fractional a, Show a, Eq a) => Show (AlphaColour a) where
  showsPrec d ac | a == 0 = showString "transparent"
                 | otherwise = showParen (d > infix_prec) showStr
   where
    showStr = showsPrec (infix_prec+1) c
            . showString " `withOpacity` "
            . showsPrec (infix_prec+1) a
    a = alphaChannel ac
    c = colourChannel ac

instance (Fractional a, Read a) => Read (AlphaColour a) where
  readsPrec d r = [(transparent,s)|("transparent",s) <- lex r]
               ++ readParen (d > infix_prec)
                  (\r -> [(c `withOpacity` o,s)
                         |(c,r0) <- readsPrec (infix_prec+1) r
                         ,("`",r1) <- lex r0
                         ,("withOpacity",r2) <- lex r1
                         ,("`",r3) <- lex r2
                         ,(o,s)  <- readsPrec (infix_prec+1) r3]) r
