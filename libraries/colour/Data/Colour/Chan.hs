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
module Data.Colour.Chan where
{- For internal use only:
   Not to be exported from the package -}

import qualified Data.List (sum)

newtype Chan p a = Chan a deriving (Eq)

empty :: (Num a) => Chan p a
empty = Chan 0

full :: (Num a) => Chan p a
full = Chan 1

scale :: (Num a) => a -> Chan p a -> Chan p a
scale s (Chan x) =  Chan (s*x)

add :: (Num a) => Chan p a -> Chan p a -> Chan p a
(Chan a) `add` (Chan b) = Chan (a+b)

invert :: (Num a) => Chan p a -> Chan p a
invert (Chan a) = Chan (1-a)

over c0 a c1 = c0 `add` scale (1-a) c1

convert :: (Fractional b, Real a) => Chan p a -> Chan p b
convert (Chan x) = Chan (realToFrac x)

sum :: (Num a) => [Chan p a] -> Chan p a
sum l = Chan (Data.List.sum [x |Chan x <- l])