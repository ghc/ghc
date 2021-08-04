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
module Data.Colour.Matrix where

import Data.List (transpose)

default (Rational)

inverse m@[[a,b,c],[d,e,f],[g,h,i]] =
  [[(e*i-f*h)/det, -(b*i-c*h)/det, (b*f-c*e)/det]
  ,[-(d*i-f*g)/det, (a*i-c*g)/det, -(a*f-c*d)/det]
  ,[(d*h-e*g)/det, -(a*h-b*g)/det, (a*e-b*d)/det]]
 where
  det = determinant m
determinant [[a,b,c],[d,e,f],[g,h,i]] =
  a*(e*i-f*h) - b*(d*i-f*g) + c*(d*h-e*g)

mult l x = map (sum . (zipWith (*) x)) l

matrixMult l m = transpose (map (mult l) (transpose m))
