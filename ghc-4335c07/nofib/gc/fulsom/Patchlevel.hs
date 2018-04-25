{-
 -  Fulsom (The Solid Modeller, written in Haskell)
 -
 -  Copyright 1990,1991,1992,1993 Duncan Sinclair
 -
 - Permissiom to use, copy, modify, and distribute this software for any 
 - purpose and without fee is hereby granted, provided that the above
 - copyright notice and this permission notice appear in all copies, and
 - that my name not be used in advertising or publicity pertaining to this
 - software without specific, written prior permission.  I makes no
 - representations about the suitability of this software for any purpose.
 - It is provided ``as is'' without express or implied warranty.
 - 
 - Duncan Sinclair 1993.
 - 
 - Version information.
 -
 -}

module Patchlevel(version,major,minor,patch) where

version = "Fulsom version 3.4"

-- hbc requires that I type these, else I get: "Bad overloading in exports"

major = 3 :: Int

minor = 4 :: Int

patch = 0 :: Int
