{-
 - Uncompress.hs
 - 
 - This program is a version of the compress utility as defined in
 - "A Technique for High Performance Data Compression", Terry A. Welch,
 - Computer, vol 17, no 6 1984, pp 8-19
 -
 -
 - Paul Sanders, Systems Research Division, British Telecom Laboratories 1992
 -
 -}

module Uncompress (main) where

import Defaults
import BinConv	  -- binary conversion routines
import Decode     -- decoding routines

main = getContents >>= \ inp ->
	putStr (uncompress inp)

{- To uncompress a string we first convert the characters to n-bit binaries
 - and then to decimals which can then be decoded.
 -}

uncompress = decode . ascii_to_codes . map fromEnum
