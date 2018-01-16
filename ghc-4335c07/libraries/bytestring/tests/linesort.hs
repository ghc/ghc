{-
Silly program to benchmark sort on smaller strings.  Splits the input
into lines, then sorts the lines.  Try this program on lines of varying
lengths.

Useful for benchmarking sort and lines.
-}

import qualified Data.ByteString.Char8 as P

main =
    print
    . sum
    . map (P.length . P.sort)
    . P.lines
    =<< P.getContents
