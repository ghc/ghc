Various Unicode-related utilities.

\begin{code}
module UnicodeUtil(
    stringToUtf8, intsToUtf8
  ) where

#include "HsVersions.h"

import Panic ( panic )
import Char  ( chr, ord )
\end{code}

\begin{code}
stringToUtf8 :: String -> String
stringToUtf8 s = intsToUtf8 (map ord s)

intsToUtf8 :: [Int] -> String
intsToUtf8 []       = ""
intsToUtf8 (c:s)
    | c >= 1 && c <= 0x7F = chr c : intsToUtf8 s
    | c < 0           = panic ("charToUtf8 ("++show c++")")
    | c <= 0x7FF      = chr (0xC0 + c `div`       0x40           ) :
                        chr (0x80 + c                  `mod` 0x40) :
                        intsToUtf8 s
    | c <= 0xFFFF     = chr (0xE0 + c `div`     0x1000           ) :
                        chr (0x80 + c `div`       0x40 `mod` 0x40) :
                        chr (0x80 + c                  `mod` 0x40) :
                        intsToUtf8 s
    | c <= 0x10FFFF   = chr (0xF0 + c `div`    0x40000           ) :
                        chr (0x80 + c `div`     0x1000 `mod` 0x40) :
                        chr (0x80 + c `div`       0x40 `mod` 0x40) :
                        chr (0x80 + c                  `mod` 0x40) :
                        intsToUtf8 s
    | otherwise       = panic ("charToUtf8 "++show c)
\end{code}
