Various Unicode-related utilities.

\begin{code}
module UnicodeUtil(
    stringToUtf8
  ) where

#include "HsVersions.h"

import Panic ( panic )
import Char  ( chr )
\end{code}

\begin{code}
stringToUtf8 :: [Int] -> String
stringToUtf8 []       = ""
stringToUtf8 (c:s)
    | c >= 1 && c <= 0x7F = chr c : stringToUtf8 s
    | c < 0           = panic ("charToUtf8 ("++show c++")")
    | c <= 0x7FF      = chr (0xC0 + c `div`       0x40           ) :
                        chr (0x80 + c                  `mod` 0x40) :
                        stringToUtf8 s
    | c <= 0xFFFF     = chr (0xE0 + c `div`     0x1000           ) :
                        chr (0x80 + c `div`       0x40 `mod` 0x40) :
                        chr (0x80 + c                  `mod` 0x40) :
                        stringToUtf8 s
    | c <= 0x10FFFF   = chr (0xF0 + c `div`    0x40000           ) :
                        chr (0x80 + c `div`     0x1000 `mod` 0x40) :
                        chr (0x80 + c `div`       0x40 `mod` 0x40) :
                        chr (0x80 + c                  `mod` 0x40) :
                        stringToUtf8 s
    | otherwise       = panic ("charToUtf8 "++show c)
\end{code}
