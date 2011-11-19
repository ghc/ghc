-- This code used to print an infinite string, by calling 'spaces'
-- with a negative argument.  There's a patch in the library now,
-- which makes 'spaces' do something sensible when called with a negative
-- argument, but it really should not happen at all.


module Main where

import Text.PrettyPrint.HughesPJ


ncat x y = nest 4 $ cat [ x, y ]

d1 = foldl1 ncat $ take 50 $ repeat $ char 'a'
d2 = parens $  sep [ d1, text "+" , d1 ]

main = print d2

