-- This code used to print an infinite string, by calling 'spaces'
-- with a negative argument.  There's a patch in the library now,
-- which makes 'spaces' do something sensible when called with a negative
-- argument, but it really should not happen at all.

module UnitPP1 where

import TestUtils

import Text.PrettyPrint.HughesPJ

ncat :: Doc -> Doc -> Doc
ncat x y = nest 4 $ cat [ x, y ]

d1, d2 :: Doc
d1 = foldl1 ncat $ take 50 $ repeat $ char 'a'
d2 = parens $  sep [ d1, text "+" , d1 ]

testPP1 :: IO ()
testPP1 = simpleMatch "PP1" expected out
  where out = show d2

expected :: String
expected =
  "(aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\n\
+                                                                                                                                                                                                   a\n\
 a\n\
a\n\
a\n\
a\n\
a\n\
a\n\
a\n\
a\n\
a\n\
a\n\
a\n\
a\n\
a\n\
a\n\
a\n\
a\n\
a\n\
a\n\
a\n\
a\n\
a\n\
a\n\
a\n\
a\n\
a\n\
a\n\
a\n\
a\n\
a\n\
a\n\
a\n\
a\n\
a\n\
a\n\
a\n\
a\n\
a\n\
a\n\
a\n\
a\n\
a\n\
a\n\
a\n\
a\n\
a\n\
a\n\
a\n\
a\n\
a)"

