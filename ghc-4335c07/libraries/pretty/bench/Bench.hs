{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}
module Main where

import Criterion.Main
import Data.List
import Text.PrettyPrint.HughesPJ

--------------------------------------------------------------------------------
f_left :: Int -> Doc
f_left n = foldl' (<>) empty (map (text . show) [10001..10000+n])

--------------------------------------------------------------------------------
f_right :: Int -> Doc
f_right n = foldr (<>) empty (map (text . show) [10001..10000+n])

--------------------------------------------------------------------------------
stuff :: String -> String -> Double -> Rational -> Int -> Int -> Int -> Doc
stuff s1 s2 d1 r1 i1 i2 i3 =
    let a = nest i1 $ text s1
        b = double d1
        c = rational r1
        d = replicate i1 (text s2 <> b <> c <+> a)
        e = cat d $+$ cat d $$ (c <> b <+> a)
        f = parens e <> brackets c <> hcat d
        g = lparen <> f <> rparen
        h = text $ s2 ++ s1
        i = map rational ([1..(toRational i2)]::[Rational])
        j = punctuate comma i
        k = nest i3 h <> (nest (i1 + i3) $ sep i) $+$ g <> cat j
        l = cat $ punctuate (comma <> b <> comma) $ replicate i3 k
    in l

--------------------------------------------------------------------------------
doc1 :: Doc
doc1 = stuff "Adsas ads" "dassdab weeaa xxxxx" 123.231321 ((-1)/5) 30 300 20

--------------------------------------------------------------------------------
doc2 :: Doc
doc2 = stuff "aDSAS ADS asdasdsa sdsda xx" "SDAB WEEAA" 1333.212 ((-4)/5) 31 301 30

--------------------------------------------------------------------------------
doc3 :: Doc
doc3 = stuff "ADsAs --____ aDS" "DasSdAB weEAA" 2533.21299 ((-4)/999) 39 399 60

--------------------------------------------------------------------------------
processTxt :: TextDetails -> String -> String
processTxt (Chr c)   s  = c:s
processTxt (Str s1)  s2 = s1 ++ s2
processTxt (PStr s1) s2 = s1 ++ s2

--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain $ [
  bgroup "<> associativity" [ bench "left"     $ nf (length . render . f_left)  10000
                            , bench "right"    $ nf (length . render . f_right) 10000
                            , bench "left20k"  $ nf (length . render . f_left)  20000
                            , bench "right20k" $ nf (length . render . f_right) 20000
                            , bench "left30k"  $ nf (length . render . f_left)  30000
                            , bench "right30k" $ nf (length . render . f_right) 30000
                            ]

  , bgroup "render" [ bench "doc1" $ nf render doc1
                    , bench "doc2" $ nf render doc2
                    , bench "doc3" $ nf render doc3
                    ]

  , bgroup "fullRender" [ bench "PageMode 1000" $ nf (fullRender PageMode 1000 4 processTxt "") doc2
                        , bench "PageMode 100" $ nf (fullRender PageMode 100 1.5 processTxt "") doc2
                        , bench "ZigZagMode" $ nf (fullRender ZigZagMode 1000 4 processTxt "") doc2
                        , bench "LeftMode" $ nf (fullRender LeftMode 1000 4 processTxt "") doc2
                        , bench "OneLineMode" $ nf (fullRender OneLineMode 1000 4 processTxt "") doc3
                        ]
  ]
