{-# LANGUAGE MultilineStrings #-}

str1 = unlines
  [ "aaa"
  , "bbb"
  , "ccc"
  ]

str2 = "aaa\n\
       \bbb\n\
       \ccc\n"

str3 = """
       aaa
       bbb
       ccc
       """

str4 = """

       aaa
       bbb
       ccc

       """

str5 = """
       aaa
       bbb
       ccc\n
       """

main = do
  print str1
  print str2
  print str3
  print str4
  print str5
