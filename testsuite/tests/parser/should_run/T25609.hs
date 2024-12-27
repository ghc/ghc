{-# LANGUAGE MultilineStrings #-}

main :: IO ()
main = do
  -- strings with comment tokens
  print """{- asdf -}"""
  print """a {- asdf -} b"""
  print """-- asdf"""
  print """{-"""

  -- strings with haddock comments
  print """{- | test -}"""
  print """{- * test -}"""
  print """{- ^ test -}"""
  print """{- $ test -}"""
  print """-- | test"""
  print """-- * test"""
  print """-- ^ test"""
  print """-- $ test"""

  -- strings with only whitespace
  print """    """
  print """


        """

  -- strings with unicode
  print """
          ★
        ★
          ★
        ★
        """
