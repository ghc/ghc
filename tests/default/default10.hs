-- | NamedDefaults doesn't interfere with traditional unnamed default combined with ExtendedDefaultRules and
-- OverloadedStrings.

{-# LANGUAGE Haskell2010, NamedDefaults, ExtendedDefaultRules, OverloadedStrings #-}

default (Int, Double, String)

main = do
  print []
  print "I'm a String"
