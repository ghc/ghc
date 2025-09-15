-- | NamedDefaults don't affect ExtendedDefaultRules and OverloadedStrings.

{-# LANGUAGE Haskell2010, ExtendedDefaultRules, NamedDefaults, OverloadedStrings #-}

import Data.Foldable (fold)

main = do
  print (fold mempty)
  print "I'm a String"
