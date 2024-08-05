-- | A named default doesn't combine with ExtendedDefaultRules.

{-# LANGUAGE Haskell2010, ExtendedDefaultRules, NamedDefaults #-}

default Num (Int, Double, String)

main = print []
