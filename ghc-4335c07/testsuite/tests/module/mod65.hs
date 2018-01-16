-- !!! Malformed lhs (pointless but legal in Haskell 1.3, rejected by Hugs)
module M where
x = let [] = "a" in 'a'
