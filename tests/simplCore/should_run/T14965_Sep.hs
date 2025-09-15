module T14965_Sep where

data Sep = Sep
  { bugVanishesWithoutThis :: [()]
  , middle :: [String]
  , orThis :: [()]
  }

catSep :: Sep -> Sep -> Sep
catSep (Sep a b c) (Sep x y z) = Sep (a ++ x) (b ++ y) (c ++ z)

cc :: Sep -> Bool
cc boost = elem "foo" $ middle boost
