{-# LANGUAGE GADTs, ExplicitForAll #-}
module Main (main) where

import GHC.Exts

newtype Age a b where
  Age :: forall b a. Int -> Age a b

data T a = MkT a

{-# NOINLINE foo #-}
foo :: (Int -> Age Bool Char) -> String
foo _ = "bad (RULE should have fired)"

{-# RULES "foo/coerce" [1] foo coerce = "good" #-}

main = putStrLn (foo Age)
