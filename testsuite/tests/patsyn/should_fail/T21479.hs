{-# Language PatternSynonyms #-}
{-# Language ViewPatterns #-}
{-# Language GADTs #-}

module T21479 where

data T a where
  MkT :: T Int

foo :: () -> T a
foo = foo

pattern T1 <- (foo -> MkT)

