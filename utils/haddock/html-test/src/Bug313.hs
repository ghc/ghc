{-# LANGUAGE Haskell2010 #-}
-- | The first list is incorrectly numbered as 1. 2. 1.; the second example
-- renders fine (1. 2. 3.).
--
-- See https://github.com/haskell/haddock/issues/313
module Bug313 where

{- |
Some text.

1. Item 1

2. Item 2

    > Some code

3. Item 3

Some more text.
-}
a :: a
a = undefined

{- |
Some text.

1. Item 1

2. Item 2

    > Some code

3. Item 3

-}
-- | Some more text.
b :: a
b = undefined
