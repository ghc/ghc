-- In this test we do -ddump-simpl, and grep for 'patError.
-- We expect *not* to see patError in the output
-- because the branch is inaccessible.  
-- In GHC 7.0 and earlier, we did get a patError in the output program.

{-# LANGUAGE TypeFamilies, GADTs #-}
module NoMatch where

data Tag
data TagExtra

--------

data Foo a where
  Foo :: String -> Foo a
  FooExtra :: Int -> Foo TagExtra

-- The cmm code for fooName does not match against 'FooExtra'
fooName :: Foo Tag -> String
fooName (Foo s) = s

--------

data Bar a where
  Bar :: String -> Bar a
  BarExtra :: a ~ TagExtra => Int -> Bar a

-- The cmm code for barName will try to pattern-match against 'BarExtra'
barName :: Bar Tag -> String
barName (Bar s) = s


