-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Compat.ReadP
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
--
-- This code was originally in Distribution.Compat.ReadP. Please see that file
-- for provenance. The tests have been integrated into the test framework.
-- Some properties cannot be tested, as they hold over arbitrary ReadP values,
-- and we don't have a good Arbitrary instance (nor Show instance) for ReadP.
--
module UnitTests.Distribution.Compat.ReadP
    ( tests
    -- * Properties
    -- $properties
    ) where

import Data.List
import Distribution.Compat.ReadP
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: [TestTree]
tests =
    [ testProperty "Get Nil" prop_Get_Nil
    , testProperty "Get Cons" prop_Get_Cons
    , testProperty "Look" prop_Look
    , testProperty "Fail" prop_Fail
    , testProperty "Return" prop_Return
    --, testProperty "Bind" prop_Bind
    --, testProperty "Plus" prop_Plus
    --, testProperty "LeftPlus" prop_LeftPlus
    --, testProperty "Gather" prop_Gather
    , testProperty "String Yes" prop_String_Yes
    , testProperty "String Maybe" prop_String_Maybe
    , testProperty "Munch" (prop_Munch evenChar)
    , testProperty "Munch1" (prop_Munch1 evenChar)
    --, testProperty "Choice" prop_Choice
    --, testProperty "ReadS" prop_ReadS
    ]

-- ---------------------------------------------------------------------------
-- QuickCheck properties that hold for the combinators

{- $properties
The following are QuickCheck specifications of what the combinators do.
These can be seen as formal specifications of the behavior of the
combinators.

We use bags to give semantics to the combinators.
-}

type Bag a = [a]

-- Equality on bags does not care about the order of elements.

(=~) :: Ord a => Bag a -> Bag a -> Bool
xs =~ ys = sort xs == sort ys

-- A special equality operator to avoid unresolved overloading
-- when testing the properties.

(=~.) :: Bag (Int,String) -> Bag (Int,String) -> Bool
(=~.) = (=~)

-- Here follow the properties:

prop_Get_Nil :: Bool
prop_Get_Nil =
  readP_to_S get [] =~ []

prop_Get_Cons :: Char -> [Char] -> Bool
prop_Get_Cons c s =
  readP_to_S get (c:s) =~ [(c,s)]

prop_Look :: String -> Bool
prop_Look s =
  readP_to_S look s =~ [(s,s)]

prop_Fail :: String -> Bool
prop_Fail s =
  readP_to_S pfail s =~. []

prop_Return :: Int -> String -> Bool
prop_Return x s =
  readP_to_S (return x) s =~. [(x,s)]

{-
prop_Bind p k s =
  readP_to_S (p >>= k) s =~.
    [ ys''
    | (x,s') <- readP_to_S p s
    , ys''   <- readP_to_S (k (x::Int)) s'
    ]

prop_Plus :: ReadP Int Int -> ReadP Int Int -> String -> Bool
prop_Plus p q s =
  readP_to_S (p +++ q) s =~.
    (readP_to_S p s ++ readP_to_S q s)

prop_LeftPlus :: ReadP Int Int -> ReadP Int Int -> String -> Bool
prop_LeftPlus p q s =
  readP_to_S (p <++ q) s =~.
    (readP_to_S p s +<+ readP_to_S q s)
 where
  [] +<+ ys = ys
  xs +<+ _  = xs

prop_Gather s =
  forAll readPWithoutReadS $ \p ->
    readP_to_S (gather p) s =~
  [ ((pre,x::Int),s')
  | (x,s') <- readP_to_S p s
  , let pre = take (length s - length s') s
  ]
-}

prop_String_Yes :: String -> [Char] -> Bool
prop_String_Yes this s =
  readP_to_S (string this) (this ++ s) =~
    [(this,s)]

prop_String_Maybe :: String -> String -> Bool
prop_String_Maybe this s =
  readP_to_S (string this) s =~
    [(this, drop (length this) s) | this `isPrefixOf` s]

prop_Munch :: (Char -> Bool) -> String -> Bool
prop_Munch p s =
  readP_to_S (munch p) s =~
    [(takeWhile p s, dropWhile p s)]

prop_Munch1 :: (Char -> Bool) -> String -> Bool
prop_Munch1 p s =
  readP_to_S (munch1 p) s =~
    [(res,s') | let (res,s') = (takeWhile p s, dropWhile p s), not (null res)]

{-
prop_Choice :: [ReadP Int Int] -> String -> Bool
prop_Choice ps s =
  readP_to_S (choice ps) s =~.
    readP_to_S (foldr (+++) pfail ps) s

prop_ReadS :: ReadS Int -> String -> Bool
prop_ReadS r s =
  readP_to_S (readS_to_P r) s =~. r s
-}

evenChar :: Char -> Bool
evenChar = even . fromEnum
