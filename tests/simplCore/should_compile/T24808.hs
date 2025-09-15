{-# OPTIONS_GHC -O -fno-cse -dno-typeable-binds -dsuppress-uniques #-}
-- -fno-cse avoids things being un-inlined via cse.

-- Tests that we inline through casts when using `inline`.
-- The test works by grepping for myFunction, seeing how often it occurs in rhss

module T24808 where

import GHC.Exts (inline)
import Data.Coerce

-- A type we can coerce
newtype MyMaybe = MyMaybe { getMaybe :: (Maybe Int) }

myFunction :: MyMaybe -> MyMaybe
myFunction (MyMaybe m) = case m of
    Nothing -> MyMaybe Nothing
    -- Make it largeish
    Just n -> MyMaybe $ Just $ succ . succ . succ . succ . succ . succ . succ . succ . succ . succ $ n

-- Inlines as expected
bar :: MyMaybe -> MyMaybe
bar = inline myFunction

-- Doesn't inline - but I think it should.
foo :: MyMaybe -> Maybe Int
foo = (inline (coerce myFunction))
