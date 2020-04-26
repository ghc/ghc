{-# OPTIONS_GHC -O2 -fforce-recomp #-}
module DataConWrapperCpr where

data Foo = Foo !Int

-- Should have the CPR property! Hence the wrapper $WFoo must have it.
foo :: Int -> Foo
foo = Foo
