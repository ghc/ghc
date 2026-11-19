{-# LANGUAGE BangPatterns, NoExperimentalDependentTypes #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas -fexpose-all-unfoldings #-}
module NativeOpenBuildControls
  ( module OpenBuildWorker, diverging, strictBuild, otherBuild, slowOne, slowBoth ) where
import OpenBuildWorker

{-# INLINE diverging #-}
diverging :: a -> b -> Shell a b
diverging x y = Shell (loop x) y
  where loop a = loop a

{-# INLINE strictBuild #-}
strictBuild :: a -> b -> Shell a b
strictBuild !x y = Shell x y

{-# INLINE otherBuild #-}
otherBuild :: a -> b -> Shell a b
otherBuild x y = OtherShell x y

{-# NOINLINE delay #-}
delay :: a -> a
delay x = x

{-# INLINE deep #-}
deep :: a -> a
deep x = delay (delay (delay (delay (delay (delay
               (delay (delay (delay (delay (delay (delay x)))))))))))

{-# INLINE slowOne #-}
slowOne :: a -> b -> Shell a b
slowOne x y = Shell (deep x) y

{-# INLINE slowBoth #-}
slowBoth :: a -> b -> Shell a b
slowBoth x y = Shell (deep x) (deep y)
