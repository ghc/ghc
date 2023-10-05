{-# LANGUAGE MagicHash #-}
-- -ffun-to-thunk is essential for the test, but the flag had been deprecated in
-- 9.4 and is off by default. It doesn't hurt to keep the regression test, though,
-- in case we accidentally drop the logic for
-- Note [Protecting the last value argument].
-- {-# OPTIONS_GHC -ffun-to-thunk #-}

module Foo where
import GHC.Exts

f :: Int -> Int#
f x = f (x+1)
