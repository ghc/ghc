{-# LANGUAGE NoImplicitPrelude #-}
module Rec where

-- This one created a black hole in Tidy,
-- when creating the tidied unfolding for foo
foo :: () -> ()
foo = foo
