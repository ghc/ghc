{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GADTs #-}
module ExtraConstraints2 where

foo :: _ => String
foo = "x"

-- No extra constraints
