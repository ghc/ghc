{-# LANGUAGE NoIncomplete #-}
{-# OPTIONS_GHC -fdefer-incomplete-patterns #-}

module A where

-- this should generate incomplete-patterns warning
foo :: Maybe a -> ()
foo Nothing = ()

data S = C1 Int | C2 Int

-- incomplete pattern
sInt s = case s of
           C1 i -> i
