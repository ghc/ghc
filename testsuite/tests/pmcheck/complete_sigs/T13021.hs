{-# OPTIONS_GHC -Woverlapping-patterns -Wincomplete-patterns #-}

module Lib where

data T = A | B | C

{-# COMPLETE B #-}

foo :: T -> ()
foo A = ()
foo B = ()
