{-# OPTIONS_GHC -Wmissing-signatures -Werror=incomplete-patterns #-}
module Werror01 where

-- this should generate missing-signatures, but not incomplete-patterns
foo () = ()
