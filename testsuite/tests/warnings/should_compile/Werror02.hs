{-# OPTIONS_GHC -Wmissing-signatures -Werror -Wwarn=missing-signatures #-}
module Werror02 where

-- this should generate missing-signatures warning
foo () = ()
