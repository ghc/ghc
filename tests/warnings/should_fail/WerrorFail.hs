{-# OPTIONS_GHC -Wmissing-signatures -Werror=incomplete-patterns #-}
module WerrorFail where

-- this should generate incomplete-patterns warning
foo :: Maybe a -> ()
foo Nothing = ()
