{-# OPTIONS_GHC -Wno-gadt-mono-local-binds #-}
module T10997 where

import T10997a

foo :: Exp a -> String
foo Tru = "True"
