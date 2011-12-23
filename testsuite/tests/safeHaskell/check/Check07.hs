{-# LANGUAGE Trustworthy, NoImplicitPrelude #-}
{-# OPTIONS_GHC -fpackage-trust #-}

-- make sure selective safe imports brings in pkg trust requirements correctly.
-- (e.g only for the imports that are safe ones)
module Check07 ( main' ) where

import safe Check07_A -- no pkg trust reqs
import Check07_B -- base pkg trust req

main' =
    let n = a (b 1)
    in n

