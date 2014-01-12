{-# LANGUAGE Trustworthy, NoImplicitPrelude #-}
{-# OPTIONS_GHC -fpackage-trust #-}

-- make sure importing a safe-infered module brings in the
-- pkg trust requirements correctly.
module Check06 ( main' ) where

import safe Check06_A

main' =
    let n = mainM 1
    in n

