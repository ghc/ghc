{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

{-
 N.B. the contract of '%mulmayoflo' is a bit weak: "Return non-zero if there is
 any possibility that the signed multiply of a and b might overflow. Return zero
 only if you are absolutely sure that it won't overflow. If in doubt, return
 non-zero." (Stg.h)

This test verifies the a stronger contract: It's expected that there are no
false positives. This requirement is e.g. met by code generation backends which
execute the multiplication to check for overflow.
-}

module Main where

import GHC.Exts

-- The argument and return types are unimportant: They're only used to force
-- evaluation, but carry no information.
foreign import prim "runCmmzh" runCmmzh# :: Word# -> Word#

main :: IO ()
main = print . show $ W# (runCmmzh# 42##)
