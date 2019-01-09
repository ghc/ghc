-- This test verfies that use of DEPRECATED symbols (record fields here)
-- when compiled with -Werror does not result in a compile failure. Only
-- by explicitly requesting -Werror=deprecations will this result in a
-- compile failure.
--
-- See the test OverloadedRecFldsFailDeprecatedFieldsWerrordeprecation which
-- verified that -Werror=deprecations results in a compile failure.
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Werror #-}

import OverloadedRecFldsWithDeprecatedFields_A

data S = MkS { foo :: Bool }

f :: T -> T
f e = e { foo = 3, bar = 3 }

s :: T -> Int
s = foo

main = return ()
