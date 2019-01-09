-- This test verifies that use of DEPRECATED symbols (record fields here)
-- when compiled with -Werror=deprecations results in a compile failure.
--
-- This test differs slightly from the overloadedrecfldsfail12 test in that this
-- marks the fields with DEPRECATED instead of WARNING and is thus unaffected by
-- -Werror by default (which leaves use of DEPRECATED symbols as a warning).
--
-- See the test OverloadedRecFldsWithDeprecatedFields which verifies that when
-- only -Werror is set the code still compiles (but still issues warnings).
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Werror=deprecations #-}

import OverloadedRecFldsFailDeprecatedFieldsWerrordeprecation_A

data S = MkS { foo :: Bool }

f :: T -> T
f e = e { foo = 3, bar = 3 }

s :: T -> Int
s = foo

main = return ()
