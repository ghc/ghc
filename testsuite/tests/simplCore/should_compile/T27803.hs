{-# OPTIONS_GHC -Wno-inline-rule-shadowing #-}
-- Switch off the (perfectly legitimate) warning

module T27803 where

import GHC.Exts( noinline )

f = Just

-- Make the rule run in phase 0 (last phase)
{-# RULES "fTrue" [0] f True = Just False #-}
wombat x = noinline f True
