{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fplugin=LatePlugin #-}

module TestLatePlugin (testBinding) where

import GHC.Exts

-- This file is edited by a core plugin at the beginning of the core pipeline so
-- that the value of testBinding becomes 111111. Then, a late plugin edits the
-- binding to set testBinding to 222222. The test then checks that the early
-- binding value is what makes it into the interface file, just to be sure that
-- changes from late plugins do not end up in interface files.

testBinding :: Int
testBinding = -1
