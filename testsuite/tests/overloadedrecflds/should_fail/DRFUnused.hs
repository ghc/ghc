{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Werror=unused-top-binds #-}

module DRFUnused (S(MkS), x, y) where

import GHC.Records

data S = MkS { foo :: Int }
data T = MkT { foo :: Int }
data U = MkU { foo :: Int }

-- Should count as a use of the foo field belonging to T, but not the others.
x = getField @"foo" (MkT 42)

-- Should count as a use of the foo field belonging to U, but not the others.
y = foo (MkU 42 :: U)
