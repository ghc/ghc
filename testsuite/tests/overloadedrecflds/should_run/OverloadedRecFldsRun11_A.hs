{-# LANGUAGE OverloadedRecordFields #-}

module OverloadedRecFldsRun11_A where

import OverloadedRecFldsRun11_B

data T = MkT { foo :: Int }

baz r = bar r
