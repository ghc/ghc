{-# LANGUAGE TypeApplications #-}

module T12045b where

import Data.Kind

x :: Int @Type
x = 5
