{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitLevelImports #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module SI36_C3 where

import SI36_A

import SI36_B1
import splice SI36_B2
import quote SI36_B3

instance C1 "C3"
instance C2 "C3"
instance C3 "C3"
