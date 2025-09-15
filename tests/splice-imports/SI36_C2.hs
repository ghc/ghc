{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitLevelImports #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module SI36_C2 where

import SI36_A

import SI36_B1
import splice SI36_B2
import quote SI36_B3

instance C1 "C2"
instance C2 "C2"
instance C3 "C2"
