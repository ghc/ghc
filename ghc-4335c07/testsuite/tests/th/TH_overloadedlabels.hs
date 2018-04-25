{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TH_overloadedlabels where

import Language.Haskell.TH
import GHC.OverloadedLabels

data T = T { sel :: Int}

instance IsLabel "sel" (T -> Int) where
  fromLabel (T n) = n

x :: Int
x = $(labelE "sel") (T 5)

y :: Int
y = $( [| #sel |] ) (T 6)
